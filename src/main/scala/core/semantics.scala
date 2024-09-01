package core

import scala.annotation.tailrec

enum Value {
  /// A computation that is stuck on a [head value][Head] that cannot be
  /// reduced further in the current scope. We maintain a 'spine' of
  /// [eliminators][Elim], that can be applied if the head becomes unstuck
  /// later on.
  ///
  /// This is more commonly known as a 'neutral value' in the type theory
  /// literature.
  case Stuck(head: Head, spine: Vector[Elim])

  /// A computation that was previously stuck a on [head value][Head], but is
  /// now unstuck due to its definition now being known.
  ///
  /// This is sometimes called a 'glued value'.
  ///
  /// It's useful to keep the head and spine of eliminations around from the
  /// [stuck value][Value::Stuck] in order to reduce the size-blowup that
  /// can result from deeply-normalizing terms. This can be useful for:
  ///
  /// - improving the performance of conversion checking
  /// - making it easier to understand read-back types in diagnostic messages
  ///
  /// See the following for more information:
  ///
  /// - [AndrasKovacs/smalltt](https://github.com/AndrasKovacs/smalltt/)
  /// - [ollef/sixty](https://github.com/ollef/sixty/)
  /// - [Non-deterministic normalization-by-evaluation](https://gist.github.com/AndrasKovacs/a0e0938113b193d6b9c1c0620d853784)
  /// - [Example of the blowup that can occur when reading back values](https://twitter.com/brendanzab/status/1283278258818002944)
  case Unstuck(head: Head, spine: Vector[Elim], value: LazyValue)

  /// The type of types.
  case TypeOfType

  /// Function types.
  ///
  /// Also known as: pi type, dependent product type.
  case FunctionType(inputNameHint: Option[String], inputType: Value, outputClosure: FunctionClosure)

  /// Function terms.
  ///
  /// Also known as: lambda abstraction, anonymous function.
  case FunctionTerm(inputName: String, outputClosure: FunctionClosure)

  /// Record types.
  case RecordType(labels: Vector[String], closure: RecordClosure)

  /// Record terms.
  case RecordTerm(labels: Vector[String], closure: RecordClosure)

  /// List terms.
  case ListTerm(entries: Vector[Value])

  /// Constants.
  case Literal(value: LiteralValue)

  /// Error sentinel.
  case Error

  def force: Value = this match {
    case Unstuck(_, _, lazyValue) => lazyValue.force
    case other => other
  }

  def tryGlobal: Option[(String, Vector[Elim])] = this match {
    case Stuck(Head.Global(name), elims) => Some(name, elims)
    case _ => None
  }
}

object Value {
  /// Create a global variable.
  def global(name: String, elims: Vector[Elim] = Vector.empty): Value =
    Value.Stuck(Head.Global(name), elims)

  /// Create a variable.
  def variable(level: VarLevel, elims: Vector[Elim] = Vector.empty): Value =
    Value.Stuck(Head.Var(level), elims)
}

enum Head {
  case Global(name: String)
  case Var(level: VarLevel)
}

enum Elim {
  case Function(input: LazyValue)
  case Record(label: String)
}

case class FunctionClosure(values: Env[Value], term: Term) {
  def apply(input: Value): Value = {
    val newValues = values :+ input
    eval(newValues, term)
  }
}

case class RecordClosure(values: Env[Value], entries: Vector[Term]) {
  def forEachEntry(onEntry: Value => Value): Unit = {
    var newValues: Env[Value] = values
    entries.foreach { entryTerm =>
      val entryValue: Value = eval(newValues, entryTerm)
      newValues = newValues :+ onEntry(entryValue)
    }
  }
}

enum LazyInit {
  case EvalTerm(values: Env[Value], term: Term)
  case ApplyElim(head: LazyValue, elim: Elim)
}

class LazyValue(value: Value | LazyInit) {
  lazy val force: Value = {
    value match {
      case LazyInit.EvalTerm(values, term) => eval(values, term)
      case LazyInit.ApplyElim(head, elim) => elim match {
        case Elim.Record(label) => recordElim(head.force, label)
        case Elim.Function(input) => functionElim(head.force, input)
      }
      case value: Value => value
    }
  }

  def applyElim(elim: Elim): LazyValue = new LazyValue(LazyInit.ApplyElim(this, elim))
}

object LazyValue {
  def elim(head: LazyValue, elim: Elim): LazyValue = new LazyValue(LazyInit.ApplyElim(head, elim))
  def eval(values: Env[Value], term: Term): LazyValue = new LazyValue(LazyInit.EvalTerm(values, term))
}

def normalize(values: Env[Value], term: Term): Term = {
  val value = eval(values, term)
  readBack(values.size, Unfold.Always, value)
}

def eval(env: Env[Value], term: Term): Value = {
  term.data match {
    case TermData.Global(name) => Globals(name) match {
      case Some((_, Some(term))) => {
        val head = Head.Global(name)
        val value = LazyValue.eval(env, term)
        Value.Unstuck(head, Vector.empty, value)
      }
      case Some((_, None)) | None => {
        val head = Head.Global(name)
        Value.Stuck(head, Vector.empty)
      }
    }

    case TermData.Local(index) => env(index) match {
      case Some(value) => value
      // case Some(value) => Value.Unstuck(Head.Var(env.indexToLevel(index)), Vector.empty, LazyValue(value))
      case None => Value.Stuck(Head.Var(env.indexToLevel(index)), Vector.empty)
    }

    case TermData.Annotated(term, _) => eval(env, term)

    case TermData.TypeOfType => Value.TypeOfType

    case TermData.RecordType(fieldTypes) => {
      val labels = fieldTypes.keys.toVector
      val closure = RecordClosure(env, fieldTypes.values.toVector)
      Value.RecordType(labels, closure)
    }

    case TermData.RecordTerm(fields) => {
      val labels = fields.keys.toVector
      val closure = RecordClosure(env, fields.values.toVector)
      Value.RecordTerm(labels, closure)
    }

    case TermData.RecordProj(record, label) => recordElim(eval(env, record), label)

    case TermData.FuncType(paramName, paramType, returnType) => {
      Value.FunctionType(paramName, eval(env, paramType), FunctionClosure(env, returnType))
    }

    case TermData.FuncTerm(paramName, body) => {
      Value.FunctionTerm(paramName, FunctionClosure(env, body))
    }

    case TermData.FuncElim(func, arg) => {
      val funcValue = eval(env, func)
      val argValue = LazyValue.eval(env, arg)
      functionElim(funcValue, argValue)
    }

    case TermData.List(terms) => Value.ListTerm(terms.map(eval(env, _)))

    case TermData.Literal(value) => Value.Literal(value)
  }
}

def recordElimType(
  termValues: Env[Value], headTerm: Term, headType: Value, label: String
): Option[Value] = {
  headType.force match {
    case Value.RecordType(labels, closure) => {
      val headValue = eval(termValues, headTerm)
      var typeValues = closure.values
      labels.zip(closure.entries).foreach { case (entryLabel, entryType) =>
        if (entryLabel == label) {
          Some(eval(typeValues, entryType))
        } else {
          val entryValue = recordElim(headValue, entryLabel)
          typeValues :+= entryValue
        }
      }
      return None
    }

    case Value.Error => Some(Value.Error)

    case _ => None
  }
}

// TODO: this should be rewrite (because scala do not support mutable parameter)
def recordElim(headValue: Value, label: String): Value = ???
//{
//  headValue match {
//    case Value.Stuck(_, spine) => Value.Stuck(headValue.asInstanceOf[Head], spine :+ Elim.Record(label))
//    case Value.Unstuck(_, spine, lazyValue) =>
//      lazyValue.force
//      Value.Unstuck(headValue.asInstanceOf[Head], spine :+ Elim.Record(label), LazyValue.elim(lazyValue, Elim.Record(label)))
//    case Value.RecordTerm(labels, closure) => {
//      val labelIterator = labels.iterator
//      var values = closure.values
//      closure.entries.foreach { entryValue =>
//        val evalValue = eval(values, entryValue)
//        if (labelIterator.hasNext && labelIterator.next() == label) return evalValue
//        else values = values :+ evalValue
//      }
//      Value.Error
//    }
//    case _ => Value.Error
//  }
//}

// TODO: this should be rewrite (because scala do not support mutable parameter)
def functionElim(headValue: Value, input: LazyValue): Value = ???
//{
//  headValue match {
//    case Value.Stuck(_, spine) =>
//      Value.Stuck(headValue.asInstanceOf[Head], spine :+ Elim.Function(input))
//    case Value.Unstuck(_, spine, lazyValue) =>
//      lazyValue.force
//      Value.Unstuck(headValue.asInstanceOf[Head], spine :+ Elim.Function(input), LazyValue.elim(lazyValue, Elim.Function(input)))
//    case Value.FunctionTerm(_, outputClosure) =>
//      outputClosure.apply(input.force)
//    case _ => Value.Error
//  }
//}

enum Unfold {
  case Never, Always
}

def readBack(size: Int, unfold: Unfold, value: Value): Term = ???
//{
//  value match {
//    case Value.Stuck(head, spine) =>
//      readBackStuck(size, unfold, head, spine)
//    case Value.Unstuck(head, spine, lazyValue) =>
//      unfold match {
//        case Unfold.Never => readBackStuck(size, unfold, head, spine)
//        case Unfold.Always => readBack(size, unfold, lazyValue.force)
//      }
//    case Value.TypeOfType => Term.generated(TermData.TypeType)
//    case Value.FunctionType(inputNameHint, inputType, outputClosure) =>
//      val varValue = Value.variable(size.nextLevel)
//      val inputTerm = readBack(size, unfold, inputType)
//      val outputTerm = readBack(size.nextSize, unfold, outputClosure.apply(varValue))
//      Term.generated(TermData.FunctionType(inputNameHint, inputTerm, outputTerm))
//    case Value.FunctionTerm(inputName, outputClosure) =>
//      val varValue = Value.variable(size.nextLevel)
//      val outputTerm = readBack(size.nextSize, unfold, outputClosure.apply(varValue))
//      Term.generated(TermData.FunctionTerm(inputName, outputTerm))
//    case Value.RecordType(labels, closure) =>
//      val types = closure.entries.map { entry =>
//        val entryType = readBack(size, unfold, eval(closure.values, entry))
//        Value.variable(size.nextLevel)
//        entryType
//      }
//      Term.generated(TermData.RecordType(labels, types))
//    case Value.RecordTerm(labels, closure) =>
//      val terms = closure.entries.map { entry =>
//        val entryTerm = readBack(size, unfold, eval(closure.values, entry))
//        Value.variable(size.nextLevel)
//        entryTerm
//      }
//      Term.generated(TermData.RecordTerm(labels, terms))
//    case Value.ArrayTerm(entries) =>
//      Term.generated(TermData.ArrayTerm(entries.map(readBack(size, unfold, _))))
//    case Value.ListTerm(entries) =>
//      Term.generated(TermData.List(entries.map(readBack(size, unfold, _))))
//    case Value.Literal(constant) => Term.generated(TermData.from(constant))
//    case Value.Error => Term.generated(TermData.Error)
//  }
//}

//def readBackStuck(size: EnvSize, unfold: Unfold, head: Head, spine: Vector[Elim]): Term = {
//  val base = head match {
//    case Head.Global(name) => Term.generated(TermData.Global(name))
//    case Head.Var(level) => Term.generated(TermData.Var(size.levelToIndex(level).get))
//  }
//
//  spine.foldLeft(base) { (acc, elim) =>
//    elim match {
//      case Elim.Function(input) =>
//        val inputTerm = readBack(size, unfold, input.force)
//        Term.generated(TermData.FunctionElim(acc, inputTerm))
//      case Elim.Record(label) =>
//        Term.generated(TermData.RecordElim(acc, label))
//    }
//  }
//}
//
//def isEqualStuck(size: EnvSize, head0: Head, spine0: Vector[Elim], head1: Head, spine1: Vector[Elim]): Boolean = {
//  if head0 != head1 || spine0.length != spine1.length then false
//  else {
//    spine0.zip(spine1).forall {
//      case (Elim.Function(input0), Elim.Function(input1)) =>
//        isEqual(size, input0.force, input1.force)
//      case (Elim.Record(label0), Elim.Record(label1)) if label0 == label1 =>
//        true
//      case _ =>
//        false
//    }
//  }
//}
//
//def isEqualFunctionClosure(size: EnvSize, closure0: FunctionClosure, closure1: FunctionClosure): Boolean = {
//  val varValue = Value.variable(size.nextLevel)
//  isEqual(size.nextSize, closure0.apply(varValue), closure1.apply(varValue))
//}
//
//def isEqualRecordClosure(size: EnvSize, closure0: RecordClosure, closure1: RecordClosure): Boolean = {
//  if closure0.entries.length != closure1.entries.length then false
//  else {
//    val zipped = closure0.entries.zip(closure1.entries)
//    zipped.forall {
//      case (entry0, entry1) =>
//        val eval0 = eval(closure0.values, entry0)
//        val eval1 = eval(closure1.values, entry1)
//        val varValue = Value.variable(size.nextLevel)
//        isEqual(size, eval0, eval1)
//    }
//  }
//}
//
//def isEqual(size: EnvSize, value0: Value, value1: Value): Boolean = {
//  (value0, value1) match {
//    case (Value.Stuck(head0, spine0), Value.Stuck(head1, spine1)) =>
//      isEqualStuck(size, head0, spine0, head1, spine1)
//    case (Value.Unstuck(head0, spine0, lazyValue0), Value.Unstuck(head1, spine1, lazyValue1)) =>
//      isEqualStuck(size, head0, spine0, head1, spine1) || {
//        isEqual(size, lazyValue0.force, lazyValue1.force)
//      }
//    case (Value.TypeOfType, Value.TypeOfType) => true
//    case (Value.FunctionType(_, inputType0, closure0), Value.FunctionType(_, inputType1, closure1)) =>
//      isEqual(size, inputType0, inputType1) && isEqualFunctionClosure(size, closure0, closure1)
//    case (Value.FunctionTerm(_, closure0), Value.FunctionTerm(_, closure1)) =>
//      isEqualFunctionClosure(size, closure0, closure1)
//    case (Value.RecordType(labels0, closure0), Value.RecordType(labels1, closure1)) =>
//      labels0 == labels1 && isEqualRecordClosure(size, closure0, closure1)
//    case (Value.RecordTerm(labels0, closure0), Value.RecordTerm(labels1, closure1)) =>
//      labels0 == labels1 && isEqualRecordClosure(size, closure0, closure1)
//        case (Value.ListTerm(entries0), Value.ListTerm(entries1)) =>
//      entries0.length == entries1.length && entries0.zip(entries1).forall(isEqual(size, _, _))
//    case (Value.Literal(constant0), Value.Literal(constant1)) =>
//      constant0 == constant1
//    case (Value.Error, _) | (_, Value.Error) => true
//    case _ => false
//  }
//}
