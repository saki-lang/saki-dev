import util.Located

import scala.annotation.targetName

package object core {

  /// A [de Bruijn index][de-bruijn-index] in the current [environment].
  ///
  /// De Bruijn indices describe an occurrence of a variable in terms of the
  /// number of binders between the occurrence and its associated binder.
  /// For example:
  ///
  /// | Representation    | Example (S combinator)  |
  /// | ----------------- | ----------------------- |
  /// | Named             | `λx. λy. λz. x z (y z)` |
  /// | De Bruijn indices | `λ_. λ_. λ_. 2 0 (1 0)` |
  ///
  /// This is a helpful representation because it allows us to easily compare
  /// terms for equivalence based on their binding structure without maintaining a
  /// list of name substitutions. For example we want `λx. x` to be the same as
  /// `λy. y`. With de Bruijn indices these would both be described as `λ 0`.
  ///
  /// [environment]: `Env`
  /// [de-bruijn-index]: https://en.wikipedia.org/wiki/De_Bruijn_index
  opaque type VarIndex = Int

  /// A de Bruijn level in the current [environment].
  ///
  /// This describes an occurrence of a variable by counting the binders inwards
  /// from the top of the term until the occurrence is reached. For example:
  ///
  /// | Representation    | Example (S combinator)  |
  /// | ----------------- | ----------------------- |
  /// | Named             | `λx. λy. λz. x z (y z)` |
  /// | De Bruijn levels  | `λ_. λ_. λ_. 0 2 (1 2)` |
  ///
  /// Levels are used in [values][semantics::Value] because they are not context-
  /// dependent (this is in contrast to [indices][LocalIndex]). Because of this,
  /// we're able to sidestep the need for expensive variable shifting in the
  /// semantics. More information can be found in Soham Chowdhury's blog post,
  /// “[Real-world type theory I: untyped normalisation by evaluation for λ-calculus][untyped-nbe-for-lc]”.
  ///
  /// [environment]: `Env`
  /// [untyped-nbe-for-lc]: https://colimit.net/posts/normalisation-by-evaluation/
  opaque type VarLevel = Int

  enum LiteralValue {
    case LiteralBool(value: Boolean)
    case LiteralInt(value: Long)
    case LiteralFloat(value: Double)
    case LiteralChar(value: Char)
    case LiteralString(value: String)
  }

  type Term = Located[TermData]

  enum TermData {
    case Global(name: String)
    case Local(index: VarIndex)
    case Annotated(term: Term, ty: Term)
    case TypeOfType

    /**
     * aka: pi type, dependent product type.
     */
    case FuncType(paramName: Option[String], paramType: Term, returnType: Term)
    /**
     * aka: lambda abstraction, anonymous function.
     */
    case FuncTerm(paramName: String, param: Term)
    /**
     * aka: function application.
     */
    case FuncElim(func: Term, arg: Term)

    case RecordType(fieldTypes: Map[String, Term])
    case RecordTerm(fields: Map[String, Term])
    /**
     * aka: field lookup.
     */
    case RecordProj(record: Term, field: String)
    case Literal(value: LiteralValue)
    case List(terms: Vector[Term])

    def located: Term = Located(this)
  }

  case class Env[Entry](entries: Vector[Entry]) {
    @targetName("push")
    def :+(entry: Entry): Env[Entry] = Env(entries :+ entry)
    def pop: Env[Entry] = Env(entries.init)
    def apply(index: VarIndex): Option[Entry] = entries.lift(index)
    @targetName("getLevel")
    def ++(other: Env[Entry]): Env[Entry] = Env(entries ++ other.entries)
    def size: Int = entries.size
    def indexToLevel(index: VarIndex): VarLevel = {
      val result = size - index - 1
      if (result < 0) {
        throw new IndexOutOfBoundsException(s"Index $index is out of bounds for environment of size $size")
      } else result
    }
  }

  object Globals {
    val typeOfType = TermData.TypeOfType.located
    val typeBool = TermData.Global("Bool").located
    val termTrue = TermData.Literal(LiteralValue.LiteralBool(true)).located
    val termFalse = TermData.Literal(LiteralValue.LiteralBool(false)).located
    val typeInt = TermData.Global("Int").located
    val typeFloat = TermData.Global("Float").located
    val typeChar = TermData.Global("Char").located
    val typeString = TermData.Global("String").located

    val entries: Map[String, (Term, Option[Term])] = Map(
      "'Type" -> (typeOfType, Some(typeOfType)),
      "Bool" -> (typeBool, None),
      "true" -> (termTrue, Some(typeBool)),
      "false" -> (termFalse, Some(typeBool)),
      "Int" -> (typeInt, None),
      "Float" -> (typeFloat, None),
      "Char" -> (typeChar, None),
      "String" -> (typeString, None),
    )

    def apply(name: String): Option[(Term, Option[Term])] = entries.get(name)
  }

}
