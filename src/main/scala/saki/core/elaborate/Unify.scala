package saki.core.elaborate

import saki.core.syntax.Term

private[core] object Unify {

  // TODO: Do we need to consider subtypes here?
  infix def unify(lhs: Term, rhs: Term): Boolean = (lhs, rhs) match {

    // Trivial cases
    case (Term.Universe, Term.Universe) => true
    case (Term.Primitive(lit1), Term.Primitive(lit2)) => lit1 == lit2
    case (Term.PrimitiveType(ty1), Term.PrimitiveType(ty2)) => ty1 == ty2
    case (Term.Variable(ref1), Term.Variable(ref2)) => ref1 == ref2

    // Lambda
    case (Term.Lambda(param1, body1), Term.Lambda(param2, body2)) => body1 unify body2.subst(param2, Term.Variable(param1))
    case (Term.Lambda(param, body), _) => rhs.etaUnify(Term.Lambda(param, body))
    case (_, Term.Lambda(param, body)) => lhs.etaUnify(Term.Lambda(param, body))

    // Dependent type
    case (Term.Pi(param1, codomain1), Term.Pi(param2, codomain2)) =>
      (param1.`type` unify param2.`type`) &&
      (codomain1 unify codomain2.subst(param2.ident, Term.Variable(param1.ident)))
    case (Term.Sigma(param1, codomain1), Term.Sigma(param2, codomain2)) =>
      (param1.`type` unify param2.`type`) &&
      (codomain1 unify codomain2.subst(param2.ident, Term.Variable(param1.ident)))

    // Function call
    case (Term.FunctionInvoke(fn1, args1), Term.FunctionInvoke(fn2, args2)) =>
      (fn1 == fn2) && (args1 zip args2).forall((a, b) => a unify b)
    case (Term.InductiveType(ind1, args1), Term.InductiveType(ind2, args2)) =>
      (ind1 == ind2) && (args1 zip args2).forall((a, b) => a unify b)
    case (Term.InductiveVariant(cons1, consArgs1, indArgs1), Term.InductiveVariant(cons2, consArgs2, indArgs2)) =>
      (cons1 == cons2) && (consArgs1 zip consArgs2).forall((a, b) => a unify b) &&
      (indArgs1 zip indArgs2).forall((a, b) => a unify b)
    case (Term.Apply(fn1, arg1), Term.Apply(fn2, arg2)) =>
      (fn1 unify fn2) && (arg1 unify arg2)

    // Projection
    case (Term.Projection(record1, field1), Term.Projection(record2, field2)) =>
      (record1 unify record2) && (field1 == field2)

    // Record
    case (Term.Record(fields1), Term.Record(fields2)) =>
      fields1.size == fields2.size &&
      fields1.forall((ident, value) => fields2.contains(ident) && (value unify fields2(ident)))
    case (Term.RecordType(fields1), Term.RecordType(fields2)) =>
      fields1.size == fields2.size &&
      fields1.forall((ident, ty) => fields2.contains(ident) && (ty unify fields2(ident)))

    case _ => false
  }
}
