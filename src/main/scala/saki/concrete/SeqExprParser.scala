package saki.concrete

import scala.annotation.targetName
import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

object SeqExprParser:

  enum Token {
    case LeftParenthesis
    case RightParenthesis
    case Atom[T](value: T)
    case Op(op: Operator)

    override def toString: String = this match {
      case LeftParenthesis => "("
      case RightParenthesis => ")"
      case Atom(value) => value.toString
      case Op(op) => op.symbol
    }

    def isBinaryOperator: Boolean = this match {
      case Op(op) => op.isInstanceOf[Operator.Binary]
      case _ => false
    }

    def isUnaryOperator: Boolean = this match {
      case Op(op) => op.isInstanceOf[Operator.Unary]
      case _ => false
    }

    def asBinaryOperator: Operator.Binary = this match {
      case Op(op) => op.asInstanceOf[Operator.Binary]
      case _ => throw new IllegalArgumentException("Expected a binary operator")
    }

    def asUnaryOperator: Operator.Unary = this match {
      case Op(op) => op.asInstanceOf[Operator.Unary]
      case _ => throw new IllegalArgumentException("Expected a unary operator")
    }
  }

  enum Associativity {
    case Left
    case Right
  }

  enum UnaryType {
    case Prefix
    case Postfix
  }

  enum Operator {
    case Unary(override val symbol: String, kind: UnaryType)
    // symbol and partial order of the binary op
    case Binary(
      override val symbol: String, associativity: Associativity,
      tighterThan: Set[Binary], sameAs: Set[Binary], looserThan: Set[Binary],
    )

    val symbol: String

    override def equals(obj: Any): Boolean = obj match {
      case that: Operator.Unary => this.symbol == that.symbol
      case that: Operator.Binary => this.symbol == that.symbol
      case _ => false
    }

    override def toString: String = symbol
  }

  extension (self: Operator.Binary) {
    def isLeftAssociative: Boolean = self.associativity == Associativity.Left
    def isRightAssociative: Boolean = self.associativity == Associativity.Right
  }

  extension (self: Operator.Unary) {
    def isPrefix: Boolean = self.kind == UnaryType.Prefix
    def isPostfix: Boolean = self.kind == UnaryType.Postfix
  }

  enum Expr {
    case Atom[T](value: T)
    case UnaryExpr(op: Operator.Unary, expr: Expr)
    case BinaryExpr(op: Operator.Binary, lhs: Expr, rhs: Expr)

    override def toString: String = this match {
      case Atom(value) => value.toString
      case UnaryExpr(op, expr) => op.kind match {
        case UnaryType.Prefix => s"${op.symbol}${expr.toString}"
        case UnaryType.Postfix => s"${expr.toString}${op.symbol}"
      }
      case BinaryExpr(op, lhs, rhs) => s"(${lhs.toString} ${op.symbol} ${rhs.toString})"
    }
  }

  class OperatorExpressionParser[T](tokens: Seq[Token], binaryOperators: Set[Operator.Binary]) {

    private var currentTokenIndex: Int = 0

    private def currentToken: Token = tokens(currentTokenIndex)

    private def consumeToken(): Token = {
      val token = currentToken
      currentTokenIndex += 1
      token
    }

    private def isEof: Boolean = currentTokenIndex >= tokens.size

    private val precedenceGraph = OperatorExpressionParser.checkedPartialOrderGraph(binaryOperators)

    private val lowestPrecedenceOperators = precedenceGraph.filter {
      (_, tighters) => tighters.isEmpty
    }.keySet

    private def parsePrimary(): Expr = {
      var expr = currentToken match {
        case token: Token.Op =>
          this.consumeToken()
          token.op match {
            case unary: Operator.Unary => Expr.UnaryExpr(unary, parsePrimary())
            case _ => throw new IllegalArgumentException("Expected a unary operator")
          }
        case Token.LeftParenthesis =>
          this.consumeToken()
          val expression = parse()
          if this.consumeToken() != Token.RightParenthesis then {
            throw new IllegalArgumentException("Expected ')'")
          }
          expression
        case token@Token.Atom(value) =>
          this.consumeToken()
          Expr.Atom(token.value)
        case _ => throw new IllegalArgumentException(s"Unexpected token: ${currentToken.toString}")
      }
      // Check if the next token is a postfix unary operator
      while !this.isEof && currentToken.isUnaryOperator && currentToken.asUnaryOperator.isPostfix do {
        val postfixOp = currentToken.asUnaryOperator
        this.consumeToken()
        expr = Expr.UnaryExpr(postfixOp, expr)
      }
      return expr
    }

    private def parseExpression(leftPart: Expr, minPrecedenceSet: Set[Operator.Binary]): Expr = {
      var lhs: Expr = leftPart
      if currentTokenIndex >= tokens.size then return lhs
      var lookahead = currentToken
      case object BreakException extends Exception
      boundary {
        // while lookahead is a binary operator whose precedence is >= min_precedence
        while lookahead.isBinaryOperator do {
          val binaryOp = lookahead.asBinaryOperator
          if binaryOp < minPrecedenceSet then return lhs
          if lookahead == Token.RightParenthesis || this.isEof then return lhs
          this.consumeToken()
          var rhs = parsePrimary()
          if currentTokenIndex < tokens.size then {
            lookahead = currentToken
            // while lookahead is a binary operator whose precedence is greater
            // than op's, or a right-associative operator whose precedence is equal to op's
            try { // `try` here is intended to create a breakable scope
              while lookahead.isBinaryOperator do {
                val lookaheadBinaryOp = lookahead.asBinaryOperator
                if {
                  lookaheadBinaryOp > binaryOp ||
                    (lookaheadBinaryOp.isRightAssociative && binaryOp <=> lookaheadBinaryOp)
                } then {
                  rhs = parseExpression(rhs, Set(lookaheadBinaryOp))
                  if this.isEof then break(Expr.BinaryExpr(binaryOp, lhs, rhs))
                  lookahead = currentToken
                } else throw BreakException
              }
            } catch {
              case BreakException => () // Break the loop
              case e: Exception => throw e
            }
          }
          lhs = Expr.BinaryExpr(binaryOp, lhs, rhs)
        }
        break(lhs)
      }
    }

    private def parse(): Expr = parseExpression(parsePrimary(), lowestPrecedenceOperators)

    lazy val expressions: Seq[Expr] = {
      var expressions = Seq[Expr]()
      while !this.isEof do {
        expressions = expressions :+ parse()
      }
      expressions
    }

    private enum Precedence {
      case Tighter, Looser, Equal
    }

    private def comparePrecedence(
      op1: Operator.Binary, op2: Operator.Binary,
      operators: Set[Operator.Binary],
    ): Precedence = {

      // Direct relationship
      if op1 == op2 then return Precedence.Equal
      else if op1.tighterThan.contains(op2) || op2.looserThan.contains(op1) then {
        return Precedence.Tighter
      } else if op1.looserThan.contains(op2) || op2.tighterThan.contains(op1) then {
        return Precedence.Looser
      }

      // Indirect equal relationship
      def dfsEquals(op: Operator.Binary, visited: Set[Operator.Binary]): Boolean = {
        if op == op2 || op.sameAs.contains(op2) || op2.sameAs.contains(op1) then {
          if visited.contains(op) then return false
        }
        op.sameAs.exists(nextOp => dfsEquals(nextOp, visited + op))
      }

      if (dfsEquals(op1, Set())) return Precedence.Equal

      // Indirect tighter relationship
      def tighter(op1: Operator.Binary, op2: Operator.Binary): Boolean = {
        // dfs use graph
        def dfs(op: Operator.Binary, visited: Set[Operator.Binary]): Boolean = {
          if (op == op2) return true
          if (visited.contains(op)) return false
          precedenceGraph.getOrElse(op, Set()).exists(nextOp => dfs(nextOp, visited + op))
        }

        dfs(op1, Set())
      }

      if (tighter(op1, op2)) Precedence.Tighter
      else if (tighter(op2, op1)) Precedence.Looser
      else throw new IllegalArgumentException(s"Undefined precedence between operators: $op1 and $op2")
    }

    extension (self: Operator.Binary) {

      @targetName("looserThan")
      def <(that: Set[Operator.Binary]): Boolean = that.forall(self < _)
      @targetName("looserThan")
      def <(that: Operator.Binary): Boolean = {
        val compareResult = comparePrecedence(self, that, binaryOperators)
        compareResult == Precedence.Looser
      }

      @targetName("looserOrEqual")
      def <=(that: Set[Operator.Binary]): Boolean = that.forall(self <= _)
      @targetName("looserOrEqual")
      def <=(that: Operator.Binary): Boolean = {
        val compareResult = comparePrecedence(self, that, binaryOperators)
        compareResult == Precedence.Looser || compareResult == Precedence.Equal
      }

      @targetName("tighterThan")
      def >(that: Set[Operator.Binary]): Boolean = that.forall(self > _)
      @targetName("tighterThan")
      def >(that: Operator.Binary): Boolean = {
        val compareResult = comparePrecedence(self, that, binaryOperators)
        compareResult == Precedence.Tighter
      }

      @targetName("tighterOrEqual")
      def >=(that: Set[Operator.Binary]): Boolean = that.forall(self >= _)
      @targetName("tighterOrEqual")
      def >=(that: Operator.Binary): Boolean = {
        val compareResult = comparePrecedence(self, that, binaryOperators)
        compareResult == Precedence.Tighter || compareResult == Precedence.Equal
      }

      @targetName("samePrecedence")
      def <=>(that: Operator.Binary): Boolean = {
        comparePrecedence(self, that, binaryOperators) == Precedence.Equal
      }
    }
  }

  object OperatorExpressionParser {
    private def partialOrderGraph(operators: Set[Operator.Binary]): Map[Operator.Binary, Set[Operator.Binary]] = {
      // Graph adjacency list
      val graph = mutable.Map[Operator.Binary, Set[Operator.Binary]]()
      // Building the directed graph based on tighter and looser relationships using dfs
      for (op <- operators) {
        if (!graph.contains(op)) graph.update(op, Set())
        for (tighterOp <- op.tighterThan) {
          assert(operators.contains(tighterOp), "Tighter operator not found in the set")
          assert(tighterOp != op, "Operator cannot be tighter than itself")
          assert(!op.looserThan.contains(tighterOp), "Operator cannot be tighter and looser at the same time")
          graph.update(op, graph.getOrElse(op, Set()) + tighterOp)
        }
        for (looserOp <- op.looserThan) {
          assert(operators.contains(looserOp), "Looser operator not found in the set")
          assert(looserOp != op, "Operator cannot be looser than itself")
          assert(!op.tighterThan.contains(looserOp), "Operator cannot be tighter and looser at the same time")
          graph.update(looserOp, graph.getOrElse(looserOp, Set()) + op)
        }
      }
      return graph.toMap
    }

    // Assuming each Binary operator is uniquely identified
    private def checkedPartialOrderGraph(
      operators: Set[Operator.Binary]
    ): Map[Operator.Binary, Set[Operator.Binary]] = {
      // Graph adjacency list
      val graph = partialOrderGraph(operators)

      // Function to detect cycle using DFS
      def hasCycle(node: Operator.Binary, visited: Set[Operator.Binary], stack: Set[Operator.Binary]): Boolean = {
        if (stack.contains(node)) return true // Cycle detected
        if (visited.contains(node)) return false

        val newVisited = visited + node
        val newStack = stack + node
        graph.getOrElse(node, Set()).exists(next => hasCycle(next, newVisited, newStack))
      }

      // Check each operator for cycle
      if (!operators.exists(op => hasCycle(op, Set(), Set()))) graph
      else throw new IllegalArgumentException("Invalid partial order (contains a cycle)")
    }

    private def reachableOperators(operators: Set[Operator.Binary]): Set[Operator.Binary] = {
      def reachable(operators: Set[Operator.Binary], visited: Set[Operator.Binary]): Set[Operator.Binary] = {
        operators.flatMap { op =>
          if (visited.contains(op)) Set()
          else Set(op) ++ reachable(op.sameAs ++ op.tighterThan ++ op.looserThan, visited + op)
        }
      }

      reachable(operators, Set())
    }
  }

  def parseExpressions(tokens: Seq[Token], binaryOperators: Set[Operator.Binary]): Seq[Expr] = {
    OperatorExpressionParser(tokens, binaryOperators).expressions
  }

end SeqExprParser
