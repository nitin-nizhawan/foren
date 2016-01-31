package com.github.nitin.nizhawan.main
import scala.scalajs.js._
import scala.scalajs.js.annotation._
import scala.util.parsing.combinator._
  


import Element.elem
object Element {
  
      private class ArrayElement(
        val contents: Array[String]
      ) extends Element
  
      private class LineElement(s: String) extends Element {
        val contents = Array(s)
        override def width = s.length
        override def height = 1
      }
  
      private class UniformElement(
        ch: Char,
        override val width: Int,
        override val height: Int
      ) extends Element {
        private val line = ch.toString * width
        def contents = Array(height).map(_=>line)
      }
  
      def elem(contents:  Array[String]): Element =
        new ArrayElement(contents)
  
      def elem(chr: Char, width: Int, height: Int): Element =
        new UniformElement(chr, width, height)
  
      def elem(line: String): Element =
        new LineElement(line)
    }

  
  abstract class Element {
    def contents:  Array[String]
  
    def width: Int = contents(0).length
    def height: Int = contents.length
  
    def above(that: Element): Element = {
      val this1 = this widen that.width
      val that1 = that widen this.width
      elem(this1.contents ++ that1.contents)
    }
  
    def beside(that: Element): Element = {
      val this1 = this heighten that.height
      val that1 = that heighten this.height
      elem(
        for ((line1, line2) <- this1.contents zip that1.contents) 
        yield line1 + line2)
    }
  
    def widen(w: Int): Element = 
      if (w <= width) this
      else {
        val left = elem(' ', (w - width) / 2, height) 
        var right = elem(' ', w - width - left.width, height)
        left beside this beside right
      }
  
    def heighten(h: Int): Element = 
      if (h <= height) this
      else {
        val top = elem(' ', width, (h - height) / 2)
        var bot = elem(' ', width, h - height - top.height)
        top above this above bot
      }
  
    override def toString = contents mkString "\n"
  }



    sealed abstract class Expr
    case class Var(name: String) extends Expr
    case class Num(num: Double) extends Expr
    case class UnOp(operator: String, arg: Expr) extends Expr
    case class BinOp(operator: String, 
        left: Expr, right: Expr) extends Expr
  
    class ExprFormatter {
  
      // Contains operators in groups of increasing precedence
      private val opGroups =
        Array(
          Set("|", "||"),
          Set("&", "&&"),
          Set("^"),
          Set("==", "!="),
          Set("<", "<=", ">", ">="),
          Set("+", "-"),
          Set("*", "%")
        )
  
      // A mapping from operators to their precedence
      private val precedence = {
        val assocs =
          for {
            i <- 0 until opGroups.length
            op <- opGroups(i)
          } yield op -> i
        Map() ++ assocs
      }
  
      private val unaryPrecedence = opGroups.length
      private val fractionPrecedence = -1

    private def format(e: Expr, enclPrec: Int): Element =
  
      e match {
  
        case Var(name) => 
          elem(name)
  
        case Num(num) => 
          def stripDot(s: String) = 
            if (s endsWith ".0") s.substring(0, s.length - 2)
            else s
          elem(stripDot(num.toString))
  
        case UnOp(op, arg) => 
          elem(op) beside format(arg, unaryPrecedence)
  
        case BinOp("/", left, right) => 
          val top = format(left, fractionPrecedence)
          val bot = format(right, fractionPrecedence)
          val line = elem('-', top.width max bot.width, 1)
          val frac = top above line above bot
          if (enclPrec != fractionPrecedence) frac
          else elem(" ") beside frac beside elem(" ")
  
        case BinOp(op, left, right) => 
          val opPrec = precedence(op)
          val l = format(left, opPrec) 
          val r = format(right, opPrec + 1)
          val oper = l beside elem(" "+ op +" ") beside r 
          if (enclPrec <= opPrec) oper
          else elem("(") beside oper beside elem(")")
      }
  
      def format(e: Expr): Element = format(e, 0)
    }



object ExpParser extends RegexParsers {   
   // atoms
   def ident = "[a-zA-Z][a-zA-Z0-9]*".r 
   def Variable:Parser[Var] = ident ^^ { case x => Var(x) }
   def num:Parser[Num] = "[0-9]+(\\.[0-9]+)?".r ^^ { case x:String => Num(x.toDouble) }
   def atom:Parser[Expr] = num ||| Variable 
  
  /* def plus = "\\+".r
   def minus = "\\-".r
   def div = "/".r
   def mul = "\\*".r

   def expr = term ~ expr_dash
   def expr_dash = ( plus ~ term ~ expr_dash ) ||| ( minus ~ term ~ expr_dash ) 
*/
   def op:Parser[String] = "\\^".r ||| "\\*".r ||| "\\+".r ||| "\\-".r ||| "/".r 
   def binExp:Parser[BinOp] = atom~op~exp ^^ { case a~b~c=>BinOp(b,a,c) }
   def exp:Parser[Expr] =  atom ||| binExp 

   def parseExp(in:String) = parse(exp,in) match {
      case Success(res,_)=>res
      case failure : NoSuccess => scala.sys.error(failure.msg)
   }
}   




@JSExport
object Foren {
  @JSExport
  def test(e:String)={ 
val f = new ExprFormatter
  
      val e1 = BinOp("*", BinOp("/", Num(1), Num(2)), 
                          BinOp("+", Var("x"), Num(1)))
      val e2 = BinOp("+", BinOp("/", Var("x"), Num(2)), 
                          BinOp("/", Num(1.5), Var("x")))
      val e3 = BinOp("/", e1, e2)
   val p = elem("Hello") beside elem("World")
   f.format(ExpParser.parseExp(e))+""
   //    ExpParser.parseExp(e)+""
  }
}
