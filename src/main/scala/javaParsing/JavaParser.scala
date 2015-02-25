package javaParsing
import org.parboiled2._
import org.parboiled2.CharPredicate
import scala.collection.immutable.Seq

trait ASTNode
case class ASTClass(val name: String, val nodes: Seq[ASTNode]) extends ASTNode
case class ASTField(val name: String, val fType: String) extends ASTNode
case class ASTMethod(val name: String, val returnType: String) extends ASTNode

class JavaParser(val input: ParserInput) extends Parser {
  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  def WS = rule { oneOrMore(WhiteSpaceChar) }
  def optWS = rule { zeroOrMore(WhiteSpaceChar) }

  def Identifier = rule {
    CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)
  }

  def Class = rule {
    Scope ~ "class" ~ WS ~ capture(Identifier) ~ optWS ~ 
    	'{' ~ optWS ~ zeroOrMore(Field | Method) ~ optWS ~ 
    	'}' ~> ((className, nodes) => ASTClass(className, nodes))
  }
  
  def Scope = rule {
    optional(("private" | "protected" | "public" | "default") ~ WS)
  }

  def Static = rule {
    optional(("static") ~ WS)
  }
  
  def Final = rule {
    optional(("final") ~ WS)
  }
  
  def Field = rule {
    Scope ~ Static ~ Final ~ capture(Identifier) ~ WS ~ capture(Identifier) ~ optWS ~ ';' ~ optWS ~> ((fType, name) => ASTField(name, fType))
  }
  
  def Method = rule {
	  Scope ~ Static ~ Final ~ capture(Identifier) ~ WS ~ capture(Identifier) ~ '(' ~ ')' ~ optWS ~ '{' ~ optWS ~ '}' ~ optWS ~> ((returnType, name) => ASTMethod(name, returnType))
  }
}