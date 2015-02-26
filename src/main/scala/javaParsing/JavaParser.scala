package javaParsing
import org.parboiled2._
import org.parboiled2.CharPredicate
import scala.collection.immutable.Seq

trait ASTNode
case class ASTClass(val name: String, val scope: ASTScope,val nodes: Seq[ASTNode]) extends ASTNode
case class ASTField(val name: String, val scope: ASTScope, val fType: String) extends ASTNode
case class ASTMethod(val name: String, val scope: ASTScope, val returnType: String) extends ASTNode
case class ASTScope(val name: String) extends ASTNode

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
    	'}' ~> ((scope, className, nodes) => ASTClass(className, scope, nodes))
  }
  
  def Scope = rule {
    optional(capture("private" | "protected" | "public" ) ~ WS) ~> ((name) => ASTScope(name.getOrElse("")))
  }

  def Static = rule {
    optional(("static") ~ WS)
  }
  
  def Final = rule {
    optional(("final") ~ WS)
  }
  
  def Field = rule {
    Scope ~ Static ~ Final ~ capture(Identifier) ~ WS ~ capture(Identifier) ~ optWS ~ ';' ~ optWS ~> ((scope, fType, name) => ASTField(name, scope, fType))
  }
  
  def Method = rule {
	  Scope ~ Static ~ Final ~ capture(Identifier) ~ WS ~ capture(Identifier) ~ '(' ~ ')' ~ optWS ~ '{' ~ optWS ~ '}' ~ optWS ~> ((scope, returnType, name) => ASTMethod(name, scope, returnType))
  }
}