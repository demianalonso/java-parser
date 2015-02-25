import org.parboiled2._
import org.parboiled2.CharPredicate

class JavaParser(val input: ParserInput) extends Parser {
  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  def WS = rule { oneOrMore(WhiteSpaceChar) }
  def optWS = rule { zeroOrMore(WhiteSpaceChar) }

  def Identifier = rule {
    CharPredicate.Alpha ~ zeroOrMore(CharPredicate.AlphaNum)
  }

  def Class = rule {
    Scope ~ "class" ~ WS ~ Identifier ~ optWS ~ '{' ~ optWS ~ zeroOrMore((Field | Method) ~ optWS) ~ optWS ~ '}'
  }
  
  def Scope = rule {
    optional(("private" | "public" | "default") ~ WS)
  }
  
  def Field = rule {
    Scope ~ Identifier ~ WS ~ Identifier ~ optWS ~ ';'
  }
  
  def Method = rule {
	  Scope ~ Identifier ~ WS ~ Identifier ~ '(' ~ ')' ~ optWS ~ '{' ~ optWS ~ '}'
  }
}