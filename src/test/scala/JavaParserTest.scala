import org.scalatest._
import org.parboiled2.ParserInput
import org.parboiled2.support.RunResult
import scala.util.Failure
import scala.util.Success
import org.parboiled2.Rule
import shapeless.HNil
import scala.util.Try
import org.parboiled2.ParseError
import org.parboiled2.ErrorFormatter
import org.parboiled2.Parser

class JavaParserTest extends FlatSpec with Matchers {
  def parseName(input: String) = {
    val parser = new JavaParser(input)

    parser.Identifier.run()
  }

  def parseClass(input: String) = {
    val parser = new JavaParser(input)

    parser.Class.run()
  }

  def parseField(input: String) = {
    val parser = new JavaParser(input)

    parser.Field.run()
  }

  def parseMethod(input: String) = {
    val parser = new JavaParser(input)

    parser.Method.run()
  }

  //  def runParser(aRule:Rule0, input:String) = {
  //    val parser = new JavaParser(input)
  //
  //    aRule.run()
  //  }

  "A name with all letters" should "be valid" in {
    parseName("aaab") shouldBe 'success
  }

  "A name with a single letter" should "be valid" in {
    parseName("x") shouldBe 'success
  }

  "A name with a letter and digits" should "be valid" in {
    parseName("bd32") shouldBe 'success
  }

  "An invalid name" should "return an error" in {
    parseName("1fds") shouldBe 'failure
  }

  "An empty class" should "be valid" in {
    parseClass("class MyClass { }") shouldBe 'success

    parseClass("""class MyClass { 

    			  }""") shouldBe 'success
    			  
    parseClass("public class MyClass { }") shouldBe 'success
  }

  "A field" should "be valid" in {
    parseField("private Integer x;") shouldBe 'success
    parseField("public String str;") shouldBe 'success
    parseField("MyClass aClass;") shouldBe 'success
  }

  "A method" should "be valid" in {
    parseMethod("private Integer meth() { }") shouldBe 'success
  }

  "A class with a field" should "be valid" in {

    parseClass("""class MyClass { 
			private Integer x;
    			  }""") shouldBe 'success

    parseClass("""class MyClass { 
			public String str;
    			  }""") shouldBe 'success

    parseClass("""class MyClass { 
			default String str;
    			  }""") shouldBe 'success

    parseClass("""class MyClass { 
			MyClass aloha;
    			  }""") shouldBe 'success

    parseClass("""class MyClass { 
			MyClass aloha;
    		private Float f;
    			  }""") shouldBe 'success

  }

  "A class with a field and method" should "be valid" in {

    parseClass("""class MyClass { 
			private Integer x;
    		public Integer meth() {}
    			  }""") shouldBe 'success
  }

  //  val formatter = new ErrorFormatter(showTraces = true)
  //  
  //  def check(nr: Rule[HNil, HNil], parser: Parser) =
  //      
  //      nr.run() match {
  //        case Failure(error: ParseError) => fail(error.format(parser, formatter))
  //        case Failure(error) => fail(error.toString)
  //        case Success(_) => 
  //    }
}