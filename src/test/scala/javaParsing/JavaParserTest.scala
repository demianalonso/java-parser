package javaParsing
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

class JavaParserTest extends FreeSpec with Matchers {
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

  "Parse simple expressions" - {
    "A name with all letters" in {
      parseName("aaab") shouldBe 'success
    }

    "A name with a single letter" in {
      parseName("x") shouldBe 'success
    }

    "A name with a letter and digits" in {
      parseName("bd32") shouldBe 'success
    }

    "An invalid name" in {
      parseName("1fds") shouldBe 'failure
    }
  }

  "Parse class expressions" - {
    "An empty class" in {
      parseClass("class MyClass { }") shouldBe 'success

      parseClass("""class MyClass { 

            }""") shouldBe 'success

      parseClass("public class MyClass { }") shouldBe 'success
    }

    "A class with a field" in {

      parseClass("""class MyClass { 
                      private Integer x;
                            }""") shouldBe 'success

      parseClass("""class MyClass { 
                      public String str;
                            }""") shouldBe 'success

      parseClass("""class MyClass { 
                      String str;
                            }""") shouldBe 'success

      parseClass("""class MyClass { 
                      MyClass aloha;
                            }""") shouldBe 'success

      parseClass("""class MyClass { 
                      MyClass aloha;
                        private Float f;
                            }""") shouldBe 'success

    }
  }

  "Parse fields and methods" - {
    "A field" in {
      parseField("private Integer x;") shouldBe 'success
      parseField("public String str;") shouldBe 'success
      parseField("protected String str;") shouldBe 'success
      parseField("MyClass aClass;") shouldBe 'success
    }

    "A method" in {
      parseMethod("private Integer meth() { }") shouldBe 'success
    }
  }

  "Parse class with fields and methods" - {
    "A class with a field and method" in {

      parseClass("""class MyClass { 
                      private Integer x;
                        public Integer meth() {}
                            }""") shouldBe 'success
    }

    "A class " in {

      val result = parseClass("""class MyClass { 
                                    private Integer x;
                                      public Integer meth() {}
                                          }""")
                                          
      val astClass = result.get
      assert(astClass.isInstanceOf[ASTClass])
      astClass.name should be("MyClass")

      astClass.nodes(0).asInstanceOf[ASTField].name should be("x")
      astClass.nodes(0).asInstanceOf[ASTField].fType should be("Integer")

      astClass.nodes(1).asInstanceOf[ASTMethod].name should be("meth")
      astClass.nodes(1).asInstanceOf[ASTMethod].returnType should be("Integer")

    }
    
    "Parse final variables" - {
      parseField("private final String x;") shouldBe 'success
      parseField("final Integer x;") shouldBe 'success    
      parseMethod("public final void m2() {}") shouldBe 'success
    }
    
    "Parse complex class declarations" - {
//      val result = parseClass("""class Example {
//                                    private static class NestedClass {
//                                        public void secretMethod() {
//                                          
//                                        }
//                                    }
//                                    public static NestedClass leakPrivateClass() {
//                                        return new NestedClass();
//                                    }
//                                }""")    
//                                
//      val astClass = result.get
//      assert(astClass.isInstanceOf[ASTClass])
//      astClass.name should be("Example")
     
    }
  }

}