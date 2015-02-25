import org.scalatest._

class JavaParserTest extends FlatSpec {
  "A test case" should "work" in {
    val parser = new JavaParser()
    assert(parser.m() == true)
  }
}