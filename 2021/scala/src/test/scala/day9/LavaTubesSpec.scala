package day9

class LavaTubesSpec extends org.scalatest.funsuite.AnyFunSuite {

  def foo: String = "foo"

  test("Foo test") {
    assert("bar" != foo)
  }
  
}
