package fpinscala.gettingstarted

import org.scalatest.FlatSpec

class MyModuleTest extends FlatSpec {

  "fib" should "return the correct fibonacci number" in {
    // 0 1 1 2 3 5 8 13 21 ...
    assert(MyModule.fib(0) == 0)
    assert(MyModule.fib(1) == 1)
    assert(MyModule.fib(2) == 1)
    assert(MyModule.fib(3) == 2)
    assert(MyModule.fib(4) == 3)
    assert(MyModule.fib(7) == 13)
  }

}
