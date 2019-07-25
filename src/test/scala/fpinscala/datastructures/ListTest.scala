package fpinscala.datastructures

import org.scalatest.FlatSpec

class ListTest extends FlatSpec {

  "exercise_3_1" should "return the third case" in {
    assert(List.exercise_3_1() == 3)
  }

  "tail" should "return a new list without the first element" in {
    assert(List.tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
  }

  "tail" should "return a new list without the first element for a single element list" in {
    assert(List.tail(List(1)) == Nil)
  }

  "tail" should "return an empty list for an empty list as input" in {
    assert(List.tail(List()) == Nil)
  }

  "setHead" should "change the first element of the list" in {
    assert(List.setHead(List(1, 2, 3, 4, 5), 10) == List(10, 2, 3, 4, 5))
  }

  "setHead" should "add an element to an empty list" in {
    assert(List.setHead(List(), 10) == List(10))
  }

  "drop" should "remove the n first elements of a list" in {
    assert(List.drop(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5))
  }

  "drop" should "return an empty list when there are no more elements to drop" in {
    assert(List.drop(List(), 2) == Nil)
    assert(List.drop(List(1), 2) == Nil)
  }

  "dropWhile" should "remove elements on a list while a condition stands" in {
    assert(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x <= 2) == List(3, 4, 5))
    assert(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x <= 10) == Nil)
    assert(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x < 0) == List(1, 2, 3, 4, 5))
  }

  "init" should "return a list with the last element removed" in {
    assert(List.init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
    assert(List.init(List()) == Nil)
    assert(List.init(List(1)) == Nil)
  }

  "exercise_3_8" should "return the same list as the input" in {
    assert(List.exercise_3_8(List(1, 2, 3, 4)) == List(1, 2, 3, 4))
  }

  "length" should "return the length of a list" in {
    assert(List.length(List(1,2,3)) == 3)
    assert(List.length(List(4,4,4,4,4,4,4)) == 7)
    assert(List.length(List()) == 0)
  }

  "foldLeft" should "work to sum or get the product of a list" in {
    assert(List.foldLeft(List(1,2,3), 0)(_+_) == 6)
    assert(List.foldLeft(List(1,2,3), 1)(_*_) == 6)
    assert(List.foldLeft(List(1,2,0), 1)(_*_) == 0)
  }

  "foldLeft" should "work to get the sum, product or length of a list" in {
    assert(List.sum3(List(1,2,3)) == 6)
    assert(List.product3(List(1,2,3)) == 6)
    assert(List.length3(List(1,2,3)) == 3)
  }
}
