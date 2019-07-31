package fpinscala.datastructures

import org.scalatest.FlatSpec

class TreeTest extends FlatSpec {
  //      b
  //    b   b
  //  4  b 8  10
  //    5 7
  val tree =
    Branch(
      Branch(
        Leaf(4),
        Branch(
          Leaf(5),
          Leaf(7)
        )
      ),
      Branch(
        Leaf(8),
        Leaf(10)
      )
    )

  "size" should "return the number of nodes (leaves and branches) in a tree" in {
    assert(Tree.size(tree) == 9)
  }

  "maximum" should "return the maximum value at a leaf in the tree" in {
    assert(Tree.maximum(tree) == 10)
  }

  "depth" should "return the number of nodes in the longer path between the root and a leaf in the tree" in {
    assert(Tree.depth(tree) == 4)
  }

  "map" should "work to add 1 to all the leaves in an int tree" in {
    val tree_plus_one =
      Branch(
        Branch(
          Leaf(5),
          Branch(
            Leaf(6),
            Leaf(8)
          )
        ),
        Branch(
          Leaf(9),
          Leaf(11)
        )
      )

    assert(Tree.map(tree)(_+1) == tree_plus_one)
  }

  "sizeWithFold" should "behave in the same way as size" in {
    assert(Tree.sizeWithFold(tree) == 9)
  }

  "maximumWithFold" should "behave in the same way as maximum" in {
    assert(Tree.maximum(tree) == 10)
  }

  "depthWithFold" should "behave in the same way as depth" in {
    assert(Tree.depth(tree) == 4)
  }

  "mapWithFold" should "behave in the same way as map" in {
    val tree_plus_one =
      Branch(
        Branch(
          Leaf(5),
          Branch(
            Leaf(6),
            Leaf(8)
          )
        ),
        Branch(
          Leaf(9),
          Leaf(11)
        )
      )

    assert(Tree.map(tree)(_+1) == tree_plus_one)
  }
}
