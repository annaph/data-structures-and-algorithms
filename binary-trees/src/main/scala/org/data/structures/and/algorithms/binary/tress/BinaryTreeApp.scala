package org.data.structures.and.algorithms.binary.tress

import org.data.structures.and.algorithms.binary.tress.BinaryTree._

object BinaryTreeApp extends App {

  // Create binary tree
  val binaryTree = BinaryTree(values = 1, 2, 3, 4, 5, 6)
  printInfo(binaryTree, message = "Basic example")

  // Check are binary trees equal
  val equalResult1 = binaryTree equal BinaryTree(values = 1, 2, 3, 4, 5, 6)
  assert(equalResult1, message = "Binary trees should be equal!")

  val equalResult2 = binaryTree equal BinaryTree(values = 1, 2, 3, 4, 5, 6, 7)
  assert(!equalResult2, message = "Binary trees should not be equal!")

  // Create complete binary tree
  val compBinaryTree = complete(values = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  printInfo(compBinaryTree, message = "Complete binary tree")

  // Create flipped binary tree
  val flippedBinaryTree1 = complete(values = 1, 2, 3, 4, 5, 6, 7).flip
  printInfo(flippedBinaryTree1)

  val flippedBinaryTree2 = Node(1, Leaf(2), Node(3, Leaf(4), Leaf(5))).flip
  printInfo(flippedBinaryTree2, message = "Flipped binary tree")

  // Check are binary trees flipped
  assert(flippedBinaryTree1 isFlipped complete(values = 1, 2, 3, 4, 5, 6, 7), message = "Should be flipped!")
  assert(flippedBinaryTree2 isFlipped Node(1, Leaf(2), Node(3, Leaf(4), Leaf(5))), message = "Should be flipped!")
  assert(!(flippedBinaryTree1 isFlipped complete(values = 1, 2, 3)), message = "Should not be flipped!")

}
