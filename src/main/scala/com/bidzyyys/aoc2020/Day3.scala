package com.bidzyyys.aoc2020

import scala.io.Source

object Day3 extends App {
  var score = 0
  val readLines: Array[String] =
    Source.fromResource("day3.txt").getLines.toArray

  val map: Array[Array[Char]] = readLines.map(_.toCharArray)

  val treeLetter = '#'

  val trees2 = countTrees(map, 1, 1, treeLetter)
  println("Trees: " + trees2)

  val trees1 = countTrees(map, 3, 1, treeLetter)
  println("Trees: " + trees1)

  val trees3 = countTrees(map, 5, 1, treeLetter)
  println("Trees: " + trees3)

  val trees4 = countTrees(map, 7, 1, treeLetter)
  println("Trees: " + trees4)

  val trees5 = countTrees(map, 1, 2, treeLetter)
  println("Trees: " + trees5)

  println("Result: " + trees1 * trees2 * trees3 * trees4 * trees5)

  def countTrees(
      map: Array[Array[Char]],
      widthStep: Int,
      heightStep: Int,
      treeLetter: Char
  ): Int = {
    var trees = 0
    val height = map.length
    val width = map.apply(0).length
    var widthPosition = widthStep
    for (heightPosition <- heightStep until height by heightStep) {
      if (map.apply(heightPosition).apply(widthPosition) == treeLetter) {
        trees = trees + 1
      }
      widthPosition = (widthPosition + widthStep) % width
    }
    trees
  }

}
