package com.bidzyyys.aoc2020

import scala.io.Source
import scala.collection.immutable.HashSet

object Day1 extends App {
  val sum: Int = 2020

  val readLines: Array[String] =
    Source.fromResource("day1.txt").getLines.toArray
  val numbers: Array[Int] = readLines.flatMap(_.toIntOption)

  val (left, right) = findPair(sum, numbers)
  println("First: " + left + ", second: " + right + "; Answer: " + left * right)

  val (first, second, third) = find3Values(sum, numbers)
  println(
    "First: " + first + ", second: " + second + ", third: " + third + "; Answer: " + first * second * third
  )

  @throws[IllegalStateException]("Pair not found")
  def findPair(sum: Int, numbers: Array[Int]): (Int, Int) = {
    var visited: HashSet[Int] = HashSet()

    for (num <- numbers) {

      if (visited.contains(sum - num)) return (num, sum - num);
      visited = visited + num
    }

    throw new IllegalStateException("Pair not found")
  }

  @throws[IllegalStateException]("Values not found")
  def find3Values(sum: Int, numbers: Array[Int]): (Int, Int, Int) = {
    for (i <- 0 until numbers.length - 2) {
      var visited: HashSet[Int] = HashSet()
      val currSum: Int = sum - numbers(i)
      for (j <- i + 1 until numbers.length) {
        if (visited.contains(currSum - numbers(j))) {
          return (numbers(i), numbers(j), currSum - numbers(j))
        }
        visited = visited + numbers(j);
      }
    }
    throw new IllegalStateException("Values not found")

  }
}
