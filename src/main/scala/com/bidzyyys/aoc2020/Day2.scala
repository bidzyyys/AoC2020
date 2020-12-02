package com.bidzyyys.aoc2020

import scala.io.Source

object Day2 extends App {
  val sum: Int = 2020

  val readLines: List[String] =
    Source.fromResource("day2.txt").getLines.toList

  val lines: List[Line] = readLines.map(extractValues)

  val validatedLines1 = lines.filter(validateLine1)
  println("Valid lines (1): " + validatedLines1.length)

  val validatedLines2 = lines.filter(validateLine2)
  println("Valid lines (2): " + validatedLines2.length)

  def extractValues(line: String): Line = {
    val Pattern = """([1-9][0-9]*)-([1-9][0-9]*) ([A-Za-z]): ([A-Za-z]+)""".r

    val Pattern(firstNumber, secondNumber, letter, word) = line

    Line(
      firstNumber.toInt,
      secondNumber.toInt,
      letter.toCharArray.apply(0),
      word
    )
  }

  def validateLine1(line: Line): Boolean = {
    val amount: Int = line.word.count(_ == line.letter)
    amount >= line.firstNumber && amount <= line.secondNumber
  }

  def validateLine2(line: Line): Boolean = {
    val word = line.word.toCharArray
    val letter = line.letter
    val firstPos = line.firstNumber - 1
    val secondPos = line.secondNumber - 1

    (word.apply(firstPos) == letter && word.apply(secondPos) != letter) || (word
      .apply(firstPos) != letter && word.apply(secondPos) == letter)
  }
  case class Line(
      firstNumber: Int,
      secondNumber: Int,
      letter: Char,
      word: String
  )
}
