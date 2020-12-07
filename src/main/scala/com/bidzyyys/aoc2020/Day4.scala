package com.bidzyyys.aoc2020

import scala.io.Source
import scala.collection.immutable.HashSet

object Day4 extends App {
  val readLines: List[String] =
    Source.fromResource("day4.txt").getLines.toList

  val passports: List[Passport] =
    collectPassportsInfo(readLines).map(extractPassportInfo)

  val validPassports1: List[Passport] = passports.filter(validatePassport1)
  println("(1) Valid passports: " + validPassports1.length)

  val validPassports2: List[Passport] = passports.filter(validatePassport2)
  println("(2) Valid passports: " + validPassports2.length)

  def collectPassportsInfo(lines: List[String]): List[String] = {
    var passportsInfo: List[String] = List()

    var passportInfo: String = ""
    for (line <- lines) {
      if (line == "") {
        passportsInfo = passportInfo :: passportsInfo
        passportInfo = ""
      } else {
        passportInfo = passportInfo + " " + line
      }
    }

    passportInfo :: passportsInfo
  }

  def extractPassportInfo(passportInfo: String): Passport = {
    var passport: Passport = new Passport

    val Pattern = """(.*):(.+)""".r

    val attributes: Array[String] = passportInfo
      .split(" ")
      .filter(_ != "")

    for (attribute <- attributes) {
      val Pattern(key, value) = attribute
      key match {
        case "byr" => passport.byr = value
        case "iyr" => passport.iyr = value
        case "eyr" => passport.eyr = value
        case "hgt" => passport.hgt = value
        case "hcl" => passport.hcl = value
        case "ecl" => passport.ecl = value
        case "pid" => passport.pid = value
        case "cid" => passport.cid = value
      }
    }
    passport
  }

  def validatePassport1(passport: Passport): Boolean = {
    passport.byr != null &&
    passport.iyr != null &&
    passport.eyr != null &&
    passport.hgt != null &&
    passport.hcl != null &&
    passport.ecl != null &&
    passport.pid != null
  }

  def validatePassport2(passport: Passport): Boolean = {
    validatePassport1(passport) &&
    validateByr(passport) &&
    validateIyr(passport) &&
    validateEyr(passport) &&
    validateHgt(passport) &&
    validateHcl(passport) &&
    validateEcl(passport) &&
    validatePid(passport)
  }

  def validateByr(passport: Passport): Boolean = {
    val byr: Int = passport.byr.toInt
    byr >= 1920 && byr <= 2002
  }
  def validateIyr(passport: Passport): Boolean = {
    val iyr: Int = passport.iyr.toInt
    iyr >= 2010 && iyr <= 2020
  }

  def validateEyr(passport: Passport): Boolean = {
    val eyr: Int = passport.eyr.toInt
    eyr >= 2020 && eyr <= 2030
  }

  def validateHgt(passport: Passport): Boolean = {
    val HgtPattern = """([0-9]+)([a-z]+)""".r

    if (!(HgtPattern matches passport.hgt)) {
      return false
    }

    val HgtPattern(hgt, heightType) = passport.hgt
    val height: Int = hgt.toInt

    heightType match {
      case "cm" => height >= 150 && height <= 193
      case "in" => height >= 59 && height <= 76
      case _    => false
    }
  }

  def validateHcl(passport: Passport): Boolean = {
    val HclPattern = """#([0-9a-f]{6})""".r
    HclPattern matches passport.hcl
  }

  def validateEcl(passport: Passport): Boolean = {
    val eyesColors: HashSet[String] =
      HashSet("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    eyesColors.contains(passport.ecl)
  }
  def validatePid(passport: Passport): Boolean = {
    val PidPattern = """([0-9]{9})""".r
    PidPattern matches passport.pid
  }

  class Passport(
      var byr: String = null,
      var iyr: String = null,
      var eyr: String = null,
      var hgt: String = null,
      var hcl: String = null,
      var ecl: String = null,
      var pid: String = null,
      var cid: String = null
  )
}
