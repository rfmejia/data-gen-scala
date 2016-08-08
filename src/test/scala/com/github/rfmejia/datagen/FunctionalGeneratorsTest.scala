package com.github.rfmejia.datagen

import org.scalatest.{ Matchers, WordSpec }

import scala.util.Random

class FunctionalGeneratorsTest extends WordSpec with Matchers {

  "A primitive generator" when {
    implicit val r = new Random(0)
    object G extends PrimitiveGenerator

    "given a valid integer bounds" should {
      "produce a random integer within a given bound" in {
        val min = -100
        val max = 100
        val times = 100

        for (i <- 1 to times) {
          val num = G.integer(min, max)
          num should be >= min
          num should be <= max
        }
      }

      "produce a random float within a given bound" in {
        val min = -100.0
        val max = 100.0
        val times = 100

        for (i <- 1 to times) {
          val num = G.float(min, max)
          num should be >= min.toDouble
          num should be <= max.toDouble
        }
      }

    }

    "given invalid integer limits" should {
      "throw an IllegalArgumentException" in {
        intercept[IllegalArgumentException] {
          G.integer(100, -100)
        }

        intercept[IllegalArgumentException] {
          G.float(100.0, -100.0)
        }
      }
    }
  }

  "A function generator" when {
    implicit val r = new Random(0)
    object G extends FunctionGenerator

    "given a function to generate even integers" should {
      "produce even numbers" in {
        def f(r: Random) = r.nextInt * 2
        val times = 100

        for (i <- 1 to 100) {
          G.function(f) % 2 shouldBe 0
        }
      }
    }
  }

  "A list generator" when {
    implicit val r = new Random(0)
    object G extends ListGenerator
    "given a range of elements and a step increment" should {
      "successfully produce a numerical stream" in {
        G.range(-100, 100, 5).toList.exists(_ % 5 != 0) shouldBe false
      }

      "successfully produce a character stream" in {
        G.range('A', 'Z', 1).map(_.toChar).mkString shouldEqual "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      }
    }
  }
}
