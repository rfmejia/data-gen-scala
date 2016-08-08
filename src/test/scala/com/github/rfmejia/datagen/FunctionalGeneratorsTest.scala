package com.github.rfmejia.datagen

import org.scalatest.{ Matchers, WordSpec }

import scala.util.Random

class FunctionalGeneratorsTest extends WordSpec with Matchers {

  object FunctionalGenerators extends PrimitiveGenerator
      with FunctionGenerator
      with ListGenerator
      with LoremGenerator
      with DateGenerator
      with IdentifierGenerator {
    val minLength = 3
    val maxLength = 6
  }

  "A primitive integer generator" when {
    implicit val r = new Random(0)
    object G extends PrimitiveGenerator

    "given a valid integer limits" should {
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
    }

    "given invalid integer limits" should {
      "throw an IllegalArgumentException" in {
        intercept[IllegalArgumentException] {
          G.integer(100, -100)
        }
      }
    }
  }
}
