package net.ximity.events.datagen

import java.time.Instant
import java.util.UUID
import scala.util.Random

trait RandomGenerator {
  val seed: Option[Long]
  val rand = seed.map(s => new Random(s)).getOrElse((new Random))

  val lorem: Lorem
}

trait Generator extends RandomGenerator {

  def bool: Boolean = rand.nextBoolean()

  def city: String = ???

  def company: String = ???

  def country(abbrev: Boolean = false): String = ???

  def date(min: Instant = Instant.MIN, max: Instant = Instant.MAX): Instant = {
    val ms = ((rand.nextDouble * max.getEpochSecond) + min.getEpochSecond).toLong
    Instant.ofEpochMilli(ms)
  }

  def domainZone: String = ???

  def firstName(gender: Gender): String = ???

  def float(min: Double = Double.MinValue, max: Double = Double.MaxValue): Double = ???

  def function[T](f: Random => T): T = f(rand)

  def gauss(mu: Double = 0, sigma: Double) = ???

  def guid: UUID = UUID.randomUUID()

  def index(): Int

  def integer(min: Int = Int.MinValue, max: Int = Int.MaxValue): Int = rand.nextInt(max)

  def lorem(num: Int, loremType: LoremType): String = lorem.lorem(num, loremType)

  def objectId: String = ???

  def phone(format: String): String = ???

  def random[T](stream: Stream[T]): Option[T] = stream.drop(rand.nextInt(stream.size - 1)).headOption

  def range(start: Int = 0, stop: Int, step: Int = 1): Stream[Int] = (start to stop by step).toStream

  //def repeat[T](min: Int, max: Option[Int]): T

  def state(): String = ???

  def street(): String = ???

  def surname(): String = ???
}

object JsonGenerator extends Generator {
  val seed = None
  val lorem = new Lorem(3, 6, rand)

  def index(): Int = ???

}

class Lorem(val minLength: Int, val maxLength: Int, val rand: Random) {
  assert(maxLength > minLength, "Lorem generator length must be non-zero.")

  private def randomLength: Int = rand.nextInt(maxLength - minLength) * minLength

  def lorem(num: Int, loremType: LoremType): String = loremType match {
    case letters => rand.nextString(num)
    case words => (for(n <- 1 to num) yield lorem(randomLength, letters)).mkString(" ")
    case sentences =>
      val ss = for(n <- 1 to num) yield {
        val s = lorem(randomLength, words)
        s.head.toUpper + s.tail + "."
      }
      ss.mkString(" ")
    case paragraphs => (for(n <- 1 to num) yield lorem(randomLength, sentences)).mkString("\n")
  }
}

sealed trait LoremType
case object letters extends LoremType
case object words extends LoremType
case object sentences extends LoremType
case object paragraphs extends LoremType

sealed trait Gender
case object Male extends Gender
case object Female extends Gender
