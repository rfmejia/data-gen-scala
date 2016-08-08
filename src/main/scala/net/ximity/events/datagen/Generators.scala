package net.ximity.events.datagen

import java.time.Instant
import java.util.UUID
import scala.util.Random


trait ListGenerator extends PrimitiveGenerator {
  def range(start: Int = 0, stop: Int, step: Int = 1): Stream[Int] = (start to stop by step).toStream

  def random[T](stream: Stream[T])(implicit r: Random): T = stream.drop(r.nextInt(stream.size - 1)).head

  def repeat[T](min: Int, max: Option[Int], f: Int => T)(implicit r: Random): Stream[T] = {
    val count = max.map(mx => integer(min, mx)) getOrElse min
    (0 to count - 1).toStream.map(f)
  }
}

trait PrimitiveGenerator {
  def bool(implicit r: Random): Boolean = r.nextBoolean()

  def integer(min: Int = 0, max: Int = Int.MaxValue)(implicit r: Random): Int =
    (r.nextInt(max - min) + min)

  def float(min: Double = Double.MinValue, max: Double = Double.MaxValue)(implicit r: Random): Double =
    (r.nextDouble() * max) + min

  def gauss(mu: Double = 0.0f, sigma: Double = 1.0f) = ???
}

trait Generator {
  def function[T](f: Random => T)(implicit r: Random): T = f(r)
}

trait IdentifierGenerator {
  def guid: UUID = UUID.randomUUID()

  def objectId: String = ???

}

trait ContactInfoGenerator {
  def city: String = ???
  def phone(format: String): String = ???
  def state(): String = ???
  def street(): String = ???
}

case class CountryData(name: String, iso3166_1: String)

trait CountryGenerator extends ListGenerator {
  val countryData: Stream[CountryData]

  def country: CountryData = random(countryData)

  def domainZone: String = ???
}

sealed trait Gender
case object Male extends Gender
case object Female extends Gender

trait NameGenerator extends ListGenerator {
  val surnames: Stream[String]
  val femaleFirstNames: Stream[String]
  val maleFirstNames: Stream[String]

  def normalize(s: String): String = {
    @scala.annotation.tailrec
    def loop(in: List[String], acc: List[String]): List[String] = in match {
      case Nil => acc
      case head :: tail =>
        val name = head.head.toUpper + head.tail.toLowerCase
        loop(tail, acc :+ name)
    }

    loop(s.split(" ").toList, List.empty).mkString(" ")
  }

  def firstName(gender: Option[Gender])(implicit r: Random): String = gender match {
    case Some(Female) => normalize(random(femaleFirstNames))
    case Some(Male) => normalize(random(maleFirstNames))
    case None => normalize(random(femaleFirstNames ++ maleFirstNames))
  }

  def surname(): String = normalize(random(surnames))
}

trait CompanyNameGenerator extends ListGenerator {
  val companyNames: Stream[String]

  def company: String = random(companyNames)

}

trait DateGenerator {
  def date(min: Instant = Instant.MIN, max: Instant = Instant.MAX)(implicit r: Random): Instant = {
    val ms = ((r.nextDouble * max.getEpochSecond) + min.getEpochSecond).toLong
    Instant.ofEpochMilli(ms)
  }
}

sealed trait LoremType
case object letters extends LoremType
case object words extends LoremType
case object sentences extends LoremType
case object paragraphs extends LoremType

trait LoremGenerator {
  val minLength: Int
  val maxLength: Int
  assert(maxLength > minLength, "Lorem generator length must be non-zero.")

  private def randomLength(implicit r: Random): Int = r.nextInt(maxLength - minLength) * minLength

  def lorem(num: Int, loremType: LoremType)(implicit r: Random): String = loremType match {
    case letters => r.nextString(num)
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

object JsonGenerator extends PrimitiveGenerator with LoremGenerator {
  implicit val r = new Random

  val minLength: Int = 3
  val maxLength = 6

  def index(): Int = ???


}

