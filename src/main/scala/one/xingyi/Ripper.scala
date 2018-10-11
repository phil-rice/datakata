package one.xingyi

import scala.io.Source

trait RipperStreamModifier[T] extends (Seq[String] => Seq[String])


trait Ripper {
  def apply[T](s: Seq[String])(implicit parser: Parser[T], ripperStreamModifier: RipperStreamModifier[T]): Seq[T] =
    ripperStreamModifier(s).map(parser)

  def apply[T: Parser : RipperStreamModifier](s: String): Seq[T] =
    apply(Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(s)).getLines().toSeq)
}

object Ripper extends Ripper
