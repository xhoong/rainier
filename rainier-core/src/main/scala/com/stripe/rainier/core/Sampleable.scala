package com.stripe.rainier.core

import com.stripe.rainier.compute._
import com.stripe.rainier.sampler.RNG

trait Sampleable[-S, +T] {
  def requirements(value: S): Set[Real]
  def get(value: S)(implicit r: RNG, n: Numeric[Real]): T

  def prepare(value: S, variables: Seq[Variable])(
      implicit r: RNG): Array[Double] => T = {
    val reqs = requirements(value).toList
    if (reqs.isEmpty) { array =>
      {
        implicit val evaluator: Evaluator =
          new Evaluator(variables.zip(array).toMap)
        get(value)
      }
    } else {
      val cf = Compiler.default.compile(variables, reqs)
      array =>
        {
          val reqValues = cf(array)
          implicit val evaluator: Evaluator =
            new Evaluator(
              variables.zip(array).toMap ++
                reqs.zip(reqValues).toMap
            )
          get(value)
        }
    }
  }
}

object Sampleable {
  implicit def generator[T]: Sampleable[Generator[T], T] =
    new Sampleable[Generator[T], T] {
      def requirements(value: Generator[T]): Set[Real] = value.requirements
      def get(value: Generator[T])(implicit r: RNG, n: Numeric[Real]): T =
        value.get
    }

  implicit val real: Sampleable[Real, Double] =
    new Sampleable[Real, Double] {
      def requirements(value: Real): Set[Real] = Set(value)
      def get(value: Real)(implicit r: RNG, n: Numeric[Real]): Double =
        n.toDouble(value)
    }

  implicit def map[K, S, T](
      implicit s: Sampleable[S, T]): Sampleable[Map[K, S], Map[K, T]] =
    new Sampleable[Map[K, S], Map[K, T]] {
      def requirements(value: Map[K, S]): Set[Real] =
        value.values.flatMap { v =>
          s.requirements(v)
        }.toSet
      def get(value: Map[K, S])(implicit r: RNG, n: Numeric[Real]): Map[K, T] =
        value.map { case (k, v) => k -> s.get(v) }.toMap
    }

  implicit def zip[A, B, X, Y](
      implicit ab: Sampleable[A, B],
      xy: Sampleable[X, Y]): Sampleable[(A, X), (B, Y)] =
    new Sampleable[(A, X), (B, Y)] {
      def requirements(value: (A, X)): Set[Real] =
        ab.requirements(value._1) ++ xy.requirements(value._2)
      def get(value: (A, X))(implicit r: RNG, n: Numeric[Real]): (B, Y) =
        (ab.get(value._1), xy.get(value._2))
    }

  implicit def seq[S, T](
      implicit v: Sampleable[S, T]): Sampleable[Seq[S], Seq[T]] =
    new Sampleable[Seq[S], Seq[T]] {
      def requirements(value: Seq[S]): Set[Real] =
        value
          .map { s =>
            v.requirements(s)
          }
          .reduce(_ ++ _)
      def get(value: Seq[S])(implicit r: RNG, n: Numeric[Real]): Seq[T] =
        value.map { s =>
          v.get(s)
        }
    }
}
