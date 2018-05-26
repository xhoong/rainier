package com.stripe.rainier.core

import com.stripe.rainier.sampler._
import com.stripe.rainier.compute._

final case class SBC[T](priorGenerators: Seq[Generator[Double]],
                        priorParams: Seq[Real],
                        posterior: RandomVariable[Distribution[T]]) {
  require(priorParams.forall(_.variables.size == 1))
  val priorGenerator = Generator.traverse(priorGenerators)
  val emptyEvaluator = new Evaluator(Map.empty)

  def simulate(sampler: Sampler, config: SBC.Config)(
      implicit rng: RNG): Seq[Seq[Int]] = {
    1.to(config.repetitions).map { _ =>
      simulateOnce(sampler, config)
    }
  }

  /*  def calibrate(sampler: Sampler, config: SBC.Config)(
      implicit rng: RNG): Seq[Double] =
    simulate(sampler, config).map { l =>
      SBC.chiSquared(l)
    }
   */
  private def simulateOnce(sampler: Sampler, config: SBC.Config)(
      implicit rng: RNG): Seq[Int] = {
    val trueValues = priorGenerator.get(rng, emptyEvaluator)
    println(trueValues)
    implicit val trueEval = new Evaluator(priorParams.zip(trueValues).toMap)
    val syntheticValues =
      posterior.map(_.generator.repeat(config.syntheticSamples)).get
    println(syntheticValues.take(10))
    val model = posterior.flatMap { d =>
      d.fit(syntheticValues).map { _ =>
        priorParams
      }
    }
    val (samples, diag) =
      model.sampleWithDiagnostics(sampler,
                                  config.warmupIterations,
                                  config.iterations,
                                  config.chains)
    println(samples.take(10))
    val maxRHat = diag.map(_.rHat).max
    val minEffectiveSampleSize = diag.map(_.effectiveSampleSize).min
    println(diag)
    require(maxRHat < config.maxRHat)
    require(minEffectiveSampleSize > config.targetSampleSize)
    val thinned = thin(samples, config.targetSampleSize)
    trueValues.zipWithIndex.map {
      case (tv, i) =>
        rank(tv, thinned.map { s =>
          s(i)
        })
    }
  }

  private def rank(value: Double, list: Seq[Double]): Int =
    list.count { n =>
      n < value
    }

  private def thin(samples: Seq[Seq[Double]], target: Int): Seq[Seq[Double]] = {
    val every = Math.floor(samples.size.toDouble / target).toInt
    samples.sliding(1, every).map(_.head).toList.reverse.take(target)
  }
}

object SBC {
  def apply[T](priors: Seq[Continuous])(
      fn: Seq[Real] => Distribution[T]): SBC[T] = {
    val priorParams = priors.map(_.param)
    val priorGenerators = priors.map(_.generator)
    val posterior = RandomVariable.traverse(priorParams).map(fn)
    SBC(priorGenerators, priorParams.map(_.value), posterior)
  }

  def apply[T](prior: Continuous)(fn: Real => Distribution[T]): SBC[T] =
    apply(List(prior)) { l =>
      fn(l.head)
    }

  def chiSquared(bins: List[(Int, Int)]): Double = ???

  case class Config(warmupIterations: Int,
                    iterations: Int,
                    syntheticSamples: Int,
                    targetSampleSize: Int,
                    chains: Int,
                    repetitions: Int,
                    maxRHat: Double)
}
