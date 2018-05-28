package com.stripe.rainier.core

import com.stripe.rainier.sampler._
import com.stripe.rainier.compute._

final case class SBC[T](priorGenerators: Seq[Generator[Double]],
                        priorParams: Seq[Real],
                        posterior: RandomVariable[(Distribution[T], Real)]) {
  require(priorParams.forall(_.variables.size == 1))
  val priorGenerator = Generator.traverse(priorGenerators)
  val emptyEvaluator = new Evaluator(Map.empty)

  def simulate(sampler: Sampler, config: SBC.Config)(
      implicit rng: RNG): Seq[Int] = {
    1.to(SBC.Repetitions).map { _ =>
      simulateOnce(sampler, config)
    }
  }

  def check(sampler: Sampler, config: SBC.Config)(implicit rng: RNG): Double = {
    val list = simulate(sampler, config)
    val binMap = list.groupBy(identity).mapValues(_.size)
    val bins = 0.until(SBC.Bins).map { b =>
      binMap.getOrElse(b, 0)
    }
    SBC.checkUniform(bins)
  }

  private def simulateOnce(sampler: Sampler, config: SBC.Config)(
      implicit rng: RNG): Int = {
    val trueValues = priorGenerator.get(rng, emptyEvaluator)
    implicit val trueEval = new Evaluator(priorParams.zip(trueValues).toMap)
    val trueOutput = posterior.map(_._2).get

    val syntheticValues =
      posterior.map(_._1.generator.repeat(config.syntheticSamples)).get
    val model = posterior.flatMap {
      case (d, r) =>
        d.fit(syntheticValues).map { _ =>
          r
        }
    }
    val (samples, diag) =
      model.sampleWithDiagnostics(sampler,
                                  config.warmupIterations,
                                  config.iterations,
                                  config.chains)
    val maxRHat = diag.map(_.rHat).max
    val minEffectiveSampleSize = diag.map(_.effectiveSampleSize).min
    val targetSampleSize = (SBC.Bins * config.binWidth) - 1
    assert(maxRHat < config.maxRHat)
    assert(minEffectiveSampleSize > targetSampleSize)
    val thinned = thin(samples, targetSampleSize)
    val rank = thinned.count { n =>
      n < trueOutput
    }
    rank / config.binWidth
  }

  private def thin(samples: Seq[Double], target: Int): Seq[Double] = {
    val every = Math.floor(samples.size.toDouble / target).toInt
    samples.sliding(1, every).map(_.head).toList.reverse.take(target)
  }
}

object SBC {
  def apply[T](priors: Seq[Continuous])(
      fn: Seq[Real] => (Distribution[T], Real)): SBC[T] = {
    val priorParams = priors.map(_.param)
    val priorGenerators = priors.map(_.generator)
    val posterior = RandomVariable.traverse(priorParams).map(fn)
    SBC(priorGenerators, priorParams.map(_.value), posterior)
  }

  def apply[T](prior: Continuous)(fn: Real => Distribution[T]): SBC[T] =
    apply(List(prior)) { l =>
      (fn(l.head), l.head)
    }

  val Bins = 5
  val Repetitions = 100
  //for 4 degrees of freedom
  val ChiSquared =
    List(
      (0.21, 0.995),
      (0.30, 0.99),
      (0.48, 0.975),
      (0.71, 0.95),
      (1.06, 0.9),
      (1.649, 0.8),
      (2.195, 0.7),
      (3.37, 0.5),
      (5.39, 0.25),
      (5.99, 0.2),
      (6.74, 0.15),
      (7.78, 0.1),
      (9.49, 0.05),
      (11.14, 0.025),
      (13.28, 0.01)
    )

  def checkUniform(bins: Seq[Int]): Double = {
    val total = bins.sum
    val expected = total.toDouble / bins.size
    val chiSquared = bins.map { n =>
      Math.pow(n - expected, 2)
    }.sum / expected
    ChiSquared.find(_._1 > chiSquared).map(_._2).getOrElse(0.005)
  }

  case class Config(warmupIterations: Int = 1000,
                    iterations: Int = 1000,
                    syntheticSamples: Int = 1000,
                    binWidth: Int = 20,
                    chains: Int = 4,
                    maxRHat: Double = 1.1)
}
