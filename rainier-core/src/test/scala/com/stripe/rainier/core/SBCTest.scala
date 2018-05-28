package com.stripe.rainier.core

import com.stripe.rainier.sampler._
import org.scalatest.FunSuite

class SBCTest extends FunSuite {
  implicit val rng: RNG = RNG.default

  def check[T](description: String, sbc: SBC[T]): Unit = {
    println(description)
    List((Walkers(100), 10000), (HMC(5), 1000)).foreach {
      case (sampler, iterations) =>
        println((sampler, iterations))
        val pValue =
          sbc.check(sampler, SBC.Config(warmupIterations = iterations))
        test(s"SBC $description, sampler = $sampler") {
          assert(pValue >= 0.9)
        }
    }
  }

  check("Normal(x,x)", SBC(Normal(0, 1)) { x =>
    Normal(x, x)
  })
}



/*
import time, sys, random
def loading(count):
    all_progress = [0] * count
    sys.stdout.write("\n" * count) # Make sure we have space to draw the bars
    while any(x < 100 for x in all_progress):
        time.sleep(0.01)
        # Randomly increment one of our progress values
        unfinished = [(i, v) for (i, v) in enumerate(all_progress) if v < 100]
        index, _ = random.choice(unfinished)
        all_progress[index] += 1
        
        # Draw the progress bars
        sys.stdout.write(u"\u001b[1000D") # Move left
        sys.stdout.write(u"\u001b[" + str(count) + "A") # Move up
        for progress in all_progress: 
            width = progress / 4
            print "[" + "#" * width + " " * (25 - width) + "]"
        
loading()
*/