package com.devcexx.libtrails.scala.suppliers

import com.devcexx.libtrails.scala.math.Linalg.Vectors._
import com.devcexx.libtrails.scala.math.MathUtil
import com.devcexx.libtrails.scala.math.MathUtil.π
import com.devcexx.libtrails.scala.{Particle, SuppliedParticle}

object CircumferenceSupplier {
  def apply(particle: Particle, radius: Float, delta: Float, offset: Float, appearInterval: Int) =
    FixedTraversableSupplier(() =>
      for (i <- 0 until math.round(2 * π / delta).toInt) yield {
        val angle = offset + delta * i
        val cos = math.cos(angle)
        SuppliedParticle(particle, Vector3f(cos.toFloat, 0, MathUtil.sinFromCos(cos, angle).toFloat).mul(radius))
      }, appearInterval)
}