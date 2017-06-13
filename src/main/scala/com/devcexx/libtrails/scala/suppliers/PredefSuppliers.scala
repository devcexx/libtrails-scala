package com.devcexx.libtrails.scala.suppliers

import com.devcexx.libtrails.scala.math.Linalg.Vectors._
import com.devcexx.libtrails.scala.math.MathUtil
import com.devcexx.libtrails.scala.math.MathUtil.π
import com.devcexx.libtrails.scala.{Particle, SuppliedParticle}

import scala.collection.mutable.ArrayBuffer

object HelixSupplier {
  def makeCircumferenceBufferFun(particle: Particle, radius: Float, delta: Float, offset: Float) = () => {
    val n = math.round(2 * π / delta).toInt
    val array = new ArrayBuffer[SuppliedParticle](n)
    for (i <- 0 until n) {
      val angle = offset + delta * i
      val cos = math.cos(angle)
      array.append(SuppliedParticle(particle, Vector3f(cos.toFloat, 0, MathUtil.sinFromCos(cos, angle).toFloat).mul(radius)))
    }
    array
  }

  def apply(particle: Particle, radius: Float, delta: Float, offset: Float, appearInterval: Int) = {
    new FixedLazySupplier(makeCircumferenceBufferFun(particle, radius, delta, offset), appearInterval) {
      override def supply(tick: Int): Traversable[SuppliedParticle] =
        Seq(shape.asInstanceOf[ArrayBuffer[SuppliedParticle]](tick % shape.size))
    }
  }
}

object CircumferenceSupplier {
  def apply(particle: Particle, radius: Float, delta: Float, offset: Float, appearInterval: Int) =
    FixedLazySupplier(HelixSupplier.makeCircumferenceBufferFun(particle, radius, delta, offset), appearInterval)
}