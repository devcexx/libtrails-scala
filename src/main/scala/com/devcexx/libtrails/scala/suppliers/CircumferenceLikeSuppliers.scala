/*
 *  This file is part of libtrails.
 *  libtrails is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  libtrails is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with libtrails.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.devcexx.libtrails.scala.suppliers

import com.devcexx.libtrails.scala.lmath.Linalg.Vectors._
import com.devcexx.libtrails.scala.lmath.MathUtil
import com.devcexx.libtrails.scala.lmath.MathUtil.π
import com.devcexx.libtrails.scala.{Particle, ParticleSupplier, SuppliedParticle}

import scala.collection.mutable.ArrayBuffer

/**
  * Object that contains methods to build a helix-based particle supplier.
  */
object HelixSupplier {
  /**
    * Builds a function that returns an ArrayBuffer, which particles describes the circumference with the given
    * parameters.
    * @param particle the particle of the circumference.
    * @param radius the radius of the circumference.
    * @param delta the distance, in radians, between each dot of the circumference.
    * @param offset the initial angle of the circumference, in radians.
    * @return A function that returns an ArrayBuffer, which particles describes the circumference with the given
    * parameters.
    */
  def mkCircumferenceBufferFun(particle: Particle, radius: Float, delta: Float, offset: Float) = () => {
    val n = math.round(2 * π / delta).toInt
    val array = new ArrayBuffer[SuppliedParticle](n)
    (for (i <- 0 until n) yield {
      val angle = offset + delta * i
      val cos = math.cos(angle)
      SuppliedParticle(particle, Vector3f(cos.toFloat, 0, MathUtil.sinFromCos(cos, angle).toFloat).mul(radius))
    }).copyToBuffer(array)
    array
  }

  /**
    * Builds a new supplier which particles describes an helix of the given parameters.
    * @param particle the particle of the helix.
    * @param radius the radius of the helix.
    * @param delta the distance, in radians, between each dot of the helix.
    * @param offset the initial angle of the helix, in radians.
    * @return a new supplier which particles describes an helix of the given parameters.
    */
  def apply(particle: Particle, radius: Float, delta: Float, offset: Float) = {
    lazy val shape = mkCircumferenceBufferFun(particle, radius, delta, offset)()
    new ParticleSupplier {
      override def supply(tick: Int): Traversable[SuppliedParticle] = Seq(shape(tick % shape.size))
    }
  }
}

/**
  * Object that allows to create a particle supplier that describes a circular shape.
  */
object CircumferenceSupplier {
  /**
    * Builds a Fixed Lazy supplier that describes a circular shape.
    * @param particle the particle of the circumference.
    * @param radius the radius of the circle.
    * @param delta the distance, in radians, between each dot of the circumference.
    * @param offset the initial angle of the circumference, in radians.
    * @param appearInterval the appear inteval of the circumference.
    * @return a Fixed Lazy supplier that describes a circular shape.
    */
  def apply(particle: Particle, radius: Float, delta: Float, offset: Float, appearInterval: Int) =
    FixedLazySupplier(HelixSupplier.mkCircumferenceBufferFun(particle, radius, delta, offset), appearInterval)
}

/**
  * Object that allows to create a particle supplier that describes a sinusoidal function.
  */
object SinusoidalSupplier {

  /**
    * Builds a function that returns an ArrayBuffer, which particles describes the sinusoidal wave with the given
    * parameters.
    * @param particle the particle of the sinusoidal function.
    * @param radius the radius of the sinusoidal function.
    * @param delta the distance, in radians, between each dot of the function.
    * @param offset the initial angle of the function, in radians.
    * @return a function that returns an ArrayBuffer, which particles describes the sinusoidal wave with the given
    * parameters.
    */
  def mkSinusoidalBufferFun(particle: Particle, radius: Float, delta: Float, offset: Float) = () => {
    val n = math.round(2 * π / delta).toInt
    val array = new ArrayBuffer[SuppliedParticle](n)
    (for (i <- 0 until n) yield
      SuppliedParticle(particle, Vector3f(0f, 0f, math.sin(offset + delta * i).toFloat).mul(radius))
    ).copyToBuffer(array)
    array
  }

  /**
    * Builds a new supplier which particles describes a sinusoidal function with the given parameters.
    * @param particle the particle of the sinusoidal function.
    * @param radius the radius of the sinusoidal function.
    * @param delta the distance, in radians, between each dot of the function.
    * @param offset the initial angle of the function, in radians.
    * @return a new supplier which particles describes a sinusoidal function with the given parameters.
    */
  def apply(particle: Particle, radius: Float, delta: Float, offset: Float) = {
    lazy val shape = mkSinusoidalBufferFun(particle, radius, delta, offset)()
    new ParticleSupplier {
      override def supply(tick: Int): Traversable[SuppliedParticle] = Seq(shape(tick % shape.size))
    }
  }
}
