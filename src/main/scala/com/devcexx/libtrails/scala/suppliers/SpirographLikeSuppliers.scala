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

import com.devcexx.libtrails.scala.{Particle, SuppliedParticle}
import com.devcexx.libtrails.scala.lmath.Linalg.Vectors.Vector3f
import com.devcexx.libtrails.scala.lmath.MathUtil
import com.devcexx.libtrails.scala.lmath.MathUtil.π

/**
  * Trait that defines a type as a kind of Spirograph.
  */
trait SpirographKind

/**
  * Trait that defines an Hypotrochoid
  */
trait Hypotrochoid extends SpirographKind

/**
  * Trait that defines an Epitrochoid
  */
trait Epitrochoid extends SpirographKind

/**
  * Represents a Spirograph.
  * @tparam A the kind of the spirograph of the current type.
  */
trait Spirograph[A <: SpirographKind] {
  /**
    * Returns the vector corresponding to the specified theta of the spirograph with the specified
    * properties.
    * @param R the radius of the director circumference.
    * @param r the radius of the rolling circumference.
    * @param h the distance between the rolling circumference and the drawing point.
    * @param theta the current angle.
    * @return the required vector.
    */
  def mkVector(R: Float, r: Float, h: Float, theta: Float): Vector3f
}

/**
  * Object that allows to create a particle supplier that shows a spirograph
  */
object SpirographSupplier {

  /**
    * Implicit object that defines the behaviour of an Epitrochoid as a Spirograph
    */
  implicit object EpitrochoidIsSpirograph extends Spirograph[Epitrochoid] {
    override def mkVector(R: Float, r: Float, h: Float, theta: Float) = Vector3f(
      (R + r) * math.cos(theta).toFloat - h * math.cos(theta * (R + r) / r).toFloat,
      0f,
      (R + r) * math.sin(theta).toFloat - h * math.sin(theta * (R + r) / r).toFloat
    )
  }

  /**
    * Implicit object that defines the behaviour of an Hypotrochoid as a Spirograph
    */
  implicit object HypotrochoidIsSpirograph extends Spirograph[Hypotrochoid] {
    override def mkVector(R: Float, r: Float, h: Float, theta: Float) = Vector3f(
      (R - r) * math.cos(theta).toFloat + h * math.cos(theta * (R - r) / r).toFloat,
      0f,
      (R - r) * Math.sin(theta).toFloat - h * Math.sin(theta * (R - r) / r).toFloat
    )
  }

  /**
    * Builds a Fixed lazy supplier that describes a spirograph.
    * @param particle the particle to be shown.
    * @param delta the distance between each particle, in radians.
    * @param directorRadius the radius of the director circumference.
    * @param rollingRadius the radius of the rolling circumference.
    * @param hdist the distance between the rolling circumference and the drawing point.
    * @param appearingInterval the appear interval, in ticks.
    * @param spirograph the implicit object that defines how is drawn the specified type of spirograph.
    * @tparam A The kind of spirograph that will be drawn. Should be either Hypotrochoid or Epitrochoid
    * @return a Fixed lazy supplier that describes the specified type of spirograph.
    */
  def apply[A <: SpirographKind](particle: Particle, delta: Float, directorRadius: Float, rollingRadius: Float,
                                 hdist: Float, appearingInterval: Int)(implicit spirograph: Spirograph[A]) =
    FixedLazySupplier(() => {

      //Two decimal precision
      val fixedDirectorRadius = (math.floor(directorRadius * 100.0f) / 100.0f).toFloat
      val fixedRollingRadius = (math.floor(rollingRadius * 100.0f) / 100.0f).toFloat

      val period = math.ceil(directorRadius / (MathUtil.gcd((directorRadius * 100).toInt,
        (rollingRadius * 100).toInt) / 100.0f)).toInt

      val finalAngle = period * (fixedRollingRadius / fixedDirectorRadius) * 2.0f * π
      val steps = math.round(finalAngle / delta).toInt

      for (i <- 0 until steps) yield {
        SuppliedParticle(particle,
          spirograph.mkVector(directorRadius, rollingRadius, hdist, i * delta))
      }

    }, appearingInterval)
}