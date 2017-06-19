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
  * Object that allows to create a particle supplier that shows a star.
  */
object StarSupplier {
  /**
    * Builds a Fixed Lazy supplier which particles describes a star with the specified parameters.
    * @param particle the particle that will be shown.
    * @param vertex the number of vertex of the star.
    * @param step the distance, in blocks, between each dot of the star lines.
    * @param highRadius the radius of the circumference circumscribed in the star.
    * @param lowRadius the radius of the minimal circumference of the star.
    * @param angleStartOffset the position of the first vertex of the star, in radians.
    * @param appearInterval the appear interval of the shape.
    * @return a Fixed Lazy supplier which particles describes a star with the specified parameters.
    */
  def apply(particle: Particle, vertex: Int, step: Float, highRadius: Float, lowRadius: Float, angleStartOffset: Float, appearInterval: Int) =
    FixedLazySupplier(() => {
      val fullAngle = (2 * π) / vertex
      val midAngle = fullAngle / 2.0f
      val linesLength = Vector3f(highRadius - (lowRadius * math.cos(midAngle)).toFloat,
        (-lowRadius * math.sin(midAngle)).toFloat, 0f).norm()

      val pointsPerLine = math.round(linesLength / step).toInt

      (for (i <- 0 until vertex) yield {
        val highAngle = angleStartOffset + fullAngle * i
        val lowAngle = highAngle + midAngle
        val nextHighAngle = highAngle + fullAngle

        val ptFrom1 = Vector3f(highRadius * math.cos(highAngle).toFloat, 0f, highRadius * math.sin(highAngle).toFloat)
        val ptTo1 = Vector3f(lowRadius * math.cos(lowAngle).toFloat, 0f, lowRadius * math.sin(lowAngle).toFloat)

        val ptFrom2 = ptTo1
        val ptTo2 = Vector3f(highRadius * math.cos(nextHighAngle).toFloat, 0f, highRadius * math.sin(nextHighAngle).toFloat)

        MathUtil.linePointsStream(ptFrom1, ptTo1.sub(ptFrom1).normalize(), step).take(pointsPerLine) ++
        MathUtil.linePointsStream(ptFrom2, ptTo2.sub(ptFrom2).normalize(), step).take(pointsPerLine)
      }).flatten.map(SuppliedParticle(particle, _))

    }, appearInterval)
}
