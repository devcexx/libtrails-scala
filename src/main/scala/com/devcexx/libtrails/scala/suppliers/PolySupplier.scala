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
import com.devcexx.libtrails.scala.lmath.MathUtil._

/**
  * Object that allows to create a particle supplier that describes a 2D-polygon.
  */
object PolySupplier {
  /**
    * Builds a new Fixed Lazy Supplier which particles describe a 2D-polygon with the specified parameters.
    * This method allows to build polygons specifying custom jumps between the vertex of it. This allow to create
    * full drawn stars and other kind of shapes, not only polygons.
    * @param particle the particle of the polygon.
    * @param vertex the number of vertex (or the number of pieces in which will be divided the circumference
    *               circumscribed to the polygon)
    * @param jumps the distance of the vertex that will be connected with lines between them. 0 means that there's no
    *              jump between vertex, and each vertex will be connected with the one that is next to it. So, a value
    *              of zero in this parameter is the required to build a regular polygon. Specifying a value that
    *              gcd(vertex, jumps + 1) = jumps + 1, jumps != 0 will result in incomplete shapes, that will have
    *              less vertex than specified. Specifying a value that gcd(vertex, jumps + 1) = 1, jumps != 0 will
    *              result in more complex shapes because will require more lines to connect all of the vertex.
    *
    * @param radius the radius of the circumference circumscribed to the polygon
    * @param step the distance, in blocks, between each dot of the lines drawn.
    * @param angleOffset the angle where the first vertex will be placed.
    * @param appearInterval the appear interval of the shape.
    * @return a new Fixed Lazy Supplier which particles describe a 2D-polygon with the specified parameters.
    */
  def apply(particle: Particle, vertex: Int, jumps: Int, radius: Float, step: Float, angleOffset: Float, appearInterval: Int) =
    FixedLazySupplier(() => {
      val fullAngle = ((2.0 * π) / vertex) * (jumps + 1)
      val linesLength = Vector3f(radius - (radius * math.cos(fullAngle)).toFloat,
        (-radius * math.sin(fullAngle)).toFloat, 0f).norm()

      var pointsPerLine = math.round(linesLength / step).toInt

      // k * (j + 1) ≡ 0 (mod n) <=> k ≡ 0 (mod n / mcd(n, j + 1)), where
      // k ≡ line count; n ≡ vertex count; j ≡ jumps between vertex.
      val lineCount = vertex / MathUtil.gcd(vertex, jumps + 1)

      (for (i <- 0 until lineCount) yield {
        val originAngle = angleOffset + i * fullAngle
        val targetAngle = originAngle + fullAngle

        val ptFrom = Vector3f(radius * math.cos(originAngle).toFloat, 0f, radius * math.sin(originAngle).toFloat)
        val ptTo = Vector3f(radius * math.cos(targetAngle).toFloat, 0f, radius * math.sin(targetAngle).toFloat)

        MathUtil.linePointsStream(ptFrom, ptTo.sub(ptFrom).normalize(), step).take(pointsPerLine)
      }).flatten.map(SuppliedParticle(particle, _))

    }, appearInterval)
}
