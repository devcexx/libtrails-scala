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

/**
  * Object that defines a method to create a linear particle supplier.
  */
object LinearSupplier {

  /**
    * Creates a new FixedLazySupplier with a linear shape.
    * @param particle the particle to be shown.
    * @param offset the offset from the supplier origin where the particle will be located.
    * @param appearInterval the appear interval of the particles.
    * @return a new FixedLazySupplier.
    */
  def apply(particle: Particle, offset: Vector3f, appearInterval: Int) =
    FixedLazySupplier(() => {
      Seq(SuppliedParticle(particle, offset))
    }, appearInterval)
}
