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

import com.devcexx.libtrails.scala.{ParticleSupplier, SuppliedParticle}

/**
  * Represents a particle supplier that has always the same representation for each tick, and its content is calculated
  * lazily the first time some part of the program access to it.
  * @param seqBuilder the function that builds the Traversable that will always be shown.
  * @param appearInterval the appearing interval of the particles of this instance.
  */
class FixedLazySupplier(val seqBuilder: () => Traversable[SuppliedParticle], val appearInterval: Int) extends ParticleSupplier {
  lazy val shape = seqBuilder()
  def supply(tick: Int) =
    if (tick % appearInterval == 0)
      shape
    else
      Seq()
}

/**
  * Companion object for the class FixedLazySupplier
  */
object FixedLazySupplier {
  /**
    * Creates a new lazy supplier from the given generation function, with an appear interval.
    * @param seqBuilder the function that will build the particle sequence.
    * @param appearInterval the appearing interval of the shapes.
    * @return a new build FixedLazySupplier
    */
  def apply(seqBuilder: () => Traversable[SuppliedParticle], appearInterval: Int) =
    new FixedLazySupplier(seqBuilder, appearInterval)
}