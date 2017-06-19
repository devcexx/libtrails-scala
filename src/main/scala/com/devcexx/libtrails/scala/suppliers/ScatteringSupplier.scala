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

import com.devcexx.libtrails.scala.{Particle, ParticleSupplier, SuppliedParticle}
import com.devcexx.libtrails.scala.lmath.Linalg.Vectors.Vector3f

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Represents a particle supplier that, in the specified interval will show random particles placed inside a defined
  * area.
  * @param particles the possible particles that might be shown.
  * @param spreadSpace the space where particles might appear.
  * @param spreadSpaceOffset the offset from the supplier origin where is placed the spread space.
  * @param minParticles the minimum number of particles that will appear in each supply operation.
  * @param maxParticles the maximum number of particles that may appear in each supply operation.
  * @param appearInterval the appear interval, in ticks.
  */
class ScatteringSupplier(particles: ArrayBuffer[Particle], spreadSpace: Vector3f, spreadSpaceOffset: Vector3f,
                         minParticles: Int, maxParticles: Int, appearInterval: Int) extends ParticleSupplier {

  override def supply(tick: Int): Traversable[SuppliedParticle] =
    if (tick % appearInterval == 0)
      (0 until Random.nextInt(maxParticles - minParticles) + minParticles + 1).toStream.map(_ =>
        SuppliedParticle(particles(Random.nextInt(particles.length)),
          spreadSpaceOffset.add(
            Vector3f(
              (spreadSpace.x * Random.nextGaussian()).toFloat,
              (spreadSpace.y * Random.nextGaussian()).toFloat,
              (spreadSpace.z * Random.nextGaussian()).toFloat
            ))))
    else Seq()
}

/**
  * Companion object for the ScatteringSupplier class.
  */
object ScatteringSupplier {

  /**
    * Creates a new Scattering Supplier with the given parameters.
    * @param particle the possible particles that might be shown.
    * @param spreadSpace the space where particles might appear.
    * @param spreadSpaceOffset the offset from the supplier origin where is placed the spread space.
    * @param minParticles the minimum number of particles that will appear in each supply operation.
    * @param maxParticles the maximum number of particles that may appear in each supply operation.
    * @param appearInterval the appear interval, in ticks.
    */
  def apply(particle: Traversable[Particle], spreadSpace: Vector3f, spreadSpaceOffset: Vector3f,
  minParticles: Int, maxParticles: Int, appearInterval: Int) = {
    val buffer = new ArrayBuffer[Particle](particle.size)
    particle.copyToBuffer(buffer)
    new ScatteringSupplier(buffer, spreadSpace, spreadSpaceOffset, minParticles, maxParticles, appearInterval)
  }
}
