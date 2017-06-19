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
package com.devcexx.libtrails.scala

import com.devcexx.libtrails.scala.lmath.ColorUtil.{Color, ColorHSV, ColorSpace, ConvertibleColorSpace, RGB}
import com.devcexx.libtrails.scala.lmath.Linalg.Vectors.Vector3f
import com.devcexx.libtrails.scala.lmath.Linalg.LinearTransf._
import com.devcexx.libtrails.scala.lmath.Linalg.Vector3
import org.bukkit.{Location, World}
import org.bukkit.entity.Player

/**
  * Represents a particle that may be ticked and spawned to the player.
  * @param effect the Minecraft effect of the particle.
  * @param id the id of the particle.
  * @param data the data of the particle.
  * @param off a function that maps the current tick of the particle with its offset.
  * @param count a function that maps the current tick of the particle with its count.
  * @param speed a function that maps the current tick of the particle with its speed.
  * @param radius the visibility radius of the particle.
  */
case class Particle(effect: Effect, id: Int, data: Int,
                    off: Int => Vector3f, count: Int => Int, speed: Int => Float, radius: Int) {

  /**
    * Spawns the particle to the specified player in a specified location, with the tick set to 0
    * @param p the target player.
    * @param loc the location of the particle.
    * @tparam A the numeric type of the location vector.
    */
  def spawn[A](p: Player, loc: Vector3[A]): Unit = spawn(p, loc, 0)

  /**
    * Spawns the particle to the specified player in a specified location, with the tick set to 0
    * @param p the target player.
    * @param x the x coordinate of the particle.
    * @param y the y coordinate of the particle.
    * @param z the z coordinate of the particle.
    */
  def spawn(p: Player, x: Float, y: Float, z: Float): Unit = spawn(p, x, y, z, 0)

  /**
    * Spawns the particle in the specified world in a specified location, with the tick set to 0
    * @param w the spawn world.
    * @param loc the location of the particle.
    * @tparam A he numeric type of the location vector.
    */
  def spawn[A](w: World, loc: Vector3[A]): Unit = spawn(w, loc, 0)

  /**
    * Spawns the particle in the specified world in a specified location, with the tick set to 0
    * @param w the spawn world.
    * @param x the x coordinate of the particle.
    * @param y the y coordinate of the particle.
    * @param z the z coordinate of the particle.
    */
  def spawn(w: World, x: Float, y: Float, z: Float): Unit = spawn(w, x, y, z, 0)

  /**
    * Spawns the particle to the specified player in a specified location.
    * @param p the target player.
    * @param loc the location of the particle.
    * @param tick the current tick of the particle, that may define most of its final properties.
    * @tparam A the numeric type of the location vector.
    */
  def spawn[A](p: Player, loc: Vector3[A], tick: Int): Unit = spawn0(p, loc.toBukkitLocation(p.getWorld), tick)

  /**
    * Spawns the particle to the specified player in a specified location.
    * @param p the target player.
    * @param x the x coordinate of the particle.
    * @param y the y coordinate of the particle.
    * @param z the z coordinate of the particle.
    * @param tick the current tick of the particle, that may define most of its final properties.
    */
  def spawn(p: Player, x: Float, y: Float, z: Float, tick: Int): Unit = spawn0(p, new Location(p.getWorld, x.toDouble, y.toDouble, z.toDouble), tick)

  /**
    * Spawns the particle in the specified world in a specified location.
    * @param w the spawn world.
    * @param loc the location of the particle.
    * @param tick the current tick of the particle, that may define most of its final properties.
    * @tparam A the numeric type of the location vector.
    */
  def spawn[A](w: World, loc: Vector3[A], tick: Int): Unit = spawn0(w, loc.toBukkitLocation(w), tick)

  /**
    * Spawns the particle in the specified world in a specified location.
    * @param w the spawn world.
    * @param x the x coordinate of the particle.
    * @param y the y coordinate of the particle.
    * @param z the z coordinate of the particle.
    * @param tick the current tick of the particle, that may define most of its final properties.
    */
  def spawn(w: World, x: Float, y: Float, z: Float, tick: Int): Unit = spawn0(w, new Location(w, x, y, z), tick)

  private def spawn0(o: Any, loc: Location, tick: Int) = {
    val voff = off(tick)
    o match {
      case player: Player => player.spigot().playEffect(loc,
        effect.bukkitEffect, id, data, voff.x, voff.y, voff.z, speed(tick), count(tick), radius)
      case world: World => world.spigot().playEffect(loc,
        effect.bukkitEffect, id, data, voff.x, voff.y, voff.z, speed(tick), count(tick), radius)
    }
  }
}

/**
  * Companion object of the Particle class
  */
object Particle {
  /**
    * Returns a vector with its components adapted to the specified RGB values, for colored particles.
    * @param r the r component. From 0 to 1
    * @param g the g component. From 0 to 1
    * @param b the b component. From 0 to 1
    * @return the requested vector.
    */
  def colorOffsetRGB(r: Float, g: Float, b: Float): Vector3f = {
    Vector3f(math.max(0.001f, r),math.max(0.001f, g),math.max(0.001f, b))
  }

  /**
    * Returns a vector with its components adapted to the specified RGB color, for colored particles.
    * @param color the color.
    * @param spaceConverter the color space converter that allows to convert from any color space to RGB.
    * @tparam CS the color space of the input particle.
    * @return the requested vector.
    */
  def colorOffset[CS <: ColorSpace](color: Color[CS])(implicit spaceConverter: ConvertibleColorSpace[CS, RGB]): Vector3f = {
    val rgb = spaceConverter.convert(color)
    colorOffsetRGB(rgb.data.r, rgb.data.g, rgb.data.b)
  }

  /**
    * Builds a colored particle with the specified parameters.
    * @param effect the effect of the particle.
    * @param visibilityRadius the visibility radius.
    * @param color a function that defines the color of the particle based on its tick.
    * @return the built particle.
    */
  def mkColorParticleWithOff(effect: EffectColorable, visibilityRadius: Int)(color: Int => Vector3f): Particle =
    Particle(effect, 0, 0, color, _ => 0, _ => 1.0f, visibilityRadius)

  /**
    * Builds a colored particle with the specified parameters.
    * @param effect the effect of the particle.
    * @param visibilityRadius the visibility radius.
    * @param color a function that defines the color of the particle based on its tick.
    * @param spaceConverter the color space converter that allows to convert from any color space to RGB.
    * @tparam CS the color space of the input color.
    * @return the built particle.
    */
  def mkColorParticleWithColor[CS <: ColorSpace](effect: EffectColorable, visibilityRadius: Int)(color: Int => Color[CS])
                                                (implicit spaceConverter: ConvertibleColorSpace[CS, RGB]): Particle =
    mkColorParticleWithOff(effect, visibilityRadius)(colorOffset[CS] _ compose color)

  /**
    * Builds a rainbow colored particle with the specified parameters.
    * @param effect the effect of the particle.
    * @param visibilityRadius the visibility radius.
    * @param initialColor the initial color of the particle.
    * @param ticksPerLoop the time, in ticks, that takes the color of the particle to change its hue value in 360. (a complete round)
    * @return the built particle.
    */
  def mkColorRainbowParticle(effect: EffectColorable, visibilityRadius: Int, initialColor: ColorHSV, ticksPerLoop: Int): Particle =
    mkColorParticleWithColor(effect, visibilityRadius)(t => {
      ColorHSV(initialColor.data.copy(h = (initialColor.data.h + t * (1.0f / ticksPerLoop) * 360) % 360))
    })
}

/**
  * Class that represents a particle placed in a specified point of the space.
  * @param particle the particle placed.
  * @param position the position of the particle.
  */
case class SuppliedParticle(particle: Particle, position: Vector3f)

/**
  * Represents an object capable to supply tick dependent particles.
  */
trait ParticleSupplier {
  /**
    * Retrieves the particles that should be shown in the specified tick.
    * @param tick the tick of the effect.
    * @return a Traversable object with the requested particles.
    */
  def supply(tick: Int): Traversable[SuppliedParticle]

  /**
    * Returns the current supplier applying the transform defined by the function f.
    * @param f a function that takes the tick value, and returns a function that maps every got particle sequence with
    *          another, depending on the initial tick value.
    * @return a particle supplier with the specified transformation.
    */
  def transformSupplier(f: Int => Traversable[SuppliedParticle] => Traversable[SuppliedParticle]) = new ParticleSupplier {
    override def supply(tick: Int) = f(tick)(ParticleSupplier.this.supply(tick))
  }

  /**
    * Combines or folds this particle supplier with another. This operation results on a particle supplier which supply
    * method is the result of appending the supply result of the first supplier with the second one.
    * @param other the other supplier.
    * @return a new particle supplier.
    */
  def combine(other: ParticleSupplier) = new ParticleSupplier {
    override def supply(tick: Int): Traversable[SuppliedParticle] = ParticleSupplier.this.supply(tick) ++ other.supply(tick)
  }

  /**
    * Returns the current supplier applying a transformation on every particle of the supplied.
    * @param f a function that takes the tick value and returns a function that maps every supplied particle with another,
    *          depending on the initial tick value.
    * @return a new particle supplier with the specified transformation.
    */
  def transformParticles(f: Int => SuppliedParticle => SuppliedParticle) = transformSupplier(t => _.map(f(t)))

  /**
    * Returns the current supplier applying a transformation on every particle vector of the supplied ones.
    * @param f a function that takes the tick value and returns a function that maps every supplied vector with another,
    *          depending on the initial tick value.
    * @return a new particle supplier with the specified transformation.
    */
  def transformVectors(f: Int => Vector3f => Vector3f) = transformSupplier(t => _.map(k => k.copy(position = f(t)(k.position))))

  /**
    * Returns the current supplier applying a translation transformation on every supplied particle.
    * @param f a function that takes the tick value and returns the offset of the translation.
    * @return a new particle supplier with the specified transformation.
    */
  def transformTranslate(f: Int => Vector3f) = transformVectors(t => translate(f(t)))

  /**
    * Returns the current supplier applying a scaling transformation on every supplied particle.
    * @param f a function that takes the tick value and returns the scale factor of the translation.
    * @return a new particle supplier with the specified transformation.
    */
  def transformScale(f: Int => Float) = transformVectors(scale[Float, Vector3f] _ compose f)

  /**
    * Returns the current supplier applying a rotation transformation on every supplied particle.
    * @param axis the axis around the rotation will be done.
    * @param f a function that takes the tick value and returns the rotation angle of the transformation.
    * @return a new particle supplier with the specified transformation.
    */
  def transformRotate(axis: Vector3f)(f: Int => Double) = transformVectors(t => rotate3(axis, f(t)))
}

/**
  * Companion object of the ParticleSupplier class
  */
object ParticleSupplier {
  /**
    * Holds the identity particle supplier: A particle supplier which each call to its supply methods returns an empty
    * sequence of particles.
    */
  val identity: ParticleSupplier = new ParticleSupplier {
    val r = Seq[SuppliedParticle]()
    override def supply(tick: Int): Traversable[SuppliedParticle] = r
  }
}