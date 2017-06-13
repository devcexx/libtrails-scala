package com.devcexx.libtrails.scala

import com.devcexx.libtrails.scala.math.Linalg.Vectors.Vector3f
import com.devcexx.libtrails.scala.math.Linalg.LinearTransf._
import com.devcexx.libtrails.scala.math.Linalg.Vector3
import org.bukkit.{Location, World}
import org.bukkit.entity.Player

case class Particle(effect: Effect, id: Int, data: Int,
                    offx: Int => Float, offy: Int => Float, offz: Int => Float,
                    count: Int => Int, speed: Int => Float, radius: Int) {

  def spawn[A](p: Player, loc: Vector3[A]): Unit = spawn(p, loc, 0)
  def spawn(p: Player, x: Float, y: Float, z: Float): Unit = spawn(p, x, y, z, 0)
  def spawn[A](w: World, loc: Vector3[A]): Unit = spawn(w, loc, 0)
  def spawn(w: World, x: Float, y: Float, z: Float): Unit = spawn(w, x, y, z, 0)
  def spawn[A](p: Player, loc: Vector3[A], tick: Int): Unit = spawn0(p, loc.toBukkitLocation(p.getWorld), tick)
  def spawn(p: Player, x: Float, y: Float, z: Float, tick: Int): Unit = spawn0(p, new Location(p.getWorld, x.toDouble, y.toDouble, z.toDouble), tick)
  def spawn[A](w: World, loc: Vector3[A], tick: Int): Unit = spawn0(w, loc.toBukkitLocation(w), tick)
  def spawn(w: World, x: Float, y: Float, z: Float, tick: Int): Unit = spawn0(w, new Location(w, x, y, z), tick)

  private def spawn0(o: Any, loc: Location, tick: Int) = {
    o match {
      case player: Player => player.spigot().playEffect(loc,
        effect.bukkitEffect, id, data, offx(tick), offy(tick), offz(tick), speed(tick), count(tick), radius)
      case world: World => world.spigot().playEffect(loc,
        effect.bukkitEffect, id, data, offx(tick), offy(tick), offz(tick), speed(tick), count(tick), radius)
    }
  }
}

case class SuppliedParticle(particle: Particle, position: Vector3f)
trait ParticleSupplier {
  def supply(tick: Int): Traversable[SuppliedParticle]
  def transformSupplier(f: Int => Traversable[SuppliedParticle] => Traversable[SuppliedParticle]) = new ParticleSupplier {
    override def supply(tick: Int) = f(tick)(ParticleSupplier.this.supply(tick))
  }

  def transformParticles(f: Int => SuppliedParticle => SuppliedParticle) = transformSupplier(t => _.map(f(t)))
  def transformVectors(f: Int => Vector3f => Vector3f) = transformSupplier(t => _.map(k => k.copy(position = f(t)(k.position))))
  def transformTranslate(f: Int => Vector3f) = transformVectors(t => translate(f(t)))
  def transformScale(f: Int => Float) = transformVectors(scale3[Float] _ compose f)
  def transformRotate(axis: Vector3f)(f: Int => Double) = transformVectors(t => rotate3(axis, f(t)))
}