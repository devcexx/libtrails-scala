package com.devcexx.libtrails.scala

import com.devcexx.libtrails.scala.math.Linalg.LinearTransf
import com.devcexx.libtrails.scala.math.Linalg.Vectors.{FloatingVector3, Vector3f}
import org.bukkit.Bukkit
import org.bukkit.entity.Entity
import org.bukkit.plugin.Plugin
import org.bukkit.scheduler.BukkitTask

class EntityTrail(plugin: Plugin, entity: Entity, directionMap: Vector3f => Vector3f, positionMap: Vector3f => Vector3f,
                  trail: ParticleSupplier, interval: Int) {

  var alive = 0
  var task: BukkitTask = _

  def this(plugin: Plugin, entity: Entity, trail: ParticleSupplier, interval: Int) {
    this(plugin, entity, EntityTrail.defaultDirectionMapFor(entity), identity, trail, interval)
  }

  def begin(): Unit = {
    if (task == null) {
      task = Bukkit.getScheduler.runTaskTimer(plugin, new Ticker(), 0, interval)
    }
  }

  def stop(): Unit = {
    if (task != null) {
      task.cancel()
      task = null
    }
  }

  def reset(): Unit = {
    alive = 0
  }

  class Ticker extends Runnable {
    override def run(): Unit = {
      if (entity.isDead || !entity.isValid) {
        stop()
      } else {
        val direction = FloatingVector3[Float](entity.getLocation.getDirection)
        val position = FloatingVector3[Float](entity.getLocation)
        trail.transformVectors(_ =>
          LinearTransf.translate(positionMap(position)) _ compose LinearTransf.rotateRenderPlane(directionMap(direction)))
          .supply(alive).foreach(p => p.particle.spawn(entity.getWorld, p.position, alive))
        alive += interval
      }
    }
  }
}

private object EntityTrail {
  import org.bukkit.entity.Arrow
  def defaultDirectionMapFor(e: Entity): Vector3f => Vector3f = {
    e match {
      case _: Arrow => v => Vector3f(-1 * v.x, -1 * v.y, v.z)
      case _ => identity
    }
  }
}