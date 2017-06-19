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

import com.devcexx.libtrails.scala.lmath.Linalg.{FloatingVector3, LinearTransf}
import com.devcexx.libtrails.scala.lmath.Linalg.Vectors.Vector3f
import org.bukkit.Bukkit
import org.bukkit.entity.Entity
import org.bukkit.plugin.Plugin
import org.bukkit.scheduler.BukkitTask

/**
  * Represents a trail which depends on the specified entity. the trail will be moved according to the specified entity
  * and will automatically stop when the entity is no longer valid.
  * @param plugin the plugin where will be registered the required tasks for the trail.
  * @param entity the entity that owns this trail.
  * @param directionMap a function that transforms the direction of the entity, to ensure the correct rendering of the
  *                     trail.
  * @param positionMap a function that transforms the absolute position of the entity, to ensure the correct rendering
  *                    of the trail.
  * @param trail the supplier that will hold this trail.
  * @param interval the ticker interval of this trail.
  */
class EntityTrail(plugin: Plugin, entity: Entity, directionMap: Vector3f => Vector3f, positionMap: Vector3f => Vector3f,
                  trail: ParticleSupplier, interval: Int) {

  var alive = 0
  var task: BukkitTask = _

  def this(plugin: Plugin, entity: Entity, trail: ParticleSupplier, interval: Int) {
    this(plugin, entity, EntityTrail.defaultDirectionMapFor(entity), identity, trail, interval)
  }

  /**
    * Runs the trail task with the specified interval. If it's already running, it has no effect.
    */
  def begin(): Unit = {
    if (task == null) {
      task = Bukkit.getScheduler.runTaskTimer(plugin, new Ticker(), 0, interval)
    }
  }

  /**
    * Stops the trail. if it was not running, it has no effect.
    */
  def stop(): Unit = {
    if (task != null) {
      task.cancel()
      task = null
    }
  }

  /**
    * Resets the ticker of the current trail.
    */
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