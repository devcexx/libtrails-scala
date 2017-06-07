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
import scala.{specialized=>spec}

object linearTransf {
  def rotate2[A](angle: Double): FloatingVector2[A] => FloatingVector2[A] = {
    val cos = math.cos(angle)
    val sin = math.sin(angle)

    x =>
      val n = x.n
      val cos1 = x.conv(cos)
      val sin1 = x.conv(sin)
      x.builder(
        n.minus(n.times(x.x, cos1), n.times(x.y, sin1)),
        n.plus(n.times(x.x, sin1), n.times(x.y, cos1))
      )
  }
  def scale2[A](factor: A): FloatingVector2[A] => FloatingVector2[A] =
    x => x.normalize(x.n.times(x.conv(x.norm()), factor))
}

trait Vector[@spec(Int, Double, Float) A] extends Traversable[A] {
  type Vec <: Vector[A]
  val n: Numeric[A]

  def add(o: Vec): Vec
  def sub(o: Vec): Vec
  def mul(k: A): Vec
  def neg(): Vec

  def dot(o: Vec): A
  def normSq(): A = this.dot(this.asInstanceOf[Vec])
  def norm() = math.sqrt(n.toDouble(normSq()))

  def distanceSq(o: Vec): A = (this - o).normSq()
  def cosine(o: Vec) = n.toDouble(dot(o)) / (norm() * o.norm())
  def angle(o: Vec) = math.acos(cosine(o))

  def +(o: Vec): Vec = add(o)
  def *(k: A): Vec = mul(k)
  def unary_-(): Vec = neg()
  def -(o: Vec): Vec = sub(o)
}

trait Vector2[A] extends Vector[A] {
  override type Vec <: Vector2[A]
  val builder: (A, A) => Vec

  val x: A
  val y: A

  override def add(o: Vec) = builder(n.plus(x, o.x), n.plus(y, o.y))
  override def sub(o: Vec) = builder(n.minus(x, o.x), n.minus(y, o.y))
  override def mul(k: A) = builder(n.times(x, k), n.times(y, k))
  override def neg() = builder(n.negate(x), n.negate(y))
  override def dot(o: Vec): A = n.plus(n.times(this.x, o.x),
    n.times(this.y, o.y))

  override def foreach[U](f: (A) => U): Unit = {
    f(x)
    f(y)
  }
}

trait IntegralVector[A] extends Vector[A] {
  override type Vec <: IntegralVector[A]
  val n: Integral[A]
}

trait FloatingVector[A] extends Vector[A] {
  override type Vec <: FloatingVector[A]
  val n: Fractional[A]

  def normalize(mod: A): Vec = normalize().mul(mod).asInstanceOf[Vec]
  def normalize(): Vec
}

final class IntegralVector2[A](override val x: A, override val y: A)
                              (implicit numeric: Integral[A])
  extends Vector2[A] with IntegralVector[A] {

  override type Vec = IntegralVector2[A]
  override val builder = new IntegralVector2(_, _)(numeric)
  override val n: Integral[A] = numeric
}

final class FloatingVector2[A](override val x: A, override val y: A)
                              (implicit numeric: Fractional[A],
                           val conv: NumberConverter[Double, A])
  extends Vector2[A] with FloatingVector[A] {

  override type Vec = FloatingVector2[A]
  override val n: Fractional[A] = numeric
  override val builder = new FloatingVector2(_, _)(numeric, conv)

  override def normalize(): FloatingVector2[A] = {
    val mod = conv(norm())
    builder(n.div(x, mod), n.div(y, mod))
  }
}

trait Vector3[A] extends Vector[A] {

  val x: A
  val y: A
  val z: A

  val builder: (A, A, A) => Vec

  override type Vec <: Vector3[A]
  override def add(o: Vec): Vec = builder(n.plus(x, o.x),
    n.plus(y, o.y), n.plus(z, o.z))
  override def sub(o: Vec): Vec = builder(n.minus(x, o.x),
    n.minus(y, o.y), n.minus(z, o.z))
  override def mul(k: A): Vec = builder(n.times(k, x),
    n.times(k, y), n.times(k, z))
  override def neg(): Vec = builder(n.negate(x),
    n.negate(y), n.negate(z))
  override def dot(o: Vec): A = n.plus(n.plus(n.times(this.x, o.x),
    n.times(this.y, o.y)), n.times(this.z, o.z))
  override def foreach[U](f: (A) => U): Unit = {
    f(x)
    f(y)
    f(z)
  }
}

final class IntegralVector3[A](override val x: A, override val y: A,
                               override val z: A)
                              (implicit numeric: Integral[A],
                               val conv: NumberConverter[Double, A])
  extends Vector3[A] with IntegralVector[A] {

  override val builder: (A, A, A) => IntegralVector3[A] =
    new IntegralVector3(_, _, _)(numeric, conv)

  override type Vec = IntegralVector3[A]
  override val n: Integral[A] = numeric
}

final class FloatingVector3[A](override val x: A, override val y: A,
                               override val z: A)
                              (implicit numeric: Fractional[A],
                               val conv: NumberConverter[Double, A])
  extends Vector3[A] with FloatingVector[A] {

  override type Vec = FloatingVector3[A]
  override val builder: (A, A, A) => FloatingVector3[A] =
    new FloatingVector3(_, _, _)(numeric, conv)

  override val n: Fractional[A] = numeric


  override def normalize(): FloatingVector3[A] = {
    val mod = conv(norm())
    builder(n.div(x, mod), n.div(y, mod), n.div(z,mod))
  }
}