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

  def translate[A <: Vector[A]](offset: A)(vector: A): A = offset

  def rotate2[A](angle: Double): FloatingVector2[A] => FloatingVector2[A] = {
    val cos = math.cos(angle)
    val sin = math.sin(angle)

    x =>
      val n = x.n
      val cos1 = x.conv.fromDouble(cos)
      val sin1 = x.conv.fromDouble(sin)
      x.builder(
        n.minus(n.times(x.x, cos1), n.times(x.y, sin1)),
        n.plus(n.times(x.x, sin1), n.times(x.y, cos1))
      )
  }

  def scale2[A](factor: A): FloatingVector2[A] => FloatingVector2[A] =
    x => x.normalize(x.n.times(x.conv.fromDouble(x.norm()), factor))

  def rotate3[A](axis: FloatingVector3[A], angle: Double): FloatingVector3[A] => FloatingVector3[A] = {
    val cos = math.cos(angle)
    val sin = math.sin(angle)
    val k = axis.normalize()

    v => {
      val n = v.n
      val cos1 = v.conv.fromDouble(cos)
      val sin1 = v.conv.fromDouble(sin)
      val cross = k × v
      val dot = n.times(k dot v, n.minus(n.one, cos1))
      v.builder(
        n.plus(n.plus(n.times(v.x, cos1), n.times(cross.x, sin1)), n.times(k.x, dot)),
        n.plus(n.plus(n.times(v.y, cos1), n.times(cross.y, sin1)), n.times(k.y, dot)),
        n.plus(n.plus(n.times(v.z, cos1), n.times(cross.z, sin1)), n.times(k.z, dot))
      )
    }
  }


}

object Vectors {
  type Vector2i = IntegralVector2[Int]
  type Vector2l = IntegralVector2[Long]
  type Vector2f = FloatingVector2[Float]
  type Vector2d = FloatingVector2[Double]

  type Vector3i = IntegralVector3[Int]
  type Vector3l = IntegralVector3[Long]
  type Vector3f = FloatingVector3[Float]
  type Vector3d = FloatingVector3[Double]

  object IntegralVector2 {
    def apply[A](x: A, y: A)(implicit n: Integral[A], conv: NumberConverter[A])
    = new IntegralVector2(x, y)(n, conv)
  }

  object FloatingVector2 {
    def apply[A](x: A, y: A)(implicit n: Fractional[A], conv: NumberConverter[A])
    = new FloatingVector2(x, y)(n, conv)
  }

  object IntegralVector3 {
    def apply[A](x: A, y: A, z: A)(implicit n: Integral[A], conv: NumberConverter[A])
    = new IntegralVector3(x, y, z)(n, conv)
  }

  object FloatingVector3 {
    def apply[A](x: A, y: A, z: A)(implicit n: Fractional[A], conv: NumberConverter[A])
    = new FloatingVector3(x, y, z)(n, conv)
  }
}

trait Vector[@spec(Int, Long, Double, Float) A] extends Traversable[A] {
  type VecGen <: Vector[A]
  type VecSelf <: VecGen

  val n: Numeric[A]
  val conv: NumberConverter[A]

  def add(o: VecGen): VecSelf
  def sub(o: VecGen): VecSelf
  def mul(k: A): VecSelf
  def neg(): VecSelf

  def dot(o: VecGen): A
  def normSq(): A
  def norm() = math.sqrt(n.toDouble(normSq()))

  def distanceSq(o: VecGen): A = (this - o).normSq()
  def cosine(o: VecGen) = n.toDouble(dot(o)) / (norm() * o.norm())
  def angle(o: VecGen) = math.acos(cosine(o))

  def +(o: VecGen): VecSelf = add(o)
  def *(k: A): VecSelf = mul(k)
  def unary_-(): VecSelf = neg()
  def -(o: VecGen): VecSelf = sub(o)
}

trait Vector2[A] extends Vector[A] {
  final override type VecGen = Vector2[A]
  override type VecSelf <: Vector2[A]
  val builder: (A, A) => VecSelf

  val x: A
  val y: A

  override def add(o: Vector2[A]) = builder(n.plus(x, o.x), n.plus(y, o.y))
  override def sub(o: Vector2[A]) = builder(n.minus(x, o.x), n.minus(y, o.y))
  override def mul(k: A) = builder(n.times(x, k), n.times(y, k))
  override def neg() = builder(n.negate(x), n.negate(y))
  override def normSq(): A = dot(this)
  override def dot(o: Vector2[A]): A = n.plus(n.times(this.x, o.x),
    n.times(this.y, o.y))

  override def foreach[U](f: (A) => U): Unit = {
    f(x)
    f(y)
  }
}

trait IntegralVector[A] extends Vector[A] {
  override val n: Integral[A]
}

trait FloatingVector[A] extends Vector[A] {
  override val n: Fractional[A]

  def normalize(mod: A): VecSelf = normalize().mul(mod).asInstanceOf[VecSelf]
  def normalize(): VecSelf
}

final class IntegralVector2[A](override val x: A, override val y: A)
                              (implicit numeric: Integral[A],
                               override val conv: NumberConverter[A])
  extends Vector2[A] with IntegralVector[A] {

  override type VecSelf = IntegralVector2[A]
  override val builder = new IntegralVector2(_, _)(numeric, conv)
  override val n: Integral[A] = numeric
}

final class FloatingVector2[A](override val x: A, override val y: A)
                              (implicit numeric: Fractional[A],
                           val conv: NumberConverter[A])
  extends Vector2[A] with FloatingVector[A] {

  override type VecSelf = FloatingVector2[A]
  override val n: Fractional[A] = numeric
  override val builder = new FloatingVector2(_, _)(numeric, conv)

  override def normalize(): FloatingVector2[A] = {
    val mod = conv.fromDouble(norm())
    builder(n.div(x, mod), n.div(y, mod))
  }
}

trait Vector3[A] extends Vector[A] {
  final override type VecGen = Vector3[A]
  override type VecSelf <: Vector3[A]
  val builder: (A, A, A) => VecSelf

  val x: A
  val y: A
  val z: A

  override def add(o: Vector3[A]): VecSelf = builder(n.plus(x, o.x),
    n.plus(y, o.y), n.plus(z, o.z))
  override def sub(o: Vector3[A]): VecSelf = builder(n.minus(x, o.x),
    n.minus(y, o.y), n.minus(z, o.z))
  override def mul(k: A): VecSelf = builder(n.times(k, x),
    n.times(k, y), n.times(k, z))
  override def neg(): VecSelf = builder(n.negate(x),
    n.negate(y), n.negate(z))
  override def dot(o: Vector3[A]): A = n.plus(n.plus(n.times(this.x, o.x),
    n.times(this.y, o.y)), n.times(this.z, o.z))
  override def normSq(): A = dot(this)

  def mixed(cross1: Vector3[A], cross2: Vector3[A]): A = cross1.cross(cross2).dot(this)

  def cross(o: Vector3[A]): VecSelf = builder(
    n.minus(n.times(y, o.z), n.times(o.y, z)),
    n.minus(n.times(z, o.x), n.times(o.z, x)),
    n.minus(n.times(x, o.y), n.times(o.x, y))
  )

  def fullAngle(o: VecGen, normal: VecGen) = {
    val dot = this.dot(o)
    val det = normal.mixed(this.asInstanceOf[VecGen], o)
    val angle = math.atan2(conv.toDouble(det), conv.toDouble(dot))
    if (angle > 0) angle
    else angle + 2 * Math.PI
  }

  override def foreach[U](f: (A) => U): Unit = {
    f(x)
    f(y)
    f(z)
  }

  def ×(o: Vector3[A]) = cross(o)
}

final class IntegralVector3[A](override val x: A, override val y: A,
                               override val z: A)
                              (implicit numeric: Integral[A],
                               override val conv: NumberConverter[A])
  extends Vector3[A] with IntegralVector[A] {

  override val builder: (A, A, A) => IntegralVector3[A] =
    new IntegralVector3(_, _, _)(numeric, conv)

  override type VecSelf = IntegralVector3[A]
  override val n: Integral[A] = numeric
}

final class FloatingVector3[A](override val x: A, override val y: A,
                               override val z: A)
                              (implicit numeric: Fractional[A],
                               override val conv: NumberConverter[A])
  extends Vector3[A] with FloatingVector[A] {

  override type VecSelf = FloatingVector3[A]
  override val builder: (A, A, A) => FloatingVector3[A] =
    new FloatingVector3(_, _, _)(numeric, conv)

  override val n: Fractional[A] = numeric


  override def normalize(): FloatingVector3[A] = {
    val mod = conv.fromDouble(norm())
    builder(n.div(x, mod), n.div(y, mod), n.div(z,mod))
  }
}