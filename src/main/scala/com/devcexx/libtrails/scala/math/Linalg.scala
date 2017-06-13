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
package com.devcexx.libtrails.scala.math
import Linalg.Vectors._
import org.bukkit.{Location, World, util}

import scala.math.Numeric._
import scala.{specialized => spec}

object Linalg {
  object LinearTransf {

    def translate(offset: Vector[_])(vector: offset.VecDimT): offset.VecSelf = offset.add(vector)
    def scale2[A](factor: A)(vector: FloatingVector2[A]): FloatingVector2[A] =
      vector.normalize(vector.n.times(vector.conv.fromDouble(vector.norm()), factor))

    def scale3[A](factor: A)(vector: FloatingVector3[A]): FloatingVector3[A] =
      vector.normalize(vector.n.times(vector.conv.fromDouble(vector.norm()), factor))

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

    def rotateRenderPlane[A](normal: FloatingVector3[A]): FloatingVector3[A] => FloatingVector3[A] = {
      //Get normal vector on the XZ plane.
      val xzvector = normal.stripY()

      //Gets the rotation angle of the normal vector
      //around the y axis.
      val yAngle = normal.kind.axis_z.fullAngle(xzvector, normal.kind.axis_y)

      //Gets the rotation axis of the transformation.
      val axis = normal.kind.axis_y × normal
      val angle = normal.angle(normal.kind.axis_y)

      //First rotation: rotates the point around the y axis
      //to place the drawing in front of the rotation axis, to avoid
      //unexpected rotations.

      //Second rotation: rotates the point around the axis of the
      //rotation between the Y axis and the new normal axis, to place
      //the drawing inside the plane.
      rotate3(axis, angle) compose rotate3(normal.kind.axis_y, yAngle + π)
    }
  }

  object Vectors {

    import NumericConvertible._

    type Vector2i = IntegralVector2[Int]
    type Vector2l = IntegralVector2[Long]
    type Vector2f = FloatingVector2[Float]
    type Vector2d = FloatingVector2[Double]

    type Vector3i = IntegralVector3[Int]
    type Vector3l = IntegralVector3[Long]
    type Vector3f = FloatingVector3[Float]
    type Vector3d = FloatingVector3[Double]

    val Vector2i = IntIsVector2
    val Vector2l = LongIsVector2
    val Vector2f = FloatIsVector2
    val Vector2d = DoubleIsVector2

    val Vector3i = IntIsVector3
    val Vector3l = LongIsVector3
    val Vector3f = FloatIsVector3
    val Vector3d = DoubleIsVector3

    trait VectorType[A] {
      type Vec <: Vector[A]

      val n: Numeric[A]
      val conv: NumericConvertible[A]
    }

    trait Vector2Type[A] extends VectorType[A] {
      override type Vec <: Vector2[A]
      val builder: (A, A) => Vec
      lazy val axis_x = builder(n.one, n.zero)
      lazy val axis_y = builder(n.zero, n.one)
      lazy val origin = builder(n.zero, n.zero)

      def apply(x: A, y: A) = builder(x, y)
    }

    trait Vector3Type[A] extends VectorType[A] {
      override type Vec <: Vector3[A]
      val builder: (A, A, A) => Vec
      lazy val axis_x = builder(n.one, n.zero, n.zero)
      lazy val axis_y = builder(n.zero, n.one, n.zero)
      lazy val axis_z = builder(n.zero, n.zero, n.one)
      lazy val origin = builder(n.zero, n.zero, n.zero)

      def apply(x: A, y: A, z: A) = builder(x, y, z)
    }

    trait Vector2IntegralType[A] extends Vector2Type[A] {
      override type Vec <: IntegralVector2[A]

      override val n: Integral[A]
      override val builder: (A, A) => Vec
    }

    trait Vector2FloatingType[A] extends Vector2Type[A] {
      override type Vec <: FloatingVector2[A]

      override val n: Fractional[A]
      override val builder: (A, A) => Vec
    }

    trait Vector3IntegralType[A] extends Vector3Type[A] {
      override type Vec <: IntegralVector3[A]

      override val n: Integral[A]
      override val builder: (A, A, A) => Vec
    }

    trait Vector3FloatingType[A] extends Vector3Type[A] {
      override type Vec <: FloatingVector3[A]

      override val n: Fractional[A]
      override val builder: (A, A, A) => Vec
    }

    implicit object IntIsVector2 extends Vector2IntegralType[Int] {
      final override type Vec = IntegralVector2[Int]
      override val n = IntIsIntegral
      override val conv = IntIsNumericConvertible
      override val builder = new IntegralVector2[Int](_, _)(this)
    }

    implicit object LongIsVector2 extends Vector2IntegralType[Long] {
      final override type Vec = IntegralVector2[Long]
      override val n = LongIsIntegral
      override val conv = LongIsNumericConvertible
      override val builder = new IntegralVector2[Long](_, _)(this)
    }

    implicit object FloatIsVector2 extends Vector2FloatingType[Float] {
      final override type Vec = FloatingVector2[Float]
      override val n = FloatIsFractional
      override val conv = FloatIsNumericConvertible
      override val builder = new FloatingVector2[Float](_, _)(this)
    }

    implicit object DoubleIsVector2 extends Vector2FloatingType[Double] {
      final override type Vec = FloatingVector2[Double]
      override val n = DoubleIsFractional
      override val conv = DoubleIsNumericConvertible
      override val builder = new FloatingVector2[Double](_, _)(this)
    }

    implicit object IntIsVector3 extends Vector3IntegralType[Int] {
      final override type Vec = IntegralVector3[Int]
      override val n = IntIsIntegral
      override val conv = IntIsNumericConvertible
      override val builder = new IntegralVector3[Int](_, _, _)(this)
    }

    implicit object LongIsVector3 extends Vector3IntegralType[Long] {
      final override type Vec = IntegralVector3[Long]
      override val n = LongIsIntegral
      override val conv = LongIsNumericConvertible
      override val builder = new IntegralVector3[Long](_, _, _)(this)
    }

    implicit object FloatIsVector3 extends Vector3FloatingType[Float] {
      final override type Vec = FloatingVector3[Float]
      override val n = FloatIsFractional
      override val conv = FloatIsNumericConvertible
      override val builder = new FloatingVector3[Float](_, _, _)(this)
    }

    implicit object DoubleIsVector3 extends Vector3FloatingType[Double] {
      final override type Vec = FloatingVector3[Double]
      override val n = DoubleIsFractional
      override val conv = DoubleIsNumericConvertible
      override val builder = new FloatingVector3[Double](_, _, _)(this)
    }

    object IntegralVector2 {
      def apply[A](x: A, y: A)(implicit kind: Vector2IntegralType[A])
      = new IntegralVector2(x, y)(kind)
    }

    object FloatingVector2 {
      def apply[A](x: A, y: A)(implicit kind: Vector2FloatingType[A])
      = new FloatingVector2(x, y)(kind)
    }

    object IntegralVector3 {
      def apply[A](x: A, y: A, z: A)(implicit kind: Vector3IntegralType[A])
      = new IntegralVector3(x, y, z)(kind)

      def apply[A](loc: Location)(implicit kind: Vector3IntegralType[A])
      = new IntegralVector3[A](kind.conv.fromDouble(loc.getX), kind.conv.fromDouble(loc.getY), kind.conv.fromDouble(loc.getZ))(kind)

      def apply[A](loc: util.Vector)(implicit kind: Vector3IntegralType[A])
      = new IntegralVector3[A](kind.conv.fromDouble(loc.getX), kind.conv.fromDouble(loc.getY), kind.conv.fromDouble(loc.getZ))(kind)
    }

    object FloatingVector3 {
      def apply[A](x: A, y: A, z: A)(implicit kind: Vector3FloatingType[A])
      = new FloatingVector3(x, y, z)(kind)

      def apply[A](loc: Location)(implicit kind: Vector3FloatingType[A])
      = new FloatingVector3[A](kind.conv.fromDouble(loc.getX), kind.conv.fromDouble(loc.getY), kind.conv.fromDouble(loc.getZ))(kind)

      def apply[A](loc: util.Vector)(implicit kind: Vector3FloatingType[A])
      = new FloatingVector3[A](kind.conv.fromDouble(loc.getX), kind.conv.fromDouble(loc.getY), kind.conv.fromDouble(loc.getZ))(kind)
    }

    object VectorImplicits {
      implicit def tuple2Vector2[A](tuple: (A, A))(implicit t: Vector2Type[A]) =
        t match {
          case t: Vector2IntegralType[A] => IntegralVector2(tuple._1, tuple._2)(t)
          case t: Vector2FloatingType[A] => FloatingVector2(tuple._1, tuple._2)(t)
          case _ => ???
        }

      implicit def tuple2Vector3[A](tuple: (A, A, A))(implicit t: Vector3Type[A]) =
        t match {
          case t: Vector3IntegralType[A] => IntegralVector3(tuple._1, tuple._2, tuple._3)(t)
          case t: Vector3FloatingType[A] => FloatingVector3(tuple._1, tuple._2, tuple._3)(t)
          case _ => ???
        }

      implicit def vector32Tuple3[A](vec: Vector3[A]): (A, A, A) = (vec.x, vec.y, vec.z)
      implicit def vector22Tuple2[A](vec: Vector2[A]): (A, A) = (vec.x, vec.y)
    }
  }

  trait Vector[@spec(Int, Long, Double, Float) A] extends Traversable[A] {
    type VecDimT <: Vector[A]
    type VecSelf <: VecDimT

    val kind: VectorType[A]
    val n = kind.n
    val conv = kind.conv

    def add(o: VecDimT): VecSelf
    def sub(o: VecDimT): VecSelf
    def mul(k: A): VecSelf
    def neg(): VecSelf

    def dot(o: VecDimT): A
    def normSq(): A
    def norm() = math.sqrt(kind.conv.toDouble(normSq()))

    def distanceSq(o: VecDimT): A = (this - o).normSq()
    def cosine(o: VecDimT) = kind.conv.toDouble(dot(o)) / (norm() * o.norm())
    def angle(o: VecDimT) = math.acos(cosine(o))

    def +(o: VecDimT): VecSelf = add(o)
    def *(k: A): VecSelf = mul(k)
    def unary_-(): VecSelf = neg()
    def -(o: VecDimT): VecSelf = sub(o)
  }

  trait Vector2[A] extends Vector[A] {
    final override type VecDimT = Vector2[A]
    override type VecSelf <: Vector2[A]

    override val kind: Vector2Type[A]
    val builder: (A, A) => VecSelf

    val x: A
    val y: A

    def x(x: A): VecSelf = builder(x, y)
    def y(y: A): VecSelf = builder(x, y)
    def stripX(): VecSelf = x(n.zero)
    def stripY(): VecSelf = y(n.zero)

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

    override def toString(): String = s"($x; $y)"
  }

  trait FloatingVector[A] extends Vector[A] {
    def normalize(mod: A): VecSelf = normalize().mul(mod).asInstanceOf[VecSelf]
    def normalize(): VecSelf
  }

  final class IntegralVector2[A](override val x: A, override val y: A)
                                (implicit override val kind: Vector2IntegralType[A])
    extends Vector2[A] {
    override type VecSelf = IntegralVector2[A]

    override val n = kind.n
    override val builder = kind.builder
  }

  final class FloatingVector2[A](override val x: A, override val y: A)
                                (implicit override val kind: Vector2FloatingType[A])
    extends Vector2[A] with FloatingVector[A] {

    override type VecSelf = FloatingVector2[A]

    override val n = kind.n
    override val builder = kind.builder

    override def normalize(): FloatingVector2[A] = {
      val mod = conv.fromDouble(norm())
      builder(n.div(x, mod), n.div(y, mod))
    }
  }

  trait Vector3[A] extends Vector[A] {
    final override type VecDimT = Vector3[A]
    override type VecSelf <: Vector3[A]

    override val kind: Vector3Type[A]
    val builder: (A, A, A) => VecSelf

    val x: A
    val y: A
    val z: A

    def x(x: A): VecSelf = builder(x, y, z)
    def y(y: A): VecSelf = builder(x, y, z)
    def z(z: A): VecSelf = builder(x, y, z)
    def stripX(): VecSelf = x(n.zero)
    def stripY(): VecSelf = y(n.zero)
    def stripZ(): VecSelf = z(n.zero)

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

    def fullAngle(o: VecDimT, normal: VecDimT) = {
      val dot = this.dot(o)
      val det = normal.mixed(this.asInstanceOf[VecDimT], o)
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

    def toBukkitLocation(world: World): Location = toBukkitLocation(world, 0f, 0f)
    def toBukkitLocation(world: World, yaw: Float, pitch: Float): Location = new Location(world, kind.conv.toDouble(x),
      kind.conv.toDouble(y), kind.conv.toDouble(z), yaw, pitch)

    override def toString(): String = s"($x; $y; $z)"
  }

  final class IntegralVector3[A](override val x: A, override val y: A,
                                 override val z: A)
                                (implicit override val kind: Vector3IntegralType[A])
    extends Vector3[A] {

    override type VecSelf = IntegralVector3[A]

    override val n = kind.n
    override val builder = kind.builder
  }

  final class FloatingVector3[A](override val x: A, override val y: A,
                                 override val z: A)
                                (implicit override val kind: Vector3FloatingType[A])
    extends Vector3[A] with FloatingVector[A] {

    override type VecSelf = FloatingVector3[A]

    override val n = kind.n
    override val builder = kind.builder

    override def normalize(): FloatingVector3[A] = {
      val mod = conv.fromDouble(norm())
      builder(n.div(x, mod), n.div(y, mod), n.div(z,mod))
    }
  }
}