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
package com.devcexx.libtrails.scala.lmath
import Linalg.Vectors._
import org.bukkit.{Location, World, util}
import com.devcexx.libtrails.scala.lmath.MathUtil.π
import scala.math.Numeric._
import scala.{specialized => spec}

/**
  * This object contains structures to work with vectors.
  */
object Linalg {

  /**
    * Object that contains methods to perform linear transformations between vectors
    */
  object LinearTransf {

    /**
      * Performs a linear traslation of the input vector.
      * @param offset the translation offset
      * @param vector the translated vector
      * @return the vector given, translated offset units.
      */
    def translate(offset: Vector[_])(vector: offset.VecDimT): offset.VecSelf = offset.add(vector)

    /**
      * Performs a scaling of the input vector.
      * @param factor the scaling factor. A scale factor of 1 means no change
      * @param vector the vector scaled.
      * @tparam A the numeric type of the input vector
      * @tparam B the type of the input vector (must be a floating vector)
      * @return The vector given, scaled factor times
      */
    def scale[A, B <: FloatingVector[A]](factor: A)(vector: B): B#VecSelf#VecSelf =
      vector.normalize(vector.n.times(vector.conv.fromDouble(vector.norm()), factor))

    /**
      * Rotates a vector in the bidimensional space.
      * @param angle the angle, in radians, that the vector will be rotated
      * @tparam A the numeric type of the input vector
      * @return a functor whose output is its input vector, rotated "angle" radians.
      */
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

    /**
      * Rotates a vector in the tridimensional space
      * @param axis the axis around the rotation will be done.
      * @param angle the angle, in radians, of the rotation that will be performed.
      * @tparam A the numeric type of the vector.
      * @return a functor whose output is its input vector, rotated "angle" radians around the "axis" axis
      */
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

    /**
      * Rotates the plane where the input vector are actually placed.
      * @param normal the normal of the new plane.
      * @tparam A the numeric type of the vector.
      * @return a functor whose output is its input vector, placed in the plane whose normal vector is the specified one
      */
    def rotateRenderPlane[A](normal: FloatingVector3[A]): FloatingVector3[A] => FloatingVector3[A] = {
      //Get normal vector on the XZ plane.
      val xzvector = normal.stripY()

      //Gets the rotation angle of the normal vector
      //around the y axis.
      val yAngle = normal.kind.axis_z.fullAngle(normal.kind.axis_y)(xzvector)

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

  /**
    * Object that contains companions, implicits and utils for working with vectors
    */
  object Vectors {

    import NumericConvertible._

    /**
      * Represents a Bidimensional vector whose each component is represented by an integer
      */
    type Vector2i = IntegralVector2[Int]

    /**
      * Represents a Bidimensional vector whose each component is represented by a long integer
      */
    type Vector2l = IntegralVector2[Long]

    /**
      * Represents a Bidimensional vector whose each component is represented by a simple floating point number
      */
    type Vector2f = FloatingVector2[Float]

    /**
      * Represents a Bidimensional vector whose each component is represented by a double floating point number
      */
    type Vector2d = FloatingVector2[Double]

    /**
      * Represents a Tridimensional vector whose each component is represented by an integer
      */
    type Vector3i = IntegralVector3[Int]

    /**
      * Represents a Tridimensional vector whose each component is represented by a long integer
      */
    type Vector3l = IntegralVector3[Long]

    /**
      * Represents a Tridimensional vector whose each component is represented by a simple floating point number
      */
    type Vector3f = FloatingVector3[Float]

    /**
      * Represents a Tridimensional vector whose each component is represented by a double floating point number
      */
    type Vector3d = FloatingVector3[Double]

    val Vector2i = IntIsVector2
    val Vector2l = LongIsVector2
    val Vector2f = FloatIsVector2
    val Vector2d = DoubleIsVector2

    val Vector3i = IntIsVector3
    val Vector3l = LongIsVector3
    val Vector3f = FloatIsVector3
    val Vector3d = DoubleIsVector3

    /**
      * Defines a vector-like type
      * @tparam A the numeric type of the vector
      */
    trait VectorType[A] {
      type Vec <: Vector[A]

      /**
        * The numeric instance for the current numeric type
        */
      val n: Numeric[A]

      /**
        * The object that allows the conversion from and to the current numeric type
        */
      val conv: NumericConvertible[A]
    }

    /**
      * Defines a bidimensional vector-like type
      * @tparam A the numeric type of the vector
      */
    trait Vector2Type[A] extends VectorType[A] {
      override type Vec <: Vector2[A]

      /**
        * The builder for the current vector type
        */
      val builder: (A, A) => Vec

      /**
        * The X-Axis of this vector type
        */
      lazy val axis_x = builder(n.one, n.zero)

      /**
        * The Y-Axis of this vector type
        */
      lazy val axis_y = builder(n.zero, n.one)

      /**
        * The space origin of this vector type
        */
      lazy val origin = builder(n.zero, n.zero)

      /**
        * Builds a new vector of the current type
        * @param x the x component of this vector
        * @param y the y component of this vector
        * @return a new vector with the specified components
        */
      def apply(x: A, y: A) = builder(x, y)
    }

    /**
      * Defines a tridimensional vector-like type
      * @tparam A the numeric type of the vector
      */
    trait Vector3Type[A] extends VectorType[A] {
      override type Vec <: Vector3[A]

      /**
        * The builder for the current vector type
        */
      val builder: (A, A, A) => Vec

      /**
        * The X-Axis of this vector type
        */
      lazy val axis_x = builder(n.one, n.zero, n.zero)

      /**
        * The Y-Axis of this vector type
        */
      lazy val axis_y = builder(n.zero, n.one, n.zero)

      /**
        * The Z-Axis of this vector type
        */
      lazy val axis_z = builder(n.zero, n.zero, n.one)

      /**
        * The space origin of this vector type
        */
      lazy val origin = builder(n.zero, n.zero, n.zero)

      /**
        * Builds a new vector of the current type
        * @param x the x component of this vector
        * @param y the y component of this vector
        * @param z the z component of this vector
        * @return a new vector with the specified components
        */
      def apply(x: A, y: A, z: A) = builder(x, y, z)
    }

    /**
      * Represents a bidimensional integral vector-like type
      * @tparam A the numeric type of the vector
      */
    trait Vector2IntegralType[A] extends Vector2Type[A] {
      override type Vec <: IntegralVector2[A]
      override val n: Integral[A]
    }

    /**
      * Represents a bidimensional floating vector-like type
      * @tparam A the numeric type of the vector
      */
    trait Vector2FloatingType[A] extends Vector2Type[A] {
      override type Vec <: FloatingVector2[A]

      override val n: Fractional[A]
    }

    /**
      * Represents a tridimensional integral vector-like type
      * @tparam A the numeric type of the vector
      */
    trait Vector3IntegralType[A] extends Vector3Type[A] {
      override type Vec <: IntegralVector3[A]

      override val n: Integral[A]
    }

    /**
      * Represents a tridimensional floating vector-like type
      * @tparam A the numeric type of the vector
      */
    trait Vector3FloatingType[A] extends Vector3Type[A] {
      override type Vec <: FloatingVector3[A]

      override val n: Fractional[A]
    }

    /**
      * A companion object for a bidimensional integer vector
      */
    implicit object IntIsVector2 extends Vector2IntegralType[Int] {
      final override type Vec = IntegralVector2[Int]
      override val n = IntIsIntegral
      override val conv = IntIsNumericConvertible
      override val builder = new IntegralVector2[Int](_, _)(this)
    }

    /**
      * A companion object for a bidimensional long vector
      */
    implicit object LongIsVector2 extends Vector2IntegralType[Long] {
      final override type Vec = IntegralVector2[Long]
      override val n = LongIsIntegral
      override val conv = LongIsNumericConvertible
      override val builder = new IntegralVector2[Long](_, _)(this)
    }

    /**
      * A companion object for a bidimensional float vector
      */
    implicit object FloatIsVector2 extends Vector2FloatingType[Float] {
      final override type Vec = FloatingVector2[Float]
      override val n = FloatIsFractional
      override val conv = FloatIsNumericConvertible
      override val builder = new FloatingVector2[Float](_, _)(this)
    }

    /**
      * A companion object for a bidimensional double vector
      */
    implicit object DoubleIsVector2 extends Vector2FloatingType[Double] {
      final override type Vec = FloatingVector2[Double]
      override val n = DoubleIsFractional
      override val conv = DoubleIsNumericConvertible
      override val builder = new FloatingVector2[Double](_, _)(this)
    }

    /**
      * A companion object for a tridimensional integer vector
      */
    implicit object IntIsVector3 extends Vector3IntegralType[Int] {
      final override type Vec = IntegralVector3[Int]
      override val n = IntIsIntegral
      override val conv = IntIsNumericConvertible
      override val builder = new IntegralVector3[Int](_, _, _)(this)
    }

    /**
      * A companion object for a tridimensional long vector
      */
    implicit object LongIsVector3 extends Vector3IntegralType[Long] {
      final override type Vec = IntegralVector3[Long]
      override val n = LongIsIntegral
      override val conv = LongIsNumericConvertible
      override val builder = new IntegralVector3[Long](_, _, _)(this)
    }

    /**
      * A companion object for a tridimensional float vector
      */
    implicit object FloatIsVector3 extends Vector3FloatingType[Float] {
      final override type Vec = FloatingVector3[Float]
      override val n = FloatIsFractional
      override val conv = FloatIsNumericConvertible
      override val builder = new FloatingVector3[Float](_, _, _)(this)
    }

    /**
      * A companion object for a tridimensional double vector
      */
    implicit object DoubleIsVector3 extends Vector3FloatingType[Double] {
      final override type Vec = FloatingVector3[Double]
      override val n = DoubleIsFractional
      override val conv = DoubleIsNumericConvertible
      override val builder = new FloatingVector3[Double](_, _, _)(this)
    }

    /**
      * Object that contains implicits to convert between vectors and tuples and vice versa
      */
    object VectorImplicits {

      /**
        * Defines a conversion between a pair and a bidimensional vector
        * @param tuple the input pair
        * @param t the vector type for the numeric type A
        * @tparam A the numeric type of the tuple
        * @return a vector built from the components of the input pair.
        */
      implicit def tuple2Vector2[A](tuple: (A, A))(implicit t: Vector2Type[A]) =
        t match {
          case t: Vector2IntegralType[A] => IntegralVector2(tuple._1, tuple._2)(t)
          case t: Vector2FloatingType[A] => FloatingVector2(tuple._1, tuple._2)(t)
          case _ => ???
        }

      /**
        * Defines a conversion between a 3-tuple to a tridimensional vector
        * @param tuple the input tuple
        * @param t the vector type for the numeric type A
        * @tparam A the numeric type of the tuple
        * @return a vector built from the components of the input pair.
        */
      implicit def tuple2Vector3[A](tuple: (A, A, A))(implicit t: Vector3Type[A]) =
        t match {
          case t: Vector3IntegralType[A] => IntegralVector3(tuple._1, tuple._2, tuple._3)(t)
          case t: Vector3FloatingType[A] => FloatingVector3(tuple._1, tuple._2, tuple._3)(t)
          case _ => ???
        }

      /**
        * Defines a conversion between a tridimensional vector to a 3-tuple
        * @param vec the input vector.
        * @tparam A the numeric type of the vector.
        * @return the tuple.
        */
      implicit def vector32Tuple3[A](vec: Vector3[A]): (A, A, A) = (vec.x, vec.y, vec.z)

      /**
        * Defines a conversion between a bidimensional vector to a pair
        * @param vec the input vector.
        * @tparam A the numeric type of the vector.
        * @return the pair.
        */
      implicit def vector22Tuple2[A](vec: Vector2[A]): (A, A) = (vec.x, vec.y)
    }
  }

  /**
    * Represents an element of an euclidean vector of a non-determined dimension space.
    * @tparam A the numeric type of the vector.
    */
  trait Vector[@spec(Int, Long, Double, Float) A] extends Traversable[A] {
    /**
      * The abstract type that first defines the dimensionality of the vector.
      */
    type VecDimT <: Vector[A]

    /**
      * The current vector type.
      */
    type VecSelf <: VecDimT

    /**
      * The VectorType object that defines the properties of the current type of the vector.
      */
    val kind: VectorType[A]

    /**
      * The numeric object that allows operations between numbers of the current numeric type of the vector.
      */
    val n = kind.n

    /**
      * The object that allows numeric conversions from and to the current numeric type.
      */
    val conv = kind.conv

    /**
      * Performs an add operation between the current vector and another.
      * @param o the other vector to add.
      * @return an instance of the current vector result of adding the current vector and the specified one.
      */
    def add(o: VecDimT): VecSelf

    /**
      * Performs a subtract operation between the current vector and another.
      * @param o the other vector to subtract to self.
      * @return an instalce of the current vector type result of subtracting the specified vector to the current.
      */
    def sub(o: VecDimT): VecSelf

    /**
      * Performs a multiplication of all the components of this vector by a scalar value.
      * @param k the scalar that will multiply all the components of this vector.
      * @return A new vector of the current type, with each component multiplied by k
      */
    def mul(k: A): VecSelf

    /**
      * Negates all the components of the current vector. This is equivalent to perform a multiplication by -1
      * @return A new vector of the current type with each component negated.
      */
    def neg(): VecSelf

    /**
      * Performs the dot product between the current vector and the given one.
      * @param o the other vector.
      * @return the result of the dot product.
      */
    def dot(o: VecDimT): A

    /**
      * Returns the norm squared of this vector. Because of this is an euclidean vector, this is equivalent to
      * calculate the dot product of this vector with itself.
      * @return The norm squared of the vector.
      */
    def normSq(): A

    /**
      * Returns the norm of this vector. Because of this is an euclidean vector, this is equivalent to calculate
      * the square root of the dot product of this vector with itself.
      * @return the norm of this vector.
      */
    def norm() = MathUtil.sqrt(kind.conv.toDouble(normSq()))

    /**
      * Returns the squared distance between this vector and another.
      * @param o the other vector.
      * @return the squared distance between the two vectors
      */
    def distanceSq(o: VecDimT): A = (this - o).normSq()

    /**
      * Returns the distance between this vector and another.
      * @param o the other vector.
      * @return the distance between the two vectors.
      */
    def distance(o: VecDimT) = (this - o).norm()

    /**
      * Returns the cosine of the angle between this vector and the specified one.
      * @param o the other vector.
      * @return
      */
    def cosine(o: VecDimT) = kind.conv.toDouble(dot(o)) / (norm() * o.norm())

    /**
      * Returns the angle, in radians, between the current vector and the specified one.
      * @param o the other vector.
      * @return the angle between the two vectors, between 0 and π.
      */
    def angle(o: VecDimT) = math.acos(cosine(o))

    /**
      * Definition of the add operator. Is equivalent to the add method.
      * @param o the other vector.
      * @return the addition of the current and other vector.
      */
    def +(o: VecDimT): VecSelf = add(o)

    /**
      * Definition of the multiplication operator. Is equivalent to the mul method.
      * @param k the multiplication factor.
      * @return the vector multiplied by k
      */
    def *(k: A): VecSelf = mul(k)

    /**
      * Defintion of the negation operator. Is equivalent to the neg method.
      * @return The current vector negated.
      */
    def unary_-(): VecSelf = neg()

    /**
      * Definition of the subtract operator. Is equivalent to the sub method.
      * @param o The other vector.
      * @return Self minus o
      */
    def -(o: VecDimT): VecSelf = sub(o)
  }

  /**
    * Represents a euclidean vector in a bidimensional vector space.
    * @tparam A the numeric type of the vector.
    */
  trait Vector2[A] extends Vector[A] {
    final override type VecDimT = Vector2[A]
    override type VecSelf <: Vector2[A]

    override val kind: Vector2Type[A]

    /**
      * The builder of the current vector type
      */
    val builder: (A, A) => VecSelf

    /**
      * The x component of this vector
      */
    val x: A

    /**
      * The y component of this vector
      */
    val y: A

    /**
      * Creates a copy of this vector with the specified x component value.
      * @param x The new x component value.
      * @return a copy of this vector with the specified x component value.
      */
    def x(x: A): VecSelf = builder(x, y)

    /**
      * Creates a copy of this vector with the specified y component value.
      * @param y The new y component value.
      * @return a copy of this vector with the specified y component value.
      */
    def y(y: A): VecSelf = builder(x, y)

    /**
      * Creates a copy of this vector with the x component set to zero.
      * @return a copy of this vector with the x component set to zero.
      */
    def stripX(): VecSelf = x(n.zero)

    /**
      * Creates a copy of this vector with the y component set to zero.
      * @return a copy of this vector with the y component set to zero.
      */
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

  /**
    * Represents a vector whose components are floating-point numbers.
    * @tparam A the numeric type of the vector.
    */
  trait FloatingVector[A] extends Vector[A] {
    /**
      * Normalize the current vector, creating a new one with the same direction of the current one with the
      * specified norm.
      * @param mod the new norm of the vector.
      * @return a new vector with the same direction of the current vector with the specified norm.
      */
    def normalize(mod: A): VecSelf#VecSelf = normalize().mul(mod)

    /**
      * Normalize the current vector, creating a new one with the same direction of the current one with the
      * norm set to one.
      * @return a new vector with the same direction of the current vector with the norm set to one.
      */
    def normalize(): VecSelf
  }

  /**
    * Represents a bidimensional vector whose components are integral-like numbers.
    * @param x the x component of the vector.
    * @param y the y component of the vector.
    * @param kind the bidimensional vector object for the numeric object A
    * @tparam A the numeric type of the vector.
    */
  final class IntegralVector2[A](override val x: A, override val y: A)
                                (implicit override val kind: Vector2IntegralType[A])
    extends Vector2[A] {
    override type VecSelf = IntegralVector2[A]

    override val n = kind.n
    override val builder = kind.builder
  }

  /**
    * Represents a bidimensional vector whose components are floating-like numbers.
    * @param x the x component of the vector.
    * @param y the y component of the vector.
    * @param kind the bidimensional vector object for the numeric object A
    * @tparam A the numeric type of the vector.
    */
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

  /**
    * Represents an euclidean vector in a tridimensional vector space.
    * @tparam A the numeric type of the vector.
    */
  trait Vector3[A] extends Vector[A] {
    final override type VecDimT = Vector3[A]
    override type VecSelf <: Vector3[A]

    override val kind: Vector3Type[A]
    /**
      * The builder of the current vector type
      */
    val builder: (A, A, A) => VecSelf

    /**
      * The x component of this vector
      */
    val x: A

    /**
      * The y component of this vector
      */
    val y: A

    /**
      * The z component of this vector
      */
    val z: A

    /**
      * Creates a copy of this vector with the specified x component value.
      * @param x The new x component value.
      * @return a copy of this vector with the specified x component value.
      */
    def x(x: A): VecSelf = builder(x, y, z)

    /**
      * Creates a copy of this vector with the specified y component value.
      * @param y The new y component value.
      * @return a copy of this vector with the specified y component value.
      */
    def y(y: A): VecSelf = builder(x, y, z)

    /**
      * Creates a copy of this vector with the specified z component value.
      * @param z The new z component value.
      * @return a copy of this vector with the specified z component value.
      */
    def z(z: A): VecSelf = builder(x, y, z)

    /**
      * Creates a copy of this vector with the x component set to zero.
      * @return a copy of this vector with the x component set to zero.
      */
    def stripX(): VecSelf = x(n.zero)

    /**
      * Creates a copy of this vector with the y component set to zero.
      * @return a copy of this vector with the y component set to zero.
      */
    def stripY(): VecSelf = y(n.zero)

    /**
      * Creates a copy of this vector with the z component set to zero.
      * @return a copy of this vector with the z component set to zero.
      */
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

    /**
      * Returns the mixed product of the current vector, where the right operand of the dot product is the current
      * vector, and the left one is the cross product between the given ones.
      * @param cross1 the left operand of the cross product.
      * @param cross2 the right operand of the first product.
      * @return the mixed product of the given vectors.
      */
    def mixed(cross1: Vector3[A], cross2: Vector3[A]): A = cross1.cross(cross2).dot(this)

    /**
      * Returns the cross product of the current vector, where the right operand is the given vector, and the left one
      * is the current vector.
      * @param o the right operand of the operation.
      * @return the cross product between the current vector and the given one.
      */
    def cross(o: Vector3[A]): VecSelf = builder(
      n.minus(n.times(y, o.z), n.times(o.y, z)),
      n.minus(n.times(z, o.x), n.times(o.z, x)),
      n.minus(n.times(x, o.y), n.times(o.x, y))
    )

    /**
      * Returns the full angle between the current vector and other, around the given axis
      * @param o the other vector.
      * @param normal the axis vector.
      * @return the angle between the current vector and the other one. The returned value is between 0 and 2π
      */
    def fullAngle(normal: Vector3[A])(o: Vector3[A]) = {
      val dot = this.dot(o)
      val det = normal.mixed(this, o)
      val angle = math.atan2(conv.toDouble(det), conv.toDouble(dot))
      if (angle > 0) angle
      else angle + 2 * Math.PI
    }

    override def foreach[U](f: (A) => U): Unit = {
      f(x)
      f(y)
      f(z)
    }

    /**
      * Definition of the cross product operator.
      * @param o the other vector.
      * @return the cross product of the current vector and the given one
      */
    def ×(o: Vector3[A]) = cross(o)

    /**
      * Builds a new Bukkit location object from the components of the current vector.
      * @param world the world of the new location object.
      * @return the new Location object.
      */
    def toBukkitLocation(world: World): Location = toBukkitLocation(world, 0f, 0f)

    /**
      * Builds a new Bukkit Location object from the components of the current vector.
      * @param world the world of the new location object.
      * @param yaw the yaw.
      * @param pitch the pitch.
      * @return the new Location object.
      */
    def toBukkitLocation(world: World, yaw: Float, pitch: Float): Location = new Location(world, kind.conv.toDouble(x),
      kind.conv.toDouble(y), kind.conv.toDouble(z), yaw, pitch)

    override def toString(): String = s"($x; $y; $z)"
  }

  /**
    * Represents a tridimensional vector whose components are integral-like numbers.
    * @param x the x component of the vector.
    * @param y the y component of the vector.
    * @param z the z component of the vector.
    * @param kind the tridimensional vector object for the numeric object A
    * @tparam A the numeric type of the vector.
    */
  final class IntegralVector3[A](override val x: A, override val y: A,
                                 override val z: A)
                                (implicit override val kind: Vector3IntegralType[A])
    extends Vector3[A] {

    override type VecSelf = IntegralVector3[A]

    override val n = kind.n
    override val builder = kind.builder
  }

  /**
    * Represents a tridimensional vector whose components are floating-like numbers.
    * @param x the x component of the vector.
    * @param y the y component of the vector.
    * @param z the z component of the vector.
    * @param kind the tridimensional vector object for the numeric object A
    * @tparam A the numeric type of the vector.
    */
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

  /**
    * Companion object of the IntegralVector2 class
    */
  object IntegralVector2 {

    /**
      * Creates a new bidimensional integral vector from the given coordinates.
      * @param x the x component.
      * @param y the y component.
      * @param kind the bidimensional vector object for the numeric object A
      * @tparam A the numeric type.
      * @return the built vector.
      */
    def apply[A](x: A, y: A)(implicit kind: Vector2IntegralType[A])
    = new IntegralVector2(x, y)(kind)
  }

  object FloatingVector2 {

    /**
      * Creates a new bidimensional floating vector from the given coordinates.
      * @param x the x component.
      * @param y the y component.
      * @param kind the bidimensional vector object for the numeric object A
      * @tparam A the numeric type.
      * @return the built vector.
      */
    def apply[A](x: A, y: A)(implicit kind: Vector2FloatingType[A])
    = new FloatingVector2(x, y)(kind)
  }

  object IntegralVector3 {

    /**
      * Creates a new tridimensional integral vector from the given coordinates.
      * @param x the x component.
      * @param y the y component.
      * @param z the z component.
      * @param kind the tridimensional vector object for the numeric object A
      * @tparam A the numeric type.
      * @return the built vector.
      */
    def apply[A](x: A, y: A, z: A)(implicit kind: Vector3IntegralType[A])
    = new IntegralVector3(x, y, z)(kind)

    /**
      * Creates a new tridimensional integral vector from the given Bukkit Location object.
      * @param loc the location object.
      * @param kind the tridimensional vector object for the numeric object A
      * @tparam A the numeric type.
      * @return the built vector.
      */
    def apply[A](loc: Location)(implicit kind: Vector3IntegralType[A])
    = new IntegralVector3[A](kind.conv.fromDouble(loc.getX), kind.conv.fromDouble(loc.getY), kind.conv.fromDouble(loc.getZ))(kind)

    /**
      * Creates a new tridimensional integral vector from the given Bukkit Vector.
      * @param loc the location object.
      * @param kind the tridimensional vector object for the numeric object A
      * @tparam A the numeric type.
      * @return the built vector.
      */
    def apply[A](loc: util.Vector)(implicit kind: Vector3IntegralType[A])
    = new IntegralVector3[A](kind.conv.fromDouble(loc.getX), kind.conv.fromDouble(loc.getY), kind.conv.fromDouble(loc.getZ))(kind)
  }

  object FloatingVector3 {

    /**
      * Creates a new tridimensional floating vector from the given coordinates.
      * @param x the x component.
      * @param y the y component.
      * @param z the z component.
      * @param kind the tridimensional vector object for the numeric object A
      * @tparam A the numeric type.
      * @return the built vector.
      */
    def apply[A](x: A, y: A, z: A)(implicit kind: Vector3FloatingType[A])
    = new FloatingVector3(x, y, z)(kind)

    /**
      * Creates a new tridimensional floating vector from the given Bukkit Location object.
      * @param loc the location object.
      * @param kind the tridimensional vector object for the numeric object A
      * @tparam A the numeric type.
      * @return the built vector.
      */
    def apply[A](loc: Location)(implicit kind: Vector3FloatingType[A])
    = new FloatingVector3[A](kind.conv.fromDouble(loc.getX), kind.conv.fromDouble(loc.getY), kind.conv.fromDouble(loc.getZ))(kind)

    /**
      * Creates a new tridimensional floating vector from the given Bukkit Vector.
      * @param loc the location object.
      * @param kind the tridimensional vector object for the numeric object A
      * @tparam A the numeric type.
      * @return the built vector.
      */
    def apply[A](loc: util.Vector)(implicit kind: Vector3FloatingType[A])
    = new FloatingVector3[A](kind.conv.fromDouble(loc.getX), kind.conv.fromDouble(loc.getY), kind.conv.fromDouble(loc.getZ))(kind)
  }
}