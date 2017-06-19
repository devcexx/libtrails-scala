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

import com.devcexx.libtrails.scala.lmath.Linalg.Vector3
import com.devcexx.libtrails.scala.lmath.Linalg.Vectors.Vector3f

import scala.annotation.tailrec

/**
  * Object that contains util math functions thay may be used in other places of the library.
  */
object MathUtil {

  /**
    * Contains the Pi constant, which is the relation between the length of the arc of the circumference and its radius.
    */
  val π = math.Pi //Just because I can

  /**
    * Calculates the inverse square root of a number, using the fast inverse square root algorithm.
    * This method is described here <a href="https://en.wikipedia.org/wiki/Fast_inverse_square_root">https://en.wikipedia.org/wiki/Fast_inverse_square_root</a>
    * @param n the number of which inverse square root will be calculated.
    * @return the inverse square root of the given number.
    */
  def isqrt(n: Double) = {
    val xhalf = 0.5 * n
    var i = java.lang.Double.doubleToRawLongBits(n)
    i = 0x5FE6EB50C7B537AAL - (i >> 1)

    var r = java.lang.Double.longBitsToDouble(i)
    r = r * (1.5 - xhalf * r * r)
    r
  }

  /**
    * Fast calculates the inverse square root of the number, inverting the result of the isqrt function.
    */
  val sqrt = isqrt _ andThen 1.0./

  /**
    * Recursively calculates the Greatest Common Divisor using the Euclides Algorithm.
    * @param a the first number.
    * @param b the second number.
    * @return the gcd between the given two numbers.
    */
  def gcd(a: Int, b: Int) = __gcd(math.min(a, b), math.max(a, b))

  @tailrec
  private def __gcd(a: Int, b: Int): Int =
    b % a match {
      case 0 => a
      case r => __gcd(r, a)
    }

  /**
    * Returns the sin of an angle from its cosine. This method uses a known cosine to fast calculate its sine
    * using the trigonometric identity `cos2(x) + sin2(x) = 1` and uses the angle to calculate its sign.
    * @param cos the cosine of the angle.
    * @param angle the angle which cosine is the given one.
    * @return the sine of the angle.
    */
  def sinFromCos(cos: Double, angle: Double) = sqrt(1 - cos * cos) * ((angle % (2 * π)).compare(π) match {
    case 0 | 1 => -1
    case _ => 1
  })

  /**
    * Returns the sin of an angle from its cosine. This method uses a known cosine to fast calculate its sine
    * using the trigonometric identity `cos2(x) + sin2(x) = 1` and uses the angle to calculate its sign.
    * @param sin the cosine of the angle.
    * @param angle the angle which cosine is the given one.
    * @return the sine of the angle.
    */
  def cosFromSin(sin: Double, angle: Double) = {
    val mappedAngle = angle % (2 * π)
    sqrt(1 - sin * sin) * (if (mappedAngle > π / 2 && mappedAngle < 3 * π / 2) -1 else 1)
  }

  /**
    * Returns a stream of vectors with the infinte dots of the line defined by a director vector.
    * @param origin The begin point of the line.
    * @param director The vector that defines the direction of the line.
    * @param step The space between each dot.
    * @return A stream with the dots of the line.
    */
  def linePointsStream[A, B <: Vector3[A]](origin: B, director: B, step: A): Stream[B#VecSelf] =
    Stream.from(0).map(k => origin + (director * director.n.times(director.conv.fromInt(k), step)))

}
