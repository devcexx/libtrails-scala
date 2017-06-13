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

trait NumericConvertible[A] {
  def fromInt(n: Int): A
  def fromLong(n: Long): A
  def fromFloat(n: Float): A
  def fromDouble(n: Double): A
  def toInt(n: A): Int
  def toLong(n: A): Long
  def toFloat(n: A): Float
  def toDouble(n: A): Double
}

object NumericConvertible {
  implicit object IntIsNumericConvertible extends NumericConvertible[Int] {
    override def fromInt(n: Int) = n
    override def fromLong(n: Long) = n.toInt
    override def fromFloat(n: Float) = n.toInt
    override def fromDouble(n: Double) = n.toInt
    override def toInt(n: Int): Int = n
    override def toLong(n: Int): Long = n.toLong
    override def toFloat(n: Int): Float = n.toFloat
    override def toDouble(n: Int): Double = n.toDouble
  }
  implicit object LongIsNumericConvertible extends NumericConvertible[Long] {
    override def fromInt(n: Int) = n.toLong
    override def fromLong(n: Long) = n
    override def fromFloat(n: Float) = n.toLong
    override def fromDouble(n: Double) = n.toLong
    override def toInt(n: Long): Int = n.toInt
    override def toLong(n: Long): Long = n
    override def toFloat(n: Long): Float = n.toFloat
    override def toDouble(n: Long): Double = n.toDouble
  }
  implicit object FloatIsNumericConvertible extends NumericConvertible[Float] {
    override def fromInt(n: Int) = n.toFloat
    override def fromLong(n: Long) = n.toFloat
    override def fromFloat(n: Float) = n
    override def fromDouble(n: Double) = n.toFloat
    override def toInt(n: Float): Int = n.toInt
    override def toLong(n: Float): Long = n.toLong
    override def toFloat(n: Float): Float = n
    override def toDouble(n: Float): Double = n.toDouble
  }
  implicit object DoubleIsNumericConvertible extends NumericConvertible[Double] {
    override def fromInt(n: Int) = n.toDouble
    override def fromLong(n: Long) = n.toDouble
    override def fromFloat(n: Float) = n.toDouble
    override def fromDouble(n: Double) = n
    override def toInt(n: Double): Int = n.toInt
    override def toLong(n: Double): Long = n.toLong
    override def toFloat(n: Double): Float = n.toFloat
    override def toDouble(n: Double): Double = n
  }
}