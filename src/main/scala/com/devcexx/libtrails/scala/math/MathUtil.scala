package com.devcexx.libtrails.scala.math

import com.devcexx.libtrails.scala.Particle
import com.devcexx.libtrails.scala.math.Linalg.Vectors.Vector3f
import com.devcexx.libtrails.scala.math.MathUtil.sqrt

import scala.annotation.tailrec

object MathUtil {

  val π = math.Pi //Just because I can

  def isqrt(n: Double) = {
    val xhalf = 0.5 * n
    var i = java.lang.Double.doubleToRawLongBits(n)
    i = 0x5FE6EB50C7B537AAL - (i >> 1)

    var r = java.lang.Double.longBitsToDouble(i)
    r = r * (1.5 - xhalf * r * r)
    r
  }
  val sqrt = isqrt _ andThen 1.0./

  @tailrec
  def gcd(a: Int, b: Int): Int =
    b % a match {
      case 0 => a
      case r => gcd(a, r)
    }

  def sinFromCos(cos: Double, angle: Double) = sqrt(1 - cos * cos) * (-(angle % (2 * π)).compare(π) match {
    case 0 | 1 => -1
    case _ => 1
  })

  def linePointsStream(p: Particle, origin: Vector3f, director: Vector3f, step: Float, n: Int): Stream[Vector3f] =
    (0 until n).toStream.map(k => origin + (director * k.toFloat))

}
