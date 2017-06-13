package com.devcexx.libtrails.scala.suppliers

import com.devcexx.libtrails.scala.{ParticleSupplier, SuppliedParticle}
class FixedTraversableSupplier(seqBuilder: () => Traversable[SuppliedParticle], appearInterval: Int) extends ParticleSupplier {
  lazy val shape = seqBuilder()
  def supply(tick: Int) = shape
}

object FixedTraversableSupplier {
  def apply(seqBuilder: () => Traversable[SuppliedParticle], appearInterval: Int) =
    new FixedTraversableSupplier(seqBuilder, appearInterval)
}