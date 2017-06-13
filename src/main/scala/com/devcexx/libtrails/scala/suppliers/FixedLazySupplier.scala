package com.devcexx.libtrails.scala.suppliers

import com.devcexx.libtrails.scala.{ParticleSupplier, SuppliedParticle}
class FixedLazySupplier(val seqBuilder: () => Traversable[SuppliedParticle], val appearInterval: Int) extends ParticleSupplier {
  lazy val shape = seqBuilder()
  def supply(tick: Int) =
    if (tick % appearInterval == 0)
      shape
    else
      Seq()
}

object FixedLazySupplier {
  def apply(seqBuilder: () => Traversable[SuppliedParticle], appearInterval: Int) =
    new FixedLazySupplier(seqBuilder, appearInterval)
}