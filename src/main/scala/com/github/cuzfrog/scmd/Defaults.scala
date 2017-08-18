package com.github.cuzfrog.scmd

private object Defaults {
  val isMandatory: Boolean = false
  val priorMatchName: Boolean = false
  /** Only when distance < arg * factor, the correction takes effect.*/
  val levenshteinDistanceThresholdFactor: Double = 0.5
}
