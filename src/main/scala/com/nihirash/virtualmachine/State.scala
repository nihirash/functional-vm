package com.nihirash.virtualmachine

case class State(pc: Int = 0, stack: List[Byte] = List.empty, vars: Map[Byte, Byte] = Map.empty, memory: Array[Byte] = Array.empty, working: Boolean = true) {
  require(pc < 65536)
}
