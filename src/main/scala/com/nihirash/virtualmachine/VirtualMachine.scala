package com.nihirash.virtualmachine

import scala.annotation.tailrec
import scala.util.Try

class VirtualMachine(memory: Array[Byte]) {
  val initialState = State(memory = memory)

  @tailrec
  final def run(state: State = initialState): State = {
    if (state.working) {
      val code: Byte = Try { state.memory(state.pc) }.toOption.getOrElse(0)
      run(Mnemonic(code).eval(state))
    } else {
      state
    }
  }
}
