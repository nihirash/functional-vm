package com.nihirash.virtualmachine

import scala.util.Try

sealed trait Mnemonic {
  def eval(state: State): State
}

object Mnemonic {
  def apply(code: Byte): Mnemonic = code match {
    case 0x0 => Nop
    case 0x1 => Fetch
    case 0x2 => Store
    case 0x3 => Push
    case 0x4 => Pop
    case 0x5 => Add
    case 0x6 => Sub
    case 0x7 => LT
    case 0x8 => JZ
    case 0x9 => JNZ
    case 0xA => Jump
    case 0xB => Eq
    case 0xC => PutNum
    case 0xD => PutC
    case 0xf => Halt
    case _ => Nop
  }

  def getByte(state: State): Byte = Try { state.memory(state.pc + 1) }.toOption.getOrElse(0)

  def pop(state: State): (State, Byte) = {
    val value: Byte = state.stack.headOption.getOrElse(0)
    val tail: List[Byte] = if (state.stack.isEmpty) List.empty else state.stack.tail
    (state.copy(stack = tail), value)
  }

  def push(state: State, value: Byte): State = state.copy(stack = value :: state.stack)

  def getWord(state: State): Int =
    (getByte(state).toInt << 8)  + Try { state.memory(state.pc + 2).toInt }.toOption.getOrElse(0)
}

case object Nop extends Mnemonic {
  override def eval(state: State): State = state.copy(pc = state.pc + 1)
}

case object Fetch extends Mnemonic {
  override def eval(state: State): State = {
    val value: Byte = state.vars.getOrElse(Mnemonic.getByte(state), 0)
    Mnemonic.push(state, value).copy(pc = state.pc + 2)
  }
}

case object Store extends Mnemonic {
  override def eval(state: State): State = {
    val varName = Mnemonic.getByte(state)
    val (tempState, value) = Mnemonic.pop(state)
    val newVars = state.vars + (varName -> value)
    state.copy(pc = state.pc + 2, vars = newVars)
  }
}

case object Push extends Mnemonic {
  override def eval(state: State): State = Mnemonic.push(state, Mnemonic.getByte(state)).copy(pc = state.pc + 2)
}

case object Pop extends Mnemonic {
  override def eval(state: State): State = Mnemonic.pop(state)._1.copy(pc = state.pc + 1)
}

case object Add extends Mnemonic {
  override def eval(state: State): State = {
    val newStack: List[Byte] = state.stack match {
      case a :: b :: tail => (a.toInt + b.toInt).toByte :: tail
      case a :: Nil => List(a)
      case Nil => List(0)
    }

    state.copy(pc = state.pc + 1, stack = newStack)
  }
}

case object Sub extends Mnemonic {
  override def eval(state: State): State = {
    val newStack: List[Byte] = state.stack match {
      case a :: b :: tail => (a.toInt - b.toInt).toByte :: tail
      case a :: Nil => List(a)
      case Nil => List(0)
    }

    state.copy(pc = state.pc + 1, stack = newStack)
  }
}

case object LT extends Mnemonic {
  override def eval(state: State): State = {
    val (tempState, a) = Mnemonic.pop(state)
    val (tempState2, b) = Mnemonic.pop(tempState)
    val value: Byte = if (a < b) 1 else 0

    state.copy(pc = state.pc + 1, stack = value :: state.stack)
  }
}

case object JZ extends Mnemonic {
  override def eval(state: State): State = {
    val (tempState, byte) = Mnemonic.pop(state)
    if (byte == 0) {
      val addr = Mnemonic.getWord(state)
      state.copy(pc = addr)
    } else {
      state.copy(pc = state.pc + 3)
    }
  }
}

case object JNZ extends Mnemonic {
  override def eval(state: State): State = {
    val (tempState, byte) = Mnemonic.pop(state)
    if (byte != 0) {
      val addr = Mnemonic.getWord(state)
      state.copy(pc = addr)
    } else {
      state.copy(pc = state.pc + 3)
    }
  }
}

case object Jump extends Mnemonic {
  override def eval(state: State): State = {
    val addr = Mnemonic.getWord(state)
    state.copy(pc = addr)
  }
}

case object Eq extends Mnemonic {
  override def eval(state: State): State = {
    val (tempState, a) = Mnemonic.pop(state)
    val (tempState2, b) = Mnemonic.pop(tempState)
    val value: Byte = if (a == b) 1 else 0

    state.copy(pc = state.pc + 1, stack = value :: state.stack)
  }
}

case object PutNum extends Mnemonic {
  override def eval(state: State): State = {
    val (newState, byte) = Mnemonic.pop(state)
    printf("%d", byte)
    state.copy(pc = state.pc + 1)
  }
}

case object PutC extends Mnemonic {
  override def eval(state: State): State = {
    val (newState, byte) = Mnemonic.pop(state)
    printf("%c", byte)
    state.copy(pc = state.pc + 1)
  }
}

case object Halt extends Mnemonic {
  override def eval(state: State): State = state.copy(working = false)
}