package com.nihirash.virtualmachine

import org.scalatest.{FreeSpec, Matchers}

class MnemonicsSpec extends FreeSpec with Matchers  {
  "NOP" - {
    "will only increment pc" in {
      val state = State(pc = 0, memory = Array[Byte](0x0, 0x1, 0x2))
      assert(state.copy(pc = state.pc + 1) == Nop.eval(state))
    }
  }

  "Fetch" - {
    "will store value of variable to stack and increment pc on 2 bytes" in {
      val state = State(vars = Map[Byte, Byte]('a'.toByte -> 'b'.toByte), memory = Array[Byte](0x1, 'a', 0x0))
      assert(
        state.copy(stack = List[Byte]('b'), pc = state.pc + 2) == Fetch.eval(state)
      )
    }

    "will puts zero if variable is empty" in {
      val state = State(vars = Map[Byte, Byte]('c'.toByte -> 'b'.toByte), memory = Array[Byte](0x1, 'a', 0x0))
      assert(
        state.copy(stack = List[Byte](0), pc = state.pc + 2) == Fetch.eval(state)
      )
    }
  }

  "Store" - {
    "will save to variable stack's top value" in {
      val state = State(stack = List[Byte](0x1, 0x2, 0x3), memory = Array[Byte](0x2, 'a', 0x0))
      assert(
        state.copy(vars = Map('a'.toByte -> 0x1.toByte), pc = state.pc + 2) == Store.eval(state)
      )
    }

    "will store zero if stack is empty" in {
      val state = State(stack = List.empty, memory = Array[Byte](0x2, 'a', 0x0))
      assert(
        state.copy(vars = Map('a'.toByte -> 0x0.toByte), pc = state.pc + 2) == Store.eval(state)
      )
    }
  }

  "Push" - {
    "will enqueue element to stack" in {
      val state = State(memory = Array[Byte](0x3, 0x22, 0x0), stack = List[Byte](0x1, 0x2))
      assert(
        state.copy(stack = List[Byte](0x22, 0x1, 0x2), pc = state.pc + 2) == Push.eval(state)
      )
    }
  }

  "Pop" - {
    "will remove head element of stack" in {
      val state = State(memory = Array[Byte](0x4, 0x0), stack = List[Byte](0x1, 0x2))
      assert(
        state.copy(stack = List[Byte](0x2), pc = state.pc + 1) == Pop.eval(state)
      )
    }

    "will do nothing if stack is empty" in {
      val state = State(memory = Array[Byte](0x4, 0x0), stack = List.empty)
      assert(
        state.copy(pc = state.pc + 1) == Pop.eval(state)
      )
    }
  }

  "Add" - {
    "will sum top 2 elements from stack and store result to it" in {
      val state = State(memory = Array[Byte](0x5, 0x0), stack = List[Byte](0x1, 0x2))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x3)) == Add.eval(state)
      )
    }

    "will store in stack same value if stack contains one element" in {
      val state = State(memory = Array[Byte](0x5, 0x0), stack = List[Byte](0x33))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x33)) == Add.eval(state)
      )
    }

    "will push zero to stack if stack empty" in {
      val state = State(memory = Array[Byte](0x5, 0x0), stack = List.empty)
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x0)) == Add.eval(state)
      )
    }
  }

  "Sub" - {
    "will substract top 2 elements from stack and store result to it" in {
      val state = State(memory = Array[Byte](0x5, 0x0), stack = List[Byte](0x2, 0x2))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x0)) == Sub.eval(state)
      )
    }

    "will store in stack same value if stack contains one element" in {
      val state = State(memory = Array[Byte](0x6, 0x0), stack = List[Byte](0x33))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x33)) == Sub.eval(state)
      )
    }

    "will push zero to stack if stack empty" in {
      val state = State(memory = Array[Byte](0x6, 0x0), stack = List.empty)
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x0)) == Sub.eval(state)
      )
    }
  }

  "LT" - {
    "will store in stack 1 if first element of stack smaller than second" in {
      val state = State(memory = Array[Byte](0x7, 0x0), stack = List[Byte](0x1, 0x2))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x1, 0x1, 0x2)) == LT.eval(state)
      )
    }

    "will store in stack 0 if first element of stack is bigger than second" in {
      val state = State(memory = Array[Byte](0x7, 0x0), stack = List[Byte](0x3, 0x2))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x0, 0x3, 0x2)) == LT.eval(state)
      )
    }

    "will store 0 in stack if stack contains one element" in {
      val state = State(memory = Array[Byte](0x7, 0x0), stack = List[Byte](0x2))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x0, 0x2)) == LT.eval(state)
      )
    }
  }

  "JZ" - {
    "will do nothing if in stack's top not zero" in {
      val state = State(memory = Array[Byte](0x8, 0x0, 0x0), stack = List[Byte](0x2))
      assert(
        state.copy(pc = state.pc + 3) == JZ.eval(state)
      )
    }

    "will change pc to new value if in stack's top is zero" in {
      val state = State(memory = Array[Byte](0x8, 0x14.toByte, 0x08.toByte), stack = List[Byte](0x0))
      assert(
        state.copy(pc = 0x1408) == JZ.eval(state)
      )
    }
  }

  "JNZ" - {
    "will change pc to new value if stack's top not zero" in {
      val state = State(memory = Array[Byte](0x9, 0x0, 0x0), stack = List[Byte](0x2))
      assert(
        state.copy(pc = 0) == JNZ.eval(state)
      )
    }

    "will do nothing if stack's top value is zero" in {
      val state = State(memory = Array[Byte](0x9, 0x14.toByte, 0x08.toByte), stack = List[Byte](0x0))
      assert(
        state.copy(pc = state.pc + 3) == JNZ.eval(state)
      )
    }
  }

  "Jump" - {
    "will change pc to new value" in {
      val state = State(memory = Array[Byte](0xA.toByte, 0x14.toByte, 0x08.toByte), stack = List[Byte](0x10))
      assert(
        state.copy(pc = 0x1408) == Jump.eval(state)
      )
    }
  }

  "Eq" - {
    "will push 1 to stack if 2 top elements of stack are equals" in {
      val state = State(memory = Array[Byte](0xB, 0x0, 0x0), stack = List[Byte](0x1, 0x1))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x1, 0x1, 0x1)) == Eq.eval(state)
      )
    }

    "will push 0 to stack if 2 top elemnt of stack not a same" in {
      val state = State(memory = Array[Byte](0xB, 0x0, 0x0), stack = List[Byte](0x2, 0x1))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x0, 0x2, 0x1)) == Eq.eval(state)
      )
    }

    "will push 1 if stack is empty" in {
      val state = State(memory = Array[Byte](0xB, 0x0, 0x0), stack = List.empty)
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x1)) == Eq.eval(state)
      )
    }

    "will push 1 if stack contains one zero element" in {
      val state = State(memory = Array[Byte](0xB, 0x0, 0x0), stack = List[Byte](0x0))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x1, 0x0)) == Eq.eval(state)
      )
    }

    "will push 0 if stack contains one non zero element" in {
      val state = State(memory = Array[Byte](0xB, 0x0, 0x0), stack = List[Byte](0x1))
      assert(
        state.copy(pc = state.pc + 1, stack = List[Byte](0x0, 0x1)) == Eq.eval(state)
      )
    }
  }

  "Halt" - {
    "will stops execution" in {
      val state = State()
      assert(
        state.copy(working = false) == Halt.eval(state)
      )
    }
  }
}
