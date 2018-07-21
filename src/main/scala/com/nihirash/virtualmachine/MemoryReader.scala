package com.nihirash.virtualmachine

import java.nio.file.{Files, Paths}

object MemoryReader {
  def apply(fileName: String): Array[Byte] =
    Files.readAllBytes(Paths.get(fileName))
}
