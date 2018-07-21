package com.nihirash.virtualmachine

import java.nio.file.{Files, Paths}

import scala.util.Try

object MemoryReader {
  def apply(fileName: String): Array[Byte] =
    Try {
      Files.readAllBytes(Paths.get(fileName))
    }.toOption.getOrElse {
      println(s"Can't open $fileName")
      Array[Byte](0xf)
    }
}
