package com.nihirash.virtualmachine

object VirtualMachineApp {
  def main(args: Array[String]): Unit = {
    val file = args.headOption
    file match {
      case None => println("Usage: java -jar virtualmachine.jar somefile.bin")
      case Some(filename) => new VirtualMachine(MemoryReader(filename)).run()
    }
  }
}
