# Simple functional stack-based virtual machine

This is mine cofe-time project of functional stack-based virtual machine.

There will be simple compiler for it later.

Example application that's prints "Hello" included as "test.bin" file.

## Building

Clone repository and build project via `sbt "assembly"`. 

Resulting file will be in `target/scala-2.12/` folder.

## Usage

```$bash
$ java -jar virtualmachine.jar test.bin
```