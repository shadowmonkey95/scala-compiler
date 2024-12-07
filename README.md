# Scala Hello World Project

Welcome to the Scala Hello World project! This project demonstrates a basic setup for running Scala code using Visual Studio Code and the command line.

---

## How to Install/Run Scala

### 1. Installation

To run Scala, you need to install **Coursier** first, then use it to install **Scala**. Follow the instructions based on your operating system:

- [Coursier Installation Guide](https://get-coursier.io/docs/cli-installation)
- [Scala Installation Guide](https://www.scala-lang.org/download/)

#### Example Installation on WSL:

1. Download and set up Coursier:
   ```bash
   $ curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs
   $ chmod +x cs
   $ ./cs setup
   ```

2. Verify Scala installation:
   ```bash
   $ scala -version
   ```

3. Add the Coursier bin directory to your PATH (remember to save and reload the .bashrc file)
    ```bash
    $ vim ~/.bashrc
    export PATH="$PATH:/root/.local/share/coursier/bin"
    ```

### 2. Running:

#### Run Directly Using `scala`

```bash
$ scala HelloWorld.scala
```

#### Compile and Run Using `scalac`

```bash
$ scalac HelloWorld.scala
$ scala HelloWorld
```
---
# Scala Code Parser

This code(scala_compiler.py) implements a parser for Scala code. The parser takes a list of tokens and processes them according to Scala's syntax to produce an Abstract Syntax Tree (AST). This parser is capable of handling various Scala constructs such as function definitions, variable declarations, string interpolations, pattern matching, collections, and more.

## Completed Tasks
### 1. Main function with annotation:
```@main def helloWorld(): Unit = {
  println("Hello, World!")
}
```
• Handles @main annotation, function definition, and unit return type.

### 2. String Interpolation:
```println(s"Hello, $name! You are $age years old.")```
• Parses string interpolation with variable insertion inside strings.

### 3. Variable Declaration:
```
val name: String = "Alice"
var age: Int = 25
```
• Parses val (immutable) and var (mutable) declarations with type annotations and initial values.

### 4. Pattern Matching
```
def describe(x: Any): String = x match {
  case 0 => "Number is zero"
  case 1 => "Number is one"
  case _ => "Unknown"
}
println(describe(1))
```
• Parses Scala's match expressions and case statements.

### 5. Collections and Mapping
```
val numbers = List(1, 2, 3, 4)
val doubled = numbers.map(_ * 2)
```
• Handles collections (e.g., List) and functional operations like map.

### 6. Imports:
```
import java.util.Date
val now = new Date()
println(now)
```
• Parses import statements and variable declarations with instantiated classes.

### 7. Traits and Inheritance:
```
trait Animal {
  def sound: String 
} 

trait Mammal {
  def hasFur: Boolean = true
} 

class Dog extends Animal with Mammal {
  def sound: String = "Woof"
}

val dog = new Dog
println(dog.sound)
println(dog.hasFur)
```
• Parses Scala's trait and class inheritance along with method definitions

### 8. Curried Function
```
def add(x: Int)(y: Int): Int = x + y 
val add5 = add(5)_ 
println(add5(10)) 
val add10 = add(10)_ 
println(add10(2))
```
• Handles curried functions, partially applied functions, and function invocations.

### 9. Generics and Lists
```
def printList[T](list: List[T]): Unit = {
  list.foreach(println)
}

val intList = List(1, 2, 3)
println(printList(intList))
val stringList = List("Scala", "Java", "Python")
println(printList(stringList))
```
• Handles generic functions and the printing of lists.
