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