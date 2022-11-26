# Table of Contents
- [1. What is it?](#1-what-is-it-)
- [2. Help](#2-help)
- [3. How to run?](#3-how-to-run-)
   + [1. Using the shell script (build.sh)](#1-using-the-shell-script--buildsh-)
   + [2. Manually.](#2-manually)
- [4. What it can do?](#4-what-it-can-do-)
- [5. Sample output](#5-sample-output)

# 1. What is it?
This is a lambda calculus type checker and evaluator. It is an F# implementation of the algorithms and concepts presented by [Emmanuel Chailloux
](https://www-apr.lip6.fr/~chaillou/) and [Romain Demangeon
](https://www-apr.lip6.fr/~demangeon/) in the
[TAS (Typage et Analyse Statique)](https://www-apr.lip6.fr/~chaillou/Public/enseignement/2022-2023/tas/) course at [Sorbonne University](https://www.sorbonne-universite.fr/).

# 2. Help

```
ml-core 1.0.0
Copyright (C) 2022 ml-core

  -f, --file           File path to the file to evaluate and infer. If no file is specified a mini-REPL will run
                       instead.

  -l, --include-lib    (Default: false) If enabeled, a library file containing some predefined functions (map, range
                       ...) will be loaded.

  -d, --debug          (Default: false) Displays alpha-conversion and detailed evaluations.

  --help               Display this help screen.

  --version            Display version information.
```



# 3. How to run?

### 1. Manually (Recommanded)
1. Install the dotnet SDK and runtime [here](https://dotnet.microsoft.com/fr-fr/download).
2. Go to the root folder (where the readme file is located).
3. Restore the projects :

    > dotnet restore

4. To run the REPL :
      
      - Go to the ml-core folder :
      
          > cd ml-core

      - run the project :
        
          > dotnet run -- [your flags] (refer to the help section above)
      - example :
        
          > dotnet run -- --include-lib --debug

        ![Sample 1](imgs/IMG%201.jpg)

5. To run the tests :
      
      - Go to the ml-tests folder :
      
          > cd ml-tests

      - run the tests :
        
          > dotnet test    

        ![Sample 2](imgs/IMG%202.jpg)

6. To run the web app :
      
      - Go to the ml-web folder :
      
          > cd ml-web

      - run the project :
        
          > dotnet run
        
        A web browser should open. If not check the console to find the url 😄
        
        ![Sample 3](imgs/IMG%203.jpg)

        Example :
         
        ![Sample 4](imgs/IMG%204.jpg)

### 2. Using the shell script (build.sh)
1. Install the dotnet SDK and runtime [here](https://dotnet.microsoft.com/fr-fr/download).
2. Go to the root folder (where the readme file is located).
3. Run the shell script using the source command (IMPORTANT).

    > source build.sh

    This will generate the binary under the out folder.

4. WITHOUT changing directory, run the REPL :
      
      > ml-core [your flags] (refer to the help section above)

    OR use a file

      > ml-core -f [your file path] [your flags] (refer to the help section above)

    there is already a program.zfs file defnied in the root folder, you can update it and run it using the following command :

      > ml-core -f program.zfs [your flags] (refer to the help section above)


    sample : 

      ![Sample 6](imgs/IMG%206.jpg)

# 4. What it can do?

Refer to the rapport.pdf file for more details !

# 5. Sample output

Here are some sample outputs with the REPL :

![Sample output](imgs/IMG%205.jpg)