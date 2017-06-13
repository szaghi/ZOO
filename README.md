<a name="top"></a>

# ZOO

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

> ZOO, **Z**aghi f**O**rtran c**O**llection, where my wild Fortran pets will survive

> please, do not feed the animals :smile:

---

| [What is ZOO?](#what-is-zoo) | [Copyrights](#copyrights) | [Install ZOO pets](#install-zoo-pets) |

---

### What is ZOO?

This my *ZOO* where you can see (and take) my *wild Fortran pets*, namely this a centralized place where you can get some of my (poor) Fortran libraries that live in their own repository. The aim is to facilitate end-users that are bothered to deal with my *extreme KISS* approach that atomizes my projects. My Fortran projects are generally KISS (Keep It Simple and Stupid) thus they are designed to complete one or two simple tasks. This approach is wonderful (IMO), but it has cons: the main cons is probably that it generates a **dependency hieararchy**. In fact, to complete one task often I exploit other libraries to complete smaller tasks. For example to handle fancy progress bars I have created [forbear](https://github.com/szaghi/forbear) that, in turn, exploits [FACE](https://github.com/szaghi/FACE) to handle colors and styles: the end-user of `forbear` can be bothered to manually resolve the `FACE` dependency if she decide to perform a manual installation of `forbear`.

> I hope that my **ZOO** will help end-users to easy handle the pre-requisites phase and it will allow a more easy installation of my poor libraries.

#### The Wild Fortran Pets

The ZOO is currently hosting (with love):

+ [BeFoR64](https://github.com/szaghi/BeFoR64), Base64 encoding/decoding library for FoRtran poor men
+ [FACE](https://github.com/szaghi/FACE), Fortran Ansi Colors (and Styles) Environment
+ [FLAP](https://github.com/szaghi/FLAP), Fortran command Line Arguments Parser for poor people
+ [FLOw](https://github.com/szaghi/FLOw), Fortran fLuid Object
+ [FiNeR](https://github.com/szaghi/FiNeR), Fortran INI ParseR and generator
+ [FITTER](https://github.com/szaghi/FITTER), Fortran tIc Toc Timer
+ [forbear](https://github.com/szaghi/forbear), Fortran (progress) B(e)ar envinronment
+ [FORESEER](https://github.com/szaghi/FORESEER), FOrtran RiEmann SolveErs EnviRonment
+ :heavy_exclamation_mark: [FURY](https://github.com/szaghi/FURY), Fortran Units (environment) for Reliable phYsical math (**issued**)
+ :heavy_exclamation_mark: [MORTIF](https://github.com/szaghi/MORTIF), MORTon Indexer (Z-order) Fortran environment (**issued**)
+ [PENF](https://github.com/szaghi/PENF), Portability Environment for Fortran poor people
+ [StringiFor](https://github.com/szaghi/StringiFor), Strings Fortran Manipulator with steroids
+ [VecFor](https://github.com/szaghi/VecFor), Vector algebra class for Fortran poor people
+ [VTKFortran](https://github.com/szaghi/VTKFortran), pure Fortran VTK (XML) API
+ [FOODIE](https://github.com/Fortran-FOSS-Programmers/FOODIE), Fortran Object-Oriented Differential-equations Integration Environment
+ [FoXY](https://github.com/Fortran-FOSS-Programmers/FoXy), Fortran XML parser for poor people
+ [WenOOF](https://github.com/Fortran-FOSS-Programmers/WenOOF), WENO interpolation Object Oriented Fortran library

These *pets* are developed by me and other enthusiast Fortraners.

Please, read the documentation of each library contained into their own repository: here no documentation is shipped.

##### One ring to rule them all
A centralized module exposing **all pets** is also provided, i.e. [zoo.f90](https://github.com/szaghi/ZOO/tree/master/src/zoo.f90), see install section for more details.

##### Note on updates

The ZOO provides always a *recent master* branch version of each library: ZOO is itself updated quite often. Anyhow, local ZOOs can be updated by the end-user by means of the provided `zoo_update.sh` script, see install section for more details.

Go to [Top](#top)

### Copyrights

The original contents of ZOO are a Free and Open Source Software (FOSS), they are distributed under a **very permissive** multi-licensing system: selectable licenses are [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html), [BSD2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD3-Clause](http://opensource.org/licenses/BSD-3-Clause) and [MIT](http://opensource.org/licenses/MIT), feel free to select the license that best matches your workflow.

Each library has its own licensing system: the end-user is kindly invited to carefully read their licenses as reported into their own repositories.

Go to [Top](#top)

### Install ZOO pets

| [Download](#download) | [Update libraries](#update_libraries) | [Build libraries](#build_libraries) |

#### Download

You have 2 possibilities:

###### Plain download the repository

By your browser: [click here](https://github.com/szaghi/ZOO/archive/master.zip)

Or from the terminal:
```shell
# by wget
wget https://github.com/szaghi/ZOO/archive/master.zip
# by curl
curl https://github.com/szaghi/ZOO/archive/master.zip
# or by whatever you like...
```

This *plain* download is straightforward, but you have to repeat it to obtain new releases.

###### Clone the repository

If you have `git` it could be more handy to clone the repository thus you can update your local clone to new versions very easily

```shell
git clone https://github.com/szaghi/ZOO
```

Then, when you need to update your local clone to a new version, simply do

```shell
cd ZOO
git pull
```

#### Update libraries

If you have downloaded the ZOO (by means of any methods) and you have `git` and a minimal `bash` environment, you have the possibility to update the libraries to the last master version without waiting for a ZOO update. To update your local ZOO simply do

```shell
cd ZOO
./zoo_update.sh
```

This command will populate the `src` directory with the latest master version of each libraries.

#### Build libraries

##### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-v6.3.1+-brightgreen.svg)]()

The main compiler used to develop this Fortran libraries collection is GNU Fortran compiler. In particular, supported versions are v6.3.1 or higher (preferable 7+).

#### Using (GNU) Make

For each library a *simple*, plain, old makefile is provided. Use them as following

```shell
make -f makefile.$library # substutite "$library" with a library name, i.e. "befor64", "face", "flap", etc... (lower case)

# e.g.
make -f makefile.penf
```

After this command you will have the (statically) compiled library into `static/` subdirectory in the ZOO root. In the same subdirectory there is also `static/mod/` which contains the compiled `.mod` files, e.g:

```shell
tree -L 2 static/
static/
├── libbefor64.a
├── ...
├── libwenoof.a
├── mod
│   ├── befor64.mod
│   ├── ...
│   └── wenoof_weights_rec_js.mod
└── obj
    ├── befor64.o
    ├── ...
    └── wenoof_weights_rec_js.o
```

##### Build all library

A global makefile is provided to build all libraries with one command:

```shell
make -f makefile
# or simply
make
```

##### Build ZOO library

> One ring to rule them all.

To be completed.

#### Using FoBiS

The users of [FoBiS](https://github.com/szaghi/FoBiS) (are still there?) have a simple fobos for doing all:

```shell
FoBiS.py build -mode [static|shared]-gnu -t src/path_to/$library.[F|f]90 -o lib$library.[a|so] # substutite "$library" with a library name, i.e. "befor64", "face", "flap", etc... (lower case)

# e.g.
FoBiS.py build -mode static-gnu -t src/BeFoR64/befor64.F90 -o libbefor64.a
```

Note that FoBiS allows also shared libraries building, not only static as Make approach.

The compiled libraries are placed in the same tree as the Make approach.

##### Build all library

```shell
FoBiS.py rule -ex makeall
```

##### Build ZOO library

> One ring to rule them all.

```shell
FoBiS.py build -mode [static|shared]-gnu-zoo
```

This will create the **monster** library `libzoo.[a|so]` that exposes of the *ZOO's Wild Pets*.

Go to [Top](#top)

---

### FAQ

> Why this project is named **ZOO**?

Aside it being another of my bad acronyms, this is really a ZOO: there is library for simple-stupid task like to colorize terminal output and a library for challenging task like the integration of general non linear ODE. Like a real ZOO, here you can find animals of any size, from *small birds* to *huge elephants*.

Go to [Top](#top)
