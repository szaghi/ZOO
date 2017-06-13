<a name="top"></a>

# ZOO

[![License](https://img.shields.io/badge/license-GNU%20GeneraL%20Public%20License%20v3,%20GPLv3-blue.svg)]()
[![License](https://img.shields.io/badge/license-BSD2-red.svg)]()
[![License](https://img.shields.io/badge/license-BSD3-red.svg)]()
[![License](https://img.shields.io/badge/license-MIT-red.svg)]()

> ZOO, **Z**aghi f**O**rtran c**O**llection, where my wild Fortran pets will survive

> please, do not feed the animals :smile:

---

| [What is ZOO?](#what-is-zoo) | [Copyrights](#copyrights) | [Install ZOO pets](#install_zoo_pets) |

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
+ [FURY](https://github.com/szaghi/FURY), Fortran Units (environment) for Reliable phYsical math
+ [MORTIF](https://github.com/szaghi/MORTIF), MORTon Indexer (Z-order) Fortran environment
+ [PENF](https://github.com/szaghi/PENF), Portability Environment for Fortran poor people
+ [StringiFor](https://github.com/szaghi/StringiFor), Strings Fortran Manipulator with steroids
+ [VecFor](https://github.com/szaghi/VecFor), Vector algebra class for Fortran poor people
+ [VTKFortran](https://github.com/szaghi/VTKFortran), pure Fortran VTK (XML) API
+ [FOODIE](https://github.com/Fortran-FOSS-Programmers/FOODIE), Fortran Object-Oriented Differential-equations Integration Environment
+ [FoXY](https://github.com/Fortran-FOSS-Programmers/FoXy), Fortran XML parser for poor people
+ [WenOOF](https://github.com/Fortran-FOSS-Programmers/WenOOF), WENO interpolation Object Oriented Fortran library

These *pets* are developed by me and other enthusiast Fortraners.

Please, read the documentation of each library contained into their own repository: here no documentation is shipped.

Go to [Top](#top)

### Copyrights

The original contents of ZOO are a Free and Open Source Software (FOSS), they are distributed under a **very permissive** multi-licensing system: selectable licenses are [GPLv3](http://www.gnu.org/licenses/gpl-3.0.html), [BSD2-Clause](http://opensource.org/licenses/BSD-2-Clause), [BSD3-Clause](http://opensource.org/licenses/BSD-3-Clause) and [MIT](http://opensource.org/licenses/MIT), feel free to select the license that best matches your workflow.

Each library has its own licensing system: the end-user is kindly invited to carefully read their licenses as reported into their own repositories.

Go to [Top](#top)

### Install ZOO pets

To be written.

#### Compiler Support

[![Compiler](https://img.shields.io/badge/GNU-v6.3.1+-brightgreen.svg)]()

The main compiler used to develop this Fortran libraries collection is GNU Fortran compiler. In particular, supported versions are v6.3.1 or higher (preferable 7+).

Go to [Top](#top)
