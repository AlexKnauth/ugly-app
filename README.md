# ugly-app

A Racket library that allows prefix, infix, postfix, and partially applied functions.

This was mainly inspired by two things:
 - [`fancy-app`](https://docs.racket-lang.org/fancy-app/index.html), a similar library for just partially applied functions
 - relations in programming-language semantics and type-checking such as `ρ ⊢ e ⇓ v` and `Γ ⊢ e : τ`

## Installation

In DrRacket, go to the `File` menu, click `Package Manager`, and click on the `Do What I Mean` tab. In the `Package Source` field put `git://github.com/AlexKnauth/ugly-app?path=ugly-app-lib`, and click Install.

Or on the command line, run the command `raco pkg install git://github.com/AlexKnauth/ugly-app?path=ugly-app-lib`.
