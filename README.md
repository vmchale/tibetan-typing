# Elm typing game

[![Build Status](https://travis-ci.org/vmchale/tibetan-tutor.svg?branch=master)](https://travis-ci.org/vmchale/tibetan-tutor)

This is an elm app I wrote trains your ability to type. It was inspired
by a [similar project](https://github.com/hoelzro/elm-typing-tutor) for Russian,
but it implements several special features for subjoined & superscript
consonants and thus was rewritten from scratch.

While it is not the first such program, it has a far more modern design than a
[2013 project](http://ieeexplore.ieee.org/document/6743395/?reload=true)
using flash.

Currently, it is a work in progress. While the implementation is sound, the
sample sentences still need refinement.

## Use

You can try it out yourself [here](http://vmchale.com/static/typing-tutor/index.html).

## Building it

The easiest way to build the project is to first install the [elm
platform](https://guide.elm-lang.org/install.html). Then:

```bash
 $ elm-make src/main.elm
```
