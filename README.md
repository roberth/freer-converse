
[![Build Status](https://travis-ci.org/roberth/freer-converse.svg?branch=master)](https://travis-ci.org/roberth/freer-converse)

# `freer-converse`

One can think of an effectful program and its effect
handler as /two communicating processes/.
This package provides the missing pieces that let you
write your programs in such a style in
`Control.Monad.Freer.Converse`.

One useful area of application is unit testing. The
@Control.Monad.Freer.TestControl@ intends to provide what
you need to write /ad-hoc test fixtures/.
