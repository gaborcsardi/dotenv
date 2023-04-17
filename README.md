
<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build
status](https://github.com/gaborcsardi/dotenv/workflows/R-CMD-check/badge.svg)](https://github.com/gaborcsardi/dotenv/actions)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/dotenv)](https://www.r-pkg.org/pkg/dotenv)
[![Coverage
Status](https://img.shields.io/codecov/c/github/gaborcsardi/dotenv/master.svg)](https://codecov.io/github/gaborcsardi/dotenv?branch=master)
<!-- badges: end -->

# dotenv — Load environment variables from .env

This package loads the variables defined in the `.env` file in the
current working directory (as reported by `getwd()`), and sets them as
environment variables.

This happens automatically when the `dotenv` package is loaded, so the
typical use-case is to just put a `library(dotenv)` call at the
beginning of your R script.

Alternatively a `dotenv::load_dot_env()` call can be used to load
variables from arbitrary files. Furthermore, you can set the environment
variable `R_DOTENV_AUTOLOAD` to `False` to disable automatic loading and
use `dotenv::dotenv_values()` to get the `.env` values in a list.

## Installation

``` r
install.packages("dotenv")
```

## Usage

You can simply put

``` r
library(dotenv)
```

at the beginning of your script, to load the environment variables
defined in `.env` in the current working directory.

## File format

The `.env` file is parsed line by line, and line is expected to have one
of the following formats:

    VARIABLE=value
    VARIABLE2="quoted value"
    VARIABLE3='another quoted variable'
    # Comment line
    export EXPORTED="exported variable"
    export EXPORTED2=another

In more details:

-   A leading `export` is ignored, to keep the file compatible with Unix
    shells.
-   No whitespace is allowed right before or after the equal sign,
    again, to promote compatilibity with Unix shells.
-   No multi-line variables are supported currently. The file is
    strictly parsed line by line.
-   Unlike for Unix shells, unquoted values are *not* terminated by
    whitespace.
-   Comments start with `#`, without any leading whitespace. You cannot
    mix variable definitions and comments in the same line.
-   Empty lines (containing whitespace only) are ignored.

It is suggested to keep the file in a form that is parsed the same way
with `dotenv` and `bash` (or other shells).
