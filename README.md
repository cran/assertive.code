[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Is the package on CRAN?](http://www.r-pkg.org/badges/version/assertive.code)](http://www.r-pkg.org/pkg/assertive.code)

# assertive.code

A set of predicates and assertions for checking the properties of code.  Most of the documentation is on the *[assertive](https://bitbucket.org/richierocks/assertive)* page.  End-users will usually want to use *assertive* directly.


### Installation

To install the stable version, type:

```{r}
install.packages("assertive.code")
```

To install the development version, you first need the *devtools* package.

```{r}
install.packages("devtools")
```

Then you can install the *assertive.code* package using

```{r}
library(devtools)
install_bitbucket("richierocks/assertive.code")
```

### Predicates

`is_binding_locked` checks to see if a variable has had its binding locked (that is, has been made read-only).

`is_debugged` checks to see if a function has a debugging flag set on it.

`is_error_free`, wraps `try`, running the code and returning `TRUE` if no error was thrown.  (The results of running the code are also returned in an attribute.)

`is_existing` checks to see if a variable exists in a given environment.

`is_if_condition` checks if a variable is suitable for use as an if condition (that is, it is `TRUE` or `FALSE`).

`is_loaded` checks whether or not a DLL (shared object) has been loaded.

`is_valid_r_code` checks whether a string is valid (parseable) R code.

`is_valid_variable_name` checks whether a string is a valid R variable name.

### Assertions

Predicates that return a vector have two corresponding assertions.  For example,
`is_valid_variable_name` has `assert_all_are_valid_variable_names` and `assert_any_are_valid_variable_names`.

Predicates returning a single logical value have one corresponding assertion.
For example, `is_debugged` has `assert_is_debugged`.