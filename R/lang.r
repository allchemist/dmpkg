#################################################################
##### R language extensions

## several switches
## MAKE IT LAZY!!!!!!!!!

# case switch, NA if no match
case <- function (EXPR, ...) {
  vars = cbind(...);
  vars[cbind(seq_along(EXPR), match(EXPR, names(list(...))))];
}

# case switch, error if no match
ecase <- function (EXPR, ...) {
  ret = case(EXPR, ...);
  if (is.na(ret)) stop('Unknown case switch for \'', EXPR, '\'') else ret;
}

# type switch, NA if no match
typecase <- function (OBJ, ...) {
  vars = cbind(...);
  type = class(OBJ);
  vars[cbind(seq_along(type), match(type, names(list(...))))];
}

# type switch, error if no match
etypecase <- function (OBJ, ...) {
  ret = typecase(OBJ, ...);
  if (is.na(ret)) stop('Unknown type switch for \'', OBJ, '\' of type \'', type, '\'') else ret;
}

## trivial list accessors

first  <- function(lst) lst[[1]];
second <- function(lst) lst[[2]];
third  <- function(lst) lst[[3]];
rest   <- function(lst) lst[-1];
null   <- function(lst) length(lst) == 0;

## string trim

trim.left  <- function(str) sub("[[:space:]]+", "", str);
trim.right <- function(str) sub("[[:space:]]+$", "", str);
trim <- function(str) trim.left(trim.right(str));
