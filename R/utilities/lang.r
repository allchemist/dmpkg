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

#################################################################
## multiple value assignment

# Generic form
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)

  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")

  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }

  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)

  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin

  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}

# Grouping the left hand side
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}

#################################################################
