### элементарные утилиты

dim1 <- function(m) {
  dim(m)[[1]];
}

dim2 <- function(m) {
  dim(m)[[2]];
}

vec <- function(n) {
  vector(length=n, mode="numeric");
}

vec_fill <- function(vec, val) {
  val = as.numeric(val);
  for (i in 1:length(vec))
    vec[i]=val;
  return(vec);
}

split.sample <- function(size, ratio) {
  mapply(function(v) {if (v <= ratio*size)
  		         1 else 2},
	 sample(1:size))
}
