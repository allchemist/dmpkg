### процедура центрирования

centering.get_params <- function(data) {
  dim2=dim2(data);
  means=vec(dim2);
  for (i in 1:dim2) {
    means[i]=mean(data[,i]);
  }
  return(list(means=means));
}

centering.apply <- function(data, params) {
  dim2=dim2(data);
  for (i in 1:dim2) {
    data[,i]=data[,i]-params$means[i];
  }
  return(data);
}
  
centering.undo <- function(data, params) {
  dim2=dim2(data);
  for (i in 1:dim2) {
    data[,i]=data[,i]+params$means[i];
  }
  return(data);
}
