### процедура автошкалирования

autoscale.get_params <- function(data) {
  dim2=dim2(data);
  means=vec(dim2);
  sds=vec(dim2);
  for (i in 1:dim2) {
    means[i]=mean(data[,i]);
    sds[i]=sd(data[,i]);
  }
  return(list(means=means, sds=sds));
}

autoscale.apply <- function(data, params) {
  dim2=dim2(data);
  for (i in 1:dim2) {
    data[,i]=(data[,i]-params$means[i])/params$sds[i];
  }
  return(data);
}
  
autoscale.undo <- function(data, params) {
  dim2=dim2(data);
  for (i in 1:dim2) {
    data[,i]=data[,i]*params$sds[i]+params$means[i];
  }
  return(data);
}
