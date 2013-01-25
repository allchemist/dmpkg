#################################################################
##### AdaBoost meta algorithm native implementation

adaboost.train <- function() {
  inputs;
  outputs;
  npats;
  lim;
  w = vec_fill(vec(npats), 1/npats);
  c = NA;
  q = NA;
  a = NA;
  cs = list();
  for (i in 1:n) {
    c = weak_train_fn(pats);
    q = adaboost_quality (c, pats, w, lim);
    a = 0.5 * log((1 - q)/(q + 1e-7));

    for (j in 1:npats)
      w[i] = w[i] * exp(-a * weak_apply(c, inputs[i,]) * outputs[i]);
    w = w/mean(w);
    cs = c(cs, list(c,a));
    }

  return(cs);
}


net <- adaboost.train(datax2
