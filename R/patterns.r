#################################################################
##### patterns interface

## moved to AllClasses.r
#setClassUnion('matrix|NA', members=c('matrix', 'logical'));
#setClass('dataframe.patterns', representation(formula='formula', data='data.frame', weights='matrix|NA'));
#setClass('matrix.patterns', representation(data='matrix', outputs='matrix', weights='matrix|NA', formula='formula'));
#setClassUnion(name='patterns', members=c('dataframe.patterns', 'matrix.patterns'));

#setMethod('patterns.check', 'dataframe.patterns',
#          function(pats) {
#            for (i in 1:length(pats@data)) {
#              if length(pats@data

## patterns dimensions

formula.parse <- function(formula) {
  formula = as.character(formula);
  list(trim(unlist(strsplit(third(formula), '+', TRUE))),
       trim(unlist(strsplit(second(formula), '+', TRUE))))
}

setGeneric('patterns.dims', function(pats) standardGeneric('patterns.dims'))

setMethod('patterns.dims', 'dataframe.patterns',
          function(pats) {
            formula=formula.parse(pats@formula);
            list(length(first(formula)),
                 length(second(formula)));
          })
            
            
setMethod('patterns.dims', 'matrix.patterns',
          function(pats) {
            list(dim1(pats@data),
                 dim1(pats@outputs));
          })

## number of patterns

setGeneric('patterns.num', function(pats) standardGeneric('patterns.num'))

setMethod('patterns.num', 'dataframe.patterns',
          function(pats) {
            dim1(pats@data);
          })

setMethod('patterns.num', 'matrix.patterns',
          function(pats) {
            d1 = dim1(pats@inputs);
            d2 = dim1(pats@outputs);
            if (d1 == d2) return(d1) else stop("Illformed matrix patterns: inputs and outputs numbers are not equal");
          })

## make patterns

patterns.make <- function(formula, data, outputs=NULL, weights=NA) {
  if (class(data)=='data.frame') new('dataframe.patterns', formula=as.formula(formula), data=data, weights=weights)
    else {if (class(data) == 'matrix') new('matrix.patterns', formula=as.formula(formula), data=data, outputs=outputs, weights=weights)
            else stop("Can't define type of patterns to create")}
}

## split patterns

setGeneric('patterns.split', function(pats, sample) standardGeneric('patterns.split'))

setMethod('patterns.split', 'dataframe.patterns',
          function(pats, sample) {
            sample =
              if (length(sample) == 1)
                split.sample(patterns.num(pats), sample) else sample;

            dsplit = split(pats@data, sample);
            wsplit = if (is.na(pats@weights)) list(NA, NA) else split(pats@weights, sample);
            return (list(
              patterns.make(pats@formula, first(dsplit),  first(wsplit)),
              patterns.make(pats@formula, second(dsplit), second(wsplit))));
          })

setMethod('patterns.split', 'matrix.patterns',
          function(pats, sample) {
            sample =
              if (length(split.sample) == 1)
                split.sample(patterns.num(pats), sample) else split.sample;

            dsplit = split(pats@data, sample);
            osplit = split(pats@outputs, sample);
            wsplit = if (is.na(pats@weights)) list(NA, NA) else split(pats@weights, sample);
            return (list(
              patterns.make (pats@formula, first(dsplit), first(osplit), first(wsplit)),
              patterns.make (pats@formula, second(dsplit), second(osplit), second(wsplit))));
          })

## convert patterns

setGeneric('as.dataframe.patterns', function(pats) standardGeneric('as.dataframe.patterns'))
setGeneric('as.matrix.patterns', function(pats) standardGeneric('as.matrix.patterns'))

setMethod('as.dataframe.patterns', 'dataframe.patterns', function(pats) pats)
setMethod('as.matrix.patterns', 'matrix.patterns', function(pats) pats)

setMethod('as.dataframe.patterns', 'matrix.patterns',
          function(pats) {
            new = patterns.make (pats@formula, as.data.frame(cbind(pats@data, pats@outputs)), pats@weights);
            colnames(new@data) <- unlist(formula.parse(new@formula));
            return(new);
          })

          
setMethod('as.matrix.patterns', 'dataframe.patterns',
          function(pats) {
            formula_names=formula.parse(pats@formula);
            new = patterns.make (pats@formula,
                                 as.matrix(pats@data[,first(formula_names)]),
                                 as.matrix(pats@data[,second(formula_names)]),
                                 pats@weights);
            colnames(new@outputs) <- second(formula_names);
            return(new);
          })

## get single pattern

setGeneric('patterns.getSingle', function(pats, idx) standardGeneric('patterns.getSingle'))

setMethod('patterns.getSingle', c('dataframe.patterns', 'numeric'),
          function (pats, idx) {
            row=pats@data[idx,];
            formula_names=formula.parse(pats@formula);
            list(as.matrix(row[,first(formula_names)]),
                 as.matrix(row[,second(formula_names)]));
          })

setMethod('patterns.getSingle', c('matrix.patterns', 'numeric'),
          function(pats, idx) {
            list(pats@data[idx,], pats@outputs[idx,]);
          })
