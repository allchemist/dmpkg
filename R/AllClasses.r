
setClassUnion('matrix|NA', members=c('matrix', 'logical'));
setClass('dataframe.patterns', representation(formula='formula', data='data.frame', weights='matrix|NA'));
setClass('matrix.patterns', representation(data='matrix', outputs='matrix', weights='matrix|NA', formula='formula'));
setClassUnion(name='patterns', members=c('dataframe.patterns', 'matrix.patterns'));
