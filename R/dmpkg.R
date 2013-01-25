# R source files load order

test <- function(arg) print(arg) 

# utilities

source("R/utilities/lang.r", local=TRUE)
source("R/utilities/trivial.r", local=TRUE)
source("R/utilities/patterns.r", local=TRUE)

# preproc

source("R/preproc/centering.r", local=TRUE)
source("R/preproc/autoscale.r", local=TRUE)

# 3dparty

source("R/3dparty/kernlab.r", local=TRUE)
