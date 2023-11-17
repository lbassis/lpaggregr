library(tidyverse)
library(devtools)
install_github("dosimont/lpaggregr")
library(lpaggregr)

TRACE <- "cholesky_11520_960_starpu_25_3_dmda_1_idcin-2.grenoble.grid5000.fr_2016-08-21_20-49-12.pjdump"

#Threshold: 0<th<1, lower value means more accuracy for retrieving the list of partitions but longer computation time
th=0.0001

#Without hierarchy
trace=parsepjdump(TRACE)

#Example of filtering
trace$data<-trace$data[(trace$data$Value %in% c('dgemm','dsyrk', 'dpotrf', 'dtrsm')),]

#No hierarchy (because it's not present in the trace, but it should be...)
micro=pjdump2micro(trace, 100,"State",FALSE)

odf<-oaggregate(micro$data, th)
qualplot(odf)
#odf$POpt
list_of_ps <- as.list(unique(odf$Partitions$Parameter, 3))
str(list_of_ps)
lapply(list_of_ps,
       function(p){
           oplot_stacked_state(omacro(odf$Partitions, micro, p)) +
               coord_cartesian(ylim=c(0,0.0100), xlim=c(0,100))
       }) -> z
z
