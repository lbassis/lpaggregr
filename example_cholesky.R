library(devtools)
install_github("dosimont/lpaggregr")
library(lpaggregr)

#Threshold: 0<th<1, lower value means more accuracy for retrieving the list of partitions but longer computation time
th=0.001

#Without hierarchy
trace=parsepjdump("cholesky_11520_960_starpu_25_3_dmda_1_idcin-2.grenoble.grid5000.fr_2016-08-21_20-49-12.pjdump")

#Example of filtering
trace$data<-trace$data[!(trace$data$Value %in% c('Idle','Sleeping')),]

#No hierarchy (because it's not present in the trace, but it should be...)
micro=pjdump2micro(trace,50,"State",FALSE)

odf<-oaggregate(micro$data, th)

odf
qualplot(odf)
oplot_stacked_state(omacro(odf$Partitions, micro, odf$POpt))
