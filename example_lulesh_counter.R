library(devtools)
install_github("dosimont/lpaggregr")
library(lpaggregr)

#Threshold: 0<th<1, lower value means more accuracy for retrieving the list of partitions but longer computation time
th=0.001

original_trace=parsepjdump("lulesh2.0.counters-only.pjdump")

trace=original_trace

trace$data<-trace$data[(trace$data$Type %in% c('PAPI_TOT_INS [Instr completed]', 'PAPI_TOT_CYC [Total cycles]')),]

micro=pjdump2micro(trace,10, "Counter")
print(micro)

odf<-oaggregate(micro$data, th)

qualplot(odf)
oplot_stacked_state(omacro(odf$Partitions, micro, odf$POpt))

hdf<-haggregate(micro$data, micro$hierarchy, th)

qualplot(hdf)
hplot_treemap_perfcounter(hmacro(hdf$Partitions, micro, hdf$POpt))
hplot_treemap_perfcounter(hmacro(hdf$Partitions, micro, hdf$Qualities[1,"Parameter"]))

trace$data<-trace$data[(trace$data$Type %in% c('PAPI_TOT_INS [Instr completed]')),]

micro=pjdump2micro(trace,10, "Counter")
hdf<-haggregate(micro$data, micro$hierarchy, th)

qualplot(hdf)
hplot_treemap_perfcounter(hmacro(hdf$Partitions, micro, hdf$POpt))
hplot_treemap_perfcounter(hmacro(hdf$Partitions, micro, hdf$Qualities[1,"Parameter"]))

trace=original_trace

trace$data<-trace$data[(trace$data$Type %in% c('PAPI_L1_DCM [L1D cache misses]')),]

micro=pjdump2micro(trace,10, "Counter")
print(micro)

hdf<-haggregate(micro$data, micro$hierarchy, th)

qualplot(hdf)
hplot_treemap_perfcounter(hmacro(hdf$Partitions, micro, hdf$POpt))
hplot_treemap_perfcounter(hmacro(hdf$Partitions, micro, hdf$Qualities[1,"Parameter"]))