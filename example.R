library(devtools)
install_github("dosimont/lpaggregr")
library(lpaggregr)

#Synthetic example: 2 processes, 2 types, 5 timeslices (keep the same order)
testArray = array(dim=c(2,2,5))
testArray[1,1,] = c(0.5, 0.2, 0.0, 0.1, 0.4)
testArray[1,2,] = c(0.5, 0.8, 1.0, 0.9, 0.6)
testArray[2,1,] = c(0.4, 0.3, 0.1, 0.2, 0.3)
testArray[2,2,] = c(0.6, 0.7, 0.9, 0.8, 0.7)
print(testArray)

#Threshold: 0<th<1, lower value means more accuracy for retrieving the list of partitions but longer computation time
th=0.001

#Temporal aggregation
r=oaggregate(testArray, th)
qualplot(r)

#Spatial aggregation
#define hierarchy: vector[index]= index's father index; vector[index]0 means index=root
h=c(3,3,0)
r=haggregate(testArray, h, th)
qualplot(r)

#Spatiotemporal aggregation
r=daggregate(testArray, h, th)
qualplot(r)

color_generator_Clusters <- function(stringlist, aggString=c("")){
  colors=rep("black", length(stringlist))
  names(colors)=stringlist
  colors["Cluster 1"]="green"
  colors["Cluster 2"]="yellow"
  colors["Cluster 3"]="red"
  colors["Cluster 4"]="darkgreen"
  colors["Cluster 5"]="deeppink"
  colors["Cluster 6"]="darkviolet"
  colors["Cluster 7"]="navajowhite1"
  colors["Cluster 8"]="orange"
  colors["Cluster 9"]="cyan"
  colors["Cluster 10"]="darkolivegreen2"
  colors["Cluster 11"]="peru"
  colors["Duration Filtered"]="black"
  colors["Noise"]="grey"
  names(colors)=stringlist
  colors
}

#Real trace example
trace=parsepjdump("nemo.exe.128tasks.chop1.clustered.clusters-only.pjdump")

trace$data<-trace$data[!(trace$data$Value %in% c('Duration Filtered','Noise')),]

micro=pjdump2micro(trace,100, "State")

#Aggregations
odf<-oaggregate(micro$data, th)
hdf<-haggregate(micro$data, micro$hierarchy, th)
#ddf<-daggregate(micro$data, micro$hierarchy, th)

#Printing the algorithm output
#head(odf)
#head(hdf)
#head(ddf)

#Generating a plot for a randomly chosen parameter
qualplot(odf)
oplot_stacked_state(omacro(odf$Partitions, micro, odf$Qualities[12,"Parameter"]), color_generator_Clusters)
qualplot(hdf)
hplot_treemap_state(hmacro(hdf$Partitions, micro, hdf$POpt), color_generator_Clusters)

trace=parsepjdump("nemo.exe.128tasks.chop1.clustered.counters-only.pjdump")

trace$data<-trace$data[(trace$data$Type %in% c('(PAPI_TOT_INS) Instr completed')),]

micro=pjdump2micro(trace,100, "Counter")
hdf<-haggregate(micro$data, micro$hierarchy, th)

qualplot(hdf)
hplot_treemap_perfcounter(hmacro(hdf$Partitions, micro, hdf$POpt))

# #Without hierarchy
# trace=parsepjdump("cholesky_11520_960_starpu_25_3_dmda_1_idcin-2.grenoble.grid5000.fr_2016-08-21_20-49-12.pjdump")
# 
# #Example of filtering
# trace$data<-trace$data[!(trace$data$Value %in% c('Idle','Sleeping')),]

#No hierarchy (because it's not present in the trace, but it should be...)
# micro=pjdump2micro(trace,50,"State",FALSE)
# 
# odf<-oaggregate(micro$data, th)
# 
# odf
# qualplot(odf)
# oplot_stacked_state(omacro(odf$Partitions, micro, odf$POpt))
