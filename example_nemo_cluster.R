library(devtools)
install_github("dosimont/lpaggregr")
library(RcppArmadillo)
library(lpaggregr)

library(sqldf)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(treemap)
library(gplots)


#Threshold: 0<th<1, lower value means more accuracy for retrieving the list of partitions but longer computation time
th=0.001


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

micro=pjdump2micro(trace,20, "State")

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
oplot_stacked_state(omacro(odf$Partitions, micro, odf$POpt), color_generator_Clusters)
oplot_stacked_state(omacro(odf$Partitions, micro, odf$Qualities[2,"Parameter"]), color_generator_Clusters)
qualplot(hdf)
hplot_treemap_state(hmacro(hdf$Partitions, micro, hdf$POpt), color_generator_Clusters)