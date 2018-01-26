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