library(sqldf)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(digest)
library(treemap)
library(gplots)

hspace=0.1

string2colorRandom<- function(string){
  digested=digest(as.character(string), serialize=FALSE)
  r=substr(digested,1,10)
  r=digest(as.character(r), serialize=FALSE)
  g=substr(digested,11,20)
  g=digest(as.character(g), serialize=FALSE)
  b=substr(digested,21,30)
  b=digest(as.character(b), serialize=FALSE)
  r=substr(r,1,2)
  g=substr(g,1,2)
  b=substr(b,1,2)
  h<-paste(r,g,b,sep="")
  if ((r>230&g>230&b>230)|(r<30&g<30&b<30)){
    h = string2colorRandom(paste(string,":-o",string,sep=""))
  }
  h
}

color_generator <- function(stringlist, aggString=c("")){
  sorted<-sort(stringlist)
  hashcoded<-rep(0, length(stringlist))
  for (i in 1:length(sorted)){
    if (sorted[i]==aggString){
      hashcoded[i]=0
    }
    else{
      hashcoded[i]=string2colorRandom(sorted[i])
    }
  }
  hexed<-format(as.hexmode(hashcoded),width=6)
  color=paste("#",hexed,sep="")
  names(color)=sorted
  color
}

factor2numeric <- function(f)
{
  if(!is.factor(f)) stop("the input must be a factor")
  as.numeric(levels(f))[as.integer(f)]
}

slicerstate <- function (trace, timeSliceNumber)
{
  trace$Type<-trace$Value
  start <- min(trace$Start)
  trace$Start <- trace$Start - start
  trace$End <- trace$End - start
  maxts <- max(trace$End)
  slicets = maxts/timeSliceNumber;
  slices <- data.frame(SliceId=1:timeSliceNumber, TsStart=(0:(timeSliceNumber-1))*slicets, TsEnd=(1:timeSliceNumber)*slicets);
  h <- sqldf('SELECT trace.ResourceId, trace.Start, trace.End, trace.Duration, trace.Type, slices.SliceId, slices.TsStart, slices.TsEnd
             FROM trace
             INNER JOIN slices
             ON (trace.Start+trace.Duration > slices.TsStart) AND (trace.End-trace.Duration < slices.TsEnd)')
  h$Duration <- NULL;
  m <- h %>% group_by(ResourceId, Start, End, SliceId, Type) %>%
    mutate(N=n(), TinTS = (min(End,TsEnd) - max(Start,TsStart))) %>%
    group_by(ResourceId, SliceId, Type, TsStart, TsEnd) %>%
    summarize (Sum=sum(TinTS), Normalized=Sum/maxts) %>%
    as.data.frame();
  p <- expand.grid(ResourceId=unique(m$ResourceId), SliceId = 1:max(m$SliceId), Type = unique(m$Type));
  p$TsStart = 0;
  p$TsEnd = 0;
  p$Sum = 0;
  p$Normalized = 0;
  n <- rbind(p, m);
  o <- n %>% group_by (ResourceId, SliceId, Type) %>%
    summarize(TsStart = max(TsStart), TsEnd = max(TsEnd), Sum = max(Sum), Normalized=max(Normalized)) %>% as.data.frame;
  return (o);
}


slicerprvcounter <- function (trace, timeSliceNumber)
{
  start <- min(trace$Start)
  trace$Start <- trace$Start - start
  trace$End <- trace$End - start
  maxts <- max(trace$End)
  trace <- trace %>% group_by(ResourceId, Type) %>% mutate(Value=lead(Value)) %>% na.omit()
  print(trace)
  trace$Value<-trace$Value/trace$Duration
  slicets = maxts/timeSliceNumber;
  slices <- data.frame(SliceId=1:timeSliceNumber, TsStart=(0:(timeSliceNumber-1))*slicets, TsEnd=(1:timeSliceNumber)*slicets);
  h <- sqldf('SELECT trace.ResourceId, trace.Type, trace.Start, trace.End, trace.Duration, trace.Value, slices.SliceId, slices.TsStart, slices.TsEnd
             FROM trace
             INNER JOIN slices
             ON (trace.Start+trace.Duration > slices.TsStart) AND (trace.End-trace.Duration < slices.TsEnd)')
  h$Duration <- NULL;
  m <- h %>% group_by(ResourceId, Type, Start, End, SliceId, Value) %>%
    mutate(N=n(), TinTS = (min(End,TsEnd) - max(Start,TsStart))) %>%
    group_by(ResourceId, SliceId, Type, TsStart, TsEnd) %>%
    summarize (Mean=weighted.mean(Value, TinTS), Normalized=Mean) %>%
    as.data.frame();
  p <- expand.grid(ResourceId=unique(m$ResourceId), SliceId = 1:max(m$SliceId), Type = unique(m$Type));
  p$TsStart = 0;
  p$TsEnd = 0;
  p$Mean = 0;
  p$Normalized = 0;
  n <- rbind(p, m);
  o <- n %>% group_by (ResourceId, SliceId, Type) %>%
    summarize(TsStart = max(TsStart), TsEnd = max(TsEnd), Mean = max(Mean), Normalized=max(Normalized)) %>% as.data.frame;
  o <- o %>% group_by(Type) %>% mutate(Normalized=Normalized/sum(Normalized))
  return (o);
}

parsepjdump <- function (file){
  
  names <- c("Nature", "ResourceId", "Type", "Start", "End", "Duration", "Depth", "Value", "a", "b", "c", "d", "e", "f", "g")
  trace <- read.table(file, sep=",", fill=TRUE, header=FALSE, strip.white=TRUE, col.names=names)
  
  trace[trace$Nature %in% 'Variable', "Value"] <- as.numeric(levels((trace[trace$Nature %in% 'Variable',"Depth"])))[(trace[trace$Nature %in% 'Variable',"Depth"])]
  trace$a <- NULL
  trace$b <- NULL
  trace$c <- NULL
  trace$d <- NULL
  trace$e <- NULL
  trace$f <- NULL
  trace$g <- NULL
  
  resources <- trace[trace$Nature %in% "Container",]
  resources$Nature <- NULL
  resources$Type <- NULL
  resources$Start <- NULL
  resources$End <- NULL
  resources$Duration <- NULL
  resources$Value <- NULL
  resources$ParentId <- resources$ResourceId
  resources$ResourceId <- resources$Depth
  resources$Depth <- NULL
  
  trace <- trace[!(trace$Nature %in% "Container"),]
  ret<-list("data"=trace,"resources"=resources)
  ret
}

pjdump2micro <- function(trace, timeSliceNumber=100, type="State", enable_hierarchy=TRUE){
  
  data <- trace$data
  if (type %in% "State"){
    data <- data[data$Nature %in% "State",]
    df <- slicerstate(data, timeSliceNumber)
  }else if (type %in% "Counter"){
    data <- data[data$Nature %in% "Variable",]
    df <- slicerprvcounter(data, timeSliceNumber)
  }
  
  time <- unique(df$SliceId)
  time <- time[order(time)]
  
  type <- unique(df$Type)
  type <- type[order(type)]
  
  if (enable_hierarchy){
    resources <- trace$resources
    
    parents <- unique(resources$ParentId)
    parents<-parents[order(parents)]
    parents<-rev(parents)
    
    #remove parents from df
    df <- df[!(df$ResourceId %in% parents),]
  }
  
  space <- unique(df$ResourceId)
  space <- space[order(space)]
  
  if (enable_hierarchy){
    hierarchy <- factor(c(as.character(space),as.character(parents)))
    hierarchy<-unique(hierarchy)
    names(hierarchy)=as.character(hierarchy)
    resources$ParentIndex=-1
    vhierarchy <- rep(-1,length(hierarchy))
    names(vhierarchy)=as.character(hierarchy)
    
    for (i in 1:length(vhierarchy)){
      resources[resources$ResourceId %in% hierarchy[i],"ParentIndex"]=match(resources[resources$ResourceId %in% hierarchy[i],"ParentId"],hierarchy)[1]
    }
    resources[1,"ParentIndex"]=0
    for (i in 1:length(vhierarchy)){
      vhierarchy[i]=resources[resources$ResourceId %in% hierarchy[i],"ParentIndex"]
    }
  }else{
    vhierarchy=0
  }
  
  dataCube <- array(0,
                    dim = c(length(space), length(type), length(time)),
                    dimnames = list("Space"=space, "Type"=type, "Time"=time)
  )
  
  for (r in 1:nrow(df)) {
    row <- df[r,]
    dataCube[as.character(row$ResourceId),as.character(row$Type),as.character(row$SliceId)] <- row$Normalized
  }
  
  ret<-list("data"=dataCube,"hierarchy"=vhierarchy)
  ret
}

getpath<-function(vhierarchy, leavesize){
  path <- rep(-1,length(vhierarchy))
  for (h in 1:leavesize){
    path[h]=h
  }
  i=leavesize+1;
  h=1;
  while(i<=length(vhierarchy)){
    if (!(vhierarchy[path[h]] %in% path)){
      path[i]=vhierarchy[path[h]]
      i=i+1
    }
    h=h+1
  }
  path
}

omacro <- function(df, micro, p){
  df <- df[df$Parameter %in% p,]
  dfdata <- melt(micro$data)
  dfdata$Start=-1
  dfdata$End=-1
  for (i in 1:nrow(df)){
    dfdata[dfdata$Time>=df[i,"Start"]&dfdata$Time<=df[i,"End"],"Start"]=df[i,"Start"]
    dfdata[dfdata$Time>=df[i,"Start"]&dfdata$Time<=df[i,"End"],"End"]=df[i,"End"]
  }
  agg <- aggregate(value ~ Space+Type+Start+End, data = dfdata, FUN = mean)
  agg
}

hmacro <- function(df, micro, p){
  df <- df[df$Parameter %in% p,]
  dfdata <- melt(micro$data)
  vhierarchy <- micro$hierarchy
  leavesize= length(unique(dfdata$Space))
  for (r in 1:nrow(df)) {
    df[r,"Space"]<-names(vhierarchy)[df[r,"Node"]]
  }
  dfdata$Parent="0"
  path=getpath(vhierarchy,leavesize)
  for (h in 1:(length(path)-1)){
    dfdata[dfdata$Space %in% names(vhierarchy)[path[h]],"Parent"]<-names(vhierarchy)[vhierarchy[path[h]]]
    dfdata2<-dfdata[dfdata$Space %in% names(vhierarchy)[path[h]],]
    dfdata2$Space<-names(vhierarchy)[vhierarchy[path[h]]]
    dfdata=rbind(dfdata,dfdata2)
  }
  agg <- dfdata[dfdata$Space %in% df$Space,]
  agg <- aggregate(value ~ Space+Type+Time+Parent, data = agg, FUN = sum)
  agg <- sqldf('SELECT agg.Space, agg.Type, agg.Time, agg.Parent, agg.value, df.Size
             FROM agg
             INNER JOIN df
             ON (agg.Space == df.Space)')
}

qualplot <- function(results){
  qualities<-results$Qualities
  popt<-results$POpt
  opt<-qualities[(qualities$Parameter %in% popt),]
  xlabel<- "Information Loss"
  ylabel<- "Complexity Reduction"
  plot<-ggplot()
  plot<-plot + geom_line(data=qualities,aes(x=Loss,y=Gain), color="black")
  plot<-plot + geom_point(data=qualities,aes(x=Loss,y=Gain), color="black")
  plot<-plot + geom_point(data=opt,aes(x=Loss,y=Gain), color="red")
  plot<-plot + theme_bw()
  plot<-plot + labs(x=xlabel,y=ylabel)
  plot
}

oplot_stacked_state <-function(agg, FUN=color_generator){
  agg <- aggregate(value ~ Type+Start+End, data = agg, FUN = mean)
  agg$Duration<-agg$End-agg$Start+1
  vcolors=FUN(unique(agg$Type))
  p<-ggplot(agg, aes(x=Start+((End-Start)/2)+0.5-(hspace/2), y=value, width=Duration-hspace, fill=Type))
  p<-p + scale_fill_manual(values = vcolors, breaks = names(vcolors), labels = names(vcolors))
  p<-p + geom_bar(stat="identity")
  p<-p + theme_bw()
  p<-p + labs(x="Time slices",y="Normalized value")
  p
}

hplot_treemap_state <-function(agg, FUN=color_generator){
  agg <- aggregate(value ~ Space+Type+Parent, data = agg, FUN = mean)
  vcolors=FUN(unique(agg$Type))
  agg$Color=vcolors[agg$Type]
  treemap(agg, index=c("Parent", "Space", "Type"), vSize="value", vColor="Color", type="color", algorithm="squarified", border.col="white", bg.labels="grey", title="")
}

hplot_treemap_perfcounter <-function(agg){
  agg <- aggregate(value ~ Space+Type+Size+Parent, data = agg, FUN = mean)
  agg$value=agg$value/agg$Size
  treemap(agg, index=c("Parent", "Space", "Type"), vSize="Size", vColor="value", type="manual", palette="RdYlBu", algorithm="squarified", border.col="white", bg.labels="grey", title="")
}
