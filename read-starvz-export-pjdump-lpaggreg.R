#!/usr/bin/Rscript
generate_hierarchy <- function(df) {
  df$Application |>
    select(Node, Resource, ResourceId, ResourceType) |>
    mutate(Node = paste0("n", Node)) |>
    distinct() |>  
    arrange(Node, ResourceType) -> df.hierarchy
  df$Application |>
    select(Start, End) |>
    summarize(Start = min(Start), End = max(End)) -> df.times
  my.start <- df.times$Start
  my.end <- df.times$End
  my.duration <- my.end - my.start

  bind_rows(
                                        # Level-0 (Root)
    tibble(Container = "Container",
           Parent = "0",
           Type = "ROOT",
           Start = my.start,
           End = my.end,
           Duration = my.duration,
           Name = "root") |>
    select(Container, Parent, Type, Start, End, Duration, Name)
   ,
                                        # Level-1 (Node)
    df.hierarchy |>
    select(Node) |>
    distinct() |>
    mutate(Container = "Container",
           Parent = "root",
           Type = "NODE",
           Start = my.start,
           End = my.end,
           Duration = my.duration,
           Name = Node) |>
    select(Container, Parent, Type, Start, End, Duration, Name)
   ,
                                        # Level-2 (Resource Type)
    df.hierarchy |>
    select(Node, ResourceType) |>
    distinct() |>  
    mutate(RT = paste(Node, ResourceType, sep="-")) |>
    mutate(Container = "Container",
           Parent = Node,
           Type = "RT",
           Start = my.start,
           End = my.end,
           Duration = my.duration,
           Name = RT) |>
    select(Container, Parent, Type, Start, End, Duration, Name)
   ,
                                        # Level-3 (Resource)
    df.hierarchy |>
    select(Node, ResourceType, ResourceId) |>
    distinct() |>  
    mutate(RT = paste(Node, ResourceType, sep="-")) |>
    mutate(Container = "Container",
           Parent = RT,
           Type = "RES",
           Start = my.start,
           End = my.end,
           Duration = my.duration,
           Name = ResourceId) |>
    select(Container, Parent, Type, Start, End, Duration, Name)
  )
}

read_starvz_export_pjdump <- function(TRACE.base, TRACE.signature) {
  TRACE.dir <- paste0(TRACE.base, "/", TRACE.signature)
  TRACE.pjdump <- paste0(TRACE.signature, ".pjdump")
  options(crayon.enabled=FALSE)
  library(tidyverse)
  library(starvz)
  df <- starvz_read(TRACE.dir)
  df$Application |>
    mutate(Nature = "State") |>
    mutate(Type = "Task") |>
    select(Nature, ResourceId, Type, Start, End, Duration, Depth, Value) |>
    write_csv(TRACE.pjdump, col_names=FALSE)
  # Hierarchy
  generate_hierarchy(df) |>
    write_csv(TRACE.pjdump, append=TRUE, col_names=FALSE)
  # Gantt-chart
  df$config$base_size <- 9
  df$config$st$labels <- "1GPU_per_NODE"
  df$config$st$abe$active <- TRUE
  my.colors <<- starvz:::extract_colors(df$Application, df$Color)  
  st.panel <- panel_st(df, agg_met = "method")
  return(st.panel)
}

options(crayon.enabled=FALSE)
library(tidyverse)
library(lpaggregr)
lpaggreg_utilization <- function(TRACE.pjdump) {
  th=0.0001
  #Without hierarchy
  trace <- parsepjdump(TRACE.pjdump)
  micro <- pjdump2micro(trace, 100,"State",FALSE)
  odf <- oaggregate(micro$data, th)
  q.plot <- qualplot(odf)
  #odf$POpt
  list_of_ps <- as.list(unique(odf$Partitions$Parameter, 3))
  str(list_of_ps)
  lapply(list_of_ps[[1]], # apply only for the first one (TODO)
       function(p){
           oplot_stacked_state(omacro(odf$Partitions, micro, p)) +
               coord_cartesian(ylim=c(0,0.0100), xlim=c(0,100)) +
               ggtitle(paste("p:", p, "optimale:", p==odf$POpt)) +
             scale_fill_manual(values = my.colors)
       }) -> z
  return(list(q.plot, z))
}

cases.list <- c(
	"traces/exageo"#,
#	"traces/chameleon_exps/chameleon_simu_vis_2W+DIF__96000__30",
#	"traces/chameleon_exps/chameleon_simu_vis_COMMG__96000__30",
#	"traces/chameleon_exps/chameleon_simu_vis_COMM1N__96000__30",
#	"traces/chameleon_exps/chameleon_simu_vis_N2GPUL__96000__30",
#	"traces/chameleon_exps/chameleon_simu_vis_BASE__96000__30"
  )
cases.list

lapply(cases.list, function(ELEMENT) {
    TRACE.signature <- basename(ELEMENT)
    TRACE.dir <- dirname(ELEMENT)
    TRACE.pjdump <- paste0(TRACE.signature, ".pjdump")
    TRACE.pdf <-  paste0(TRACE.signature, ".pdf")
    pdf(file=TRACE.pdf)    
    p <- read_starvz_export_pjdump(TRACE.dir, TRACE.signature)
    print(p)
    p <- lpaggreg_utilization(TRACE.pjdump)
    print(p)
    dev.off()
  }) -> z
