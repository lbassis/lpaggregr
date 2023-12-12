#!/usr/bin/Rscript
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
  lapply(list_of_ps,
       function(p){
           oplot_stacked_state(omacro(odf$Partitions, micro, p)) +
               coord_cartesian(ylim=c(0,0.0100), xlim=c(0,100)) +
               ggtitle(paste("p:", p, "optimale:", p==odf$POpt)) +
             scale_fill_manual(values = my.colors)
       }) -> z
  return(list(q.plot, z))
}

cases.list <- c(
	"traces/exageo",
	"traces/chameleon_exps/chameleon_simu_vis_2W+DIF__96000__30",
	"traces/chameleon_exps/chameleon_simu_vis_COMMG__96000__30",
	"traces/chameleon_exps/chameleon_simu_vis_COMM1N__96000__30",
	"traces/chameleon_exps/chameleon_simu_vis_N2GPUL__96000__30",
	"traces/chameleon_exps/chameleon_simu_vis_BASE__96000__30"
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
