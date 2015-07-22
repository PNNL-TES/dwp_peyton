# Process Picarro data for DWP lab experiment
# Ben Bond-Lamberty April 2015

source("0-functions.R")

SCRIPTNAME  	<- "3-plots.R"
SUMMARYDATA      <- paste0(OUTPUT_DIR, "summarydata.csv")  # output from script 2


# ==============================================================================
# Main 

sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in summary data...")
summarydata <- read_csv(SUMMARYDATA)
summarydata$DATETIME <- ymd_hms(summarydata$DATETIME)
print_dims(summarydata)
print(summary(summarydata))

summarydata <- subset(summarydata, CH4_dry <= 3.0)

p1 <- ggplot(summarydata, aes(DATETIME, CO2_dry, color=DWP_core)) + geom_point()
p1 <- p1 + facet_grid(Source~Rep, scales="free")
p1 <- p1 + theme(axis.text.x = element_text(angle = 90))
print(p1)
save_plot("CO2_source_rep")
p1 <- ggplot(summarydata, aes(DATETIME, CO2_dry, color=MinDepth_cm)) + geom_point()
p1 <- p1 + facet_grid(Source~Rep, scales="free")
p1 <- p1 + theme(axis.text.x = element_text(angle = 90))
print(p1)
save_plot("CO2_source_rep_depth")
p2 <- ggplot(summarydata[summarydata$CH4_dry < 15,], aes(DATETIME, CH4_dry, color=DWP_core)) + geom_point()
p2 <- p2 + facet_grid(Source~Rep, scales="free")
p2 <- p2 + theme(axis.text.x = element_text(angle = 90))
print(p2)
save_plot("CH4_source_rep")
p2 <- ggplot(summarydata[summarydata$CH4_dry < 15,], aes(DATETIME, CH4_dry, color=MinDepth_cm)) + geom_point()
p2 <- p2 + facet_grid(Source~Rep, scales="free")
p2 <- p2 + theme(axis.text.x = element_text(angle = 90))
print(p2)
save_plot("CH4_source_rep_depth")

printlog("Plotting individual depths...")
for(dwpd in unique(summarydata$Depth_cm)) {
  printlog("Plotting", dwpd)
  d <- subset(summarydata, Depth_cm==dwpd) %>% melt(measure.vars=c("CO2_dry", "CH4_dry"))
  p <- ggplot(d, aes(ELAPSED_TIME/60/60, value, color=Site)) + geom_point()
  p <- p + facet_grid(variable~Trt, scales="free")
  p <- p + ggtitle(paste("Depth", dwpd)) + xlab("Elapsed time (hours)")
  print(p)
  save_plot(paste0("DWP_depth_", dwpd))
}

printlog("Plotting individual cores...")
for(dwpc in unique(summarydata$DWP_core)) {
  printlog("Plotting", dwpc)
  d <- subset(summarydata, DWP_core==dwpc) %>% melt(measure.vars=c("CO2_dry", "CH4_dry"))
  p <- ggplot(d, aes(ELAPSED_TIME/60/60, value, color=Rep)) + geom_point()
  p <- p + facet_grid(variable~Trt, scales="free")
  p <- p + ggtitle(paste("DWP core", dwpc)) + xlab("Elapsed time (hours)")
  print(p)
  save_plot(paste0("DWP_core_", dwpc))
}

printlog("Plotting individual valves...")
for(dwpv in unique(summarydata$Valve)) {
  printlog("Plotting", dwpv)
  d <- subset(summarydata, Valve==dwpv) %>% melt(measure.vars=c("CO2_dry", "CH4_dry"))
  p <- ggplot(d, aes(ELAPSED_TIME/60/60, value, color=Rep)) + geom_point()
  p <- p + facet_grid(variable~Trt, scales="free")
  p <- p + ggtitle(paste("DWP valve", dwpv)) + xlab("Elapsed time (hours)")
  print(p)
  save_plot(paste0("DWP_valve_", dwpv))
}

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
