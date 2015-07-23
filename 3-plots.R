# Process Peyton's Picarro data for DWP lab experiment
# Ben Bond-Lamberty July 2015

source("0-functions.R")

SCRIPTNAME  	<- "3-plots.R"
SUMMARYDATA      <- paste0(OUTPUT_DIR, "summarydata.csv")  # output from script 2

# ==============================================================================
# Main 

sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in summary data...")
summarydata <- read_csv(SUMMARYDATA)
summarydata$DATETIME <- ymd_hms(summarydata$DATETIME, tz="America/Los_Angeles")
print_dims(summarydata)
print(summary(summarydata))

summarydata <- subset(summarydata, !is.na(CORE))

p <- ggplot(summarydata, aes(DATETIME, CORE, color=STRUCTURE)) + geom_point()
p <- p + ggtitle("QC - measurement time of day")
print(p)
save_plot("QC_time_of_day")

sdata <- subset(summarydata, CORE != "Ambient")

p <- ggplot(sdata, aes(MOISTURE, max_CO2, color=INPUT)) + geom_boxplot()
p <- p + facet_grid(STRUCTURE~WETTING)
print(p)
save_plot("QC_CO2")

p <- ggplot(sdata, aes(MOISTURE, max_CH4, color=INPUT)) + geom_boxplot()
p <- p + facet_grid(STRUCTURE~WETTING)
print(p)
save_plot("QC_CH4")

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
