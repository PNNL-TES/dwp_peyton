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
summarydata$DATETIME <- ymd_hms(summarydata$DATETIME)
print_dims(summarydata)
print(summary(summarydata))

p <- ggplot(summarydata, aes(DATETIME, hour(DATETIME), color=samplenum))
p <- p + geom_point() + ggtitle("QC - measurement time of day")
print(p)
save_plot("QC_time_of_day")

print(p + geom_)
printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
