# Process Peyton's Picarro data for DWP lab experiment
# Ben Bond-Lamberty July 2015

source("0-functions.R")

SCRIPTNAME  	<- "4-plots.R"
FLUXDATA      <- paste0(OUTPUT_DIR, "fluxdata.csv")  # output from script 3
RAWDATA_SAMPLES      <- paste0(OUTPUT_DIR, "rawdata_samples.csv.gz")  # output from script 2

# ==============================================================================
# Main 

sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in flux data...")
fluxdata <- read_csv(FLUXDATA)
fluxdata$DATETIME <- ymd_hms(fluxdata$DATETIME, tz="America/Los_Angeles")
print_dims(fluxdata)
print(summary(fluxdata))

fluxdata <- subset(fluxdata, !is.na(CORE))

p <- ggplot(fluxdata, aes(DATETIME, CORE, color=STRUCTURE)) + geom_point()
p <- p + ggtitle("QC - measurement time of day")
print(p)
save_plot("QC_time_of_day")

sdata <- subset(fluxdata, CORE != "Ambient")

# Histograms of the flux numbers
p <- ggplot(sdata, aes(x = CO2_flux_mgC_s)) + geom_histogram()
p <- p + facet_grid(STRUCTURE~WETTING)
print(p)
save_plot("QC_CO2_flux_distribution")
p <- ggplot(sdata, aes(x = CH4_flux_mgC_s)) + geom_histogram()
p <- p + facet_grid(STRUCTURE~WETTING)
print(p)
save_plot("QC_CH4_flux_distribution")

p <- ggplot(sdata, aes(MOISTURE, CO2_flux_mgC_s, color=INPUT)) + geom_boxplot()
p <- p + facet_grid(STRUCTURE~WETTING)
print(p)
save_plot("QC_CO2")

p <- ggplot(sdata, aes(MOISTURE, CH4_flux_mgC_s, color=INPUT)) + geom_boxplot()
p <- p + facet_grid(STRUCTURE~WETTING)
print(p)
save_plot("QC_CH4")

# QC negative-flux data
negs <- subset(sdata, CO2_flux_mgC_s < 0.0)
rawdata_negs <- read_csv(RAWDATA_SAMPLES) %>% filter(samplenum %in% negs$samplenum)



printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log