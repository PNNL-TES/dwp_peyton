# Process Picarro data for Peyton's DWP lab experiment
# Ben Bond-Lamberty July 2015

source("0-functions.R")

SCRIPTNAME  	<- "2-summarize.R"
RAWDATA      <- paste0(OUTPUT_DIR, "rawdata.csv.gz")  # output from script 1

library(stringr)

# ==============================================================================
# Main 

sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in raw data...")
rawdata <- gzfile(RAWDATA) %>% readr::read_csv()
print_dims(rawdata)
print(summary(rawdata))

# Fractional solenoid values mean that the analyzer was shifting
# between two samples. Discard these.
printlog( "Removing fractional MPVPosition" )
rawdata <- subset(rawdata, MPVPosition == trunc(MPVPosition))

# Make true dates
printlog( "Converting date/time info to POSIXct..." )
rawdata$DATETIME <- ymd_hms(paste(rawdata$DATE, rawdata$TIME), tz="America/Los_Angeles")
# Force Picarro timestamps to Pacific time (UTC -8:00)
printlog("Picarro timestamps to Pacific time")
rawdata$DATETIME <- rawdata$DATETIME - 60 * 60 * 8
printlog("First timestamp:")
print(min(rawdata$DATETIME))
printlog("Last timestamp:")
print(max(rawdata$DATETIME))

printlog( "Sorting by date..." )
rawdata <- arrange(rawdata, DATETIME)

# Assign a different sample number to each sample group 
# (we know we're on a new sample when MPVPosition changes)
printlog("Assigning sample numbers...")
oldsampleflag <- with(rawdata, c(FALSE, MPVPosition[-length(MPVPosition)] == MPVPosition[-1]))
rawdata$samplenum <- cumsum(!oldsampleflag)


printlog("Computing elapsed seconds...")
rawdata_samples <- rawdata %>%
  group_by(samplenum) %>%
  mutate(elapsed_seconds = (FRAC_HRS_SINCE_JAN1 - min(FRAC_HRS_SINCE_JAN1)) * 60 * 60) #%>%
#  filter(elapsed_seconds >= min(FLUXWINDOW_S) & elapsed_seconds <= max(FLUXWINDOW_S))

# Load MPVPosition map
printlog("Loading valve map data...")
valvemap <- read_csv("data/DWP2014_Respiration Sample Key_21July2015.csv")
printlog( "Converting date/time info to POSIXct..." )
valvemap$STARTDATE <- mdy(valvemap$STARTDATE, tz="America/Los_Angeles")
valvemap$ENDDATE <- mdy(valvemap$ENDDATE, tz="America/Los_Angeles")
valvemap$valvemaprow <- 1:nrow(valvemap)
printlog("Trimming whitespace from categorical fields...")
valvemap$WETTING <- str_trim(valvemap$WETTING)
valvemap$INPUT <- str_trim(valvemap$INPUT)
valvemap$MOISTURE <- str_trim(valvemap$MOISTURE)
valvemap$STRUCTURE <- str_trim(valvemap$STRUCTURE)

printlog("Visualizing valve map...")
p <- ggplot(valvemap, aes(STARTDATE, MPVPosition, xend=ENDDATE, yend=MPVPosition, color=CORE))
p <- p + geom_segment(size=2) + scale_color_discrete(guide=FALSE)
p <- p + geom_text(aes(label=CORE), size=4, hjust=.5, vjust=-.5)
p <- p + ggtitle("Valve map data (showing core numbers")
print(p)
save_plot("valvemap")

# QC the valve map
dupes <- valvemap %>% 
  group_by(paste(STARTDATE, ENDDATE), MPVPosition, CORE) %>% 
  summarise(n=n()) %>% 
  filter(n > 1)
if(nrow(dupes)) {
  printlog("WARNING - MULTIPLE CORES ASSIGNED TO A VALVE ON A GIVEN DATE")
  print(dupes)
  printlog("WARNING - this will screw up the matching to Picarro data")
}


# Function to match up Picarro data with mapping file data
# This is done by date and valve number (see plot saved above)
matchfun <- function(DATETIME, MPVPosition) {
  row <- which(DATETIME >= valvemap$STARTDATE & 
                 DATETIME <= valvemap$ENDDATE & 
                 MPVPosition == valvemap$MPVPosition)
  if(length(row) != 1) row <- NA
  row
}

printlog( "Computing summary statistics for each sample..." )
# The window in which we look for min and max concentrations
MAX_MINCONC_TIME <- 10  # the minimum concentration has to occur in first 10 s
MIN_MAXCONC_TIME <- 2  # the maximum concentration can't occur in first 2 s

# We want to apply different criteria here, so three different pipelines
# to compute the min and max gas concentrations
summarydata_min <- rawdata_samples %>%
  filter(elapsed_seconds <= MAX_MINCONC_TIME) %>%
  group_by(samplenum) %>%
  summarise(
    min_CO2 = min(CO2_dry),
    min_CO2_time = nth(elapsed_seconds, which.min(CO2_dry)),
    min_CH4 = min(CH4_dry),
    min_CH4_time = nth(elapsed_seconds, which.min(CH4_dry))
  )

summarydata_max <- rawdata_samples %>%
  filter(elapsed_seconds >= MIN_MAXCONC_TIME) %>%
  group_by(samplenum) %>%
  summarise(
    max_CO2 = max(CO2_dry),
    max_CO2_time = nth(elapsed_seconds, which.max(CO2_dry)),
    max_CH4 = max(CH4_dry),
    max_CH4_time = nth(elapsed_seconds, which.max(CH4_dry))
  )

summarydata_other <- rawdata_samples %>%
  group_by(samplenum) %>%
  summarise(
    DATETIME = mean(DATETIME),
    N = n(),
    MPVPosition	= mean(MPVPosition),
    h2o_reported = mean(h2o_reported),
    valvemaprow = matchfun(floor_date(mean(DATETIME), "day"), MPVPosition)
  )

# Merge pieces together to form final summary data set
summarydata <- summarydata_other %>%
  left_join(summarydata_max, by="samplenum") %>% 
  left_join(summarydata_min, by="samplenum")

printlog("Merging Picarro and mapping data...")
summarydata <- left_join(summarydata, valvemap, by=c("MPVPosition", "valvemaprow"), all.x=TRUE)

printlog("Number of samples for each core:")
print(summarydata %>% group_by(CORE) %>% summarise(n()) %>% as.data.frame())

printlog("Summaries for max_CH4 and max_CO2:")
summary(summarydata$max_CO2)
summary(summarydata$max_CH4)

core_na <- filter(summarydata, is.na(CORE))
printlog("NOTE:", nrow(core_na), "samples have no matching core numbers; removing")
save_data(core_na, scriptfolder = FALSE)

printlog("NOTE: cores in the valve map but not in the summary data:")
print(setdiff(valvemap$CORE, summarydata$CORE))

# Done! Drop unnecessary columns and save

summarydata <- summarydata %>%
  filter(!is.na(CORE)) %>%
  select(-MPVPosition, -valvemaprow)

save_data(summarydata, scriptfolder=FALSE)

#summarydata <- subset(summarydata, !is.na(CORE), select=-c(MPVPosition, valvemaprow))	
save_data(rawdata_samples, scriptfolder=FALSE, gzip=TRUE)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
