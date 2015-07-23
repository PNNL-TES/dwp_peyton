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


FLUXWINDOW_S <- c(5, 120)
printlog("Computing elapsed seconds using flux window of", FLUXWINDOW_S, "...")
rawdata_trunc <- rawdata %>%
  group_by(samplenum) %>%
  mutate(elapsed_seconds = (FRAC_HRS_SINCE_JAN1 - min(FRAC_HRS_SINCE_JAN1)) * 60 * 60) %>%
  filter(elapsed_seconds >= min(FLUXWINDOW_S) & elapsed_seconds <= max(FLUXWINDOW_S))

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

# Function to match up Picarro data with mapping file data--done by date and valve number
matchfun <- function(DATETIME, MPVPosition) {
  row <- which(DATETIME >= valvemap$STARTDATE & 
                 DATETIME <= valvemap$ENDDATE & 
                 MPVPosition == valvemap$MPVPosition)
  if(length(row) != 1) row <- NA
  row
}

printlog( "Computing summary statistics for each sample..." )
summarydata <- rawdata_trunc %>%
  group_by(samplenum) %>%
  summarise(
    EPOCH_TIME = mean(EPOCH_TIME),
    DATETIME = mean(DATETIME), #floor_date(mean(DATETIME), "day"),
    ALARM_STATUS = mean(ALARM_STATUS),
    INST_STATUS	= mean(INST_STATUS),
    N = n(),
    MPVPosition	= mean(MPVPosition),
    
    #    fluxwindow_s = FLUXWINDOW_S,
    max_CO2 = max(CO2_dry),
    max_CH4 = max(CH4_dry),
    
    h2o_reported = mean(h2o_reported),
    
    valvemaprow = matchfun(floor_date(mean(DATETIME), "day"), MPVPosition)
  ) %>%
  arrange(samplenum)


printlog("Merging Picarro and mapping data...")
summarydata <- merge(summarydata, valvemap, by=c("MPVPosition", "valvemaprow"), all.x=TRUE)

printlog("Number of samples for each core:")
print(summarydata %>% group_by(CORE) %>% summarise(n()) %>% as.data.frame())

printlog("Summaries for max_CH4 and max_CO2:")
summary(summarydata$max_CO2)
summary(summarydata$max_CH4)

# Done! Drop unnecessary columns and save

summarydata <- subset(summarydata, 
                      select=-c(MPVPosition, valvemaprow, samplenum,
                                EPOCH_TIME, ALARM_STATUS, INST_STATUS))	
save_data(summarydata, scriptfolder=FALSE)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
