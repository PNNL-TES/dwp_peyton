# Process Picarro data for DWP lab experiment
# Ben Bond-Lamberty April 2015

source("0-functions.R")

SCRIPTNAME  	<- "2-summarize.R"
RAWDATA      <- paste0(OUTPUT_DIR, "rawdata.csv.gz")  # output from script 1


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
rawdata$DATETIME <- ymd_hms(paste(rawdata$DATE, rawdata$TIME))

printlog( "Sorting by date..." )
rawdata <- arrange(rawdata, DATETIME)

# Assign a different sample number to each sample group 
# (we know we're on a new sample when MPVPosition changes)
printlog("Assigning sample numbers...")
oldsampleflag <- with(rawdata, c(FALSE, 
                                  MPVPosition[-length(MPVPosition)] == MPVPosition[-1] &
                                    trt[-length(trt)] == trt[-1]))
rawdata$samplenum <- cumsum(!oldsampleflag)

FLUXWINDOW_S <- 30

printlog("Computing elapsed seconds...")
rawdata <- rawdata %>%
  group_by(samplenum) %>%
  filter(trt == "injection data") %>%
  mutate(elapsed_seconds = (FRAC_HRS_SINCE_JAN1 - min(FRAC_HRS_SINCE_JAN1)) * 60 * 60) %>%
  filter(elapsed_seconds <= FLUXWINDOW_S)

printlog( "Computing summary statistics for each sample..." )
summarydata <- rawdata %>%
  group_by(samplenum) %>%
  summarise(
    EPOCH_TIME = mean(EPOCH_TIME),
    DATETIME = mean(DATETIME),
    ALARM_STATUS = mean(ALARM_STATUS),
    INST_STATUS	= mean(INST_STATUS),
    N = n(),
    Valve	= mean(MPVPosition),
    
    fluxwindow_s = FLUXWINDOW_S,
    mean_CO2_early = mean(CO2_dry[which(elapsed_seconds < 1/3 * FLUXWINDOW_S)]),
    mean_CH4_early = mean(CH4_dry[which(elapsed_seconds < 1/3 * FLUXWINDOW_S)]),
    mean_CO2_late = mean(CO2_dry[which(elapsed_seconds >= 2/3 * FLUXWINDOW_S)]),
    mean_CH4_late = mean(CH4_dry[which(elapsed_seconds >= 2/3 * FLUXWINDOW_S)]),
    m_CO2 = (mean_CO2_late - mean_CO2_early) / (2/3 * FLUXWINDOW_S),
    m_CH4 = (mean_CH4_late - mean_CH4_early) / (2/3 * FLUXWINDOW_S),
    CH4_dry = mean(CH4_dry),
    CO2_dry = mean(CO2_dry),
    
    h2o_reported = mean(h2o_reported),
    Injection = mean(injection),
    Rep = paste(unique(rep), collapse=","),
    Trt = paste(unique(trt), collapse=",")
  )


# Load MPVPosition map
printlog("Loading valve map data and merging...")
valvemap <- read_csv("data/DWP_valve assignment 3 March2105.csv")
summarydata <- merge(summarydata, valvemap, by=c("Injection", "Rep", "Valve"))

# Injection 2 was different than injection 1: it consisted of three cores in sequence,
# always on valve 12, monitored continuously (alternating with an ambient on valve 10).
# Valve 11 appears in the raw data but is an artifact. From Sarah Fansler's email:
# Core #4 injected with 6 ml methane at 3 pm on June 26.
# Core #2 injected with 6 ml methane at 10:22 am on June 30.
# Core #7 injected at 10:56 am on July 3.
# Data pulled at 3:25 pm on 7 July.
inj2 <- with(summarydata, Injection == 2 & Valve == 12)
summarydata$DWP_core[inj2] <- "7"
# Note that the Picarro is on UTC already
summarydata$DWP_core[inj2 & summarydata$DATETIME < ymd_hm("2015-07-03 10:56")] <- "2"
summarydata$DWP_core[inj2 & summarydata$DATETIME < ymd_hm("2015-06-30 10:22")] <- "4"

# Assign 'Source' field
summarydata$Source <- "Core"
summarydata$Source[summarydata$DWP_core == "ambient"] <- "Ambient"
summarydata$Source[summarydata$DWP_core == "CH4 blank"] <- "Blank"

# Load field data to get depth information
printlog("Loading field data and merging...")
fielddata <- read_csv("data/DWP Core field data.csv")
fielddata$Notes <- fielddata$SampleDate <- fielddata$SamplePoint <- NULL
summarydata <- merge(summarydata, fielddata, all.x = TRUE)

# Load mass data
printlog("Loading mass data and merging...")
massdata <- read_csv("Core inj log_mass 9March2015.csv", datadir = "data/")
summarydata <- merge(summarydata, massdata)

printlog("Computing min depth...")
summarydata$MinDepth_cm <- as.numeric(str_extract(summarydata$Depth_cm, "^[0-9]*"))

printlog("Computing elapsed times...")
summarydata <- summarydata %>% 
  group_by(Rep, DWP_core, Trt) %>% 
  mutate(ELAPSED_TIME=as.numeric(DATETIME - min(DATETIME)))

# Done!
save_data(summarydata, scriptfolder=FALSE)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
