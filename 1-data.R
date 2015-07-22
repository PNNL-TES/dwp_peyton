# Process Picarro data for DWP lab experiment
# This script reads all available data, writing to a `rawdata` file
# Ben Bond-Lamberty April 2015

source("0-functions.R")

library(stringr)

SCRIPTNAME  	<- "1-data.R"
DATA_DIR      <- "data/"
INJECTION_DIR_PATTERN <- "^CH4 injection "  # regex pattern for data directories

# -----------------------------------------------------------------------------
# read a single output file, returning data frame
read_outputfile <- function(fqfn) {
  printlog("Reading", fqfn)
  stopifnot(file.exists(fqfn))
  
  f <- fqfn
  if(grepl(".gz$", fqfn)) {
    f <- gzfile(fqfn)
  } else if(grepl(".zip$", fqfn)) {
    f <- unz(fqfn)
  }
  
  d <- read.table(f, header=T)
  print_dims(d)
  
  # Add ancillary data
  d$file <- basename(fqfn)
#  d$dir <- dirname(fqfn)
  
  return(d)
} # read_outputfile

# -----------------------------------------------------------------------------
# scan a directory and process all files in it, returning tempfile names
process_directory <- function(input_path, tempfile, injection, rep, trt) {
  samplenum <- 0
  filelist <- list.files(path=input_path, pattern="dat$|dat.gz$|dat.zip$", recursive=T)
  ncolumns <- NA
  for(f in seq_along(filelist)) {
    d <- read_outputfile(file.path(input_path, filelist[f]))
    d$injection <- injection
    d$rep <- rep
    d$trt <- trt
    
    if(f > 1 & ncol(d) != ncolumns)
      stop("Columns differ between files!")
    ncolumns <- ncol(d)

    first <- !file.exists(tempfile)
    write.table(d, tempfile, row.names=FALSE, append=!first, col.names=first, sep=",")
  }
}


# ==============================================================================
# Main 

sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Data directory is", DATA_DIR)

dirlist <- list.files(DATA_DIR, pattern=INJECTION_DIR_PATTERN)

tf <- tempfile()
printlog("Tempfile is", tf)

for(dir in dirlist) {
  printlog("Processing", dir)
  
  # Directory names should follow form "$INJECTION_DIR_PATTERN x rep y"
  # where x is the injection number and y is the rep name. Extract these
  d1 <- str_replace(dir, INJECTION_DIR_PATTERN, "") %>% 
    str_trim()
  injection <- str_extract(d1, "^[0-9]* ") %>% as.numeric()
  rep <- str_extract(d1, "[A-Z]*$")
  printlog("injection", injection, "rep", rep)
  for(trt in list.files(file.path(DATA_DIR, dir))) {
    process_directory(file.path(DATA_DIR, dir, trt), tf, injection, rep, trt)
  }
}

printlog(SEPARATOR)
printlog("Reading in full data set...")
rawdata <- readr::read_csv(tf)
print_dims(rawdata)
print(summary(rawdata))

printlog("Writing output file...")
save_data(rawdata, scriptfolder=FALSE, gzip=TRUE)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
