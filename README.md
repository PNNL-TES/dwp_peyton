# dwp_peyton
R code for [@apeyton_smith](https://twitter.com/apeyton_smith) above/below wetting experiment. This pipeline starts with reading CO2/CH4 data as downloaded from the Picarro analyzer, processes it, computes mass-normalized fluxes and cumulative respired C, does some QC, and generates figures and statistics.

This code makes heavy use of the [dplyr](https://github.com/hadley/dplyr) package. All scripts write log files that include dates, package and R version numbers, diagnostics, etc. They are designed to be run in numerical order.

# What these scripts do
## `0-functions.R` 
Loads required libraries and provides support functions for all the other scripts, including

* `printlog()` - a time-stamped output function
* `save_plot()` - saves a ggplot figure into the `OUTPUT_DIR` directory
* `save_data()` - saves a data frame to CSV, optionally gzip'd, into the `OUTPUT_DIR` directory
* `read_csv()` - reads a CSV file, optionally gzip'd

## `1-data.R` 
Reads all raw Picarro data from a specified directory, concatenates it all together, and writes the resulting data frame to a `rawdata.csv.gz` file.

## `2-summarize.R` 
This is the heart of the pipeline. It performs the following major steps:

* Read in the raw data file written by the previous script
* Compute a new `DATETIME` field, converting the Picarro timestamps from UTC to Pacific Coast time
* Assign sample numbers. A *sample* is a group of gas concentration measurements made all on the same multiplexer valve and continuous in time
* Compute elapsed seconds for each measurement in the sample; the first gets a value of 0.
* Loads the *valve map*. This is a file that maps the valve numbers (`MPVPosition`) to actual experiment cores. A number of minor cleanup operations are performed on the map data, and a figure is generated for QC purposes (to help identify mis-assigned or missing cores, for example)
* Compute summary statistics for each sample. This has the following substeps, each its own `dplyr` pipeline: (i) for each sample, find the minimum CO2 and CH4 concentrations observed within the first `MAX_MINCONC_TIME` seconds; (ii) find the maximum concentration of each gas that occurs after the minimum, but before `MAX_MAXCONC_TIME` seconds; (iii) compute the mean `DATETIME`, number of observations, etc.; (iv) using the date information and the valve number, match the sample to a row in the valve map
* Merge the summarized Picarro data and the valve map data
* Compute `elapsed_minutes` *across* samples. Basically each core has a `STARTDATETIME` assigned to it in the valve map, and we compute how many minutes have elapsed since then
* Print diagnostics and do some QC, in particular looking for orphan samples and/or orphan cores (data mismatches)
* Write the final summarized data to `summarydata.csv`

## `3-fluxes.R` 

This script computes actual carbon fluxes by gas.

* Read in summary data written by the previous script
* For each sample summary, uses the min/max gas concentrations and time to compute the flux this implies using the ideal gas law. This uses volume (dependent in part on headspace volume, as given in the valve map file), pressure, temperature, universal gas constant, and sample mass (also specified in the valve map data)
* We compute the instantaneous flux both normalized (µmol/g soil/s) and absolute (mgC/hr), and then, based on `elapsed_minutes`, the cumulative emission for each gas
* Write the final data to `fluxdata.csv`

## `4-plots.R` 

This script makes a variety of plots: QC plots, histograms of the computed fluxes, flux comparisons broken down by various factors, and does a naive (currently) statistical summary, fitting a linear model using `WETTING`, `STRUCTURE`, etc.

