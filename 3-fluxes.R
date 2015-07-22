# Process Picarro data for DWP lab experiment
# Ben Bond-Lamberty April 2015

source("0-functions.R")

SCRIPTNAME  	<- "3-fluxes.R"
SUMMARYDATA      <- paste0(OUTPUT_DIR, "summarydata.csv")  # output from script 2


# ==============================================================================
# Main 

sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

printlog("Reading in summary data...")
summarydata <- read_csv(SUMMARYDATA)

library(lubridate)
summarydata$DATETIME <- ymd_hms(summarydata$DATETIME)
print_dims(summarydata)
print(str(summarydata))

printlog("Reading and merging headspace data...")
hs <- read_csv("data/DWP2013 headspace_cm.csv")
summarydata <- merge(summarydata, hs, all.x=TRUE)
print_dims(summarydata)

printlog("Filtering data...")
fluxdata <- summarydata %>%
  filter(Trt == "injection data", Source == "Core") %>%
  select(DWP_core, Injection, Rep, samplenum, m_CO2, m_CH4, ELAPSED_TIME, 
         Site, Depth_cm, MinDepth_cm, CoreMassPostInjection_g, headspace_in_core_cm)

# Flux rates are ppm/s (CO2) or ppb/s (CH4) in `summarydata`
# We want to convert these to mg C/g soil/s
# A = dC/dt * V/M * Pa/RT (cf. Steduto et al. 2002), where
# 	A is the flux (µmol/g/s)
#	  dC/dt is raw respiration as above (mole fraction/s)
# 	V is total chamber volume (cm3)
#	  M is [dry] soil mass (g)
#	  Pa is atmospheric pressure (kPa)
#	  R is universal gas constant (8.3 x 10^3 cm3 kPa mol-1 K-1)
#	  T is air temperature (K)

# The instrument tubing is 455 cm long by ID 1/16"
V_tubing <- (1/16 * 2.54 / 2 ) ^ 2 * pi * 455
# Headspace on the core is 7.3 cm diameter by 4 cm height.
V_headspace <- (7.3 / 2) ^ 2 * pi * fluxdata$headspace_in_core_cm
# Internal volume of Picarro?
V_picarro <- 0
fluxdata$V <- V_tubing + V_headspace + V_picarro

Pa 			<- 101						# kPa				(Richland is ~120 m asl)
R 			<- 8.3145e+3			# cm3 kPa K−1 mol−1
Tair    <- 273.1 + 20     # unknown

# Calculate mass-corrected respiration, µmol/g soil/s
fluxdata$CO2_flux_umol_g_s <- fluxdata$m_CO2 / 1e6 * # from ppm/s to mole fraction/s
  fluxdata$V / fluxdata$CoreMassPostInjection_g * Pa / (R * Tair)
fluxdata$CH4_flux_umol_g_s <- fluxdata$m_CH4 / 1e6 * # from ppm/s to mole fraction/s
  fluxdata$V / fluxdata$CoreMassPostInjection_g * Pa / (R * Tair)

# Convert from µmol/g soil/s to mgC/s
fluxdata$CO2_flux_mgC_s <- fluxdata$CO2_flux_umol_g_s * fluxdata$CoreMassPostInjection_g / # get rid of /g soil
  1e6 * # to mol 
  12 *  # to g C
  1000  # to mg C
fluxdata$CH4_flux_mgC_s <- fluxdata$CH4_flux_umol_g_s * fluxdata$CoreMassPostInjection_g / # get rid of /g soil
  1e6 * # to mol 
  16 *  # to g C
  1000  # to mg C

# Right now looks like I'm off by x1000 (too small)? 

fluxdata <- fluxdata[complete.cases(fluxdata),]

# Each core had 6 ml CH4 injected = 0.006 L / 22.413 L/mol = 0.0002677017802 mol x 12 gC/mol = 3.21 mg C


printlog("Computing cumulative C emission...")
fluxdata <- fluxdata %>%
  group_by(Rep, DWP_core) %>%
  arrange(ELAPSED_TIME) %>%
  mutate(CO2_flux_mgC = CO2_flux_mgC_s * (ELAPSED_TIME - lag(ELAPSED_TIME)),
         cumCO2_flux_mgC = c(0, cumsum(CO2_flux_mgC[!is.na(CO2_flux_mgC)])),
         CH4_flux_mgC = CH4_flux_mgC_s * (ELAPSED_TIME - lag(ELAPSED_TIME)),
         cumCH4_flux_mgC = c(0, cumsum(CH4_flux_mgC[!is.na(CH4_flux_mgC)])))

# We ran a subsequent check using cores, 2, 4, and 7, monitoring them continuously to make sure
# we didn't miss any methane or CO2 'burps'. Split off those data separately.
printlog("Splitting data by injection...")
fluxdata_247check <- filter(fluxdata, Injection == 2)
fluxdata <- filter(fluxdata, Injection != 2)

fluxdata_labels <- fluxdata %>%
  group_by(Rep, DWP_core, Depth_cm) %>%
  arrange(ELAPSED_TIME) %>%
  summarise(ELAPSED_TIME = last(ELAPSED_TIME),
            cumCO2_flux_mgC = last(cumCO2_flux_mgC),
            cumCH4_flux_mgC = last(cumCH4_flux_mgC))

printlog("Plotting...")
fluxdata$Depth_cm <- factor(fluxdata$Depth_cm, levels=c("0-30", "30-60", "60-90", "90-120", "120-150", "150-180", "180-210", "210-240"))

p <- ggplot(fluxdata, aes(ELAPSED_TIME/60/60, cumCO2_flux_mgC, color=Depth_cm, group=DWP_core))
p <- p + geom_line() + facet_wrap(~Depth_cm)
p <- p + xlab("Elapsed time (hours)")
p <- p + geom_text(data=fluxdata_labels, aes(label=DWP_core), vjust=-0.5, size=3, show_guide=FALSE)
print(p)
save_plot("cumulative_CO2")

p <- ggplot(fluxdata, aes(ELAPSED_TIME/60/60, cumCH4_flux_mgC, color=Depth_cm, group=DWP_core))
p <- p + geom_line() + facet_wrap(~Depth_cm)
p <- p + xlab("Elapsed time (hours)")
p <- p + geom_text(data=fluxdata_labels, aes(label=DWP_core), vjust=-0.5, size=3, show_guide=FALSE)
print(p)
save_plot("cumulative_CH4")

p <- ggplot(fluxdata, aes(ELAPSED_TIME/60/60, cumCO2_flux_mgC + cumCH4_flux_mgC, color=Depth_cm, group=DWP_core))
p <- p + geom_line() + facet_wrap(~Depth_cm)
p <- p + xlab("Elapsed time (hours)")
p <- p + geom_text(data=fluxdata_labels, aes(label=DWP_core), vjust=-0.5, size=3, show_guide=FALSE)
print(p)
save_plot("cumulative_C")

printlog("Plotting injection 2 data...")
checkplotdata <- fluxdata_247check %>% 
  select(ELAPSED_TIME, Depth_cm, DWP_core, cumCO2_flux_mgC, cumCH4_flux_mgC) %>%
  mutate(cumC_flux_mgC = cumCO2_flux_mgC + cumCH4_flux_mgC) %>%
  melt(measure.vars=c("cumCO2_flux_mgC", "cumCH4_flux_mgC", "cumC_flux_mgC"))
p <- ggplot(checkplotdata, aes(ELAPSED_TIME/60/60, value, color=factor(DWP_core)))
p <- p + geom_line() + facet_grid(variable ~ ., scales='free_y')
p <- p + xlab("Elapsed time (hours)") + ylab("Cumulative C flux (mg C)")
print(p)
save_plot("injection2")


save_data(fluxdata)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
