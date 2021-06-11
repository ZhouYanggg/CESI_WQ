###################################################################################################
# CESI project part 7-Environment Setting for FigureBasin
# Overall Descriptions: 
# This section defines all the variables and set up the environment for FigureBasin script.
# In specific, the following variables will be introduced:
# 1. Loading hydat;
# 2. Define list of previously computed metrics;
# 3. Identify the list of stations of interest & year of reference;
# 4. Define geospatial data that will be applied to map creating.
###################################################################################################

###########      Library             ###########
library(tidyhydat)     # HYDAT access
library(dplyr)         # for select function
library(rgdal)         # for readOGR
library(sp)            # processing spatial data

###########     Obtain flow data     ###########
# If the most recent Hydat.sqlite file is not already in the Packages folder, it will be downloaded.
# The default location of the database will then be set to the file.
# If the file is already downloaded, it will not be downloded again;the default location of 
# the databse will be set to the existing file.
# Keeping a file folder with old Hydat versions to be able to reproduce old work is highly suggested
# ***Make sure Dependencies folder exists. ***
# set hydat location, download if not found
hy_file <- "./Dependencies/Hydat"
if( length( grep("Hydat.sqlite", list.files(hy_file)))==0){
  # hydat file will be downloaded in dependencies and subsequent tidyhydat calls will use this file
  # Highly suggest keeping a file folder with old Hydat versions to be able to reproduce old work
  download_hydat(dl_hydat_here = hy_file)
  hy_db <- paste0(hy_file, "/Hydat.sqlite3")
  hy_set_default_db(hydat_path = hy_db)
  
} else {
  hy_db <- paste0(hy_file, "/Hydat.sqlite3")
  hy_set_default_db(hydat_path = hy_db)
}


##### List of previously calculated metrics #####
# List of variable names
list.var <- c("ann_mean_yield", "pot_days", "pot_events", "pot_max_dur", "X1_day_max", 
              "dr_days", "dr_events", "dut_max_dur", "X7_day_min")
list.var.name <- c("Annual Mean Yield", "Number of Flood Days", "Number of Flood Events",
                   "Maximum Flood Event Duration", "Maximum 1-day Yield",
                   "Number of Drought Days", "Number of Drought Events", 
                   "Maximum Drought Event Duration", "Minimum 7-day Yield")


## Defining list of stations and reference year ##
# all RHBN-U stations:
stations <- read.csv("./Dependencies/RHBN_U.csv", header = TRUE)
stn.list.full <- as.character(stations$STATION_NUMBER)
# reference year for snapshot (and end of trends?)
ref <- 2016
# reference range for animations
ref.range <- c(2001:2016)
# start year for trend calculations; legacy - can probably be hard-wired below
trend.minyr <- 1970
# set aggregate method - mean or median
aggmethod <- "median"


############ Loading Geospatial Data ############
# load geospatial data up front
# Load Pearse drainage area and water body shapefiles
#WaterBodies<- readOGR(dsn = "C:/Users/noteboomm/Documents/CESI/CESI_WQI_Calculator_V_3/Map_Creation", 
                      #"MainWaterBodiest")
WaterBodies<- readOGR(dsn = "./Dependencies", "MainWaterBodiest")
# load ecozone layer (basemap options)
CanadaBound <- readOGR(dsn = "./Dependencies", "CanadaBound")
crs <- basins@proj4string
# Load station points as spatial object, and apply projection information
stn_data <- as.data.frame(hy_stations() %>% select(STATION_NUMBER, LATITUDE, LONGITUDE))
stn_data <- stn_data[stn_data$STATION_NUMBER %in% stn.list.full,]
stn_xy   <- stn_data[,c("LONGITUDE", "LATITUDE")]
crs_wgs  <- CRS( "+init=epsg:4326")
stn_plot <- SpatialPointsDataFrame(coords = stn_xy, data = stn_data, 
                                  proj4string = crs_wgs)
stn_plot <- sp::spTransform(stn_plot, CRSobj = crs)
CanadaBound_proj <- sp::spTransform(CanadaBound, CRSobj = crs)