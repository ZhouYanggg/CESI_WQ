###################################################################################################
# CESI project part 1- Download the files& Set up the path
# Overall Desciptions: 
# This section deals with the intialization of the project in the following ways:
# 1. Guarantees the existance of Hydat.sqlite file.This file is mandatory for subsequent codes.
# 2. Set up the path for the hydat file, which will be used by subsequent calls.
###################################################################################################

###########      Library             ###########
library(tidyhydat)     # HYDAT access

###########     Obtain flow data     ###########
# If the most recent Hydat.sqlite file is not already in the Packages folder, it will be downloaded.
# The default location of the database will then be set to the file.
# If the file is already downloaded, it will not be downloded again;the default location of 
# the databse will be set to the existing file.
# Keeping a file folder with old Hydat versions to be able to reproduce old work is highly suggested
# ***Make sure Dependencies folder exists. ***
hy_file <- "../Dependencies/Hydat"
if( length( grep("Hydat", list.files(hy_file)))==0){
  
  # hydat file will be downloaded in dependencies and subsequent tidyhydat calls will use this file
  download_hydat(dl_hydat_here = hy_file)
  hy_db <- paste0(hy_file, "/Hydat.sqlite3")
  hy_set_default_db(hydat_path = hy_db)
  
} else { #if the file is already downloaded
  hy_db <- paste0(hy_file, "/Hydat.sqlite3")
  hy_set_default_db(hydat_path = hy_db)
}

area_file <- "../Dependencies/RHBN_NandU_watershedareas.csv"
area <- read.table(area_file, sep=',', header=T)