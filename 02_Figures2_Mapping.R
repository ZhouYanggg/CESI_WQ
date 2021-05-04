###################################################################################################
# CESI project part 9-Mapping
# Overall Descriptions: 
# This section takes all the metrics calculated before to create maps regarding the metrics
# as well as the trend detected using Mann-Kendall Test.
###################################################################################################


###########        Library         ###########
#library("lubridate")
#library("latex2exp")
#library("purrr")
#library("viridis")    # colour scales: try 'cividis' or 'viridis'
#library("scales")
#library("reshape2")
#library("rgdal")
#library("rgeos")
#library("sf")        # For plotting maps
#library("ggplot2") 
library("dplyr")     # For data tidying and data tables
library("grDevices")

#### Creating aggregate and map figures #####
if (dir.exists("./Figures/National_Quantiles")==FALSE){
  dir.create("./Figures/National_Quantiles")
}
if (dir.exists("./Figures/National_Trends")==FALSE){
  dir.create("./Figures/National_Trends")
}
for (i in 1:length(num)){ 
  num2 <- 1 #num[i]
  # Identify variable for this figure
  var2 <- list.var[num2]
  var.name2 <- list.var.name[num2]
  min <- min(results.all[results.all$variable==var2, paste0("result.", ref)], na.rm = TRUE)
  max <- max(results.all[results.all$variable==var2, paste0("result.", ref)], na.rm = TRUE)
  limits <- list(min =min, max=max)
  
  # create a single equi-distant colour scale
  l.min <- limits$min
  l.max <- limits$max
  
  #### Creating Map #####
  # what follows is a bunch of rather inelegant steps to define limits and station collections
  # that have different status and different colour/style on the maps.
  stn.trend <- as.character(results.all[!is.na(results.all$CATTrend) & results.all$variable==var2, "STATION_NUMBER"])
  stn.trendhi <- as.character(results.all[(results.all[["CATTrend"]]=="Confident Upward" | results.all[["CATTrend"]]=="Confident Downward")
                                         & results.all$variable==var2 & !is.na(results.all[["CATTrend"]]), "STATION_NUMBER"])
  stn.trendlo <- as.character(results.all[(results.all[["CATTrend"]]=="Likely Upward" | results.all[["CATTrend"]]=="Likely Downward")
                                         & results.all$variable==var2 & !is.na(results.all[["CATTrend"]]), "STATION_NUMBER"])
  stn.trendno <- as.character(results.all[(results.all[["CATTrend"]]=="Uncertain")
                                         & results.all$variable==var2 & !is.na(results.all[["CATTrend"]]), "STATION_NUMBER"])
  stn_fullt <- stn_plot[stn_plot$STATION_NUMBER %in% stn.trend,]
  stn_fullthi <- stn_plot[stn_plot$STATION_NUMBER %in% stn.trendhi,]
  stn_fulltlo <- stn_plot[stn_plot$STATION_NUMBER %in% stn.trendlo,]
  stn_fulltno <- stn_plot[stn_plot$STATION_NUMBER %in% stn.trendno,]
  stn_greyt <- stn_plot[!(stn_plot$STATION_NUMBER %in% stn.trend),]
 
  # Some definitions for colours and drawing order in the maps
  Ind.2016.var <- results.all[results.all$variable==var2,]
  stn_fullt@data <- left_join(stn_fullt@data, Ind.2016.var, by=c("STATION_NUMBER")) # trends
  stn_fullthi@data <- left_join(stn_fullthi@data, Ind.2016.var, by=c("STATION_NUMBER")) # high conf trends
  stn_fulltlo@data <- left_join(stn_fulltlo@data, Ind.2016.var, by=c("STATION_NUMBER")) # low conf trends
  stn_fulltno@data <- left_join(stn_fulltno@data, Ind.2016.var, by=c("STATION_NUMBER")) # no trend
  
  #colours for shade variation for confidence
  stn_fullt@data$ColTrend <- case_when(stn_fullt@data$Z >=  1.28                            ~ "#0000bb",
                                       stn_fullt@data$Z >=  0.52 & stn_fullt@data$Z <  1.28 ~ "#0000bb", # #7070bb for shading
                                       stn_fullt@data$Z >  -0.52 & stn_fullt@data$Z <  0.52 ~ "#009b00",
                                       stn_fullt@data$Z <= -0.52 & stn_fullt@data$Z > -1.28 ~ "#bb0000", # #bb7070 for shading
                                       stn_fullt@data$Z <= -1.28                            ~ "#bb0000")
  stn_fullthi@data$ColTrend <- case_when(stn_fullthi@data$Z > 0                        ~ "#0000bb",
                                         stn_fullthi@data$Z < 0                        ~ "#bb0000")
  stn_fulltlo@data$ColTrend <- case_when(stn_fulltlo@data$Z > 0                        ~ "#0000bb", # #7070bb for shading
                                         stn_fulltlo@data$Z < 0                        ~ "#bb0000") # #bb7070 for shading

  
  stn_fullt@data$shape    <- case_when(stn_fullt@data$Z      >= 0.52 ~ 24, # up triangle
                                       stn_fullt@data$Z      <=-0.52 ~ 25, # down triangle
                                       abs(stn_fullt@data$Z) <  0.52 ~ 21) # circle
  stn_fullthi@data$shape    <- case_when(stn_fullthi@data$Z  >  0    ~ 24, # up triangle
                                         stn_fullthi@data$Z  <  0    ~ 25) # down triangle
  stn_fulltlo@data$shape    <- case_when(stn_fulltlo@data$Z  >  0    ~ 24, # up triangle
                                         stn_fulltlo@data$Z  <  0    ~ 25) # down triangle
  
  # sizes for using shade variation for confidence
  stn_fullt@data$size     <- case_when(abs(stn_fullt@data$Z) >= 0.52 ~ 1,
                                       abs(stn_fullt@data$Z) <  0.52 ~ 0.8)
  
  # loop through reference years to make snapshot map for each year, to be animated externally
  for (refyear in ref.range){
    print(refyear)
    stn.quant <- as.character(results.all[!is.na(results.all[[paste0("rank.",refyear)]])
                                         & results.all$variable==var2, "STATION_NUMBER"])
    stn_fullq <- stn_plot[stn_plot$STATION_NUMBER %in% stn.quant,]
    stn_fullq@data <- left_join(stn_fullq@data, Ind.2016.var, by=c("STATION_NUMBER")) # quantiles
    stn_fullq@data$Colours  <- case_when(stn_fullq@data[[paste0("rank.",refyear)]] > 75 ~ "#0000bb", # viridis yellow=fde725
                                         stn_fullq@data[[paste0("rank.",refyear)]] <= 75 & stn_fullq@data[[paste0("rank.",refyear)]] >= 25 ~ "#009b00", # viridis green=1f9e89
                                         stn_fullq@data[[paste0("rank.",refyear)]] < 25 ~ "#bb0000") #, # viridis purple=482878
    
    
#### Quantile map for flow magnitude of 2016 vs historic ####
    png(filename =paste0("./Figures/National_Quantiles/Map_",refyear,"_", var.name2, "_Quantile_RHBN_U.png"),
        width = 20, height = 18, res=600, units = "cm")
    # plot(basins, col=grey.colors(7, start = 0.9, end=0.9), border="black") # use this line for drainage basin base
    plot(CanadaBound_proj, col=grey.colors(7, start = 0.9, end=0.9), border="black") # use this line for ecozone base
    plot(WaterBodies, col="lightskyblue1", border="steelblue1", add=TRUE)
    # plot(stn_greyq, pch=20, col="grey20", cex=1, add=TRUE) # don't plot all the stations with no/insufficient data
    plot(stn_fullq, pch=21, bg=stn_fullq@data$Colours, col=stn_fullq@data$Colours, add=TRUE) #col="#000000",
    par(mar=c(0,0,0,0), usr=c(-145, -50, 40, 85))
    title(refyear, col = "#000000", line = -3 )
    dev.off()
  }
  
  
#### Trend map coloured by trend confidence, shapes for direction of trend ####
  png(filename =paste0("./Figures/National_Trends/Map_", var.name2, "_Trends1970-",refyear,"_RHBN_U.png"),
      width = 20, height = 18, res=600, units = "cm")
  # plot(basins, col=grey.colors(7, start = 0.9, end=0.9), border="black") # use this line for drainage basin base
  plot(CanadaBound_proj, col=grey.colors(7, start = 0.9, end=0.9), border="black") # use this line for ecozone base
  plot(WaterBodies, col="lightskyblue1", border="steelblue1", add=TRUE) 
  # plot(stn_greyt, pch=20, col="grey20", cex=1, add=TRUE)
  plot(stn_fulltno, pch = 21, bg = "#009b00", cex = 0.8,
       col = "#009b00", add = TRUE)
  plot(stn_fulltlo, pch = stn_fulltlo@data$shape, bg = stn_fulltlo@data$ColTrend, cex = 1,
       col = stn_fulltlo@data$ColTrend, add = TRUE)
  plot(stn_fullthi, pch = stn_fullthi@data$shape, bg = stn_fullthi@data$ColTrend, cex = 1,
       col = stn_fullthi@data$ColTrend, add = TRUE)
  par(mar=c(0,0,0,0), usr=c(-145, -50, 40, 85))
  dev.off()
}  