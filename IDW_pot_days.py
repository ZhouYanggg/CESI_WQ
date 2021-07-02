from qgis import processing

#metrics = ["ann_mean_yield","pot_days","pot_events","pot_max_dur", "pot_mean_dur",
#           "X1_day_max", "dr_days","dut_max_dur","X7_day_min"]

metrics = ["pot_max_dur"]

for metric in metrics:
    inshp = "C:/Users/noteboomm/Documents/CESI/00_Shapefiles/RHBN_U_pts_potmaxdur_trend_May2021hydat.shp"
    outstring = "C:/Users/noteboomm/Documents/CESI/Rasters/"+metric+"_NB"


    # Surface for trends
    attrib = "5"
    print("'INTERPOLATION_DATA':'"+inshp+"::~::0::~::"+str(attrib)+"::~::0'")
    print("'OUTPUT':'"+outstring+"_trend_RHBN-U_IDW5_10k.tif'")

    processing.run("qgis:idwinterpolation",
                   {'INTERPOLATION_DATA':inshp+'::~::0::~::'+str(attrib)+'::~::0',
                   'DISTANCE_COEFFICIENT':5,
                   'EXTENT':'-2313851.626876211,3003894.9530195706,-675872.3326588598,2666752.7970590233 [EPSG:3978]',
                   'PIXEL_SIZE':10000,
                   'OUTPUT':outstring+'_trend_RHBN-U_IDW5_10k.tif'})
