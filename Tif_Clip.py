from qgis import processing
import os

metrics = ["ann_mean_yield","pot_days","pot_events","pot_max_dur",
           "pot_mean_dur", "X1_day_max", "dr_days","dut_max_dur",
           "X7_day_min"]

metrics = ["pot_mean_dur"]

for metric in metrics:
    for year in range(1970,2020):

        processing.run("gdal:cliprasterbymasklayer", 
                      {'INPUT':'C:/Users/noteboomm/Documents/CESI/Rasters/'+metric+'_'+str(year)+'_abs_RHBN-U_IDW5_10k.tif',
                       'MASK':'C:/Users/noteboomm/Documents/Canada_Reference/CanadaBound.shp',
                       'SOURCE_CRS':None,'TARGET_CRS':None,'NODATA':None,'ALPHA_BAND':False,
                       'CROP_TO_CUTLINE':True,'KEEP_RESOLUTION':True,'SET_RESOLUTION':False,'X_RESOLUTION':None,
                       'Y_RESOLUTION':None,'MULTITHREADING':False,'OPTIONS':'','DATA_TYPE':0,'EXTRA':'',
                       'OUTPUT':'C:/Users/noteboomm/Documents/CESI/Rasters/_temp.tif'})
                       
        processing.run("gdal:cliprasterbymasklayer", 
                      {'INPUT':'C:/Users/noteboomm/Documents/CESI/Rasters/_temp.tif',
                       'MASK':'C:/Users/noteboomm/Documents/Canada_Reference/CanadaBound_noQEIslands.shp',
                       'SOURCE_CRS':None,'TARGET_CRS':None,'NODATA':None,'ALPHA_BAND':False,
                       'CROP_TO_CUTLINE':False,'KEEP_RESOLUTION':True,'SET_RESOLUTION':False,'X_RESOLUTION':None,
                       'Y_RESOLUTION':None,'MULTITHREADING':False,'OPTIONS':'','DATA_TYPE':0,'EXTRA':'',
                       'OUTPUT':'C:/Users/noteboomm/Documents/CESI/Rasters/'+metric+'_'+str(year)+'_abs_RHBN-U_IDW5_10k_clip.tif'})
        
        os.remove('C:/Users/noteboomm/Documents/CESI/Rasters/_temp.tif')