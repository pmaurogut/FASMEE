# TODO: Add comment
# 
# Author: Paco
###############################################################################
library(rvest)
library(plyr)
library(sf)

shape_index<-st_read("C:/Users/Paco/CorvallisWS/FASMEE/Data/COE/MERGE_LAS_SERVER_with_path.shp")
test_plots<-st_read("C:/Users/Paco/CorvallisWS/FASMEE/Data/tileindex/Test_plots.shp")
st_crs(test_plots)<-st_crs(polygons)

