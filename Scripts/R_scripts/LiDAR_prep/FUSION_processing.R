# TODO: Add comment
# 
# Author: Paco
###############################################################################
library("lidR")
library("rgdal")
library("raster")
library("sf")
library("rgeos")
library("parallel")
library("foreach")
memory.limit(12000)
#Back transform plots or processing polygons for NOAA it has to be back in 
#GRS80 lat long before interacting with the las files

#This function makes a catalog with the intersections
#The output is a spatial polygon dataframe with fields for 
#the path of the tile and the path where each chunk will end up
make_pus<-function(Sp_catalog,plots,Tile_URL="URL",Plot_ID="Plot_ID",
		tilesfolder,plotsfolder,chunksfolder,
		dsn=getwd(),outputfile=NULL){

## 	TODO: Test which and proceed accordingly. (Maybe not necessary).
	if(!inherits(Sp_catalog,"sf")){
		
		LIDAR_catalog<-st_sf(Sp_catalog)
		
	}
	Sp_catalog<-st_intersection(Sp_catalog)
	Sp_catalog<-st_collection_extract(Sp_catalog,"POLYGON")
	PU<-st_intersection(Sp_catalog,plots)
	PU<-as(PU,"Spatial")
	if(any(colnames(PU@data)=="origins")){
		
		PU<-PU[,-which(colnames(PU@data)=="origins")]
		
	}
	
	
	PU$Tile_local_path<-sub("(.*\\/)",
			paste(tilesfolder,"/",sep=""),PU@data[,Tile_URL])
	PU$Chunk_local_path<-paste(chunksfolder,"/",c(1:dim(PU)[1]),".las",sep="")
	PU$Plot_local_path<-paste(plotsfolder,"/",PU[[Plot_ID]],".las",sep="")
	
	if(!is.null(outputfile)){
		
		writeOGR(PU,dsn,outputfile,driver="ESRI Shapefile")
		
	}
		
	PU	
	
}

#This just makes the folders where everything goes
make_pu_folders<-function(tilesfolder,plotsfolder,chunksfolder){

		dir.create(tilesfolder)
		dir.create(plotsfolder)
		dir.create(chunksfolder)
}

#This functions just splits the catalog by tiles (or other field)
make_list_of_pus<-function(PU,Tile_loc="Tile_local_path"){
	
	tiles<-unique(PU@data[,Tile_loc])
	names(tiles)<-sub("(.*\\/)","Tile_",tiles)
	lapply(tiles,FUN=function(x,PU,Tile_loc){
				
				PU[PU[[Tile_loc]]==x,]
				
			},PU=PU,Tile_loc=Tile_loc)
	
	
}

#This function takes the a subset of geoms (in SpatialDataFrame format). Make the clip of its
#corresponging tile and ad a TRUE FALSE depending on the success of the clipping
#returns the geoms with the field for the success
clip_tileR<-function(geoms,tile_path){
	geoms@data$success<-FALSE
	lastile<-readLAS(tile_path)
	for(i in 1:dim(geoms)[1]){
		try(
		{
			geom_i<-geoms[i,]
			outfile<-geoms@data[i,"Chunk_local_path"]
			output<-lasclip(lastile,geom_i)
			writeLAS(output[[1]],outfile)
			geoms@data[i,"success"]<-TRUE
		}
		)	
	}
	geoms
	
}

#This function tries the download and clips all geoms in the tile
#It supposed to work with the geoms that intersect a single tile
download_clip<-function(geoms,URL="URL",Tiles_path="Tile_local_path"){
	
	tile_URL<-geoms@data[1,URL]
	tile_path<-geoms@data[1,Tiles_path]
	a<-try(download.file(tile_URL,tile_path,mode="wb"))
	if(class(a)=="try-error"){stop("download-failed")}
	clip_tileR(geoms,tile_path)
}

#Glues the other pieces together and filters the successes. Here I will 
#include all the removal of X Y coordinates etc
make_acquisition<-function(make_pu_args,fileter_field=NULL,.export=c(),...){
	
	PUs<-do.call(make_pus,make_pu_args,envir=as.environment(make_pu_args))
	if(!is.null(filter_field)){
		
		PUs<-PUs@data[PUs@data[fileter_field,],]
		
	}
	
	PUs<-make_list_of_pus(PUs)
#	This is a line I uncomment for testing
#	PUs<-PUs[1:10]
	.export<-c(.export,"clip_tileR","download_clip")
	a<-foreach(x=PUs,...,.export=.export)%dopar% try(download_clip(x))
#	removes the fails in the download
	a<-a[sapply(a,inherits,"SpatialPolygonsDataFrame")]
	a<-do.call(rbind,a)
#	removes the fails in the clipping
	a[a@data[,"success"],]
	
}


#All catalogs neeed to be in Lambert (Always needed for NOAA)
#All plots to Lambert

LIDAR_catalog<-st_read("FASMEE/Data/Tile_indexes/Test/Test_LAS.shp",
		stringsAsFactors =FALSE)
plots<-st_read("FASMEE/Data/Tile_indexes/Test/test_points.shp")
buffered_plots<-st_buffer(plots,dist=250*(1/0.3048))
buffered_plots[,"keep"]<-TRUE
buffered_plots2<-st_buffer(plots,dist=1000*(1/0.3048))
buffered_plots[,"keep"]<-FALSE
buffered_plots4<-rbind(buffered_plots,buffered_plots2)

plots<-buffered_plots
outputfolder<-getwd()
make_pu_args<-list(
		Sp_catalog=LIDAR_catalog,
		plots=plots,
		Tile_URL="URL",
		Plot_ID="Plot_ID",
		tilesfolder=paste(outputfolder,"/tiles",sep=""),
		plotsfolder=paste(outputfolder,"/plots",sep=""),
		chunksfolder=paste(outputfolder,"/chunks",sep=""),
		dsn=getwd(),outputfile=NULL
)
loaded<-.packages()
ncores<-detectCores()-2
cl <- makeCluster(ncores,outfile="debug_file.txt")
doParallel::registerDoParallel(cl)
acquisition<-make_acquisition(make_pu_args,.packages=loaded)
stopCluster(cl)

