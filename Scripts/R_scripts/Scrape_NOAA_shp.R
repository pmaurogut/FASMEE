# TODO: Add comment
# 
# Author: Paco
###############################################################################
library(rvest)
library(plyr)
library(sf)
library(raster)

#helper function to process the html table with the index
process_row<-function(trnode){
	
	column<-trnode%>%html_nodes("td")%>%html_text()
	index<-which(column=="Index SHP")
	result<-as.data.frame(t(column),stringsAsFactors=FALSE)
	link_to_Index<-trnode%>%html_nodes("td")%>%
			"[["(index)%>%html_nodes("a")%>%html_attr("href")
	result[,"link"]<-link_to_Index
	result
}

#Total tabular index of shapefiles with tiles This setep can be done manually. 
#what can be "DEM" downloads the DEM footprints, "Imagery" it downlads the 
#image footprints. Or anything else downloadt the las tile boundaries
get_noaa_table<-function(what="point_cloud"){
	if(what=="DTM"){what<-"DEM"}
	URL<-switch(what,
			"DEM" = "https://coast.noaa.gov/htdata/raster1/index.html",
			"imagery" = "https://coast.noaa.gov/htdata/raster1/index.html",
			"https://coast.noaa.gov/htdata/lidar1_z/"
	)
	NOAA_index<-read_html(URL)
	NOAA_table<-NOAA_index%>%html_nodes("table")
	if(what=="imagery"){
		cols<-c(1,2,7,8)
		colnames<-c("Year","DatasetName","NOAA_ID","link")
		NOAA_table<-NOAA_table[2]
	}else{
		NOAA_table<-NOAA_table[1]
		if(what=="DEM"){
			cols<-c(1,2,7,8)
			colnames<-c("Year","DatasetName","NOAA_ID","link")
		}else{
			cols<-c(1,2,3,9,10)
			colnames<-c("Year","DatasetName","Geoid","NOAA_ID","link")
		}
	}
	NOAA_table_headers<-NOAA_table%>%html_nodes("thead")%>%
			html_nodes("tr")%>%html_nodes("th")%>%html_text()
	NOAA_table<-NOAA_table%>%html_nodes("tbody")%>%
			html_nodes("tr")
	NOAA_table<-ldply(NOAA_table,process_row)
	NOAA_table<-NOAA_table[,cols]
	colnames(NOAA_table)<-colnames
	NOAA_table

}

#This function downloads the shp index of tiles and merge them. Makes a shapefile 
#with the merge tiles in the specidied folder and returns a vector
#with the NOAA ID of the acquisitions that failed downloading
#out_path=Output folder,out_file=Name of the shapefile that will be generated
get_noaa_shp<-function(NOAA_table,out_path=getwd(),out_file="NOAA_index.shp"){
	
	total_index<-NULL
	failed<-c()
	files<-dim(NOAA_table)[1]
	
	for(i in 1:files){
		
		print(paste("Reading file ",i, "of",files, "files"))
		link<-NOAA_table[i,"link"]
		NOAA_code<-NOAA_table[i,"NOAA_ID"]
		out_folder<-paste(out_path,"/",NOAA_code,sep="")
		download<-paste(out_path,"/downloaded.zip",sep="")
		download.file(link,download,mode="wb")
		unzip(download,exdir=out_folder)
		
		out_shp<-list.files(out_folder,"^.*\\.shp$",full.names=TRUE)
		success<-tryCatch(
				{
					polygons<-st_read(out_shp, stringsAsFactors = FALSE,quiet=TRUE)
#					st_crs(polygons)<-4326
					polygons[,"DatasetName"]<-NOAA_table[i,"DatasetName"]
					polygons[,"Year"]<-NOAA_table[i,"Year"]
					polygons[,"Geoid"]<-NOAA_table[i,"Geoid"]
					polygons[,"NOAA_ID"]<-NOAA_table[i,"NOAA_ID"]
					
					if(is.null(total_index)){
						
						total_index<-polygons
						
					}else{
						
						new_cols_index<-colnames(polygons)[!colnames(polygons)%in%
										colnames(total_index)]
						new_cols_polygons<-colnames(total_index)[!colnames(total_index)%in%
										colnames(polygons)]
						if(length(new_cols_polygons)>0){
							polygons[,new_cols_polygons]<-NA
						}
						if(length(new_cols_index)>0){
							total_index[,new_cols_index]<-NA
						}
						
						total_index<-rbind(total_index,polygons)
					}
					file.remove(download)
					unlink(out_folder, recursive = TRUE)
					TRUE
				},
				error=function(cond){
					message(cond)
					print(paste("Dataset ",NOAA_code," failed",sep=""))
					FALSE
				}
		)
		print(success)
		if(!success){failed<-c(failed,NOAA_code)}
		print(paste(length(failed), "acquisitions failed"))
		
	}
	st_write(total_index,paste(out_path,out_file,sep="/") ,delete_layer=TRUE)
	failed
}

#Usage example
#Get the tabular index
NOAA_LiDAR_table<-get_noaa_table()
NOAA_DTM_table<-get_noaa_table(what="DEM")
NOAA_imagery_table<-get_noaa_table(what="imagery")
#Any filtering by year,Dataset name, geoid or NOAA_ID should come here
#i.e. before geting the tiles you can filter the files from for example 2016.
#NOAA_table<-NOAA_table[NOAA_table$Year==2016,]

#This was made without filetering anything to create the shp I shared
#Five tiles failed and one has to be removed manually afterwards.
#I also think that once you have the index I sent you there is no need to 
#regenerate it. It is probably easier to just query  the shapefile either
#using its fields or spatially.
out_path<-"FASMEE/Data/Tile_indexes/NOAA"

out_file<-"PointClouds_Index.shp"
failed<-get_noaa_shp(NOAA_LiDAR_table,out_path,out_file)

out_file<-"DTMs_Index2.shp"
failed_DTM<-get_noaa_shp(NOAA_DTM_table[1:10,],out_path,out_file)

out_file<-"Imagery_Index.shp"
failed_imagery<-get_noaa_shp(NOAA_imagery_table,out_path,out_file)
