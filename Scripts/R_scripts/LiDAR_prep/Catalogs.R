# TODO: Add comment
# 
# Author: Paco
###############################################################################

library(sp)
library(lidR)
library(raster)
library(rgdal)

#This function reads the extent of different rasters in a folder and 
#creates a data.frame with that information. The CRS and the full file path
#The output is tagged with the class RasterCatalog
rastercatalog<-function(folder,ext="tif",...){
	
	files <- list.files(folder, full.names = TRUE, pattern = paste("\\.",ext,"$",sep=""),...)
	headers <- lapply(files, function(x) {
				layer<-raster(x)
#				print(proj4string(layer))
				res<-data.frame(Min.X=layer@extent@xmin,Min.Y=layer@extent@ymin,
						Max.X=layer@extent@xmax,Max.Y=layer@extent@ymax,
						filename=x[1],CRS=as.character(crs(layer)),stringsAsFactors=FALSE)
				res
			})
	headers <- do.call(rbind.data.frame, headers)
	
	class(headers) = append(c("RasterCatalog"),class(headers))
	
	return(headers)
}

#Spatial catalogs. Catalogs as SpatialPolygonDataframe
#The next five function create a SpatialPolygonDataframe from: a 
#1 data.frame a 2 LASCatalog from LiDR package a 3 RasterCatalog created with the function
#rastercatalog or 4 a string giving a path. In the later case the function creates 
#a catalog (Either LASCatalog or RasterCatalog) based on the extension indicated ext and
#then make the catalog spatial.
#The expand argument was included to buffer the catalogs for something I needed at that time
SpatialCatalog<-function(catalog,expand=0,...){
	
	UseMethod("SpatialCatalog",catalog)
	
}

SpatialCatalog.data.frame<-function(catalog,expand=0,
		cols=c("Min.X","Max.X","Min.Y","Max.Y"),CRS=NA){
	
	catalog$ID<-c(1:length(catalog[,1]))
	row.names(catalog)<-catalog$ID

	result<-by(catalog,catalog$ID,function(x,y,z){
				res<-expand.grid(x=c(x[,z[1]]-expand,x[,z[2]]+expand),
						y=c(x[,z[3]]-expand,x[,z[4]]+expand))
				res<-res[order(res$x,res$y),]
				res<-res[c(1,2,4,3),]
				res<-rbind(res,res[1,])
				rownames(res)<-c(1:5)
				res<-as.matrix(res)
				attr(res,"ID")<-x[1,y]
				res
				
			},y="ID",z=cols)
	result<-lapply(result,function(x){Polygons(list(Polygon(x)),attr(x,"ID"))})
	IDs<-names(result)
	
	result<-SpatialPolygons(result,proj4string=CRS(CRS))
	result<-SpatialPolygonsDataFrame(result,data=catalog)

}

SpatialCatalog.LAScatalog<-function(catalog,expand=0,CRS=NULL){
	
	if(is.null(CRS)){CRS<-as.character(catalog@crs)}
	catalog<-data.frame(catalog@data)
	catalog$CRS<-CRS
	SpatialCatalog(catalog,expand=expand,CRS=CRS)
	
}

SpatialCatalog.RasterCatalog<-function(catalog,expand=0,CRS=NULL){
	
	catalog$ID<-c(1:length(catalog[,1]))
	row.names(catalog)<-catalog$ID
	if(is.null(CRS)){CRS<-catalog[1,"CRS"]}
	SpatialCatalog(data.frame(catalog),expand=expand,CRS=CRS)

}

SpatialCatalog.character<-function(catalog,ext="tif",expand=0,CRS=NULL,...){
	
	if(ext%in%c("las","laz")){
		catalog<-catalog(catalog,...)
		catalog<-SpatialCatalog(catalog,expand=expand,CRS=CRS)
	}else{
		catalog<-rastercatalog(catalog,ext,...)
		catalog<-SpatialCatalog(catalog,expand=expand,CRS=CRS)
	}

	
}

#Uses SpatialCatalog.character to make a catalog from a string and 
#stores it using the output path and output name specified in the second and third arguments.
#... are passed to writeOGR
path_to_shp_catalog<-function(path,out_path,name,
		expand=0,CRS=NULL,ext=NULL,recursive=TRUE,...){
	
	catalog<-SpatialCatalog(path,ext=ext,expand=expand,CRS=CRS,recursive=recursive)
	writeOGR(obj=catalog, dsn=out_path, layer=name, driver="ESRI Shapefile",
			...)
	return(catalog)
	
}
#This  creates a SpatialPolygonDataframe object
#rcat<-SpatialCatalog("PATH")
#This  creates a SpatialPolygonDataframe object and stores it
#rcat<-path_to_shp_catalog("PATH_TO_TIF_FILES",
#		"OUT_PATH","NAME_WITHOUT_SHP",expand=0,ext=NULL,overwrite_layer=TRUE)
#This  creates a SpatialPolygonDataframe object
#lascatalog<-SpatialCatalog("PATH_TO_LIDAR_FILES",ext="las")
#This  creates a SpatialPolygonDataframe object and stores it
#lascatalog<-path_to_shp_catalog("PATH_TO_TIF_FILES",
#		"OUT_PATH","NAME_WITHOUT_SHP",expand=0,ext=NULL,overwrite_layer=TRUE)

