
# TODO: Add comment
# 
# Author: Paco
###############################################################################
library("lidR")
library("plyr")
library("rgdal")
library("raster")
library("sf")
library("rgeos")
library("parallel")
library("foreach")
memory.limit(12000)

#helper function to call FUSION from R. Sets the r woring directory to the FUSION (or other) folder.
#If other folder is specified. Paths are given relative to that folder and FUSION commands must start with
#the FUSION path
shell.in.dir <- function(dir, ...) {
	current.wd <- getwd()
	on.exit(setwd(current.wd))
	setwd(dir)
	shell(...)
}
#function that sends processing tasks to FUSION. The first argument is the command
#The second argument a named list with switches and the last a string with the remaining arguments
FUSION_command<-function(command,switches_list,args){
	
	myArgs <- match.call()
	if("switches_list" %in% names(myArgs)){
		switches<-""
		for(i in names(switches_list)){
			
			if(switches_list[i]==""){
				
				switches<-paste(switches, paste("/",i,sep=""),sep=" ")
				
			}else{
				
				switches<-paste(switches, paste("/",i,":",switches_list[i],sep=""),sep=" ")
				
			}
			
		}
		paste(command,switches,args,sep=" ")
	}else{
		paste(command,args,sep=" ")
	}
	
}

#Back transform plots or processing polygons for NOAA. Everything has to go back to 
#GRS80 lat long before interacting with the las files 
#makes FIA clips located at different distances and azimuths from the plot center.
#Coordinates have to be projecte in plots and distances acording to the CRS
make_FIA_clips<-function(plots,
		azimuths=c(0,0,0,0,0,120,120,240,240),declination_field="DECLINATION",
		distances=c(0,0,0,120,120,120,120,120,120),
		radii=c(178.9,24,58.9,24,58.9,24,58.9,24,58.9),
		type=c("100mBuffer","Micro","Macro","Micro","Macro","Micro","Macro","Micro","Macro"),
		XFIELD="XCOORD",
		YFIELD="YCOORD",Buffer=TRUE){
	
	plots@data$newID<-c(1:length(plots@data[,1]))
	result<-adply(plots@data,.margins=1,function(x,azimuths,distances,radii,
					type,XFIELD,YFIELD,PLOT_ID,declination_field){
				reps<-length(azimuths)
				res<-x[rep(1,reps),]
				res[,"AZIMUTH"]<-azimuths
				res[,"Corrected_AZIMUTH"]<-azimuths
				azimuths<-azimuths + res[,declination_field]
				res[,XFIELD]<-res[,XFIELD]-distances*sin(2*pi*azimuths/360)
				res[,YFIELD]<-res[,YFIELD]+distances*cos(2*pi*azimuths/360)
				res[,"XMIN"]<-res[,XFIELD]-radii
				res[,"YMIN"]<-res[,YFIELD]-radii
				res[,"XMAX"]<-res[,XFIELD]+radii
				res[,"YMAX"]<-res[,YFIELD]+radii
				res$RADIUS<-radii
				res$DISTANCE<-distances
				res[,"TYPE"]<-type
				
				res$NEW_PLOT_ID<-paste(res[,"newID"],c(1:reps),sep="_")
				res
			},
			azimuths=azimuths,distances=distances,type=type,
			radii=radii,XFIELD=XFIELD,YFIELD=YFIELD,
			declination_field=declination_field)
	result<-SpatialPointsDataFrame(coordinates(result[,c(XFIELD,YFIELD)]),data=result,
			proj4string=crs(plots))
	if(Buffer){
		
		return(gBuffer(result,byid=TRUE,id=result@data[,"NEW_PLOT_ID"],
				width=result@data[,"RADIUS"]))
		
	}else{
		
		return(result)
		
	}
	
}

#get_tiles returns all the LAS or raster tiles that intersect a plot. The plot 
#can be buffered or just a point. The retsult contains a field "filename"
#with the file location of the tile. If there are more than one intersecting
#tiles new records are generated for the plot, one for each tile.
get_tiles<-function(plots,catalog,ID="PLOT",fileID="filename",
		newfilenameID="filenameLAS"){

	intersections<-st_intersects(plots,catalog,sparse=TRUE)
	plots<-plots[which(lengths(intersections)>0),]
	intersections<-intersections[which(lengths(intersections)>0)]
	plots$intersects<-intersections
	result<-alply(plots,.margins=1,.fun=function(x,y,z){
				intersects<-unlist(x$intersects)
				res<-x[rep(1,length(intersects)),]
				res[,newfilenameID]<-y[intersects,z,drop=TRUE]
				res
			},y=catalog,z=fileID)
	do.call(rbind,result)

}

make_fusion_catalog<-function(files,folder,FUSION_path="C:\\FUSION"){
	file<-paste(folder,"index.txt",sep="")
	catalog<-paste(folder,"Catalog.csv",sep="")
	file<-normalizePath(file)
	catalog<-normalizePath(catalog)
	list_of_files<-normalizePath(files)
	writeLines(list_of_files,file)
	shell.in.dir(FUSION_path,FUSION_command("catalog",switches=list("newindex"=""),
					args=paste(file,catalog,sep=" ")))
	return(list(fileList=file,catalog=catalog))
}

#Clips using LiDR. Writes an file with the ids of the clips in outfolder. 
#Clips are also stored in outfolder. The fields in fields are stored as well.
clip_LiDR<-function(intersects,
		lasfilename="filenameLAS",
		fields=c("PLOT","NEW_PLOT_ID",
			"XCOORD","YCOORD","RADIUS","AZIMUTH","DISTANCE","TYPE",
			"XMIN","YMIN","XMAX","YMAX"),
		outfolder="D:/OLC_ROGUE_2012/CLIPS/"){
	
	res1<-ddply(intersects,fields[1],function(x,y,z){
	
				big<-x[x$RADIUS==max(x$RADIUS),]
				small<-x[!x$RADIUS==max(x$RADIUS),]
				subplot<-z[2]
				files<-big[,y]
				print(files)
				las<-readLAS(files)
				las<-lasclipCircle(las,big[1,z[3]],big[1,z[4]],big[1,z[5]])
				out_big<-paste(outfolder,big[1,z[2]],".las",sep="")
				writeLAS(las,out_big)
				bigdf<-data.frame(big[1,z])
				bigdf$filename<-out_big
				print(class(las))
				
				res<-ddply(small,subplot,
					.fun=function(part,z,las){
					
					clip<-lasclipCircle(las,part[1,z[3]],part[1,z[4]],part[1,z[5]])
					try({out<-paste(outfolder,part[1,z[2]],".las",sep="")
					print(out)
					writeLAS(clip,out)})
					ret<-data.frame(part[1,z])
					ret$filename<-out
					ret
					
				},z=z,las=las)
		
				rbind(bigdf,res)

			},y=lasfilename,z=fields,.progress="tk")
	
	write.csv(res1,paste(outfolder,"LUT.csv",sep=""),row.names=FALSE)
	
}

#Catalogs need to be transformed to Lambert for NOAA downloads
#All plots to Lambert or the projected system of the catalog.
lambertCRS<-"+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +units=ft +no_defs"
plots<-read.csv("FASMEE/Data/FIA_database/PLOT.csv")
plots<-plots[plots$STATECD==41,]
plots$DECLINATION<-ifelse(is.na(plots$DECLINATION),0,plots$DECLINATION)
plots<-ddply(plots,"PLOT",function(x){
			
			res<-x[order(x[,"INVYR"],decreasing=TRUE),]
			res[1,]
			
		})
plots<-SpatialPointsDataFrame(as.matrix(plots[,c("LON","LAT")]),data=plots,
		coords.nrs=c(24,23),
		proj4string=crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plots<-spTransform(plots,CRS(lambertCRS))
plots@data[,c("XCOORD","YCOORD")]<-coordinates(plots)
plots<-make_FIA_clips(plots)
plots<-st_as_sf(plots)

point_folder<-c("D:/OLC_ROGUE_2012/ALL_POINTS")
LIDAR_catalog<-SpatialCatalog(point_folder,ext="las",
		CRS=lambertCRS)
LIDAR_catalog<-st_as_sf(LIDAR_catalog)
las_plots<-get_tiles(plots,LIDAR_catalog)
clip_LiDR(las_plots[,c("filenameLAS","PLOT","NEW_PLOT_ID",
			"XCOORD","YCOORD","RADIUS","AZIMUTH","DISTANCE","TYPE",
			"XMIN","YMIN","XMAX","YMAX")])

#STOP HERE---------------------------------------------------------------

#RUN THIS ONLY FOR FUSION. IT will index a lot of las tiles and take a lot of time
make_fusion_catalog(unique(las_plots$filenameLAS),folder=point_folder)

#This is only needed if you want to use FUSION. It converts the rasters
#in ESRI grid format to asc and then transform to plans dtm for fusion. Takes time
raster_folder<-c("D:/OLC_ROGUE_2012/BARE_EARTH")
raster_catalog<-SpatialCatalog(raster_folder,ext="ovr",
		CRS=lambertCRS)
for(i in raster_catalog$filename){
	
	folder<-gsub(".ovr","",i,fixed=TRUE)
	folder<-normalizePath(folder)
	command<-paste("C:\\OSGeo4W64\\bin\\gdal_translate -co FORCE_CELL_SIZE=YES -co DECIMAL_PRECISION=4 -of AAIGrid ",
			folder," ",paste(folder,".asc",sep=""),sep="")
	shell(command)
	args<-paste(paste(folder,".dtm",sep=""),"F F 2 0 2 3",paste(folder,".asc",sep=""))
	shell(FUSION_command("C:\\FUSION\\ASCII2DTM",args=args))
	
}
raster_catalog<-SpatialCatalog(raster_folder,ext="asc",
		CRS=lambertCRS)
raster_catalog<-st_as_sf(raster_catalog)
DTM_plots<-get_tiles(plots,raster_catalog,newfilenameID="filenameDTM")

#These are the switches that I received from Patrick
switches_list<-list(
	"MINHT"=2,
	"ABOVE"=2,
	"OUTLIER"="-30,150",
	"STRATA"= "0.5,1,2,4,8,16,32,48,64"
)

command<-FUSION_command("cloudmetrics",switches_list,"14.las 14.csv")
FUSION_path<-"C:\\FUSION"
shell.in.dir(FUSION_path,command)