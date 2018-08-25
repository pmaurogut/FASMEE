# TODO: Add comment
# 
# Author: Paco
###############################################################################

library(sp)
library(sf)
library(tidyverse)
library(purrrlyr)
library(lidR)
library(raster)
library(rgdal)
library(rgeos)

folder<-"C:/Users/Paco/OneDrive for Business/BLM_Forested_Only"
##main function body
memory.limit(12000)
catalog<-catalog(folder)

rastercatalog<-function(folder,ext="tif",...){

	files <- list.files(folder, full.names = TRUE, pattern = paste("\\.",ext,"$",sep=""),...)
	headers <- lapply(files, function(x) {
				layer<-raster(x)
#				print(proj4string(layer))
				res<-data.frame(Min.X=layer@extent@xmin,Min.Y=layer@extent@ymin,
						Max.X=layer@extent@xmax,Max.Y=layer@extent@ymax,
						filename=x[1],proj4string=proj4string(layer),stringsAsFactors=FALSE)
				res
			})
	headers <- do.call(rbind.data.frame, headers)
	
	class(headers) = append(c("RasterCatalog","Catalog"),class(headers))

	return(headers)
}
	
SpatialCatalog<-function(catalog,expand=0,...){
	
	UseMethod("SpatialCatalog",catalog)
	
}

SpatialCatalog.Catalog<-function(catalog,expand=0,CRS=NULL){
	
	catalog$ID<-c(1:length(catalog[,1]))
	row.names(catalog)<-catalog$ID
	if(is.null(CRS)){CRS<-catalog[1,"proj4string"]}
	res<-by(catalog,catalog$ID,function(x,y){
				
				res<-expand.grid(x=c(x$Min.X-expand,x$Max.X+expand),
						y=c(x$Min.Y-expand,x$Max.Y+expand))
				res<-res[order(res$x,res$y),]
				res<-res[c(1,2,4,3),]
				res<-rbind(res,res[1,])
				rownames(res)<-c(1:5)
				res<-as.matrix(res)
				attr(res,"ID")<-x[1,y]
				res
				
			},y="ID")
	
	res<-lapply(res,function(x){Polygons(list(Polygon(x)),attr(x,"ID"))})
	IDs<-names(res)
	print(CRS)
	res<-SpatialPolygons(res,proj4string=CRS(CRS))
	res<-SpatialPolygonsDataFrame(res,data=as.data.frame(catalog))
	
}

SpatialCatalog.character<-function(folder,ext="tif",expand=0,CRS=NULL){

	catalog<-rastercatalog(folder,ext=ext,...)
	
	catalog$ID<-c(1:length(catalog[,1]))
	row.names(catalog)<-catalog$ID
	if(is.null(CRS)){CRS<-catalog[1,"proj4string"]}
	res<-by(catalog,catalog$ID,function(x,y){
				
				res<-expand.grid(x=c(x$Min.X-expand,x$Max.X+expand),y=c(x$Min.Y-expand,x$Max.Y+expand))
				res<-res[order(res$x,res$y),]
				res<-res[c(1,2,4,3),]
				res<-rbind(res,res[1,])
				rownames(res)<-c(1:5)
				res<-as.matrix(res)
				attr(res,"ID")<-x[1,y]
				res
			},"ID")
	
	res<-lapply(res,function(x){Polygons(list(Polygon(x)),attr(x,"ID"))})
	IDs<-names(res)
	res<-SpatialPolygons(res,proj4string=CRS(CRS))
	res<-SpatialPolygonsDataFrame(res,data=as.data.frame(catalog))
	
}

path_to_shp_catalog<-function(path,out_path,name,ext="tif",expand=0,CRS=NULL,ext=NULL,recursive=TRUE,...){

	if(is.null(ext)){
		
		catalog<-catalog(path,...)
		catalog<-SpatialCatalog(catalog,expand=expand,CRS=CRS)
		writeOGR(obj=catalog, dsn=out_path, layer=name, driver="ESRI Shapefile")
		return(catalog)
	}else{
		
		catalog<-rastercatalog(path,ext,...)
		print(catalog[,"proj4string"])
		catalog<-SpatialCatalog(catalog,expand=expand,CRS=CRS)
		writeOGR(obj=catalog, dsn=out_path, layer=name, driver="ESRI Shapefile")
		return(catalog)
	}
	
}

rcat<-rastercatalog("C:/Users/Paco/OneDrive for Business/BLM_Forested_Only")
path_to_shp_catalog("C:/Users/Paco/OneDrive for Business/BLM_Forested_Only",
		"C:/Users/Paco","Index",ext="tif",recursive=FALSE)


cell2cloud<-function(percentiles,percentile_values,
		count_total,bins){

	percentiles<-as.numeric(as.vector(percentiles))
	percentile_values<-percentile_values[!is.na(percentiles)]
	percentiles<-percentiles[!is.na(percentiles)]
	if(length(percentiles)<3){
		
		return(bins[-1]*0)
		
	}else{
		
		cell_density<-approxfun(percentiles,percentile_values,rule=2)
		scaled_hist<-cell_density(bins)
		scaled_hist<-diff(scaled_hist)*count_total
		if(length(scaled_hist)==length(bins[-1])){
			
			return(scaled_hist)
			
		}else{
			
			return(bins[-1]*0)
			
		}
		
		
	}
	
}

dens2percentiles<-function(densbins,locations,percentiles,rule=2){
		
	if(sum(densbins>0)<3){
		
		percentiles<-data.frame(percentile=percentiles)
		percentiles$value<-0
		percentiles
		
	}else{
		
		densbins<-cumsum(densbins)/sum(densbins)
		percentf<-approxfun(densbins,locations,rule=rule)
		percentiles<-data.frame(percentile=percentiles)
		percentiles$value<-percentf(percentiles$percentile)
		percentiles
	
	}

}

grid2MUpercentiles<-function(data,field,index=c(3:17),countindex,
		a=c(0.01,0.05,0.1,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.75,0.8,0.9,0.95,0.99),
		b=c(0.01,0.05,0.1,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.75,0.8,0.9,0.95,0.99)){
	
	field<-substitute(field)
	plyr::ddply(data,eval(field),function(x,a,b){
				
				binned_hist<-2*c(-2000:2000)/10
				to_modify<-bined_hist[-1]*0
				count2<-0
				res<-by_row(x[,c(countindex,index)],
						function(y,a){
							
							count<-y[[1]][1]
							
							if(is.na(count)){
								
								return(FALSE)
								
							}else{
								
								res2<-cell2cloud(y[,-1],a,count,binned_hist)
							
								if(sum(res2)==0){
									
									count2<<-count2+1
#									print(c(count,count2))
									return(FALSE)
									
								}else{
									count2<<-0
									to_modify<<-to_modify+res2
#									print(to_modify[to_modify>0])
									return(TRUE)
								}
								
							}
						},
						a,
						.to="success")
				
				binned_hist<-binned_hist[-1]-diff(binned_hist)/2
				percentiles<-dens2percentiles(to_modify,binned_hist,b)
				
				percentiles2<-spread_(percentiles,"percentile","value",sep="_")
				
			},
			a=c(0.01,0.05,0.1,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.75,0.8,0.9,0.95,0.99),
			b=c(0.01,0.05,0.1,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.75,0.8,0.9,0.95,0.99),
			.parallel = FALSE,.progress="tk")

}

create_MU_las<-function(polygon,files,ID,path=getwd()){
	
	folder<-paste(path,"/",ID,sep="")
	dir.create(folder)
	st_write(polygon,paste(folder,"/",ID,".shp",sep=""))
	write.table(data.frame(files=files),paste(folder,"/index.txt",sep=""),
			sep="\t",col.names=FALSE,row.names=FALSE)
#	for(i in list_of_files){
#		
##		dir.create(folder)
#		
#	}
	
}

poly_clipper<-function(polygons,files,ID,path){
	
	polygons_data<-as.data.frame(polygons)
	by(polygons_data,list(ID=polygons_data[,ID]),function(x,files,ID,path){
				
				IDpol<-x[[ID]][1]
				polygon<-polygons[polygons[[ID]]==IDpol,]
				files<-as.character(x[[files]])
				create_MU_las(polygon,files,IDpol,path)
				
			},files=files,ID=ID,path=path)
	
}

make_dtm_tile<-function(files,extent,tilename,snap="out"){
	
	for(i in seq_along(files)){
		
		if(i==1){
			
			raster1<-raster(files[i])
			crs(raster1)<-CRS("+init=epsg:2994")
			print("Raster merged")
			b <- as(extent(extent), 'SpatialPolygons')	
			crs(b) <- crs(raster1)
			b<-alignExtent(b,raster1,snap=snap)
			raster1<-crop(raster1,b)
			print("Raster merged")
			print(b)
			
		}else{
			
			raster2<-raster(files[i])
			crs(raster2)<-CRS("+init=epsg:2994")
			raster1<-merge(raster1,raster2,ext=b)
			
		}
		
	}
	
	print(paste("Raster ",tilename," Written",sep=""))
	writeRaster(raster1,filename=tilename,
			format="ascii",overwrite=TRUE)
	
	
	
}

make_dtm_lastile<-function(dtmfiles,tilefiles,xmin,xmax,ymin,ymax,snap="out"){
	
	for(i in seq_along(dtmfiles)){
		
		if(i==1){
			
			raster1<-raster(dtmfiles[i])
			crs(raster1)<-CRS("+init=epsg:2994")
			print("Raster merged")
			b <- as(extent(extent), 'SpatialPolygons')	
			crs(b) <- crs(raster1)
			b<-alignExtent(b,raster1,snap=snap)
			raster1<-crop(raster1,b)
			print("Raster merged")
			print(b)
			
		}else{
			
			raster2<-raster(dtmfiles[i])
			crs(raster2)<-CRS("+init=epsg:2994")
			raster1<-merge(raster1,raster2,ext=b)
			
		}
		
	}
	

	for(i in seq_along(tilefiles)){
		
		tilename<-tilename<-gsub("(\\.las)$",".asc",tilename)
		print(paste("Raster ",tilename," Written",sep=""))
		writeRaster(raster1,filename=tilename,
				format="ascii",overwrite=TRUE)
		
	}
	
}

make_dtm_lastile<-function(dtmfiles,tilefiles,xmin,xmax,ymin,ymax,snap="out"){
	
	print(length(dtmfiles))
	for(i in seq_along(dtmfiles)){
		
		if(i==1){
			
			raster1<-raster(dtmfiles[i])
			crs(raster1)<-CRS("+init=epsg:2994")
		
		}else{
			
			raster2<-raster(dtmfiles[i])
			crs(raster2)<-CRS("+init=epsg:2994")
			raster1<-merge(raster1,raster2)
			print("Raster built")
		}
		
	}
	
	
	for(i in seq_along(tilefiles)){
		
		b<-extent(c(xmin[i],xmax[i],ymin[i],ymax[i]))
		b<-as(b,'SpatialPolygons')	
		crs(b)<-CRS("+init=epsg:2994")
		raster2<-crop(raster1,b)
		tilename<-gsub("(\\.las)$",".asc",tilefiles[i])
		print(paste("Raster ",tilename," Written",sep=""))
		writeRaster(raster2,filename=tilename,
				format="ascii",overwrite=TRUE)
		
	}
	
}

generate_dtm_tiles_by_raster<-function(polygons,lasfield,dtmfield,extentfields=c("Min_X","Max_X","Min_Y","Max_Y")){
	
	polygons<-as.data.frame(polygons)
	polygons<-polygons[,-dim(polygons)[2]]
	
	polygons[,lasfield]<-as.character(polygons[,lasfield])
	polygons[,dtmfield]<-as.character(polygons[,dtmfield])
	
	counts<-by(polygons[c(lasfield,dtmfield)],polygons[,lasfield],function(x,dtmfield,lasfield){
				
				res<-data.frame(lasfile=x[1,lasfield],n=length(x[,dtmfield]),
						dtmtiles=paste(x[,dtmfield],collapse="_&&_"),
						stringsAsFactors=FALSE)
				res
			},dtmfield=dtmfield,lasfield=lasfield)
	counts<-do.call(rbind,counts)
	polygons<-merge(polygons,counts,by.x=lasfield,by.y="lasfile")
	
	by(polygons,polygons[,"n"],function(x,lasfield,extentfields){
				
				by(x,x[,"dtmfiles"],function(y,lasfield,extentfields){
					
					files<-unlist(strsplit(y[1,"dtmtiles"],"_&&_"))
					lasfiles<-x[,lasfield]
					make_dtm_lastile(files,lasfiles,
						y[,extentfields[1]],y[,extentfields[2]],
						y[,extentfields[3]],y[,extentfields[4]])
					
				},lasfield=lasfield,extentfields=extentfields)

			},lasfield=lasfield,dtmfield=dtmfield,extentfields=extentfields)
	
	
}

tif2asc<-function(infile,...){
	
	outfile<-tilename<-gsub("(\\.tif)$",".asc",tilename)
	raster1<-raster(infile)
	writeRaster(raster1,filename=outfile,
			format="ascii",overwrite=TRUE)
	
	
}

normalize_clip<-function(lasfile,dtmfile,outputfile=NULL){
	print(Sys.time())
	dtm<-raster(dtmfile)
	lasin<-readLAS(lasfile)
	out<-lasnormalize(lasin,dtm)
	if(is.null(outputfile)){outputfile<-gsub("(\\.asc)$","_normalized.las",dtmfile)}
	writeLAS(out,outputfile)
	print(Sys.time())
}

normalize_clips<-function(data,lasfield,dtmfield,outputfield=NULL){
	
	by(data,data[,lasfield],function(x,dtmfield){
				
				dtmfile<-as.character(x[1,dtmfield])
				lasfile<-as.character(x[1,lasfield])
				
				if(!is.null(outputfield)){
					outputfile<-as.character(x[1,outputfield])
				}else{
					outputfile<-NULL
				}
				
				normalize_clip(lasfile,dtmfile,outuptfile)
				
			},dtmfield=dtmfield)
	
}
