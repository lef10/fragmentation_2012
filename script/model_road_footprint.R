library(dplyr)
library(sf)
library(forcats)
library(units)
library(terra)
library(raster)
library(ggplot2)
library(nabor)

##### position of the file ##############
dossier_geopackages <- "data/UA_2012_3035_eu"
file_gpkg <- list.files(path = dossier_geopackages, recursive = TRUE, pattern = paste0("\\.gpkg$"), full.names = TRUE)

# take the data
UA_2012 <- lapply(file_gpkg[1:785],function(gpkg){
  
  sf <- st_read(gpkg,quiet=TRUE)
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  message(paste(fua_name, fua_code))
  
  return(sf)
})


# classification who recode the urban spaces, natural spaces and the linear structure
c <- lapply(UA_2012, function(sf) {
  
  classif <- sf %>% mutate(classif = rlc.urbancode(code_2012))
  classif <- classif %>% mutate(classif_factor = rlc.urbanclassif(code_2012))
  classif <- subset(classif, classif != "4") 
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/classif/UA_", fua_code, fua_name, "_c_12.rds")
  saveRDS(classif, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(classif) 
})


###############################################################################

# one raster per city, res = 100 m 

raster100 <- lapply(c, function(sf) {
  
  UA_raster0 <- rast(sf, res = 100)
  UA_raster <- rasterize(sf, UA_raster0, "classif") 
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/raster100/UA_", fua_code, fua_name, "_r_12.rds")
  saveRDS(UA_raster, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(UA_raster) 
})


###############################################################################
# road analysis
# create the cutoff with the raster analysis
footprint <- Map(function(ras, sf){
  
  msk <- ifel(ras == 2, NA, 1)
  raster_1 <- mask(ras, mask = msk)
  
  raw_xyv <- SpatRaster.to.xyv(raster_1)
  
  context<-nabor::knn(data=raw_xyv[,1:2], query=raw_xyv[,1:2], k=9) #get the 9 nearest neighbours including itself
  
  resolution <- res(raster_1)
  
  diag <- resolution*sqrt(2)+ 0.0001
  
  directneighb<-rowSums(context$nn.dists <= diag)
  
  
  raw_xyv$inurban<-directneighb>=7
  raw_xyv$v2 <-raw_xyv$v
  raw_xyv[raw_xyv$v == 3 & raw_xyv$inurban == TRUE, "v2"] <- 1
  raw_xyv[raw_xyv$v == 3 & raw_xyv$inurban == FALSE, "v2"] <- NA
  
  original_urban_xy <- raw_xyv[raw_xyv$v == 1, 1:2]
  
  original_urban_context <-nabor::knn(data= original_urban_xy, query=raw_xyv[,1:2], k=2)
  raw_xyv$not_atleast1urbanaround<-original_urban_context$nn.dists[,2]>diag[1] #except for itself is 2nd housing neighb beyond the diagonal
  
  raw_xyv$v3<-raw_xyv$v2
  exurbanroad<-raw_xyv$v ==3 & raw_xyv$inurban==TRUE & raw_xyv$not_atleast1urbanaround==TRUE
  raw_xyv$v3[exurbanroad]<-NA
  
  raw_xyv[raw_xyv$v3==1,]
  
  myxyz<-raw_xyv[,c(1,2,7)]
  
  # new urban with urban road
  rast_raw_xyv <- rast(myxyz, type = "xyz")
  
  urban_footprint <- as.polygons(rast_raw_xyv)
  urban_footprint_sf <- sf::st_as_sf(urban_footprint)
  crs <- st_crs("EPSG:3035")
  urban_footprint_sf_3035 <- st_set_crs(urban_footprint_sf, crs)
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/footprint/UA_", fua_code, fua_name, "_fp_12.rds")
  saveRDS(urban_footprint_sf_3035, output_filename)
  
  message(paste(fua_code, fua_name))
  
  return(urban_footprint_sf_3035)
}, raster100, c)



