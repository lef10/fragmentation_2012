library(dplyr)
library(sf)
library(forcats)
library(units)
library(terra)
library(raster)
library(ggplot2)
library(nabor)

files_classif <- list.files(path = "output/rds/shape_index/classif", recursive = TRUE, pattern = paste0("\\.rds$"), full.names = TRUE)

c <- lapply(files_classif[1:785],function(rds){
  
  sf_rds <- readRDS(file = rds)
  
  return(sf_rds) 
})

files_footprint <- list.files(path = "output/rds/shape_index/footprint", recursive = TRUE, pattern = paste0("\\.rds$"), full.names = TRUE)

footprint <- lapply(files_footprint[1:785],function(rds){
  
  sf_rds <- readRDS(file = rds)
  
  return(sf_rds) 
})

files_multivector <- list.files(path = "output/rds/shape_index/urban/cas1/multivector_u", recursive = TRUE, pattern = paste0("\\.rds$"), full.names = TRUE)

multivector <- lapply(files_multivector[1:785],function(rds){
  
  sf_rds <- readRDS(file = rds)
  
  return(sf_rds) 
})

files_u_j99 <- list.files(path = "output/rds/shape_index/urban/cas2/u_j99", recursive = TRUE, pattern = paste0("\\.rds$"), full.names = TRUE)

u_j99 <- lapply(files_u_j99[1:785],function(rds){
  
  sf_rds <- readRDS(file = rds)
  
  return(sf_rds) 
})



#################
# INTERSECTION to put the road with the urban class

multivector <- Map(function(sf, sf2){
  
  # Check and possibly transform CRS
  if (!identical(st_crs(sf), st_crs(sf2))) {
    sf <- st_transform(sf, crs = st_crs(sf2))
  }
  
  road <- sf2[sf2$classif == 3, ]
  
  
  road_inurban <- st_intersection(road, sf)
  
  UA_urban <- sf2[sf2$classif == 1, ]
  UA_urban$v3 <- UA_urban$classif
  
  
  footprint_urban <- rbind(UA_urban, road_inurban)
  
  
  fua_name <- st_drop_geometry(sf2)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf2)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas1/multivector_u/UA_", fua_code, fua_name, "_u_i_12.rds")
  saveRDS(footprint_urban, output_filename)
  
  message(paste(fua_name, fua_code))
  
  
  return(footprint_urban)
}, footprint, c)

################################################################################
# Cas 1 - u_i without road

u_i <- lapply(c, function(sf) {
  
  UA_urban <- sf[sf$classif == 1, ] 
  UA_urban$v3 <- UA_urban$classif
  
  urban_i <- subset(UA_urban, !code_2012 %in% c("12210", "12220", "12230"))
  
  urban_i$a <- st_area(urban_i)
  urban_i$p <- perimeter(urban_i)
  urban_i$s <- pa.shape(urban_i)
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas1/u_i/UA_", fua_code, fua_name, "_u_i_12.rds")
  saveRDS(urban_i, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(c) 
})

# mean_u_i
mean_u_i <- lapply(u_i, function(sf) {
  
  mean_s <- sf %>% 
    group_by(fua_code, fua_name) %>% 
    summarise(m_u.i = mean(s))
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas1/m_u_i/UA_",fua_code, fua_name, "_m_u_i_12.rds")
  saveRDS(mean_s, output_filename)
  
  message(paste(fua_name, fua_code))  

return(mean_s) 
})

long_sums_m_u_i<- do.call(rbind, mean_u_i)
saveRDS(long_sums_m_u_i, "output/rds/shape_index/urban/cas1/long_sums_m_u_i.rds")


# coef u_i

reg_u_i <- Map(function(sf){
  
  sf$p <- units::drop_units(sf$p)
  sf$a <- units::drop_units(sf$a)
  
  reg <- summary(lm(p ~ sqrt(a), data = sf))$coefficients[,c(1,4)]
  reg$r2 <- summary(lm(p ~ sqrt(a), data = sf))$r.squared
  
  reg$rmse <- sqrt(mean((sf$p - (predict((lm(p ~ sqrt(a), data = sf)), sf)))^2))
  
  
  names(reg)[1] <- "intercept"
  names(reg)[2] <- "estimate"
  names(reg)[3] <- "p_value_i"
  names(reg)[4] <- "p_value_e"
  
  reg$fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  reg$fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas1/coef_u_i/UA_", fua_code, fua_name, "_coef_s_u_i_12.rds")
  saveRDS(reg, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(reg)
}, u_i)

long_sums_reg_u_i <- do.call(rbind, reg_u_i)
long_sums_reg_u_i <- as.data.frame(long_sums_reg_u_i)
saveRDS(long_sums_reg_u_i, "output/rds/shape_index/urban/cas1/long_sums_coef_s_u_i.rds")


####################################################"
# Cas 2 - Union & cast

# aggregation
u_j <- lapply(multivector, function(sf) {
  
  union <- st_union(sf)
  
  union_cast <- st_cast(union, "POLYGON")
  union_cast_sf <- st_as_sf(union_cast)
  
  union_cast_sf$fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  union_cast_sf$fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  # pa calculation
  
  union_cast_sf$a <- st_area(union_cast_sf)
  union_cast_sf$p <- perimeter(union_cast_sf)
  union_cast_sf$s <- pa.shape(union_cast_sf)
  
  # percentage
  union_cast_sf$perc_total <- sum(union_cast_sf$a)
  union_cast_sf$perc <- union_cast_sf$a/union_cast_sf$perc_total
  union_cast_sf$m_u_j <- mean(union_cast_sf$s)
  
  union_cast_sf3035 <- st_transform(union_cast_sf, crs = 3035) 
  union_cast_sf3035 <- st_sf(union_cast_sf3035)
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas2/u_j/UA_", fua_code, fua_name, "_u_j_12.rds")
  saveRDS(union_cast_sf3035, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(union_cast_sf3035) 
})

# union_mean
mean_u_j <- lapply(u_j, function(sf) {
  
  mean_s <- sf %>% 
    group_by(fua_code, fua_name) %>% 
    summarise(m_u_j = mean(s))
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas2/m_u_j/UA_", fua_code, fua_name, "_m_u_j_12.rds")
  saveRDS(mean_s, output_filename)
  
  message(paste(fua_name, fua_code))  
  
  return(mean_s) 
})

long_sums_m_u_j <- do.call(rbind, mean_u_j)
saveRDS(long_sums_m_u_j, "output/rds/shape_index/urban/cas2/long_sums_m_u_j.rds")


# coef u_j
reg_u_j <- Map(function(sf){
  
  sf$p <- units::drop_units(sf$p)
  sf$a <- units::drop_units(sf$a)
  
  reg <- summary(lm(p ~ sqrt(a), data = sf))$coefficients[,c(1,4)]
  reg$r2 <- summary(lm(p ~ sqrt(a), data = sf))$r.squared
  
  reg$rmse <- sqrt(mean((sf$p - (predict((lm(p ~ sqrt(a), data = sf)), sf)))^2))
  
  names(reg)[1] <- "intercept"
  names(reg)[2] <- "estimate"
  names(reg)[3] <- "p_value_i"
  names(reg)[4] <- "p_value_e"
  
  reg$fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  reg$fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas2/coef_u_j/UA_",fua_code, fua_name, "_coef_s_u_j_12.rds")
  saveRDS(reg, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(reg)
}, u_j)

long_sums_reg_u_j <- do.call(rbind, reg_u_j)
long_sums_reg_u_j <- as.data.frame(long_sums_reg_u_j)
saveRDS(long_sums_reg_u_j, "output/rds/shape_index/urban/cas2/long_sums_coef_u_j.rds")

############## cas2 u_j99, without 1% max ################

u_j99 <- Map(function(sf){
  
  j99 <- subset(sf, s < quantile(s, 0.99))
  
  j99$perc99 <- sum(j99$perc)
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas2/u_j99/UA_", fua_code, fua_name, "_u_j99_12.rds")
  saveRDS(j99, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(j99)
}, u_j)


mean_u_j99 <- lapply(u_j99, function(sf) {
  
  mean_s <- sf %>% 
    group_by(fua_code, fua_name) %>% 
    summarise(m_u.j99 = mean(s))
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas2/m_u_j99/UA_", fua_code, fua_name, "_m_u_j99_12.rds")
  saveRDS(mean_s, output_filename)
  
  message(paste(fua_name, fua_code))  
  
  return(mean_s) 
})

long_sums_m_u_j99 <- do.call(rbind, mean_u_j99)
saveRDS(long_sums_m_u_j99, "output/rds/shape_index/urban/cas2/long_sums_m_u_j99.rds")



# coef s_u_j99
reg_u_j99 <- Map(function(sf){
  
  sf$p <- units::drop_units(sf$p)
  sf$a <- units::drop_units(sf$a)
  
  reg <- summary(lm(p ~ sqrt(a), data = sf))$coefficients[,c(1,4)]
  reg$r2 <- summary(lm(p ~ sqrt(a), data = sf))$r.squared
  
  reg$rmse <- sqrt(mean((sf$p - (predict((lm(p ~ sqrt(a), data = sf)), sf)))^2))
  
  names(reg)[1] <- "intercept"
  names(reg)[2] <- "estimate"
  names(reg)[3] <- "p_value_i"
  names(reg)[4] <- "p_value_e"
  
  reg$fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  reg$fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas2/coef_u_j99/UA_", fua_code, fua_name, "_coef_s_u_j99_12.rds")
  saveRDS(reg, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(reg)
}, u_j99)

long_sums_reg_u_j99 <- do.call(rbind, reg_u_j99)
long_sums_reg_u_j99 <- as.data.frame(long_sums_reg_u_j99)
saveRDS(long_sums_reg_u_j99, "output/rds/shape_index/urban/cas2/long_sums_coef_u_j99.rds")


# coef z_u_j99
reg_z_u_j99 <- Map(function(sf){
  
  sf$p <- units::drop_units(sf$p)
  sf$a <- units::drop_units(sf$a)
  
  reg <- summary(lm(p ~ 0 +sqrt(a), data = sf))$coefficients[,c(1,2,3,4)]
  reg$r2 <- summary(lm(p ~ 0 + sqrt(a), data = sf))$r.squared
  
  reg$rmse <- sqrt(mean((sf$p - (predict((lm(p ~ 0 + sqrt(a), data = sf)), sf)))^2))
  
  names(reg)[1] <- "estimate"
  names(reg)[4] <- "pvalue"
  
  reg$fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  reg$fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas2/coef_z_u_j99/UA_", fua_code, fua_name, "_coef_z_u_j99_12.rds")
  saveRDS(coef, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(reg)
}, u_j99)

long_sums_reg_z_u_j99 <- bind_rows(reg_z_u_j99)
long_sums_reg_z_u_j99 <- as.data.frame(long_sums_reg_z_u_j99)
saveRDS(long_sums_reg_z_u_j99, "output/rds/shape_index/urban/cas2/long_sums_coef_z_u_j99.rds")


###################################################
# Cas 3 - Union _ aggregation of all the u_j

u_g <- Map(function(sf, sf2){
  
  union <- st_union(sf)
  union_sf <- st_as_sf(union)
  
  union_sf$fua_name <- st_drop_geometry(sf2)[1,"fua_name"]
  union_sf$fua_code <- st_drop_geometry(sf2)[1,"fua_code"]
  
  union_sf$a <- st_area(union_sf)
  union_sf$p <- perimeter(union_sf)
  union_sf$s <- pa.shape(union_sf)
  
  union_sf3035 <- st_transform(union_sf, crs = 3035) 
  union_sf3035 <- st_sf(union_sf3035)
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/urban/cas3/u_g/UA_", fua_name, fua_code, "_u_g_12.rds")
  saveRDS(union_sf3035, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(union_sf3035)
}, u_j, multivector)


long_sums_u_g <- do.call(rbind, u_g)
saveRDS(long_sums_u_g, "output/rds/shape_index/urban/cas3/long_sums_u_g.rds")
















