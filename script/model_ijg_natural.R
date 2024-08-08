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


files_multivector <- list.files(path = "output/rds/shape_index/natural/cas1/multivector_v", recursive = TRUE, pattern = paste0("\\.rds$"), full.names = TRUE)

multivector <- lapply(files_multivector[1:785],function(rds){
  
  sf_rds <- readRDS(file = rds)
  
  return(sf_rds) 
})

files_v_j <- list.files(path = "output/rds/shape_index/natural/cas2/v_j", recursive = TRUE, pattern = paste0("\\.rds$"), full.names = TRUE)

v_j <- lapply(files_v_j[1:785],function(rds){
  
  sf_rds <- readRDS(file = rds)
  
  return(sf_rds) 
})

#################
# DIFFERENCE to put the natural road in the natural class

multivector <- Map(function(sf, sf2){
  
  if (!identical(st_crs(sf), st_crs(sf2))) {
    sf <- st_transform(sf, crs = st_crs(sf2))
  }
  
  road <- sf2[sf2$classif == 3, ]
  
  
  road_inatural <- st_difference(road, sf)
  
  UA_natural <- sf2[sf2$classif == 1, ]
  UA_natural$v3 <- UA_natural$classif
  
  
  footprint_natural <- rbind(UA_natural, road_inatural)
  
  
  fua_name <- st_drop_geometry(sf2)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf2)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/natural/cas1/multivector_v/UA_", fua_code, fua_name, "_v_i_12.rds")
  saveRDS(footprint_natural, output_filename)
  
  message(paste(fua_name, fua_code))
  
  
  return(footprint_natural)
}, footprint, c)


################################################################################
# Cas 1 - v_i without road

v_i <- lapply(c, function(sf) {
  
  UA_natural <- sf[sf$classif == 2, ] 
  UA_natural$v3 <- UA_natural$classif
  
  natural_i <- subset(UA_natural, !code_2012 %in% c("12210", "12220", "12230"))
  
  natural_i$a <- st_area(natural_i)
  natural_i$p <- perimeter(natural_i)
  natural_i$s <- pa.shape(natural_i)
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/natural/cas1/v_i/UA_", fua_code, fua_name, "_v_i_12.rds")
  saveRDS(natural_i, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(c) 
})

# mean_u_i
mean_v_i <- lapply(v_i, function(sf) {
  
  mean_s <- sf %>% 
    group_by(fua_code, fua_name) %>% 
    summarise(m_v.i = mean(s))
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/natural/cas1/m_v_i/UA_", fua_name, fua_code, "_m_v_i_12.rds")
  saveRDS(mean_s, output_filename)
  
  message(paste(fua_name, fua_code))  
  
  return(mean_s) 
})

long_sums_m_v_i<- do.call(rbind, mean_v_i)
saveRDS(long_sums_m_v_i, "output/rds/shape_index/natural/cas1/long_sums_m_v_i.rds")


# coef u_i

reg_v_i <- Map(function(sf){
  
  sf$p <- units::drop_units(sf$p)
  sf$a <- units::drop_units(sf$a)
  
  reg <- summary(lm(p ~ sqrt(a), data = sf))$coefficients[,c(1,4)]
  reg$r2 <- summary(lm(p ~ sqrt(a), data = sf))$r.squared
  
  reg$rmse <- sqrt(mean((sf$p - (predict((lm(p ~ sqrt(a), data = sf)), sf)))^2))
  
  
  names(reg)[1] <- "intercept"
  names(reg)[2] <- "estimate"
  names(reg)[3] <- "t_value"
  names(reg)[4] <- "p_value"
  
  reg$fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  reg$fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/natural/cas1/coef_v_i/UA_", fua_code, fua_name, "_coef_v_i_12.rds")
  saveRDS(reg, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(reg)
}, v_i)

long_sums_reg_v_i <- do.call(rbind, reg_v_i)
long_sums_reg_v_i <- as.data.frame(long_sums_reg_v_i)
saveRDS(long_sums_reg_v_i, "output/rds/shape_index/natural/cas1/long_sums_coef_v_i.rds")


####################################################"
# Cas 2 - Union & cast

# aggregation
v_j <- lapply(multivector, function(sf) {
  
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
  
  output_filename <- paste0("output/rds/shape_index/natural/cas2/v_j/UA_", fua_code, fua_name, "_v_j_12.rds")
  saveRDS(union_cast_sf3035, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(union_cast_sf3035) 
})

# union_mean
mean_v_j <- lapply(v_j, function(sf) {
  
  mean_s <- sf %>% 
    group_by(fua_code, fua_name) %>% 
    summarise(m_v_j = mean(s))
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/natural/cas2/m_v_j/UA_", fua_code, fua_name, "_m_v_j_12.rds")
  saveRDS(mean_s, output_filename)
  
  message(paste(fua_name, fua_code))  
  
  return(mean_s) 
})

long_sums_m_v_j <- do.call(rbind, mean_v_j)
saveRDS(long_sums_m_v_j, "output/rds/shape_index/natural/cas2/long_sums_m_v_j.rds")


# coef v_j
reg_v_j <- Map(function(sf){
  
  sf$p <- units::drop_units(sf$p)
  sf$a <- units::drop_units(sf$a)
  
  reg <- summary(lm(p ~ sqrt(a), data = sf))$coefficients[,c(1,4)]
  reg$r2 <- summary(lm(p ~ sqrt(a), data = sf))$r.squared
  
  reg$rmse <- sqrt(mean((sf$p - (predict((lm(p ~ sqrt(a), data = sf)), sf)))^2))
  
  names(reg)[1] <- "intercept"
  names(reg)[2] <- "estimate"
  names(reg)[3] <- "t_value"
  names(reg)[4] <- "p_value"
  
  reg$fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  reg$fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/natural/cas2/coef_v_j/UA_",fua_code, fua_name, "_coef_v_j_12.rds")
  saveRDS(reg, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(reg)
}, v_j)

long_sums_reg_v_j <- do.call(rbind, reg_v_j)
long_sums_reg_v_j <- as.data.frame(long_sums_reg_v_j)
saveRDS(long_sums_reg_v_j, "output/rds/shape_index/natural/cas2/long_sums_coef_v_j.rds")

############## cas2 v_j99, without 1% max ################

v_j99 <- Map(function(sf){
  
  j99 <- subset(sf, s < quantile(s, 0.99))
  
  j99$perc99 <- sum(j99$perc)
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/natural/cas2/v_j99/UA_", fua_code, fua_name, "_v_j99_12.rds")
  saveRDS(j99, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(j99)
}, v_j)


mean_v_j99 <- lapply(v_j99, function(sf) {
  
  mean_s <- sf %>% 
    group_by(fua_code, fua_name) %>% 
    summarise(m_v.j99 = mean(s))
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/natural/cas2/m_v_j99/UA_", fua_code, fua_name, "_m_v_j99_12.rds")
  saveRDS(mean_s, output_filename)
  
  message(paste(fua_name, fua_code))  
  
  return(mean_s) 
})

long_sums_m_v_j99 <- do.call(rbind, mean_v_j99)
saveRDS(long_sums_m_v_j99, "output/rds/shape_index/natural/cas2/long_sums_m_v_j99.rds")


# coef v_j99
reg_v_j99 <- Map(function(sf){
  
  sf$p <- units::drop_units(sf$p)
  sf$a <- units::drop_units(sf$a)
  
  reg <- summary(lm(p ~ sqrt(a), data = sf))$coefficients[,c(1,4)]
  reg$r2 <- summary(lm(p ~ sqrt(a), data = sf))$r.squared
  
  reg$rmse <- sqrt(mean((sf$p - (predict((lm(p ~ sqrt(a), data = sf)), sf)))^2))
  
  names(reg)[1] <- "intercept"
  names(reg)[2] <- "estimate"
  names(reg)[3] <- "t_value"
  names(reg)[4] <- "p_value"
  
  reg$fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  reg$fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/natural/cas2/coef_v_j99/UA_", fua_code, fua_name, "_coef_v_j99_12.rds")
  saveRDS(reg, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(reg)
}, v_j99)

long_sums_reg_v_j99 <- do.call(rbind, reg_v_j99)
long_sums_reg_v_j99 <- as.data.frame(long_sums_reg_v_j99)
saveRDS(long_sums_reg_v_j99, "output/rds/shape_index/natural/cas2/long_sums_reg_v_j99.rds")


# coef z_u_j99
reg_z_v_j99 <- Map(function(sf){
  
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
  
  output_filename <- paste0("output/rds/shape_index/natural/cas2/coef_z_u_j99/UA_", fua_code, fua_name, "_coef_z_u_j99_12.rds")
  saveRDS(coef, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(reg)
}, v_j99)

long_sums_reg_z_v_j99 <- bind_rows(reg_z_v_j99)
long_sums_reg_z_v_j99 <- as.data.frame(long_sums_reg_z_v_j99)
saveRDS(long_sums_reg_z_v_j99, "output/rds/shape_index/natural/cas2/long_sums_coef_z_v_j99.rds")



###################################################
# Cas 3 - Union _ aggregation of all the u_j

# stop after Odense DK003L2

v_g <- Map(function(sf){
  
  union <- st_union(sf)
  union_sf <- st_as_sf(union)
  
  union_sf$fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  union_sf$fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  union_sf$a <- st_area(union_sf)
  union_sf$p <- perimeter(union_sf)
  union_sf$s <- pa.shape(union_sf)
  
  union_sf3035 <- st_transform(union_sf, crs = 3035) 
  union_sf3035 <- st_sf(union_sf3035)
  
  fua_name <- st_drop_geometry(sf)[1,"fua_name"]
  fua_name <- substr(basename(fua_name),1,8)
  fua_code <- st_drop_geometry(sf)[1,"fua_code"]
  
  output_filename <- paste0("output/rds/shape_index/natural/cas3/v_g/UA_", fua_code, fua_name, "_v_g_12.rds")
  saveRDS(union_sf3035, output_filename)
  
  message(paste(fua_name, fua_code))
  
  return(union_sf3035)
}, v_j[1:785])


long_sums_v_g <- do.call(rbind, v_g)
saveRDS(long_sums_v_g, "output/rds/shape_index/natural/cas3/long_sums_v_g.rds")
