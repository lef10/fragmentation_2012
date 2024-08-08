#' SpatRaster.to.xyv
#'
#' Returns a data frame with x,y coordinates and values from a single layered
#' Spatial Raster (terra) object
#' 
#####
# # NB a simpler code wrapper with extract incl xy and with na is smarter:
# # SpatRaster.extract.xyv<-function(sr){
# # xyv<-terra::extract(sr,1:terra::ncell(sr),xy=TRUE)
# # xyv[!is.na(xyv[,3]),]}
# Yet, SpatRaster.to.xyv  is >2x quicker for the large rasters tested 
#####
# 
#' @param sr a single layered Spatial Raster
#' @param na.omit removes NA cells from output data frame. (This most probably the main purpose
#'  of the transformation from a raster form to a vector form)
#' @param use.levels uses labels as v if input sr has a levels data frame 
#' @param numeric.levels returns value v of labels as numeric, otherwise as factor
#'
#' @return a data frame with ncol = 3 (x,y,v) and nrow = number of non-NA
#' cells. If na.omit=FALSE all cells (terra::ncell()) are returned in rows.
#'  
#' @export
#'
#' @examples
#'  r <- terra::rast(nrows=10, ncols=10, xmin=0, xmax=300,ymin=0, ymax=300)
#'  terra::values(r) <- factor(rep(1:5,20))
#'  terra::values(r)[2:4] <- NA
#'  SpatRaster.to.xyv(r)
#'  
#'  #With elevation example from package
#'  filename <- system.file("ex/elev.tif", package="terra")
#'  r <- terra::rast(filename)
#'  SpatRaster.to.xyv(r)
#'  
SpatRaster.to.xyv<-function(sr,na.omit=TRUE,use.levels=TRUE,numeric.levels=TRUE){
  
  #coords
  xyv<-data.frame(terra::xyFromCell(sr, 1:terra::ncell(sr)))
  
  #values
  il<-terra::levels(sr) # values and levels table
  i<-terra::values(sr)[,1]  #values only but integer for categorical not labels
  
  if(use.levels==FALSE | !is.data.frame(il[[1]]) ) { #no data frame means no labels or not categorical
    xyv[,"v"]<-i
  } else {
    xyv[,"v"]<-il[[1]][as.vector(i),2]
    if(numeric.levels==TRUE){
      xyv[,"v"]<-as.numeric(xyv[,"v"])
      }
    else {
      xyv[,"v"]<-as.factor(xyv[,"v"])
      }
  } #replace cell values (integers) by levels

  n<-nrow(xyv)
  no.na<-!is.na(xyv[,"v"])
  sum.na<-n-sum(no.na)
  pc.na<-round(100*sum.na/n, digits=2)
  message(paste(sum.na, "NA's', i.e", pc.na,"% of",n,"cells"))
  
  if (na.omit==TRUE){
    xyvna<-xyv[no.na,]
    message("NA's removed")}
  else {
    xyvna<-xyv
    message("NA's kept")}
  
  return(xyvna)
}

