library(RNetCDF)
library(raster)

setwd("H:/ArcGIS/water_dem/watergap_wfas_corrected_Jan2015/consumption")

# TBD: read (OK); average over time (decades +/- 5) (OK); map to simu (remaining)

#####SSP2
# -- open file, print info, get values & close
ncdf_file <- 'watergap_ssp2_rcp6p0_ind_ww_annual_2005_2100.nc'
fid <- open.nc(ncdf_file); print.nc(fid); dat <- read.nc(fid); close.nc(fid)

# -- read data bundle
summary(dat)
lat_data <- dat[[1]]; lon_data <- dat[[2]]; tim_data <- dat[[3]]; var_data <- dat[[4]]
rm('dat')


# -- average over time
#typeof(var_data); class(var_data); dim(var_data)
# 2005 to 2015 i.e. 11 years
i_years_to_avg_2010 <- (2010-2005)+1+seq(-5,5,1) 
var_tvg_2010 <- apply(X=var_data[,,i_years_to_avg_2010], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2015 to 2025 i.e. 11 years
i_years_to_avg_2020 <- (2020-2015)+11+seq(-5,5,1) 
var_tvg_2020 <- apply(X=var_data[,,i_years_to_avg_2020], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2030 <- (2030-2025)+21+seq(-5,5,1) 
var_tvg_2030 <- apply(X=var_data[,,i_years_to_avg_2030], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2040 <- (2040-2035)+31+seq(-5,5,1) 
var_tvg_2040 <- apply(X=var_data[,,i_years_to_avg_2040], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2050 <- (2050-2045)+41+seq(-5,5,1) 
var_tvg_2050 <- apply(X=var_data[,,i_years_to_avg_2050], MARGIN=c(1,2), FUN=mean,na.rm=T)


# -- map it & link to simus 

# NB: for me the easiest would be to make the link with rasters, then average values over simus

# create a template raster for the incoming data
lon_res <- lon_data[2]-lon_data[1]
lat_res <- lat_data[2]-lat_data[1]
r_2010 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                     xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                     ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2020 <-  raster(ncols=length(lon_data),nrows=length(lat_data),
                  xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                  ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2030 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2040 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2050 <-  raster(ncols=length(lon_data),nrows=length(lat_data),
                  xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                  ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

#2010
my_order_cols<- seq(1,dim(var_tvg_2010)[2])
var_tvg_2010_rearr <- var_tvg_2010[,rev(my_order_cols)]
values_2010 <- as.vector(as.matrix(var_tvg_2010_rearr))
values(r_2010) <- values_2010

#2020
my_order_cols<- seq(1,dim(var_tvg_2020)[2])
var_tvg_2020_rearr <- var_tvg_2020[,rev(my_order_cols)]
values_2020 <- as.vector(as.matrix(var_tvg_2020_rearr))
values(r_2020) <- values_2020

#2030
my_order_cols<- seq(1,dim(var_tvg_2030)[2])
var_tvg_2030_rearr <- var_tvg_2030[,rev(my_order_cols)]
values_2030 <- as.vector(as.matrix(var_tvg_2030_rearr))
values(r_2030) <- values_2030

#2040
my_order_cols<- seq(1,dim(var_tvg_2040)[2])
var_tvg_2040_rearr <- var_tvg_2040[,rev(my_order_cols)]
values_2040 <- as.vector(as.matrix(var_tvg_2040_rearr))
values(r_2040) <- values_2040

#2050
my_order_cols<- seq(1,dim(var_tvg_2050)[2])
var_tvg_2050_rearr <- var_tvg_2050[,rev(my_order_cols)]
values_2050 <- as.vector(as.matrix(var_tvg_2050_rearr))
values(r_2050) <- values_2050

rast_stack<-stack(r_2010,r_2020,r_2030,r_2040,r_2050)
writeRaster(rast_stack,filename="SSP2_induse_ww_ave.tiff", format="GTiff", overwrite=TRUE)

ind_check_2010<-subset(values_2010,values_2010>0)
ind_value_2010<-sum(ind_check_2010)

#####SSP2
# -- open file, print info, get values & close
ncdf_file <- 'watergap_ssp2_rcp6p0_dom_ww_annual_2005_2100.nc'
fid <- open.nc(ncdf_file); print.nc(fid); dat <- read.nc(fid); close.nc(fid)

# -- read data bundle
summary(dat)
lat_data <- dat[[1]]; lon_data <- dat[[2]]; tim_data <- dat[[3]]; var_data <- dat[[4]]
rm('dat')

# -- average over time
#typeof(var_data); class(var_data); dim(var_data)
# 2005 to 2015 i.e. 11 years
i_years_to_avg_2010 <- (2010-2005)+1+seq(-5,5,1) 
var_tvg_2010 <- apply(X=var_data[,,i_years_to_avg_2010], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2015 to 2025 i.e. 11 years
i_years_to_avg_2020 <- (2020-2015)+11+seq(-5,5,1) 
var_tvg_2020 <- apply(X=var_data[,,i_years_to_avg_2020], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2030 <- (2030-2025)+21+seq(-5,5,1) 
var_tvg_2030 <- apply(X=var_data[,,i_years_to_avg_2030], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2040 <- (2040-2035)+31+seq(-5,5,1) 
var_tvg_2040 <- apply(X=var_data[,,i_years_to_avg_2040], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2050 <- (2050-2045)+41+seq(-5,5,1) 
var_tvg_2050 <- apply(X=var_data[,,i_years_to_avg_2050], MARGIN=c(1,2), FUN=mean,na.rm=T)


# -- map it & link to simus 

# NB: for me the easiest would be to make the link with rasters, then average values over simus

# create a template raster for the incoming data
lon_res <- lon_data[2]-lon_data[1]
lat_res <- lat_data[2]-lat_data[1]
r_2010 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2020 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2030 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2040 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2050 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

#2010
my_order_cols<- seq(1,dim(var_tvg_2010)[2])
var_tvg_2010_rearr <- var_tvg_2010[,rev(my_order_cols)]
values_2010 <- as.vector(as.matrix(var_tvg_2010_rearr))
values(r_2010) <- values_2010

#2020
my_order_cols<- seq(1,dim(var_tvg_2020)[2])
var_tvg_2020_rearr <- var_tvg_2020[,rev(my_order_cols)]
values_2020 <- as.vector(as.matrix(var_tvg_2020_rearr))
values(r_2020) <- values_2020

#2030
my_order_cols<- seq(1,dim(var_tvg_2030)[2])
var_tvg_2030_rearr <- var_tvg_2030[,rev(my_order_cols)]
values_2030 <- as.vector(as.matrix(var_tvg_2030_rearr))
values(r_2030) <- values_2030

#2040
my_order_cols<- seq(1,dim(var_tvg_2040)[2])
var_tvg_2040_rearr <- var_tvg_2040[,rev(my_order_cols)]
values_2040 <- as.vector(as.matrix(var_tvg_2040_rearr))
values(r_2040) <- values_2040

#2050
my_order_cols<- seq(1,dim(var_tvg_2050)[2])
var_tvg_2050_rearr <- var_tvg_2050[,rev(my_order_cols)]
values_2050 <- as.vector(as.matrix(var_tvg_2050_rearr))
values(r_2050) <- values_2050

rast_stack<-stack(r_2010,r_2020,r_2030,r_2040,r_2050)
writeRaster(rast_stack,filename="SSP2_domuse_ww_ave.tiff", format="GTiff", overwrite=TRUE)

dom_check_2010<-subset(values_2010,values_2010>0)
dom_value_2010<-sum(dom_check_2010)

#####SSP1
# -- open file, print info, get values & close
ncdf_file <- 'watergap_ssp1_rcp4p5_ind_ww_annual_2005_2100.nc'
fid <- open.nc(ncdf_file); print.nc(fid); dat <- read.nc(fid); close.nc(fid)

# -- read data bundle
summary(dat)
lat_data <- dat[[1]]; lon_data <- dat[[2]]; tim_data <- dat[[3]]; var_data <- dat[[4]]
rm('dat')

# -- average over time
#typeof(var_data); class(var_data); dim(var_data)
# 2005 to 2015 i.e. 11 years
i_years_to_avg_2010 <- (2010-2005)+1+seq(-5,5,1) 
var_tvg_2010 <- apply(X=var_data[,,i_years_to_avg_2010], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2015 to 2025 i.e. 11 years
i_years_to_avg_2020 <- (2020-2015)+11+seq(-5,5,1) 
var_tvg_2020 <- apply(X=var_data[,,i_years_to_avg_2020], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2030 <- (2030-2025)+21+seq(-5,5,1) 
var_tvg_2030 <- apply(X=var_data[,,i_years_to_avg_2030], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2040 <- (2040-2035)+31+seq(-5,5,1) 
var_tvg_2040 <- apply(X=var_data[,,i_years_to_avg_2040], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2050 <- (2050-2045)+41+seq(-5,5,1) 
var_tvg_2050 <- apply(X=var_data[,,i_years_to_avg_2050], MARGIN=c(1,2), FUN=mean,na.rm=T)


# -- map it & link to simus 

# NB: for me the easiest would be to make the link with rasters, then average values over simus

# create a template raster for the incoming data
lon_res <- lon_data[2]-lon_data[1]
lat_res <- lat_data[2]-lat_data[1]
r_2010 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2020 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2030 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2040 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2050 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

#2010
my_order_cols<- seq(1,dim(var_tvg_2010)[2])
var_tvg_2010_rearr <- var_tvg_2010[,rev(my_order_cols)]
values_2010 <- as.vector(as.matrix(var_tvg_2010_rearr))
values(r_2010) <- values_2010

#2020
my_order_cols<- seq(1,dim(var_tvg_2020)[2])
var_tvg_2020_rearr <- var_tvg_2020[,rev(my_order_cols)]
values_2020 <- as.vector(as.matrix(var_tvg_2020_rearr))
values(r_2020) <- values_2020

#2030
my_order_cols<- seq(1,dim(var_tvg_2030)[2])
var_tvg_2030_rearr <- var_tvg_2030[,rev(my_order_cols)]
values_2030 <- as.vector(as.matrix(var_tvg_2030_rearr))
values(r_2030) <- values_2030

#2040
my_order_cols<- seq(1,dim(var_tvg_2040)[2])
var_tvg_2040_rearr <- var_tvg_2040[,rev(my_order_cols)]
values_2040 <- as.vector(as.matrix(var_tvg_2040_rearr))
values(r_2040) <- values_2040

#2050
my_order_cols<- seq(1,dim(var_tvg_2050)[2])
var_tvg_2050_rearr <- var_tvg_2050[,rev(my_order_cols)]
values_2050 <- as.vector(as.matrix(var_tvg_2050_rearr))
values(r_2050) <- values_2050

rast_stack<-stack(r_2010,r_2020,r_2030,r_2040,r_2050)
writeRaster(rast_stack,filename="SSP1_ind_ww_ave.tiff", format="GTiff", overwrite=TRUE)

ind_ssp1_check_2010<-subset(values_2010,values_2010>0)
ind_ssp1_value_2010<-sum(ind_ssp1_check_2010)



# -- open file, print info, get values & close
ncdf_file <- 'watergap_ssp1_rcp4p5_dom_ww_annual_2005_2100.nc'
fid <- open.nc(ncdf_file); print.nc(fid); dat <- read.nc(fid); close.nc(fid)

# -- read data bundle
summary(dat)
lat_data <- dat[[1]]; lon_data <- dat[[2]]; tim_data <- dat[[3]]; var_data <- dat[[4]]
rm('dat')

# -- average over time
#typeof(var_data); class(var_data); dim(var_data)
# 2005 to 2015 i.e. 11 years
i_years_to_avg_2010 <- (2010-2005)+1+seq(-5,5,1) 
var_tvg_2010 <- apply(X=var_data[,,i_years_to_avg_2010], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2015 to 2025 i.e. 11 years
i_years_to_avg_2020 <- (2020-2015)+11+seq(-5,5,1) 
var_tvg_2020 <- apply(X=var_data[,,i_years_to_avg_2020], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2030 <- (2030-2025)+21+seq(-5,5,1) 
var_tvg_2030 <- apply(X=var_data[,,i_years_to_avg_2030], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2040 <- (2040-2035)+31+seq(-5,5,1) 
var_tvg_2040 <- apply(X=var_data[,,i_years_to_avg_2040], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2050 <- (2050-2045)+41+seq(-5,5,1) 
var_tvg_2050 <- apply(X=var_data[,,i_years_to_avg_2050], MARGIN=c(1,2), FUN=mean,na.rm=T)


# -- map it & link to simus 

# NB: for me the easiest would be to make the link with rasters, then average values over simus

# create a template raster for the incoming data
lon_res <- lon_data[2]-lon_data[1]
lat_res <- lat_data[2]-lat_data[1]
r_2010 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2020 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2030 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2040 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2050 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

#2010
my_order_cols<- seq(1,dim(var_tvg_2010)[2])
var_tvg_2010_rearr <- var_tvg_2010[,rev(my_order_cols)]
values_2010 <- as.vector(as.matrix(var_tvg_2010_rearr))
values(r_2010) <- values_2010

#2020
my_order_cols<- seq(1,dim(var_tvg_2020)[2])
var_tvg_2020_rearr <- var_tvg_2020[,rev(my_order_cols)]
values_2020 <- as.vector(as.matrix(var_tvg_2020_rearr))
values(r_2020) <- values_2020

#2030
my_order_cols<- seq(1,dim(var_tvg_2030)[2])
var_tvg_2030_rearr <- var_tvg_2030[,rev(my_order_cols)]
values_2030 <- as.vector(as.matrix(var_tvg_2030_rearr))
values(r_2030) <- values_2030

#2040
my_order_cols<- seq(1,dim(var_tvg_2040)[2])
var_tvg_2040_rearr <- var_tvg_2040[,rev(my_order_cols)]
values_2040 <- as.vector(as.matrix(var_tvg_2040_rearr))
values(r_2040) <- values_2040

#2050
my_order_cols<- seq(1,dim(var_tvg_2050)[2])
var_tvg_2050_rearr <- var_tvg_2050[,rev(my_order_cols)]
values_2050 <- as.vector(as.matrix(var_tvg_2050_rearr))
values(r_2050) <- values_2050

rast_stack<-stack(r_2010,r_2020,r_2030,r_2040,r_2050)
writeRaster(rast_stack,filename="SSP1_domuse_ww_ave.tiff", format="GTiff", overwrite=TRUE)

ssp1_dom_check_2010<-subset(values_2010,values_2010>0)
ssp1_dom_value_2010<-sum(ssp1_dom_check_2010)



#####ssp3
# -- open file, print info, get values & close
ncdf_file <- 'watergap_ssp3_rcp6p0_ind_ww_annual_2005_2100.nc'
fid <- open.nc(ncdf_file); print.nc(fid); dat <- read.nc(fid); close.nc(fid)

# -- read data bundle
summary(dat)
lat_data <- dat[[1]]; lon_data <- dat[[2]]; tim_data <- dat[[3]]; var_data <- dat[[4]]
rm('dat')

# -- average over time
#typeof(var_data); class(var_data); dim(var_data)
# 2005 to 2015 i.e. 11 years
i_years_to_avg_2010 <- (2010-2005)+1+seq(-5,5,1) 
var_tvg_2010 <- apply(X=var_data[,,i_years_to_avg_2010], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2015 to 2025 i.e. 11 years
i_years_to_avg_2020 <- (2020-2015)+11+seq(-5,5,1) 
var_tvg_2020 <- apply(X=var_data[,,i_years_to_avg_2020], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2030 <- (2030-2025)+21+seq(-5,5,1) 
var_tvg_2030 <- apply(X=var_data[,,i_years_to_avg_2030], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2040 <- (2040-2035)+31+seq(-5,5,1) 
var_tvg_2040 <- apply(X=var_data[,,i_years_to_avg_2040], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2050 <- (2050-2045)+41+seq(-5,5,1) 
var_tvg_2050 <- apply(X=var_data[,,i_years_to_avg_2050], MARGIN=c(1,2), FUN=mean,na.rm=T)


# -- map it & link to simus 

# NB: for me the easiest would be to make the link with rasters, then average values over simus

# create a template raster for the incoming data
lon_res <- lon_data[2]-lon_data[1]
lat_res <- lat_data[2]-lat_data[1]
r_2010 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2020 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2030 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2040 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2050 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

#2010
my_order_cols<- seq(1,dim(var_tvg_2010)[2])
var_tvg_2010_rearr <- var_tvg_2010[,rev(my_order_cols)]
values_2010 <- as.vector(as.matrix(var_tvg_2010_rearr))
values(r_2010) <- values_2010

#2020
my_order_cols<- seq(1,dim(var_tvg_2020)[2])
var_tvg_2020_rearr <- var_tvg_2020[,rev(my_order_cols)]
values_2020 <- as.vector(as.matrix(var_tvg_2020_rearr))
values(r_2020) <- values_2020

#2030
my_order_cols<- seq(1,dim(var_tvg_2030)[2])
var_tvg_2030_rearr <- var_tvg_2030[,rev(my_order_cols)]
values_2030 <- as.vector(as.matrix(var_tvg_2030_rearr))
values(r_2030) <- values_2030

#2040
my_order_cols<- seq(1,dim(var_tvg_2040)[2])
var_tvg_2040_rearr <- var_tvg_2040[,rev(my_order_cols)]
values_2040 <- as.vector(as.matrix(var_tvg_2040_rearr))
values(r_2040) <- values_2040

#2050
my_order_cols<- seq(1,dim(var_tvg_2050)[2])
var_tvg_2050_rearr <- var_tvg_2050[,rev(my_order_cols)]
values_2050 <- as.vector(as.matrix(var_tvg_2050_rearr))
values(r_2050) <- values_2050

rast_stack<-stack(r_2010,r_2020,r_2030,r_2040,r_2050)
writeRaster(rast_stack,filename="ssp3_ind_ww_ave.tiff", format="GTiff", overwrite=TRUE)

ind_ssp3_check_2010<-subset(values_2010,values_2010>0)
ind_ssp3_value_2010<-sum(ind_ssp3_check_2010)



# -- open file, print info, get values & close
ncdf_file <- 'watergap_ssp3_rcp6p0_dom_ww_annual_2005_2100.nc'
fid <- open.nc(ncdf_file); print.nc(fid); dat <- read.nc(fid); close.nc(fid)

# -- read data bundle
summary(dat)
lat_data <- dat[[1]]; lon_data <- dat[[2]]; tim_data <- dat[[3]]; var_data <- dat[[4]]
rm('dat')

# -- average over time
#typeof(var_data); class(var_data); dim(var_data)
# 2005 to 2015 i.e. 11 years
i_years_to_avg_2010 <- (2010-2005)+1+seq(-5,5,1) 
var_tvg_2010 <- apply(X=var_data[,,i_years_to_avg_2010], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2015 to 2025 i.e. 11 years
i_years_to_avg_2020 <- (2020-2015)+11+seq(-5,5,1) 
var_tvg_2020 <- apply(X=var_data[,,i_years_to_avg_2020], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2030 <- (2030-2025)+21+seq(-5,5,1) 
var_tvg_2030 <- apply(X=var_data[,,i_years_to_avg_2030], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2040 <- (2040-2035)+31+seq(-5,5,1) 
var_tvg_2040 <- apply(X=var_data[,,i_years_to_avg_2040], MARGIN=c(1,2), FUN=mean,na.rm=T)

# 2025 to 2035 i.e. 11 years
i_years_to_avg_2050 <- (2050-2045)+41+seq(-5,5,1) 
var_tvg_2050 <- apply(X=var_data[,,i_years_to_avg_2050], MARGIN=c(1,2), FUN=mean,na.rm=T)


# -- map it & link to simus 

# NB: for me the easiest would be to make the link with rasters, then average values over simus

# create a template raster for the incoming data
lon_res <- lon_data[2]-lon_data[1]
lat_res <- lat_data[2]-lat_data[1]
r_2010 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2020 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2030 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2040 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

r_2050 <- raster(ncols=length(lon_data),nrows=length(lat_data),
                 xmn=min(lon_data)-lon_res/2, xmx=max(lon_data)+lon_res/2, 
                 ymn=min(lat_data)-lat_res/2, ymx=max(lat_data)+lat_res/2)

#2010
my_order_cols<- seq(1,dim(var_tvg_2010)[2])
var_tvg_2010_rearr <- var_tvg_2010[,rev(my_order_cols)]
values_2010 <- as.vector(as.matrix(var_tvg_2010_rearr))
values(r_2010) <- values_2010

#2020
my_order_cols<- seq(1,dim(var_tvg_2020)[2])
var_tvg_2020_rearr <- var_tvg_2020[,rev(my_order_cols)]
values_2020 <- as.vector(as.matrix(var_tvg_2020_rearr))
values(r_2020) <- values_2020

#2030
my_order_cols<- seq(1,dim(var_tvg_2030)[2])
var_tvg_2030_rearr <- var_tvg_2030[,rev(my_order_cols)]
values_2030 <- as.vector(as.matrix(var_tvg_2030_rearr))
values(r_2030) <- values_2030

#2040
my_order_cols<- seq(1,dim(var_tvg_2040)[2])
var_tvg_2040_rearr <- var_tvg_2040[,rev(my_order_cols)]
values_2040 <- as.vector(as.matrix(var_tvg_2040_rearr))
values(r_2040) <- values_2040

#2050
my_order_cols<- seq(1,dim(var_tvg_2050)[2])
var_tvg_2050_rearr <- var_tvg_2050[,rev(my_order_cols)]
values_2050 <- as.vector(as.matrix(var_tvg_2050_rearr))
values(r_2050) <- values_2050

rast_stack<-stack(r_2010,r_2020,r_2030,r_2040,r_2050)
writeRaster(rast_stack,filename="ssp3_domuse_ww_ave.tiff", format="GTiff", overwrite=TRUE)

ssp3_dom_check_2010<-subset(values_2010,values_2010>0)
ssp3_dom_value_2010<-sum(ssp3_dom_check_2010)









# fill values with incoming data 
#coordinates(r_template)[1:3,] 
# values are a vector of values starting from upper right corner, then going to east first, then to south
# while the raster values are starting from bottom right
#my_order_rows<- seq(1,dim(var_tvg_2010)[1])
#var_tvg_2010_rearr2 <- var_tvg_2010[rev(my_order_rows),]
#values_2010 <- as.vector(as.matrix(var_tvg_2010_rearr2))
#values(r_template) <- values_2010


my_order_cols<- seq(1,dim(var_tvg_2010)[2])
var_tvg_2010_rearr <- var_tvg_2010[,rev(my_order_cols)]
values_2010 <- as.vector(as.matrix(var_tvg_2010_rearr))
values(r_template) <- values_2010

library(BAMMtools)
library(rasterVis)
library(rgdal)

breaks<-getJenksBreaks(values_2010, 50, subset = NULL)

clevs<-c(0,2,4,6,8,10,12,14,16,18,20,50)

ccols<-c("#5D00FF", "#002EFF","#00B9FF","#00FFB9" ,"#00FF2E","#5DFF00","#E8FF00", "#FF8B00","red", "#FF008B","#E800FF")

palette(ccols)
  
plot(r_template,breaks=breaks, col=palette(ccols))

plot(r_template, col=palette(ccols))

levelplot(r_template)

# link to simu - that I let you do ...

# read simu raster

simu<-raster("H:/ArcGIS/SA-water_basin_connect/simU_raster_Jun16.tif")
colrow<-raster("H:/ArcGIS/SA-water_basin_connect/raster_0_5deg1.tif")


resample(r_template,colrow,method="bilinear",filename="resample.tif")


levelplot(colrow)
# attribute value to simu raster from the incoming raster
# get the values @ 5' & aggregate to SimU level

temp<-zonal(r_template,simu, 'sum')