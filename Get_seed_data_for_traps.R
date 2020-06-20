###############################################################################
################ Annotating trap/camera points in the USA with ################
#################### seed data from the nearest FIA plots #####################
######################## within the same habitat type #########################
###############################################################################

#Please note that this script does not work if points are in Hawaii, Puerto Rico or
#Canada/Mexico.  To get those to work you will need to add the appropriate rasters for
#Forest type and land cover

setwd("C:/Users/Arielle/Desktop/NRC/GJAM/NEON/Getting_seed_data_for_cams")

library(dplyr)
library(neonUtilities)
library(raster)
library(sp)
library(sf)
library(nngeo)
library(matrixStats)
library(abind)

`%notin%` <- Negate(`%in%`)

################ Read in the point data ################################
#Small mammal trap locations
traps<-read.csv("C:/Users/Arielle/Desktop/NRC/GJAM/NEON/mammalData_2019/mam_pertrapnight.csv", stringsAsFactors = FALSE)
#remove Puerto Rico
traps<-traps[traps$siteID!="GUAN",]
traps<-traps[traps$siteID!="LAJA",]

#Note: I downloaded the data from the NEON website, could also
#be downloaded within R though

#Stack the zipped tables into one file
#stackByTable("NEON_count-small-mammals.zip", 
#              savepath = NA, 
#              folder = FALSE,
#              saveUnzippedFiles = FALSE, 
#              dpID = NA, nCores = 1)

#Pulling the plot, lat and long columns
trap_coords2<-traps[,c(6,10,11)]
trap_coords<-as.data.frame(trap_coords2 %>% group_by(plotID)%>%
  summarize(lat=mean(decimalLatitude),
            long=mean(decimalLongitude)))

#Seed/mast survey locations (FIA)
load("C:/Users/Arielle/Desktop/NRC/GJAM/NEON/Clark_Mast_data_2020/Clark_Mast_data_latest_ver/pred_seed.Rdata")
seed<-out
seed$site<-row.names(seed)
#Remove Canada
seed2<-seed[!grepl("CNFI", seed$site),]
seed_coords<-seed2[,c(1:2)]
row.names(seed_coords)<-NULL

#Camera locations
cams<-read.csv("Camera_locations_2019.csv", stringsAsFactors = FALSE)

###############################################################################
#################### Convert to spatial points etc.  ##########################
###############################################################################

#Mammal Traps
coordinates(trap_coords)<-c("long", "lat")
proj4string(trap_coords) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
trap_coords_proj<-spTransform(trap_coords, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat"))

#Seed plots
coordinates(seed2)<-c("lon", "lat")
proj4string(seed2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
seed_coords_proj<-spTransform(seed2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat"))

#Cameras
coordinates(cams)<-c("Longitude", "Latitude")
proj4string(cams) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
cam_coords_proj<-spTransform(cams, 
                             CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
                                 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD27 +units=m 
                                 +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,
                                 @ntv2_0.gsb,@ntv1_can.dat"))

###############################################################################
################ Read in the forest type/nlcd rasters #########################
########################## and extract to points ##############################
###############################################################################

#####Lower 48
#Forest types
FTRast2 <- list.files("C:/Users/Arielle/Desktop/NRC/GJAM/NEON/Getting_seed_data_for_cams/conus_forest-type",
                     full.names = T,pattern = '.tif$')
FTRast3<-raster(FTRast2)
trap_FT_48<-extract(FTRast3,trap_coords_proj)
seed_FT_48<-extract(FTRast3,seed_coords_proj)
cam_FT_48<-extract(FTRast3,cam_coords_proj)
rm(FTRast3, FTRast2)

#NLCD
NLCD<-raster("C:/Users/Arielle/Desktop/NRC/GJAM/NEON/Getting_seed_data_for_cams/NLCD_2011_Land_Cover_L48_20190424/NLCD_2011_Land_Cover_L48_20190424.img")
trap_coords_proj2<-spTransform(trap_coords, crs(NLCD))
seed_coords_proj2<-spTransform(seed2, crs(NLCD))
cam_coords_proj2<-spTransform(cams, crs(NLCD))
trap_NLCD_48<-extract(NLCD,trap_coords_proj2)
seed_NLCD_48<-extract(NLCD,seed_coords_proj2)
cam_NLCD_48<-extract(NLCD,cam_coords_proj2)
rm(NLCD)

#####Alaska
#Forest types
FTRast_AK <- raster("C:/Users/Arielle/Desktop/NRC/GJAM/NEON/Getting_seed_data_for_cams/ak_forest-type/AK_forest_rec1.tif")
trap_coords_proj3<-spTransform(trap_coords, crs(FTRast_AK))
seed_coords_proj3<-spTransform(seed2, crs(FTRast_AK))
cam_coords_proj3<-spTransform(cams, crs(FTRast_AK))
trap_FT_AK<-extract(FTRast_AK,trap_coords_proj3)
seed_FT_AK<-extract(FTRast_AK,seed_coords_proj3)
cam_FT_AK<-extract(FTRast_AK,cam_coords_proj3)
rm(FTRast_AK)

#NLCD
NLCD_AK<-raster("C:/Users/Arielle/Desktop/NRC/GJAM/NEON/Getting_seed_data_for_cams/NLCD_2011_Land_Cover_AK_20200213/NLCD_2011_Land_Cover_AK_20200213.img")
trap_coords_proj4<-spTransform(trap_coords, crs(NLCD_AK))
seed_coords_proj4<-spTransform(seed2, crs(NLCD_AK))
cam_coords_proj4<-spTransform(cams, crs(NLCD_AK))
trap_NLCD_AK<-extract(NLCD_AK,trap_coords_proj4)
seed_NLCD_AK<-extract(NLCD_AK,seed_coords_proj4)
cam_NLCD_AK<-extract(NLCD_AK,cam_coords_proj4)
rm(NLCD_AK)

###############################################################################
##################### Combine lower 48 and Alaska #############################
###############################################################################
trap_FT_48[is.na(trap_FT_48)] <- trap_FT_AK[is.na(trap_FT_48)]
trap_NLCD_48[is.na(trap_NLCD_48)] <- trap_NLCD_AK[is.na(trap_NLCD_48)]

seed_FT_48[is.na(seed_FT_48)] <- seed_FT_AK[is.na(seed_FT_48)]
seed_NLCD_48[is.na(seed_NLCD_48)] <- seed_NLCD_AK[is.na(seed_NLCD_48)]

cam_FT_48[is.na(cam_FT_48)] <- cam_FT_AK[is.na(cam_FT_48)]
cam_NLCD_48[is.na(cam_NLCD_48)] <- cam_NLCD_AK[is.na(cam_NLCD_48)]

##############################################################################
########################## If Forest Type is 0, ##############################
###################### pull corresponding NLCD value #########################
##############################################################################
trap_FT_48[trap_FT_48==0] <- trap_NLCD_48[trap_FT_48==0]
trap_habitat<-trap_FT_48
#Convert Alaska sedge to grass for later merging
trap_habitat[which(trap_habitat==72)]<-71

seed_FT_48[seed_FT_48==0] <- seed_NLCD_48[seed_FT_48==0]
seed_habitat<-seed_FT_48

cam_FT_48b<-cam_FT_48
cam_NLCD_48b<-cam_NLCD_48
cam_FT_48b[cam_FT_48b==0] <- cam_NLCD_48b[cam_FT_48b==0]
cam_habitat<-cam_FT_48b
cam_habitat[which(cam_habitat==72)]<-71

###############################################################################
#################### Annotate traps and cameras ###############################
####### with the 3 nearest FIA plots within the same forest type ##############
###############################################################################
#Find the nearest seed plots within the same habitat type
#Annotate the trap and camera layers with the averaged seed data 
#from those seed plots weighted by distance

trap_coords_proj2$habitat<-trap_habitat
seed_coords_proj2$habitat<-seed_habitat
cam_coords_proj3<-cam_coords_proj2
cam_coords_proj3$habitat<-cam_habitat

trap_coords_proj2b<-st_as_sf(trap_coords_proj2)
seed_coords_proj2b<-st_as_sf(seed_coords_proj2)
cam_coords_proj2b<-st_as_sf(cam_coords_proj3)

#Number of neighbors
k<-3
spp<-dim(s)[2]-3
dat<-dat2<-dat_c<-dat_c2<-list()

#Traps
for(i in unique(seed_coords_proj2b$habitat)){
  #Pull those traps and FIA plots in the same habitat
  t<-trap_coords_proj2b[trap_coords_proj2b$habitat==i,]
  s<-seed_coords_proj2b[seed_coords_proj2b$habitat==i,]
  #Get just the seed data for those plots
  s2<-st_drop_geometry(s[,1:spp])
  if(nrow(t)==0){next}
  else{
    dat[[i]]<-matrix(NA, nrow=nrow(t), ncol=k)
    dat2[[i]]<-matrix(NA, nrow=nrow(t), ncol=spp)
    for(j in 1:nrow(t)){
      #Find the 3 nearest neighbor FIA plots to the traps in the same habitat
      #and return their associated seed data and distance
      nn_dist<-unlist(st_nn(t[j,], s, k=k, returnDist=TRUE))
      dat[[i]][j,]<-nn_dist[1:k]
      #Average the seed data over the 3 plots using a weighted average
      #based on distance
      dat2[[i]][j,]<-colWeightedMeans(as.matrix(s2[dat[[i]][j,],]), 
                                      nn_dist[(k+1):length(nn_dist)])
      #Make sure to also return the trap plot ID
      row.names(dat2[[i]])<-t$plotID
    }
  }
}

#Do the same thing with the cameras
for(i in unique(seed_coords_proj2b$habitat)){
  c<-cam_coords_proj2b[cam_coords_proj2b$habitat==i,]
  s<-seed_coords_proj2b[seed_coords_proj2b$habitat==i,]
  s2<-st_drop_geometry(s[,1:spp])
  if(nrow(c)==0){next}
  else{
    dat_c[[i]]<-matrix(NA, nrow=nrow(c), ncol=k)
    dat_c2[[i]]<-matrix(NA, nrow=nrow(c), ncol=spp)
    for(j in 1:nrow(c)){
      nn_dist<-unlist(st_nn(c[j,], s, k=k, returnDist=TRUE))
      dat_c[[i]][j,]<-nn_dist[1:k]
      dat_c2[[i]][j,]<-colWeightedMeans(as.matrix(s2[dat_c[[i]][j,],]), 
                                        nn_dist[(k+1):length(nn_dist)])
      row.names(dat_c2[[i]])<-c$Deployment.Name
    }
  }
}

#Remove null list elements (those habitats which did not have
#any traps or cameras)
dat2<-dat2[-which(sapply(dat2, is.null))]
dat_c2<-dat_c2[-which(sapply(dat_c2, is.null))]

#Combine into one data frame
traps_with_seeds<-as.data.frame(do.call("rbind", dat2))
cams_with_seeds<-as.data.frame(do.call("rbind", dat_c2))

#Merge back into original data files
traps_with_seeds$plotID<-row.names(traps_with_seeds)
traps_with_seeds2<-merge(trap_coords@data, traps_with_seeds, by="plotID", 
              all.x=TRUE)
colnames(traps_with_seeds2)[2:(spp+1)]<-names(seed2)[1:spp]

cams_with_seeds$Deployment.Name<-row.names(cams_with_seeds)
cams_with_seeds2<-merge(cams@data, cams_with_seeds, 
                        by="Deployment.Name", 
                        all.x=TRUE)
colnames(cams_with_seeds2)[3:(spp+2)]<-names(seed2)[1:spp]

#Save files
save(traps_with_seeds2, file="traps_with_seeds.rda")
save(cams_with_seeds2, file="cams_with_seeds.rda")
