rm(list=ls())
###############
## libraries ##
library(rgdal)
library(spBayes)
library(geoR)
library(gstat)
library(MBA)
###############
grd <- readGDAL("lidar-covariates/lidar-covariates-5x5.tif")
names(grd) <- as.character(read.table("lidar-covariates/band-names-5x5")[,1])
## P.Heights ##
p.heights <- cbind(grd[["P50"]], grd[["P95"]])
p.heights.dat <- as.data.frame(cbind(p.heights, seq(1, nrow(p.heights),1)))
colnames(p.heights.dat) <- c("P50","P95","index")
p.heights.dat <- p.heights.dat[complete.cases(p.heights),]
## coordinates ##
coords <- coordinates(grd)
coords <- as.data.frame(cbind(coords, seq(1, nrow(coords),1)))
colnames(coords) <- c("x","y","index")
coords <- coords[complete.cases(p.heights),]
## Biomass ##
biomass <- grd[["biomass"]]
biomass <- as.data.frame(cbind(biomass, seq(1, length(biomass),1)))
colnames(biomass) <- c("biomass","index")
biomass <- biomass[complete.cases(p.heights),]
## NCI Biomass ##
NCI.biomass <- grd[["NCI.biomass"]]
NCI.biomass <- as.data.frame(cbind(NCI.biomass, seq(1, length(NCI.biomass),1)))
colnames(NCI.biomass) <- c("NCI.biomass","index")
NCI.biomass <- NCI.biomass[complete.cases(p.heights),]
## Point.counts ##
point.counts <- grd[["point.counts"]]
point.counts <- as.data.frame(cbind(point.counts, seq(1, length(point.counts),1)))
colnames(point.counts) <- c("point.counts","index")
point.counts <- point.counts[complete.cases(p.heights),]
## Tree.height ##
tree.height <- grd[["tree.height"]]
tree.height <- as.data.frame(cbind(tree.height, seq(1, length(tree.height),1)))
colnames(tree.height) <- c("tree.height","index")
tree.height <- tree.height[complete.cases(p.heights),]
## Tree.height.elev ##
tree.height.elev <- grd[["tree.height.elev5"]]
tree.height.elev <- as.data.frame(cbind(tree.height.elev, seq(1, length(tree.height.elev),1)))
colnames(tree.height.elev) <- c("tree.height","index")
tree.height.elev <- tree.height.elev[complete.cases(p.heights),]
## stem count ##
stem.count <- grd[["stem.count"]]
stem.count <- as.data.frame(cbind(stem.count, seq(1, length(stem.count),1)))
colnames(stem.count) <- c("stem.count","index")
stem.count <- stem.count[complete.cases(p.heights),]
## Elelvation ##
elevation <- grd[["Elevation"]]
elevation <- as.data.frame(cbind(elevation, seq(1, length(elevation),1)))
colnames(elevation) <- c("elevation","index")
elevation <- elevation[complete.cases(p.heights),]
## DEM elevation ##
dem.elevation <- grd[["dem.elevation"]]
dem.elevation <- as.data.frame(cbind(dem.elevation, seq(1, length(dem.elevation),1)))
colnames(dem.elevation) <- c("dem.elevation","index")
dem.elevation <- dem.elevation[complete.cases(p.heights),]
## canopy.tops ##
canopy.top <- grd[["canopy.top"]]
canopy.top <- as.data.frame(cbind(canopy.top, seq(1, length(canopy.top),1)))
colnames(canopy.top) <- c("canopy.top","index")
canopy.top <- canopy.top[complete.cases(p.heights),]
## mean DBH ##
mean.dbh <- round(grd[["mean.dbh"]],2)
mean.dbh <- as.data.frame(cbind(mean.dbh, seq(1, length(mean.dbh),1)))
colnames(mean.dbh) <- c("mean.dbh","index")
mean.dbh <- mean.dbh[complete.cases(p.heights),]
## set.seed
set.seed(1)
## create holdout data
y.hold <- as.data.frame(sample(biomass             [["index"]],round(0.2*nrow(biomass))))
colnames(y.hold) <- c("index")
y.hold <- cbind(biomass[biomass                    [["index"]] %in% y.hold[["index"]],1],
                NCI.biomass[NCI.biomass            [["index"]] %in% y.hold[["index"]],1],
                stem.count[stem.count              [["index"]] %in% y.hold[["index"]],1],
                canopy.top[canopy.top              [["index"]] %in% y.hold[["index"]],1],
                mean.dbh[mean.dbh                  [["index"]] %in% y.hold[["index"]],])
colnames(y.hold) <- c("biomass","NCI.biomass","stem.count","canopy.top","mean.dbh","index")

x.hold <- cbind(p.heights.dat[p.heights.dat        [["index"]] %in% y.hold[["index"]],1:2],
                point.counts[point.counts          [["index"]] %in% y.hold[["index"]],1],
                tree.height[tree.height            [["index"]] %in% y.hold[["index"]],1],
                tree.height.elev[tree.height.elev  [["index"]] %in% y.hold[["index"]],1],
                elevation[elevation                [["index"]] %in% y.hold[["index"]],])
colnames(x.hold) <- c("P50","P95","point.counts","tree.height","tree.height.elev","elevation","index")

coords.hold <- coords[coords                       [["index"]] %in% y.hold[["index"]],]
## set model data
y.mod <- cbind(biomass[!biomass                    [["index"]] %in% y.hold[["index"]],1],
               NCI.biomass[!NCI.biomass            [["index"]] %in% y.hold[["index"]],1],
               stem.count[!stem.count              [["index"]] %in% y.hold[["index"]],1],
               canopy.top[!canopy.top              [["index"]] %in% y.hold[["index"]],1],
               mean.dbh[!mean.dbh                  [["index"]] %in% y.hold[["index"]],])

colnames(y.mod) <- c("biomass","NCI.biomass","stem.count","canopy.top","mean.dbh","index")

x.mod <- cbind(p.heights.dat[!p.heights.dat        [["index"]] %in% y.hold[["index"]],1:2],
               point.counts[!point.counts          [["index"]] %in% y.hold[["index"]],1],
               tree.height[!tree.height            [["index"]] %in% y.hold[["index"]],1],
               tree.height.elev[!tree.height.elev  [["index"]] %in% y.hold[["index"]],1],
               elevation[!elevation                [["index"]] %in% y.hold[["index"]],1],
               dem.elevation[!dem.elevation        [["index"]] %in% y.hold[["index"]],])

colnames(x.mod) <- c("P50","P95","point.counts","tree.height","tree.height.elev","elevation","dem.elevation","index")

coords.mod <- coords[!coords                       [["index"]] %in% y.hold[["index"]],]
write.csv(y.hold, "model_data/y.hold", row.names=FALSE)
write.csv(x.hold, "model_data/x.hold", row.names=FALSE)
write.csv(coords.hold, "model_data/coords.hold", row.names=FALSE)
write.csv(y.mod, "model_data/y.mod", row.names=FALSE)
write.csv(x.mod, "model_data/x.mod", row.names=FALSE)
write.csv(coords.mod, "model_data/coords.mod", row.names=FALSE)
