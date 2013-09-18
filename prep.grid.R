rm(list=ls())
###############
## libraries ##
library(sp)
library(maptools)
library(geoR)
library(spBayes)
library(rgdal)
library(rgl)
library(RColorBrewer)
library(sqldf)
source("functions/read.lidar.R")
source("functions/get.p.R")
source("functions/grab.points.R")
source("functions/tiles.R")

## SOUTH WEST CORNER OF PLOT
## 18 19 26 NORTH
## 65 49 3 WEST
## CONVERT TO UTM
## Universal Transverse Mercator (UTM):
## Zone: 20
## Easting: 202184
## Northing: 2028323

## Field data
tree.dat <- read.csv("model_data/LFDP_Censuses_15_Dataset.csv", header = TRUE)
tree.dat <- tree.dat[tree.dat$CENSUS == 5,]#c("X","Y")]

## LiDAR data
lidar.dat <- read.lidar("lidar-data-clip")
lidar.dat <- as.data.frame(lidar.dat)
coordinates(lidar.dat) <- ~x+y

## Add UTM Coordinates
tree.dat$UTM.X <- 202184  + tree.dat$X
tree.dat$UTM.Y <- 2028323 + tree.dat$Y

## Set coordinate reference system
coordinates(tree.dat) <- ~UTM.X+UTM.Y
proj4string(lidar.dat) <- CRS("+init=epsg:26920")
lidar.dat <- spTransform(lidar.dat, CRS("+proj=utm +zone=20N datum=NAD83 +init=epsg:26920"))
proj4string(tree.dat) <- proj4string(lidar.dat)
tree.dat <- spTransform(tree.dat, CRS("+proj=utm +zone=20N datum=NAD83 +init=epsg:26920"))
## Horizontal Coordinates: UTM z 20N NAD83 [EPSG: 26920]  -  Vertical Coordinates: NAVD88 (geoid 03) [EPSG: 5703]
## MAKE GRID
tree.coords <- coordinates(tree.dat)
grd <- tiles(tree.coords, x.cnt=64, y.cnt=100)

## visualize
plot(tree.dat, axes = TRUE, pch=17)
for(i in 1:length(grd)){
    polygon(grd[[i]], border = 'red')
    print(i)
}

## point.index.list <- list(rep(NA, length(grd)))
## points <- slot(lidar.dat,"coords")
## ## get indicies of lidar points in each grid cell
## for(i in 1:length(grd)){
##     point.index.list[[i]] <- pointsInPoly(grd[[i]],points)
##     print(i)
##  }
## save("point.index.list",file = "model_data/point.index.list")


## ## ## load up the saved pointsInPoly object

## load("model_data/point.index.list")

## ## ## ## Shells to Construct Covariates
## mean.points <- matrix(NA, length(grd))
## stdv.points <- matrix(NA, length(grd))
## sort.points <- matrix(NA, length(grd), 20)

## incs <- seq(.05,1,by=.05)

## for(i in 1:length(grd)){
##         ## adding in this if statement to check if there's any point in the grid cell speeds things up tremendously
##     if(length(point.index.list[[i]]) > 20){
##         mean.points[i] <- mean(lidar.dat$z[ point.index.list[[i]] ])
##         ##         ## standard deviation
##         stdv.points[i] <- sd(lidar.dat$z[ point.index.list[[i]] ])
##         ##         ## percentile heights
##         sorted.z <- sort(lidar.dat$z[ point.index.list[[i]]])
##         sort.points[i,] <- sorted.z[round(length(sorted.z)*incs,0)]
##     } else {
##         sort.points[i,] <- rep(NA,20)
##     }
##     print(i)
## }
## 1
## pt.cnt <- c()
## for(i in 1:length(grd)){pt.cnt[i] <- length(point.index.list[[i]])}

## sort.points <- as.data.frame(sort.points)
## names(sort.points) <- paste("P",as.character(seq(5,100, length.out = 20)), sep = "")
## colnames(mean.points) <- 'mean'
## colnames(stdv.points) <- 'stdv.points'

## ## ADD INDEX ##
## sort.points <- cbind(sort.points, seq(1,length(grd),1))
## colnames(sort.points)[21] <- 'Plot_1'
## write.table(mean_points, file="model_data/mean_points.csv", row.names=FALSE)
## write.table(stdv.points, file="model_data/stdv_points.csv", row.names=FALSE)
## write.csv(sort.points, file="model_data/sort_points.csv", row.names=FALSE)

## stdv_points <- read.csv("model_data/stdv_points.csv", header=TRUE)
## mean_points <- read.csv("model_data/mean_points.csv", header=TRUE)
## sort_points <- read.csv("model_data/sort_points.csv", header=TRUE)

## ## join sort_points and elev5
## elevation.points <- sqldf("select elev5.Plot_1,
##                                   Quad_subquad,
##                                   Quad, Subquad,
##                                   Elevation,
##                                   P5, P10, P15, P20, P25, P30, P35, P40, P45,
##                                   P50, P55, P60, P65, P70, P80, P85, P90, P95, P100
##                            from elev5, sort_points
##                            where elev5.Plot_1 = sort_points.Plot_1")
## write.csv(elevation.points,"model_data/elevation_points.csv", row.names=FALSE)

## ## canopy.points <- sqldf("select Plot_1,
##                                Quad_subquad,
##                                TOP_96
##                         from elevation_points, canopy
##                         where canopy.Quad_and_sub_96 = elevation_points.Quad_subquad")
## elevation.points$tree.height <- elevation.points$P95 - elevation.points$Elevation

total_biomass <- read.csv("model_data/total_biomass.csv", header=TRUE)
biomass <- as.data.frame(total_biomass[,6])
colnames(biomass) <- c('biomass')
load("model_data/point.counts")
point.counts <- as.data.frame(point.counts)
NCI.biomass <- read.csv("model_data/NCI.biomass.csv", header=TRUE)
stem.counts <- read.csv("model_data/stem.count", header=TRUE)
canopy.tops <- read.csv("model_data/canopy_dat.csv", header=TRUE)
mean.dbh <- read.csv("model_data/mean.dbh.csv", header=TRUE)
elev_dat <- read.csv("model_data/elevation_points.csv", header=TRUE)
elev_dat <- elev_dat[with(elev_dat, order(Plot_1)), ]

##

elev_dat$biomass <- as.numeric(as.matrix(biomass))
elev_dat$point.counts <- as.numeric(as.matrix(point.counts))
elev_dat$NCI.biomass <- as.numeric(as.matrix(NCI.biomass[,1]))
elev_dat$stem.count <- as.numeric(as.matrix(stem.counts))
elev_dat$canopy.top <- as.numeric(as.matrix(canopy.tops[,3]))
elev_dat$mean.dbh <- as.numeric(as.matrix(mean.dbh[,1]))

##
cellSize <- c(5,5)
cellRes  <- c(64,100)
cellCenterOffset <- c(202186.5,2028325.5)
## make spatial grid ##
grdSP <- SpatialGrid(GridTopology(cellCenterOffset, cellSize, cellRes), proj4string = proj4string(lidar.dat))
grdSP <- SpatialGridDataFrame(grdSP, data = elev_dat)
grdSP <- flipVertical(grdSP)

## DEM ##
dem.idw <- readGDAL(fname="dem/output.idw.asc")
proj4string(dem.idw) <- CRS("+proj=utm +zone=20N datum=NAD83 +init=epsg:26920")
grd.coords <- as.data.frame(coordinates(grdSP))
coordinates(grd.coords) <- ~s1+s2
proj4string(grd.coords) <- CRS("+proj=utm +zone=20N datum=NAD83 +init=epsg:26920")
grd.dem <- over(grd.coords, dem.idw, returnList = FALSE) ## //this yields the same result and is not depreciated
names(grd.dem) <- 'elevation'

grdSP$dem.elevation <- grd.dem[[1]]
grdSP$tree.height <- grdSP$P95 - grdSP$dem.elevation
grdSP$tree.height.elev5 <- grdSP$P95 - grdSP$Elevation
spplot(grdSP['mean.dbh'], col.regions = terrain.colors(20))

## write covariates
writeGDAL(grdSP,"lidar-covariates/lidar-covariates-5x5.tif") ## doesn't write out column names
write.table(names(grdSP),"lidar-covariates/band-names-5x5", row.names=FALSE, col.names=FALSE)

## 3D Plot
## rescale - ???
lidar <- as.data.frame(cbind(coordinates(lidar.dat), lidar.dat$z))
colnames(lidar) <- c('x','y','z')
elev <- as.data.frame(cbind(coordinates(grdSP), grdSP[["Elevation"]]))
colnames(elev) <- c('x','y','z')
p.heights <- as.data.frame(cbind(coordinates(grdSP),grdSP[["P95"]]))
colnames(p.heights) <- c('x','y','z')
canopy.viz <- as.data.frame(cbind(coordinates(grdSP),grdSP[["canopy.top"]]))
colnames(canopy.viz) <- c('x','y','z')
plot3d(lidar, col='green', cex=2)
top.400 <- elev[elev$z > 400,]
plot3d(top.400, col='red', add= TRUE, cex=10)
plot3d(elev, col='darkblue', add=TRUE)


## Elevation/canopy/vegetation data
elev5 <- read.csv("elevation/LFDP_env5m.txt", header=TRUE, as.is=TRUE, sep =",")
canopy <- read.csv("model_data/LFDP_canopy.csv", header=TRUE)
vegetation <- read.csv("model_data/vegetation.csv", header=TRUE)
Elevation.site.summary <- read.csv("elevation/ElevationalRiverDataCoordinatesandElevation.txt", header=TRUE)
Elevation.Icacos <- read.csv("elevation/ElevationalRiverdataIcacos.txt", header=TRUE)
Elevation.Mameyes <- read.csv("elevation/ElevationaRiverDataMameyes.txt", header=TRUE)
Elevation.Sonadora <- read.csv("elevation/ElevationaRiverDataSonadora.txt", header=TRUE)

## fix canopy
canopy <- canopy[!is.na(canopy$TOP_92),]
canopy <- canopy[-770,]
colnames(canopy)[24] <- c("Quad_and_sub_96")
write.csv(canopy,"model_data/canopy.csv",row.names=FALSE)
canopy <- read.csv("model_data/canopy.csv",header=TRUE)
canopy.dat <- sqldf("SELECT Plot_1, Quad_subquad, TOP_96
                 FROM elev_dat, canopy
                 WHERE elev_dat.Quad_subquad = canopy.Quad_and_sub_96")
write.csv(canopy.dat,"model_data/canopy_dat.csv",row.names=FALSE)
1
## ALLOMETRY
## SPECIES names + counts
species.names = as.matrix(unique(tree.dat$SPECIES))
species.codes <- read.csv('model_data/LFDP_spp_list2004.csv')
counts = matrix(NA, 139, 1)
for (i in 1:139){
    counts[i] <- nrow(tree.dat[tree.dat$SPECIES == species.names[i],])
}

species.counts <- cbind(as.data.frame(species.names),as.data.frame(counts))
colnames(species.counts) <- c('SPECIES.CODE','COUNT')
species <- merge(species.counts, species.codes,all = TRUE)
species <- species[with(species, order(-COUNT)),]
write.csv(species, "model_data/species.csv", row.names=FALSE)

## Biomass
## total_biomass <- read.csv("model_data/total_biomass.csv", header=TRUE)
## wait what about compensating for the missing lidar

## Tweleve dominant SPECIES
## First three entries are in order of importance according to Uriarte et al. 2004, following those, the list is alphabetical
## 1) Prestoea acuminata (PREMON) Sierran palm ---------- monocot - COUNT: 10989
## 2) Casearia arborea (CASARB) casearia ------------------ dicot - COUNT: 5227
## 3) Dacryodes excelsa (DACEXC) candletree --------------- dicot - COUNT: 1567
## 4) Alchornea latifolia (ALCLAT) achiotillo ------------- dicot - COUNT: 458
## 5) Buchenavia tetraphylla (BUCTET) fourleaf buchenavia - dicot - COUNT: 276
## 6) Cecropia schreberiana (CECSCH) pumpwood ------------- dicot - COUNT: 1974
## 7) Guarea guidonia (GUAGUI) American muskwood ---------- dicot - COUNT: 615
## 8) Inga laurina (INGLAU) sacky sac bean ---------------- dicot - COUNT: 1386
## 9) Manilkara bidentata (MANBID) bulletwood ------------- dicot - COUNT: 2039
## 10) Schefflera morototoni (SCHMOR) matchwood ----------- dicot - COUNT: 1509
## 11) Sloanea berteriana (SLOBER) bullwood --------------- dicot - COUNT: 3818
## 12) Tabebuia heterophylla (TABHET) white cedar --------- dicot - COUNT: 672

## Remaining monocots.
## Heliconia caribaea (HELCAR) lobsterclaw -------------- monocot - COUNT: 1130  ----> was not able to find biomass allometry, omitted.
## Roystonea borinquena (ROYBOR) royal palm ------------- monocot - COUNT: 43
## Musaceae Musa (MUSSPP) banana ------------------------ monocot - COUNT: 3
## Cyathea borinquena (CYABOR) birdwing treefern ----------- fern - COUNT: 1

species <- read.csv("model_data/species.csv", header=TRUE)

dicot.df <- subset(tree.dat, SPECIES != "PREMON" & SPECIES != "HELCAR" & SPECIES != "ROYBAR" & SPECIES != "MUSSPP" & SPECIES != "CYABOR")
dicot.df$UTM.X <- 202184  + dicot.df$X
dicot.df$UTM.Y <- 2028323 + dicot.df$Y

dicot.list <- list(rep(NA, length(grd)))
dicot.points <- dicot.df@coords

for(i in 1:length(grd)){
    dicot.list[[i]] <- pointsInPoly(grd[[i]],dicot.points)
    print(i)
}

## Shell for dicot biomass
dicot.biomass <- matrix(NA, length(grd))
## calculate the sum of biomass for dicotyledonous trees in each grid pixel, equation from Scatena et al. 1993
for(i in 1:length(grd)){
       dicot.biomass[i] <- round(sum(exp(2.475*log(dicot.df$DIAM[dicot.list[[i]]]) - 2.399), na.rm=TRUE), digits = 3)
       print(i)
}
dicot_biomass <- read.csv("model_data/dicot_biomass.csv", header=TRUE)

## Calculate PREMON biomass
premon <- tree.dat[tree.dat$SPECIES=='PREMON',]
premon.ht.const <- 0.039
## crown allometry equation provided by Maria
premon$HEIGHT <- 1.35+((20-1.35)*(1-exp(-1.0*premon.ht.const*premon$DIAM)))

## index.points
premon.list <- list(rep(NA, length(grd)))
premon.points <- slot(premon, "coords")
for(i in 1:length(grd)){
    premon.list[[i]] <- pointsInPoly(grd[[i]], premon.points)
}
## Scatena et al. 1993 equation
premon.biomass <- matrix(NA, length(grd))
for(i in 1:length(grd)){
    premon.biomass[i] = round(sum(7.7 * premon$HEIGHT[premon.list[[i]]] + 6.8, na.rm=TRUE), digits=3)
}

## ADD INDEX ##
premon.biomass <- cbind(premon.biomass, seq(1,length(grd),1))
colnames(premon.biomass) <- c('Biomass','Plot_1')
write.csv(premon.biomass, "model_data/premon_biomass.csv", row.names=FALSE)
total.biomass <- premon.biomass[,1]+dicot_biomass[,1]
total.biomass <- cbind(total.biomass, seq(1,length(grd),1))
colnames(total.biomass) <- c('Biomass','Plot_1')
## write.csv(total.biomass, "model_data/total_biomass.csv", row.names=FALSE)

total_biomass <- sqldf("select elev5.Plot_1,
                               Quad_subquad,
                               Quad,
                               Subquad,
                               Elevation,
                               Biomass
                        from elev5, total_biomass
                        where elev5.Plot_1 = total_biomass.Plot_1")

total_biomass <- total_biomass[with(total_biomass, order(Plot_1)), ]
write.csv(total_biomass, "model_data/total_biomass.csv", row.names=FALSE)

## NCI.biomass
NCI.biomass.list <- list(rep(NA, length(grd)))
NCI.points <- slot(tree.dat,"coords")
for(i in 1:length(grd)){
    NCI.biomass.list[[i]] <- pointsInPoly(grd[[i]], NCI.points)
    print(i)
}

NCI.biomass <- matrix(NA, length(grd))
for(i in 1:length(grd)){
    NCI.biomass[i] <- sum(tree.dat$NCI.BIOMASS[NCI.biomass.list[[i]]],na.rm=TRUE)
    print(i)
}

NCI.biomass <- as.data.frame(cbind(NCI.biomass, seq(1,length(grd),1)))
colnames(NCI.biomass) <- c('NCI.biomass','index')
write.csv(NCI.biomass, "model_data/NCI.biomass.csv", row.names=FALSE)

## Number of stems
stem.count.list <- list(rep(NA, length(grd)))
stem.points <- slot(tree.dat,"coords")
for(i in 1:length(grd)){
    stem.count.list[[i]] <- pointsInPoly(grd[[i]],stem.points)
    print(i)
}

stem.count <- matrix(NA, length(grd))
for(i in 1:length(grd)){
    stem.count[i] <- sum(length(stem.count.list[[i]]), na.rm=TRUE)
    print(i)
}
colnames(stem.count) <- c('stem.count')
write.csv(stem.count,"model_data/stem.count", row.names=FALSE)

## mean DBH
dbh.count.list <- list(rep(NA, length(grd)))
dbh.points <- slot(tree.dat,"coords")
for(i in 1:length(grd)){
    dbh.count.list[[i]] <- pointsInPoly(grd[[i]],dbh.points)
    print(i)
}

mean.dbh <- matrix(NA, length(grd))
for(i in 1:length(grd)){
    mean.dbh[i] <- mean(tree.dat$DIAM[dbh.count.list[[i]]], na.rm=TRUE)
    print(i)
}

mean.dbh <- as.data.frame(cbind(mean.dbh, seq(1,length(grd),1)))
colnames(mean.dbh) <- c('mean.dbh','index')
write.csv(mean.dbh,"model_data/mean.dbh.csv", row.names=FALSE)


## final amounts ##
load('model_data/dicot.biomass.5x5')
load('model_data/premon.biomass.5x5')

##########################################################
## Use if interested in multiplying by a scaling factor ##
##########################################################
pixel.scaling.factor <- 10000/160000
dicot.biomass.scale <- dicot.biomass * pixel.scaling.factor
premon.biomass.scale <- premon.biomass * pixel.scaling.factor
total.biomass.scale <- premon.biomass.scale + dicot.biomass.scale
total.biomass <- premon.biomass+dicot.biomass

## Andy's instructions
## "...sum the stem biomass within each pixel then multiply that total pixel biomass value by the pixel scaling factor, i.e., 10,000m2/(pixel
## area) m2. The pixel-level forest variable summary serves as the response variable, i.e., y's in the subsequent regression analysis..."
## point.count.index <- list(rep(NA, length(grd)))
## points <- slot(lidar.dat, "coords")
## for(i in 1:length(grd)){
##     point.count.index[[i]] <- pointsInPoly(grd[[i]],points)
##     print(i)
## }
## save("point.count.index",file="model_data/point.count.index")

## load("model_data/point.count.index")
## point.counts <- matrix(NA, length(grd))
## for(i in 1:length(grd)){
##     point.counts[i] <- length(point.count.index[[i]])
##     print(i)
## }
## colnames(point.counts) <- c("point.counts")
##  save("point.counts",file="model_data/point.counts")

y <-rnorm(30)
x <-factor(sample(letters,30,replace=TRUE))
X <-as(x,"sparseMatrix")
class(X)
attr(,"package")
dim(X)

Run the regression:

MatrixModels:::lm.fit.sparse(t(X),y)
