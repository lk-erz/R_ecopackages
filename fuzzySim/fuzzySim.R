# install.packages("fuzzySim")
library(fuzzySim)
library(rgdal)
library(vegan)

# Load rotifers sample dataset
data(rotifers)
help(rotifers)  
head(rotifers)

### Preparing species occurence data ###

# species names too long for column names -> abbreviate with spCodes
rotifers$spcode <- spCodes(rotifers$species, sep.species = "_", nchar.gen = 1, nchar.sp = 5, nchar.ssp = 0, sep.spcode = "")
# important thing is that the names are unique -> see function output

# fuzzySim needs presence-absence data with species in cols!
rotifers_presabs <- splist2presabs(rotifers, sites.col = "TDWG4", sp.col = "spcode", keep.n = FALSE)
head(rotifers_presabs)

#-------------------------------------------------------------------------------
### Mapping binary species occurence data ###

# use rgdal to download the TDWG level 4 maps
TDWG4shp <- readOGR(dsn = "https://raw.githubusercontent.com/tdwg/wgsrpd/master/geojson/level4.geojson")

# append species data to the respective region
TDWG4shp@data <- data.frame(TDWG4shp@data, rotifers_presabs[match(TDWG4shp@data$Level4_cod, rotifers_presabs$TDWG4), ])

# spplot from sp package
# choose species in param "zcol"
print(spplot(TDWG4shp, zcol = "Abrigh", col.regions = rev(heat.colors(256)), main = expression(paste(italic("A. brightwellii"), " occurrence records"))))

#-------------------------------------------------------------------------------
### Converting binary to fuzzy occurrence data ###

#load data that contain also coordinates
data(rotif.env)
rotif_env <- rotif.env # convert for naming conventions
head(rotif_env)

# to convert presence_absence occurence data to fuzzy data:
#     1) trend surface analysis (TSA)
      help(multTSA)
#     2) inverse distance interpolation
      help(distPres)

## 1)
# 3rd-degree polynomial with stepwise selection of terms:
rotifers_tsa <- multTSA(rotif_env, sp.cols = 18:47, coord.cols = c("Longitude", "Latitude"), id.col = 1, degree = 3, step = TRUE)
# now we have spatial trends in occurence for each species
head(rotifers_tsa)
# check if value range is (0, 1)
range(rotifers_tsa[ , -1])

# append results to the map table and plot
TDWG4shp@data <- data.frame(TDWG4shp@data, rotifers_tsa[match(TDWG4shp@data$Level4_cod, rotifers_tsa$TDWG4), ]) 
print(spplot(TDWG4shp, zcol = "Abrigh_TS", col.regions = rev(heat.colors(256)), main = expression(paste(italic("A. brightwellii"), " TSA"))))
# consider trying different polynimoial degrees or stepwise selection!!

## 2)
rotifers_invdist <- distPres(rotif_env, sp.cols = 18:47, coord.cols = c("Longitude", "Latitude"), id.col = 1, p = 1, inv = TRUE, suffix = "_D")
# quick check of results
head(rotifers_invdist)
range(rotifers_invdist[ , -1])

# Note that inverse distance to presence is calculated only for absence localities; 
# distPres maintains the value 1 for presences
TDWG4shp@data <- data.frame(TDWG4shp@data, rotifers_invdist[match(TDWG4shp@data$Level4_cod, rotifers_invdist$TDWG4), ]) 
print(spplot(TDWG4shp, zcol = "Abrigh_D", col.regions = rev(heat.colors(256)), main = expression(paste(italic("A. brightwellii"), " inverse distance"))))

# Note that distance is also not always a good fuzzy representation of a species’ 
# occurrence area, as geographical and environmental barriers may cause sharp 
# local variations in species’ occurrence patterns.

#-------------------------------------------------------------------------------
### Distribution models with environmental data ###

# Another way of obtaining fuzzy versions of species occurrence is to build 
# distribution models based on the relationship between species presence/absence 
# and a set of geographical, environmental and/or human variables.
help(multGLM) # for more information

rotifers_fav <- multGLM(data = rotif_env, sp.cols = 18:47, var.cols = 5:17, id.col = 1)
head(rotifers_fav$predictions)

# append predictions to the map data and plot
TDWG4shp@data <- data.frame(TDWG4shp@data, rotifers_fav$predictions[match(TDWG4shp@data$Level4_cod, rotifers_fav$predictions$TDWG4), ]) 
print(spplot(TDWG4shp, zcol = "Abrigh_F", col.regions = rev(heat.colors(256)), main = expression(paste(italic("A. brightwellii"), " favourability"))))

#-------------------------------------------------------------------------------
### Calculating (fuzzy) similarity among species occurrence patterns ###

# now use these favourability model predictions as fuzzy occurrence values
# get a matrix of pair-wise fuzzy similarity among these fuzzy species’ distributions, 
# by comparing these columns with e.g. the fuzzy Jaccard similarity index
fuz_sim_mat <- simMat(rotifers_fav$predictions[ , 32:61], method = "Jaccard")

# similarity matrix
image(x = 1:ncol(fuz_sim_mat), y = 1:nrow(fuz_sim_mat), z = fuz_sim_mat, col = rev(heat.colors(256)), xlab = "", ylab = "", axes = FALSE, main = "Fuzzy Jaccard similarity")
axis(side = 1, at = 1:ncol(fuz_sim_mat), tick = FALSE, labels = colnames(fuz_sim_mat), las = 2, cex.axis = 0.8)
axis(side = 2, at = 1:nrow(fuz_sim_mat), tick = FALSE, labels = rownames(fuz_sim_mat), las = 2, cex.axis = 0.8)

# build cluster dendrogram from the similarity matrix using "hclust"
fuz_dendro <- hclust(as.dist(1 - fuz_sim_mat), method = "average")
plot(fuz_dendro, main = "Fuzzy dendrogram", xlab = "", ylab = "", sub = "Fuzzy Jaccard index\nUPGMA clustering")

# build a similarity matrix from the original binary presence-absence data
# to compare with the fuzzy similarity results
bin_sim_mat <- simMat(rotif_env[ , 18:47], method = "Jaccard")

mantel(bin_sim_mat, fuz_sim_mat, method = "spearman")

