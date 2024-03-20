library(geodata)

# NOTE: all data can be stored in a variable and used from there
# or be downloaded locally!

#-------------------------------------------------------------------------------
##### MAPS #####
# store data in current dir
gadm(country="ITA", version="latest", level=1, resolution=1, path=getwd())
ita <- readRDS("gadm/gadm41_ITA_1_pk.rds")

# alternatively in variable
#ita <- gadm(country="ITA", version="latest", level=1, resolution=1, path=tempdir())

typeof(ita) # downloaded data is a S4 class SpatVector

plot(ita)
text(ita, label=ita$NAME_1, cex=0.5, pos=1)

#-------------------------------------------------------------------------------
##### ELEVATION DATA #####
elevation_30s(country="ITA", mask=TRUE, path=getwd())
ita_elev <- rast("ITA_elv_msk.tif")

plot(ita_elev, col=terrain.colors(25))
lines(ita, lwd=1, col="gray")
mtext("Elevation [m]", side = 4, line = 0, at = mean(range(values(ita_elev))))


# calculate aspect, slope for better visualization
slope <- terrain(ita_elev, "slope", unit="radians")
aspect <- terrain(ita_elev, "aspect", unit="radians")
hill <- shade(slope, aspect, 45, 270)

plot(hill, col=grey(0:100/100), legend=FALSE, mar=c(2,2,1,4))
plot(ita_elev, col=terrain.colors(25, alpha=0.45), add=TRUE, main="Topography")
lines(ita, lwd=1, col="gray")
mtext("Elevation [m]", side = 4, line = 0, at = mean(range(values(ita_elev))))

#-------------------------------------------------------------------------------
##### TEMPERATURE DATA (WORLDCLIM) #####
ita_temp <- worldclim_country("ITA", var="tavg", path=tempdir())

colormap <- colorRampPalette(c("blue", "red"))(100)
plot(ita_temp, 
     main=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
     col=colormap,
     range=c(-15, 35))
