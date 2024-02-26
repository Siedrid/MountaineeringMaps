# Mountaineering Maps Project
library(gpx)
library(sf)
library(xlsx)
library(XML)
hard_drive <- "F"
hikes.path <- paste0(hard_drive, ":/EAGLE/Maps/Hikes/")
hike.info <- read.xlsx(paste0(hard_drive, ":/EAGLE/Maps/Tables/hike_information.xlsx"), sheetIndex = 1)
setwd(hikes.path)

read.gpx <- function(path){
  gpx.parsed <- htmlTreeParse(file = path, useInternalNodes = TRUE)
  coords <- xpathSApply(doc = gpx.parsed, path = "//trkpt", fun = xmlAttrs)
  elevation <- xpathSApply(doc = gpx.parsed, path = "//trkpt/ele", fun = xmlValue)
  
  df <- data.frame(
    lat = as.numeric(coords["lat", ]),
    lon = as.numeric(coords["lon", ]),
    elevation = as.numeric(elevation)
  )
  return(df)
}

fls <- list.files(hikes.path)
gpx.lst <- list()

for (i in 1:length(fls)){
  # get mountain name from fl name
  gpx.name <- strsplit(fls[i], ".gpx")[[1]][1]
  table.name <- gpx.name %>% gsub("ss", "ß", .) %>% gsub("oe", "ö", .) %>% gsub("_", " ", .)
  
  hike.gpx <- read.gpx(fls[i])
  gpx.df <- hike.gpx[c("lon", "lat", "elevation")]
  gpx.mat <- matrix(unlist(gpx.df),,3)
  
  if (table.name %in% hike.info$Mountain){
    hike.lin <- st_linestring(gpx.mat) 
    df <- st_sfc(hike.lin) %>% data.frame()
    df$name <- c()
    df$name <- table.name
    if (i == 1){
      df1 <- df
      
    }else{
    df1 <- rbind(df1, df)
    }
  }
}

df.merged <- merge(df1, hike.info, by.x = "name", by.y = "Mountain")
df.sf <- st_as_sf(df.merged)
df.sf <- st_set_crs(df.sf, "epsg:4326")
df.st.trans <- st_transform(df.sf, "epsg:3035")

st_write(df.st.trans, paste0(hard_drive, ":/EAGLE/Maps/Hike_shps/hikes-with_elevation.gpkg"), driver = "GPKG")

# list of Peaks in the Alps to gpkg

rawdata <- read.csv("F:/EAGLE/Maps/Peaks/Alps_peaks.csv", sep = ",")

rawdata <- rawdata[c("Elevation", "Prom", "Name", "Lat_dec", "Long_dec")]
colnames(rawdata) <- c("Elevation", "Prom", "Name", "Lat", "Lon")
peaks.sf <- st_as_sf(rawdata, coords = c("Lon", "Lat"))
peaks.sf <- st_set_crs(peaks.sf, "epsg:4326")
st_write(peaks.sf, "F:/EAGLE/Maps/Peaks/Alps_peaks.gpkg", driver = "GPKG")

# list of Huts to gpkg
df <- read.xlsx("F:/EAGLE/Maps/Tables/alpine_huts.xlsx", sheetIndex = 1)
huts.sf <- st_as_sf(df, coords = c("Lon", "Lat"))
huts.sf <- st_set_crs(huts.sf, "epsg:4326")
st_write(huts.sf, "F:/EAGLE/Maps/Tables/alpine_huts.gpkg", driver = "GPKG")
