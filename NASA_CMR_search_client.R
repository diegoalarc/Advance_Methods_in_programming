## An R client for the NASA CMR EarthData API
## This code is a prototype developed for the
## getSpatialData package and the EAGLE MET2 course.
## Please do not distribute without referencing the getSpatialData project.

library(httr)
library(sys)
library(xml2)
library(getPass)
library(magrittr)
library(sf)
library(geosphere)
library(raster)
# RStoolbox, ggplot2, mapview, mapedit

# NASA CMR earth data url
url <- "https://cmr.earthdata.nasa.gov/search/"

# search for SRTM collections
url.query <- paste0(url, "collections?keyword=Shuttle%20Radar%20Topography%20Mission")

# semd request
tryCatch({
  response <- GET(url.query) %>% content()
  response <- response$feed$entry
  response_colls <- sapply(response, function(x) x[["title"]])
})

# match collections including the keyword "1 arc"
coll <- response[[grep("1 arc second V003", response)]]
names(coll)

# define a spatial extent to query for
aoi <- mapedit::drawFeatures()

# convert to matrix
aoi.matrix <- aoi$geometry[[1]][[1]]
aoi.matrix <- aoi.matrix[!duplicated(aoi.matrix),]

# sort coordinates counter-clockwise
aoi.center <- st_centroid(aoi$geometry)
cc.angles <- bearing(aoi.matrix, aoi.center[[1]][1:2])
aoi.matrix <- aoi.matrix[rev(order(cc.angles)),]

# start with most western coordinate
cc.first <- which.min(aoi.matrix[,1])
if(cc.first > 1){
  aoi.matrix <- aoi.matrix[c(cc.first:nrow(aoi.matrix), 1:(cc.first-1)),]
}
aoi.matrix <- rbind(aoi.matrix, aoi.matrix[1,])

# assemble json query string and add further query keywords
query.json <- paste0("polygon=",
                     paste0(apply(aoi.matrix, MARGIN=1, function(x) paste0(x, collapse = "%2C")), collapse = "%2C"))
query.coll <- paste0("echo_collection_id=", coll$id)
query.sort <- "sort_key%5B%5D=-start_date&page_size=20"
query.url <- paste0(url, "granules.json?", query.json, "&", query.coll, "&", query.sort)

# query API for defined product
tryCatch({
  response <- GET(query.url) %>% content()
  response <- response$feed$entry
})

# number of avialable datasets
length(response)

# build a records data.frame containing all returned records
fields <- names(response[[1]])
records <- do.call(rbind, lapply(response, function(x){
  links <- grep("http", unlist(x$links), value = T)
  names(links) <- NULL
  
  y <- data.frame(x[fields != "links"], stringsAsFactors = F)
  y$links <- list(links)
  colnames(y) <- fields
  
  y$download_url <- links[grep("hgt.zip$", links)]
  return(y)
}))

View(records)

# create file names and attempt download
records$file <- paste0("/home/jas24ny/Downloads/", records$producer_granule_id)

response <- mapply(x = records$download_url, y = records$file, function(x, y){
  GET(x, write_disk(y, overwrite = T), progress())
}, SIMPLIFY = F) # will fail
http_status(response[[1]])
# download fails sicne API requires us to authenticate

# CMR as many other NASA APIs use the URS authentication service
# based on OAUTH
# for this, we need a netrc authentification file
file.netrc <- file.path(Sys.getenv("HOME"),'.netrc', fsep = .Platform$file.sep)
con.netrc <- file(file.netrc)

# .netrc file is filled with your credentials
writeLines(c("machine urs.earthdata.nasa.gov",
             sprintf("login %s", getPass(msg = "Enter URS username:")),
             sprintf("password %s", getPass(msg = "Enter URS password:"))), con.netrc)
close(con.netrc)

# repeat the query, now using the authentificaton file and allowing a sesson cookie
response <- mapply(x = records$download_url, y = records$file, function(x, y){
  GET(x, write_disk(y, overwrite = T),
  progress(), config(netrc = T, netrc_file = file.netrc), set_cookies("LC" = "cookies"))
}, SIMPLIFY = F)
http_status(response[[1]])

# unzip all files
records$file_unzipped <- unlist(lapply(records$file, function(x){
  unzip(x, exdir = paste0(head(strsplit(x, "/")[[1]], n=-1), collapse = "/"))
}))

# load all files
r <- lapply(records$file_unzipped, raster)

# some messy viz with ggplot2
RStoolbox::ggR(r[[1]], coord_equal = F, geom_raster = T) + 
  ggplot2::coord_sf(crs = st_crs(r[[1]]), datum = st_crs(r[[1]])) +
  viridis::scale_fill_viridis(name = "Elevation [m]", option = "inferno")

# and with mapview
mapview::mapview(r[[1]])

# get to know all possibilties of the CMR API here: https://cmr.earthdata.nasa.gov/search/site/docs/search/api.html
# adapt the code for: other query parameters, other products, time filtering etc.