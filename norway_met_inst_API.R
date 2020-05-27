# required packages for our client
library(httr)
library(xml2)
library(magrittr)
library(pbapply)
library(magick)

# base url
url_api <- "https://api.met.no/weatherapi"


## Part 1: Extracting the products and API version by parsing the overview page's HTML

# check what we get back from there
reply <- GET(url_api)
http_status(reply)

# extract info from body
reply_content <- content(reply)
class(reply_content)
View(reply_content)

# crawl for table
xml_prod <- xml_find_first(reply_content, "//table")

# extract the head of the table
prod_head <- xml_children(xml_prod)[[1]] %>%
  xml_contents() %>%
  xml_children() %>%
  xml_text()

# extract the body of the table
prod_body <- xml_children(xml_prod)[[2]] %>% 
  xml_children() %>%
  
  lapply(function(x){
    x <- xml_contents(x)
    xml_text(x)[seq(1, length(x), by = 2)]
  })

# create products data.frame
products <- do.call(rbind.data.frame, prod_body)
colnames(products) <- prod_head
View(products)


## Part 2: communicate with one of the products APIs
prod <- products[products$Product == "Radar",]
# to be continued

# assemble the request URL and doing the request
url.prod <- paste0(url_api, "/", tolower(prod$Product), "/", prod$Version, "/")
avail.req <- paste0(url.prod, "available?area=southeastern_norway&type=accumulated_06h")
avail.resp <- GET(avail.req) %>% content()

# extract the names of the nodes
prod.head <- avail.resp %>% xml_children() %>% '['(1) %>% xml_contents()
prod.head <- head(prod.head, n=-1) %>%  as.character()
prod.head <- c(sapply(prod.head, function(x) gsub("</", "", strsplit(x, "name>")[[1]][2]), USE.NAMES = F), "url")

# extract the values of the nodes
prod.body <- avail.resp %>% xml_children() %>% 
  
  lapply(function(x){
    x <- xml_children(x)
    c(sapply(head(x, n=-1), function(y) xml_children(y) %>% '['(2) %>% xml_text()),
      tail(x, n=1) %>% xml_text())
  })

# assemble a records data.frame
records <- do.call(rbind.data.frame, prod.body)
colnames(records) <- prod.head
View(records)
records <- records[records$content == "image",]

# get all maps from our request
dir <- paste0(tempdir(), "/API_met")
dir.create(dir)
records$file <- paste0(dir, "/map_", 1:nrow(records), ".png")

# check for URLs that do not work properly (server issues?)
working <- !sapply(records$url, http_error, USE.NAMES = F)
records <- records[working,]

# download and read images
maps <- pbmapply(x = records$url, y = records$file, function(x, y){
  catch <- GET(x, write_disk(y, overwrite = T))
  image_read(y)
})

# combine images into GIF
maps <- do.call(c, maps)
image_write_gif(maps, "/home/jas24ny/Downloads/radar.gif", delay = 0.25)
