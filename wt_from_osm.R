

lapply(c(
  'data.table','splitstackshape',
  'sf','osmdata','rgdal','rvest',
  'mapview'),
  require, character.only = TRUE)

##################################
# WT locations within bounding box
##################################

# load('wt_raw.RData') # loads the example raw dataset dowloaded on 10/01/2022
# load('wt.RData')     # loads the example clean dataset made from wt_raw

wt_raw <- getbb ("Metropolitan France",featuretype='land') %>%
  opq (timeout = 600) %>%
  add_osm_feature("generator:source", "wind") %>% # queries are built adding osm_features, specified in terms of key-value pairs
  osmdata_sf (quiet = FALSE)

# mapview(wt_raw$osm_lines)         # nothing interesting
# mapview(wt_raw$osm_multilines)    # empty
# mapview(wt_raw$osm_multipolygons) # nothing interesting

mapview(wt_raw$osm_polygons)+   # some WTs are recorded as poly's instead of points
  mapview(wt_raw$osm_points)    # includes the vertices of osm_polygons 
   

# identify points corresponding to WTs mapped as poly's and replace them by their centroids

find_poly_points<-st_intersects(wt_raw$osm_points,wt_raw$osm_polygons) 

wt_raw$osm_points$poly_points<-as.logical(do.call(rbind,lapply(find_poly_points,FUN=length)))

poly_centroids<-st_centroid(wt_raw$osm_polygons) 

poly_centroids$element_type=    'way'  # required to find the date of creation of the feature
wt_raw$osm_points$element_type= 'node'

wt<-rbind(
  wt_raw$osm_points[wt_raw$osm_points$poly_points==F,c('osm_id','element_type','geometry')],
  poly_centroids[,c('osm_id','element_type','geometry')]) # osm_id in case we want to later source other info from wt_raw


# add administrative divisions

france <- st_read('france_political') # custom-made dataset transformed from IGN data

wt <- st_join(wt,france, join = st_within)

wt$in_france<-!is.na(wt$code_dept) # offshore WTs are in_france == F

rm(find_poly_points,poly_centroids,france)


##############################
# obtain feature creation date
##############################

# scrap the date of the oldest edit from the feature's history page 
# ugly and slow, find alternative. parallel threads?

history_urls<-paste('https://www.openstreetmap.org/',
                    wt$element_type,
                    '/',
                    wt$osm_id,
                    '/history',sep='')

history_pages<-lapply(history_urls,FUN=read_html)

history_oldest<-lapply(history_pages,FUN=function(x){
  as.character(html_nodes(x,"abbr")[length(html_nodes(x,"abbr"))])})

history_collapsed<-data.table(do.call(rbind,history_oldest))

history<-cSplit(indt=history_collapsed,splitCols='V1',sep=" ")[,c(3,4,5)]

history[,first_record:=as.Date(paste(V1_03,V1_04,V1_05,sep = '/'),format='%d/%b/%Y')]

wt$first_record  <-history$first_record

wt$first_year  <-history$V1_05


mapview(wt,zcol='first_year')


rm(list = ls(pattern = "^history"))


