
#
# Extract wind turbine coordinates from the openstreetmap database openstreetmap.org
#


# Arguments
#
# country : geographical territory for which to extract the data (e.g. 'France', 'Alsace', 'Bas-Rhin').
# creation_year : obtain the year when the WT feature was created? If TRUE, will proceed to time-consuming web scrapping
# dist_landuse : name of OSM land use categories to calculate the shortest distance to. https://wiki.openstreetmap.org/wiki/Land_use

# Value
#
# osm_id : openstreetmap feature identifier
# origin : original feature type (point or polygon)
# in_country : logical. Indicates whether the wind turbine is within the borders of the specified "country"
# dist_border : distance to the borders of the specified "country"
# first_record : date when the feature was created in the database
# geometry : georeferenced object data
# year : year when the feature was created in the database
# dist_ : distance to the specified land use category, if dist_landuse != NULL

osm_wt <- function(country = 'France métropolitaine', creation_year = F , dist_landuse = NULL){ 
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load('data.table','splitstackshape','stringr',
                 'sf','osmdata','rvest')
  
  
  # positions of turbines
  
  
  osm_sf <- getbb (country, featuretype = 'land') %>%
    opq (timeout = 600) %>%
    add_osm_feature("generator:source", "wind") %>%
    osmdata_sf (quiet = FALSE)
  
  osm_sf$osm_points$origin = "point"
  
  
  # convert positions recorded as polygons into points, representing the centroid of each polygon
  
  if(nrow(osm_sf$osm_polygons) > 0){
  
  find_poly_points <- st_intersects(osm_sf$osm_points, osm_sf$osm_polygons) # find WTs mapped both as point and polygon
  
  osm_sf$osm_points$poly_points <- sapply(find_poly_points, FUN = length) > 0
  
  poly_centroids <- st_centroid(osm_sf$osm_polygons)
  
  poly_centroids$origin = "polygon" # required to find the date of creation of the feature
  
  osm_clean <- rbind( osm_sf$osm_points[osm_sf$osm_points$poly_points == F, c('osm_id', 'origin', 'geometry')],
                      poly_centroids[,c('osm_id', 'origin', 'geometry')]) # osm_id to later source other info
  
  }else{
    
    osm_clean <- osm_sf$osm_points[, c('osm_id', 'origin', 'geometry')]
  
    }
  
  # in_country field indicates whether the position falls inside the country or not
    
  thecountry <- opq(bbox = country) %>%
    add_osm_feature(key = 'name', value = country) %>% 
    osmdata_sf (quiet = FALSE)
  
  osm_clean$in_country <-  sapply(st_intersects(osm_clean, thecountry$osm_multipolygons), FUN = function(x) length(x) > 0)
  
  osm_clean$dist_border <- st_distance(osm_clean, thecountry$osm_lines[st_nearest_feature(osm_clean, thecountry$osm_lines),], by_element = T)
  
  
  # creation_year
  
  if(creation_year == T){
    
    history_urls_point <- paste('https://www.openstreetmap.org/api/0.6/node/', osm_clean[osm_clean$origin == 'point',]$osm_id, '/history', sep = '')
    
    n_wt = length(history_urls_point)
  
  if(nrow(osm_sf$osm_polygons)>0){
    
    history_urls_poly <- paste('https://www.openstreetmap.org/api/0.6/way/', osm_clean[osm_clean$origin == 'polygon',]$osm_id, '/history',sep = '')
    
    n_wt = n_wt + length(history_urls_point)
  }
  
#  if (menu(c("Yes", "No"), title = paste(n_wt, "wind turbines found, proceed with web scraping to obtain creation dates?")) == 1){
    
    print("Scraping. This may take a while.")
    
    history_pages_point <- lapply(history_urls_point, FUN = read_html)
    
    history_oldest_point <- lapply(history_pages_point, FUN = function(x){ as.data.table(as.character(html_elements(x, "node")[1])) })
    
    history_collapsed_point <- rbindlist(history_oldest_point, fill = TRUE, use.names = TRUE)
    
    history_point <- cSplit(indt = history_collapsed_point, splitCols = 'V1', sep = " ")[, c(2, 6)]
    
    # osm_id & first_record
    
    history_point[, `:=` (osm_id = gsub(".*?([0-9]+).*", "\\1", V1_02),
                          first_record = apply(do.call('rbind',str_extract_all(V1_06, "\\d+"))[,1:3], 1 , paste , collapse = "/" ))]
    
    if(nrow(osm_sf$osm_polygons)>0){
      
      history_pages_poly <- lapply(history_urls_poly, FUN = read_html)
      
      history_oldest_poly <- lapply(history_pages_poly, FUN = function(x){ as.data.table(as.character(html_elements(x, "way")[1]))})
      
      history_collapsed_poly <- rbindlist(history_oldest_poly, fill = T, use.names = T)
      
      history_poly <- cSplit(indt = history_collapsed_poly, splitCols = 'V1', sep = " ")[, c(2, 6)]
      
      history_poly[, `:=` (osm_id = gsub(".*?([0-9]+).*", "\\1", V1_002),
                           first_record = apply( do.call('rbind', str_extract_all(V1_006, "\\d+"))[,1:3] , 1 , paste , collapse = "/" ))]

      history <- rbind(history_point[, c('osm_id','first_record')],
                       history_poly[, c('osm_id','first_record')])
    }else{
      history <- history_point[, c('osm_id','first_record')]
    }
    
    osm_clean <- merge(x = osm_clean,
                       y = history,
                       by = 'osm_id', all.x = T)
    
    osm_clean$year <- format(as.Date(osm_clean$first_record,format="%Y/%m/%d"), "%Y") # extracts the year
#  } 
  }
  
  
  # shortest distance to OSM land use categories
  
  
  if(!is.null(dist_landuse)){
  
    osm_categories <- list()
    
    for(i in dist_landuse){
      
      osm_categories[[i]] <- getbb (country, featuretype = 'land') %>%
        opq (timeout = 600) %>%
        add_osm_feature("landuse",i) %>%
        osmdata_sf (quiet = FALSE)
      
      osm_categories[[i]] <- rbind(osm_categories[[i]]$osm_polygons[, c('osm_id','geometry')], osm_categories[[i]]$osm_multipolygons[, c('osm_id','geometry')])
      
      osm_clean[, paste0('dist_',i)] = st_distance(osm_clean, osm_categories[[i]][st_nearest_feature(osm_clean, st_make_valid(osm_categories[[i]])),], by_element = T)
      
      }
    }
  
  return(osm_clean)
  
  }





