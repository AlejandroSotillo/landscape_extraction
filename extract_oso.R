#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~/|\ ^._.^ /|\~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# extracts from OSO polygon files:
# total area, median area, mean area, total N parcels, shannon index in area types
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#####################################################################################
#
#                                 REQUIRED FIELDS
#
#
# dt              input data, containing fields "Longitude", "Latitude" and a unique row identifier
#
# id_field        name of the unique row identifier in dt
#
# path_dat        file path for the OSO shapefiles
#                 e.g. 'E:/OneDrive/work_conc/data/cesbio/poly'
#
# path_dept       full file path, name and extension for the france_political layer
#                 e.g. 'E:/OneDrive/work_conc/data/political/france_political_20220728.RData'
# 
#
# buffer_radius   desired values for buffer radius in meters
#                 e.g. c(500,1000,1500)
#                 e.g. 500
#
# path_key        full file path, name and extension for the osokey dataset
#                 e.g. 'E:/OneDrive/work_conc/data/cesbio/osokey.csv'
# 
# key             area-classifying key to use. Possible options:
#                 "type","category","category_broad","type_fr","pf_code"
#
# key_value       subset of categories of the classifying key to base calculations on
#                 default key_value = 'all' calculates for all categories


# Source of shapefiles:
# https://www.theia-land.fr/en/ceslist/land-cover-sec/
# https://www.theia-land.fr/en/product/land-cover-map/


extract_oso <- function(dt,
                        id_field,
                        path_dat,
                        path_dept,
                        buffer_radius,
                        path_key,
                        key,
                        key_value){

# load required packages

lapply(c('data.table',
         'dplyr',
         'sf',
         'eRTG3D'),
       require, character.only = TRUE)



# create spatial object from input dataset


if(!is.sf.3d(dt)){
  
  dt_sf<-st_as_sf(dt,coords = c('Longitude','Latitude'),remove=F) %>%
    st_set_crs(4326) %>%
    st_transform(crs=2154)
  
}else{
  
  dt_sf<-dt %>%
    st_transform(crs=2154)
  
}

# add code_dept in case it is missing from input dataset



if(path_dept != F){
  
  load(path_dept)
  
  dt_sf<-st_join(dt_sf,france_political$dep_L93) }


# load osokey

osokey<-fread(path_key)

if(any(key_value != 'all')){
  osokey <- osokey[get(key) %in% key_value]
}


# load oso layers and extract data, by departement
# loads the corresponding departement and its neighbors
# to avoid missing data at points close to departement borders



landscape<-list()
length(landscape)<-length(key_value)
names(landscape)<-key_value


for(i in unique(dt_sf$code_dept)){
  
  paths<-as.list(paste0(
    path_dat,'/','departement_',
    c(i,
      france_political$dep[unlist(st_touches(france_political$dep[france_political$dep$code_dept==i,],
                                             france_political$dep)),]$code_dept),
    '.shp'))
  
  
  oso <- lapply(paths,FUN= function(x) {merge(x = st_read(x)[,c('Classe','geometry')],
                                              y = osokey[,c('Classe',key),with = F],
                                              by = 'Classe', all.x = F, all.y = F)})
  
  polys<-list()
  
  for(j in unique(as.data.frame(oso[[1]])[,2])){
  
    polys[[j]] <- do.call('rbind', lapply(oso, FUN = function(x){
      subset(oso[[1]], subset = as.data.frame(oso[[1]])[,2] == j)}))
    
    landscape[[j]][[paste('dep',i,sep='_')]]  <-list()
    
    }
  

  lines<-lapply(polys,FUN=function(x){ st_cast(x,"MULTILINESTRING")})
  
  
  for(j in buffer_radius){
    
    buffers <- st_buffer(dt_sf[dt_sf$code_dept==i,],j)[,c(id_field,'geometry')]
    
    buffers_intersec_poly <- vector(mode='list',length = length(landscape))
    buffers_intersec_line <- vector(mode='list',length = length(landscape))
    
    names(buffers_intersec_poly)<-names(landscape)
    names(buffers_intersec_line)<-names(landscape)
    
    buffers_intersec_poly <- suppressWarnings(lapply(polys, FUN = function(x){
      st_intersection(x, buffers)}))
    buffers_intersec_line <- suppressWarnings(lapply(lines, FUN = function(x){
      st_intersection(x, buffers)}))
    
    for(k in names(landscape)){ if (nrow(buffers_intersec_line[[k]])>0){
      
      buffers_intersec_poly[[k]]$area   <- st_area(buffers_intersec_poly[[k]])
      
      buffers_intersec_line[[k]]$length <- st_length(buffers_intersec_line[[k]])
      
      landscape[[k]][[paste('dep',i,sep='_')]][[paste('buffer',j,sep='_')]] <- merge(
          x = aggregate(area ~ as.data.frame(buffers_intersec_poly[[k]])[,id_field],
                      data = buffers_intersec_poly[[k]], FUN = sum),
          y = aggregate(length ~ as.data.frame(buffers_intersec_line[[k]])[,id_field],
                        data = buffers_intersec_line[[k]], FUN = sum),
          by.x = 'as.data.frame(buffers_intersec_poly[[k]])[, id_field]',
          by.y = 'as.data.frame(buffers_intersec_line[[k]])[, id_field]',
          all.x = T, all.y = T) 
    }}
  }
  
  }

out<-list()

for(i in names(landscape)){
  
  out[[i]]<-rbindlist(
    lapply(landscape[[i]], FUN = function(x){ rbindlist(x, idcol = 'buffer')}),
    idcol = 'departement')
  
  } 

out <- rbindlist(out, idcol = key)   

setnames(out, old = 'as.data.frame(buffers_intersec_poly[[k]])[, id_field]', new = id_field)

return(out)


}