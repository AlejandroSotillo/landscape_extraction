#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~/|\ ^._.^ /|\~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# total hedge length (m) and density (m/m^2) within buffers
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

######################################################################################
#
#                                 REQUIRED FIELDS
#
#
# dt              input data, containing fields "Longitude" and "Latitude" and optionally a "code_dept" field. Can be data.table or sf
#
# id_field        unique row identifier in dt. Give as character. e.g: 'part_nuit'
#
# path_hedge      file path for the "bocage" files
#                 e.g. 'E:/OneDrive/work_conc/data/env/bocage'
#
# path_dept       full file path, name and extension for the france_political layer
#                 e.g. "E:/OneDrive/work_conc/data/political/france_political/france_political_20220728.RData"
#                 
#
# buffer_radius   desired values for buffer radius
#                 e.g. c(500,1000,1500)
#                 e.g. 500


extract_hedge <- function(dt,
                          id_field,
                          path_hedge,
                          path_dept,
                          buffer_radius){
  

  
  # load required packages
  
  lapply(c('data.table','reshape','dplyr','stringr','tools','sf',
           'eRTG3D'),require, character.only = TRUE)
  
  # create spatial object from input dataset
  
  if(is.null(dt$code_dept)){
    
    dtnames=c(id_field,'Longitude','Latitude')
    
  }else{
    
    dtnames=c(id_field,'code_dept','Longitude','Latitude')
    
  }
  
  ##
  
  if(!is.sf.3d(dt)){
    
    dt_sf<-st_as_sf(unique(as.data.table(dt)[,..dtnames]),
                    coords = c('Longitude','Latitude'),remove=F) %>%
      st_set_crs(4326) %>%
      st_transform(crs=2154)
    
  }else{
    
    dt_sf<-dt
    
  }
  
  
  
  # add code_dept in case it is missing from input dataset
  
  load(path_dept)
  
  if(is.null(dt$code_dept)){
    
    dt_sf<-st_join(dt_sf,france_political$dep_L93) }
  
  landscape<-list()
  
  # load information layer and extract data, by departement
  # loads each time the corresponding departement and its neighbors
  # (avoid missing data at points close to departement borders)
  
  for(i in unique(dt_sf$code_dept)){
    
    hedge<-lapply(paste0( path_hedge, '/DNSB-HAIES_1-0__SHP_L93_D0',
                          c(france_political$dep_L93[i,]$code_dept_dd,
                            france_political$dep_L93[unlist(st_touches(france_political$dep_L93[i,],france_political$dep_L93)),]$code_dept_dd),
                          '_2020-06-24/HAIE-LINEAIRE.shp'),
                  st_read)
    
    hedge<-do.call('rbind',hedge)
    
    landscape[[paste('dept',i,sep='_')]]<-list()
    
    for(k in buffer_radius){
      
      buffers <- st_buffer(dt_sf[dt_sf$code_dept==i,],k)
      
      suppressWarnings(
      buffers_intersec <- st_intersection(hedge,buffers)
      )
      
      buffers_intersec$length <-  st_length(buffers_intersec)
      
      landscape[[paste('dept',i,sep='_')]][[paste('buffer',k,sep='_')]]<-as.data.table(
        buffers_intersec)[,.(hedge_length=sum(length),
                             hedge_density=sum(length)/(pi*(k^2))), by=id_field]
      }
    if(length(buffer_radius)>1){
      landscape[[paste('dept',i,sep='_')]]<-rbindlist(landscape[[paste('dept',i,sep='_')]],idcol='buffer_size')
    }else{
      landscape[[paste('dept',i,sep='_')]]<-landscape[[paste('dept',i,sep='_')]][[paste('buffer',k,sep='_')]]
      landscape[[paste('dept',i,sep='_')]]$buffer_size=paste(k)
    }
    }

  # row-bind list
  
  if(length(unique(dt_sf$code_dept))>1){
    dt_product<-rbindlist(landscape,idcol='dept')
  }else{
    dt_product<-landscape[[paste('dept',i,sep='_')]]
    dt_product$dept=paste(unique(dt_sf$code_dept))
  }
  
  return(dt_product)
  
  }
