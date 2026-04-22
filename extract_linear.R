#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~/|\ ^._.^ /|\~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# total linear feature length (m) and density (m/m^2) within buffers from RData files
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

######################################################################################
#
#                                 REQUIRED FIELDS
#
#
# dt              input data, containing fields "Longitude" and "Latitude". Can be data.table or sf
#
# id_field        unique row identifier in dt
#
# path_lines      file path for the directory containing the line feature files
#                 these files are simple features stored in RData format
#                 separated and named by departement (Ain.RData, Aisne.RData, Allier.RData, etc.)
#                 e.g. 'E:/OneDrive/work_conc/data/route500/dep/road_rail'
#
# path_dept       full file path, name and extension for the france_political layer
#                 e.g. "E:/OneDrive/work_conc/data/political/france_political/france_political_20221006.RData"
#
# buffer_radius   desired values for buffer radius in meters
#                 e.g. c(500,1000,1500)
#                 e.g. 500


extract_linear <- function(dt,
                           id_field,
                           path_lines,
                           path_dept,
                           buffer_radius,
                           save_sf = F){

  # load required packages
  
  lapply(c('data.table','reshape','dplyr','stringr','sf','eRTG3D'), require, character.only = TRUE)
  
  load(path_dept)
  
  
  # create spatial object from input dataset

  
  if(is.null(dt$departement)){
    
    dtnames = c(id_field, 'Longitude', 'Latitude')
    
  }else{
    
    dtnames = c(id_field, 'departement', 'Longitude', 'Latitude')
    
  }
  
  
  if(is.sf.3d(dt)){
    
    dt_sf <- dt %>% st_transform(crs = st_crs(france_political$dep_L93))
    
  }else{
    
    dt_sf <- st_as_sf(unique(as.data.table(dt)[,..dtnames]), coords = c('Longitude','Latitude'), remove = F) %>%
      st_set_crs(4326) %>% st_transform(crs = st_crs(france_political$dep_L93))
    
  }
  
  
  # add dept in case it is missing from input dataset
  
  if(path_dept != F & is.null(dt$departement)){ dt_sf <- st_join(dt_sf, france_political$dep_L93) }
  
  
  landscape <- list()
  
  # load information layer and extract data, by departement
  # loads each time the corresponding departement and its neighbors
  # (avoid missing data at points close to departement borders)
  

  for(i in unique(dt_sf$dept)){
    
    lines_dt <- new.env()
    
    sapply( paste0( path_lines,'/',
                    c(i,
                      france_political$dep_L93[unlist(st_touches(france_political$dep_L93[france_political$dep_L93$departement==i,],
                                                                 france_political$dep_L93)),]$departement),
                    '.RData'),
            FUN=function(file) {load(file, envir=lines_dt)})
    
    lines_dt <- do.call('rbind',as.list(lines_dt))
  
    for(k in buffer_radius){
      
      buffers <- st_buffer(dt_sf[dt_sf$dept==i,],k)
      
      suppressWarnings(
        buffers_intersec <- st_intersection(lines_dt,buffers)
      )
      
      buffers_intersec$length <-  st_length(buffers_intersec)
      
      if(save_sf == TRUE){ save(buffers_intersec, file = paste0('dep', i, '_', k, 'm', '.RData')) }
      
      landscape[[paste(i)]][[paste('buffer',k,sep='_')]]<-as.data.table(
        buffers_intersec)[,.(feature_length=sum(length),
                             feature_density=sum(length)/(pi*(k^2))), by=id_field]
    }
    if(length(buffer_radius)>1){
      landscape[[paste(i)]]<-rbindlist(landscape[[paste(i)]],idcol='buffer_size')
    }else{
      landscape[[paste(i)]]<-landscape[[paste(i)]][[paste('buffer',k,sep='_')]]
      landscape[[paste(i)]]$buffer_size=paste(k)
    }
  }

  
  # row-bind list
  
  if(length(unique(dt_sf$dept)) > 1){
    dt_product <- rbindlist(landscape, idcol = 'dept')
  }else{
    dt_product <- landscape[[paste(i)]]
    dt_product$dept = paste(unique(dt_sf$dept))
  }
  
  return(dt_product)
  
}

