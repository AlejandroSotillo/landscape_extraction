
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~/|\ ^._.^ /|\~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# extract_topo
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~❀𖤣𖥧𖡼⊱✿⊰𖡼𖥧𖤣❀~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#####################################################################################
#
#                                 REQUIRED FIELDS
#
#
# dt              input data, containing fields "Longitude", "Latitude" and a unique row identifier
#
# id_field        name of the unique row identifier in dt
#
# path_dat        file path for the extracted BDTOPO folders, make sure there are no .zip folders left here
#                 e.g. 'D:/OneDrive/work_conc/data/cesbio/poly'
#
# path_dept       full file path, name and extension for the france_political layer
#                 e.g. "D:/OneDrive/work_conc/data/political/france_political/france_political_20220728.RData"
#
# category_broad  as organized within the topo folder (\BDTOPO_3-3_TOUSTHEMES_SHP_LAMB93_D001_2023-06-15\BDTOPO\1_DONNEES_LIVRAISON_2023-06-00117\BDT_3-3_SHP_LAMB93_D001-ED2023-06-15)
#                 possible values: ADMINISTRATIF BATI HYDROGRAPHIE LIEUX_NOMMES OCCUPATION_DU_SOL SERVICES_ET_ACTIVITES TRANSPORT ZONES_REGLEMENTEES
#
# category_detail as organized within the category_broad folder
#                 possible values within OCCUPATION_DU_SOL: ESTRAN HAIE ZONE_DE_VEGETATION
#
#



extract_bdtopo <- function(dt,
                           id_field,
                           path_dat,
                           path_dept,
                           category_broad,
                           category_detail){
  
  # load required packages
  
  lapply(c('data.table',
           'dplyr',
           'sf',
           'eRTG3D',
           'stringi'),
         require, character.only = TRUE)
  
  
  # recognize alternative grammar for coordinate variables
  
  dt <- as.data.table(dt)
  
  if(is.null(dt$Longitude)){
    if(!is.null(dt$longitude)&!is.null(dt$latitude)){
      dt[, `:=` (Longitude = longitude, Latitude = latitude)]
    }else if (!is.null(dt$lon)&!is.null(dt$lat)){
      dt[, `:=` (Longitude = lon, Latitude = lat)]
    }
  }
  
  
  
  # create spatial object from input dataset
  
  
  if(!is.sf.3d(dt)){
    
    dt_sf<-st_as_sf(dt,coords = c('Longitude','Latitude'), remove = F) %>%
      st_set_crs(4326) %>%
      st_transform(crs=2154)
    
  }else{
    
    dt_sf<-dt %>%
      st_transform(crs=2154)
    
  }
  
  
  
  # add code_dept in case it is missing from input dataset
  
  if(path_dept != F & is.null(dt_sf$code_dept)){
    
    load(path_dept)
    
    dt_sf<-st_join(dt_sf,france_political$dep_L93) }
  
  
  # extraction
  
  
  thedistances <- list()
  
  for(i in unique(dt_sf$code_dept)){
    
    # get the topo file
    
    thedepts = as.numeric(c(france_political$dep_L93[i,]$code_dept,
                            france_political$dep_L93[unlist(st_touches(france_political$dep_L93[i,],france_political$dep_L93)),]$code_dept))
    
    thepattern = paste(paste0('D',formatC(thedepts, width = 3, format = "d", flag = "0")),collapse = '|')  
    
    if(stri_sub(path_dat,-1,-1) == '/'){
      path1 = paste0(path_dat, list.files(path_dat, thepattern ),'/BDTOPO/')
    }else{
      path1 = paste(path_dat, list.files(path_dat, thepattern ),'BDTOPO/',sep='/')  
      }
  
    path2 = paste0(path1, list.files(path1, pattern = '1_DONNEES_LIVRAISON'))
    
    path3 = paste(path2, grep(list.files(path2), pattern = 'md5', invert = T, value = T),category_broad,category_detail, sep = '/')
    
    topofile = paste0(path3,'.shp')
    
    polys <- lapply(topofile, st_read)
    
    polys <- do.call('rbind',polys)

    
    thedistances[[paste0('dep_',i)]] <- cbind(
      as.data.table(dt_sf[dt_sf$code_dept == i,])[,..id_field],
      distance = st_distance(dt_sf[dt_sf$code_dept == i,], polys[st_nearest_feature(dt_sf[dt_sf$code_dept == i,],polys),], by_element = T))
  
  }
  
  out <- rbindlist(thedistances, idcol = 'dep')
  
  return(out)
  
  }


