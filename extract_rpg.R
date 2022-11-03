#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~/|\ ^._.^ /|\~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#
# extracts from RPG polygon files:
# total area, median area, mean area, total N parcels, shannon index in crop types
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

######################################################################################
#
#                                 REQUIRED FIELDS
#
#
# dt              input data, containing fields "Longitude" and "Latitude"
#
# id_field        unique row identifier in dt
#
# path_dat        file path for the RPG files
#                 e.g. 'E:/OneDrive/work_conc/data/env/rpg/dep_polys'
#
# path_dept       full file path, name and extension for the france_political layer
#                 e.g. "E:/OneDrive/work_conc/data/political/france_political_20220728.RData"
#
# buffer_radius   desired values for buffer radius in meters
#                 e.g. c(500,1000,1500)
#                 e.g. 500

extract_rpg <- function(dt,
                        id_field,
                        path_dat,
                        path_dept,
                        buffer_radius){

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

    dt_sf<-dt

    }
  
# add code_dept in case it is missing from input dataset



if(path_dept != F){
  load(path_dept)
  dt_sf<-st_join(dt_sf,france_political$dep_L93) }

landscape<-list()

# load information layer and extract data, by departement
# loads each time the corresponding departement and its neighbors
# to avoid missing data at points close to departement borders


for(i in unique(dt_sf$code_dept)){

  paths<-as.list(paste0(
    path_dat,'/', 'dep_',
    c(france_political$dep_L93[i,]$code_dept,
      france_political$dep_L93[unlist(st_touches(france_political$dep_L93[i,],france_political$dep_L93)),]$code_dept),
    '.RData'))
  
  polys<-list()
  
  for(j in 1:length(paths)){
    load(paths[[j]])  
    polys[[j]]<-dep 
  }
  
  polys<-do.call('rbind',polys)

  polys<-polys[polys$arable==TRUE,][,c(1,14)]

  landscape[[paste('dept',i,sep='_')]]<-list()
  
  for(k in buffer_radius){
    
    buffers <- st_buffer(dt_sf[dt_sf$code_dept==i,],k)
    
    suppressWarnings(
      buffers_intersec <- st_intersection(polys,buffers)
    )
    
    # total, median and mean area of arable rpg parcels
    
    if(nrow(buffers_intersec)>0){
      buffers_intersec$area<- st_area(buffers_intersec)
      
      landscape[[paste('dept',i,sep='_')]][[paste('buffer',k,sep='_')]]<-as.data.table(
        buffers_intersec)[,.(rpg_area_total=sum(area),
                             rpg_area_median=median(area),
                             rpg_area_mean=mean(area),
                             rpg_n_parcels=.N), by=id_field]
  
      # shannon's diversity index among parcels within buffer
      
      shannon_rpg<-dcast(
        as.data.table(buffers_intersec),get(id_field)  ~ CODE_CULTU,
        value.var = "area", fun.aggregate = sum)
      
      shannon_rpg[,sumarea:=rowSums(.SD),
                  .SDcols = 2:ncol(shannon_rpg)]
      
      for(l in names(shannon_rpg)[-ncol(shannon_rpg)][-1]){
        shannon_rpg[,paste('calc',l,sep='_'):=ifelse(
          as.numeric(get(l))>0,
          (as.numeric(get(l)/sumarea))*log(as.numeric(get(l)/sumarea)),0)]
      }
      
      shannon_rpg[,shannon := -rowSums(.SD),
                  .SDcols = grep('calc', names(shannon_rpg))]
      
      landscape[[paste('dept',i,sep='_')]][[paste('buffer',k,sep='_')]][
        ,rpg_shannon :=shannon_rpg$shannon]
      
    }}
  landscape[[paste('dept',i,sep='_')]]<-rbindlist(landscape[[paste('dept',i,sep='_')]],idcol='buffer_size')
    }


# row-bind list

dt_product<-rbindlist(landscape,idcol='dept')

return(dt_product)

}
