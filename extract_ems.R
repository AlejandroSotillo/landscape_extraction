
###############
# eurometropole
###############


library(sf)
library(stringr)
library(mapview)


odata3d <- st_read('D:/OneDrive/work_stras/dat/shp/odata3d_maquette/odata3d_maquette/odata3d_maquette.shp')


#
# trees
#

urls <- odata3d$arbres_lod1.2[!is.na(odata3d$arbres_lod1.2)]


# download

for(i in 1:length(urls)){
  download.file(urls[i], paste0("D:/OneDrive/work_stras/dat/shp/arbres_lod1_2/zip/", word(urls[i], -1L, sep = "\\/")))
  rm(i) }

rm(urls)


# unzip

lapply(list.files('D:/OneDrive/work_stras/dat/shp/arbres_lod1_2/zip/', full.names = T), FUN = function(x){
  unzip(x, exdir = 'D:/OneDrive/work_stras/dat/shp/arbres_lod1_2') })


# import

tree <- lapply(list.files('D:/OneDrive/work_stras/dat/shp/arbres_lod1_2/', pattern = 'shp', full.names = T), FUN = st_read)

names(tree) = paste0('t',word(list.files('D:/OneDrive/work_stras/dat/shp/arbres_lod1_2/', pattern = 'shp'), 1, sep = "\\."))


# plot

mapviewOptions(fgb = FALSE)

mapview(odata3d)

mapview(tree[paste0('t','22_23')])


