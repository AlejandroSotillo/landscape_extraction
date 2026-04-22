
get_bd_topo <- function(thedate = '2023-06-15', thepath, thedepts){
  
  require(downloader)
  require(archive)
  
  options(timeout = max(9000, getOption("timeout")))
  
  depnums <- paste0('D',formatC(thedepts, width = 3, format = "d", flag = "0"))
  
  depnums_date <- paste(depnums, thedate, sep = '_')
  
  theurls = paste0(
    'https://wxs.ign.fr/859x8t863h6a09o9o6fy4v60/telechargement/prepackage/BDTOPOV3-TOUSTHEMES-DEPARTEMENT-PACK_232$BDTOPO_3-3_TOUSTHEMES_SHP_LAMB93_',
    depnums_date, '/file/BDTOPO_3-3_TOUSTHEMES_SHP_LAMB93_', depnums_date, '.7z')
  
  # download
  
  for(i in 1:length(depnums)){
    download(theurls[i], dest = paste0(thepath,depnums[i],".7zip"), mode="wb")
    archive_extract(archive = paste0(thepath,depnums[i],".7zip"), dir = thepath)
  }
  
}
