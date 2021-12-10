library(raster)
library(tictoc)

## Pós processamento 

dataset <- c('alta_2008_nir',
             'alta_2008',
             'alta_2012_nir',
             'alta_2012',
             'alta_2019',
             'baixa_2008',
             'baixa_2015',
             'baixa_2020',
             'media_2009',
             'media_2014',
             'media_2020')

class <- list(
  alta_2008_nir = raster(paste('./classes/class_','alta_2008_nir','.tif',sep='')),
  alta_2008 = raster(paste('./classes/class_','alta_2008','.tif',sep='')),
  alta_2012_nir = raster(paste('./classes/class_','alta_2012_nir','.tif',sep='')),
  alta_2012 = raster(paste('./classes/class_','alta_2012','.tif',sep='')),
  alta_2019 = raster(paste('./classes/class_','alta_2019','.tif',sep='')),
  baixa_2008 = raster(paste('./classes/class_','baixa_2008','.tif',sep='')),
  baixa_2015 = raster(paste('./classes/class_','baixa_2015','.tif',sep='')),
  baixa_2020 = raster(paste('./classes/class_','baixa_2020','.tif',sep='')),
  media_2009 = raster(paste('./classes/class_','media_2009','.tif',sep='')),
  media_2014 = raster(paste('./classes/class_','media_2014','.tif',sep='')),
  media_2020 = raster(paste('./classes/class_','media_2020','.tif',sep=''))
)

color_pallete = c('#009d22','#ffff97','#666666','#377eb8','#f30029')

par(mfrow=c(1,2))

aplicar_filtro <- function (tam_filtro,nome_dataset,raster) {
  tic('Aplicação do filtro')
  cat(nome_dataset)
  
  #beginCluster()
  #b <- clusterR(raster, raster::focal, args = list(
  #  w = tam_filtro,
  #  fun = modal
  #)
  #)
  #endCluster()
  
  b <- focal(x = raster,
             w = tam_filtro,
             fun = modal)
  
  toc()
  
  crs(b) <- '+proj=utm +zone=24 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '
  
  raster::writeRaster(b,paste('./classes/post_processed/class_',nome_dataset,'_modal.tif',sep=''), overwrite = TRUE)
  
  plot(raster,
       col=color_pallete, 
       legend=FALSE,
       main=nome_dataset,
       yaxt='n',
       xaxt='n',
       ann=FALSE)
  
  plot(b,
       col=color_pallete, 
       legend=FALSE,
       main=nome_dataset,
       yaxt='n',
       xaxt='n',
       ann=FALSE)
}

# Filtragem da alta resolucao
for (i in 1:5) {
  
  aplicar_filtro(matrix(1,5,5),dataset[[i]],class[[i]])
  
}

# Filtragem da baixa resolucao
for (i in 6:9) {
  
  aplicar_filtro(matrix(1,3,3),dataset[[i]],class[[i]])
  
}

# Filtragem da media resolucao
for (i in 9:11) {
  
  aplicar_filtro(matrix(1,3,3),dataset[[i]],class[[i]])
  
}
