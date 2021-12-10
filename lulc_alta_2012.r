library(rgdal)
library(raster)
library(caret)
library(snow)
library(doParallel)
library(tictoc)

set.seed(13)

## Carregando dados

ano <- '2012'
resolucao <- 'alta'

dataset <- paste(resolucao,'_',ano,sep='')

dataset_media_b5 <- raster(paste('./',resolucao,'/','media_2014_b5','.tif',sep=''))

imagem <- stack(paste('./',resolucao,'/',dataset,'.tif',sep=''))

dataset_media_b5 <- resample(dataset_media_b5,imagem,method='bilinear')

imagem <- stack(imagem,dataset_media_b5)

treinamento <- shapefile(paste('./',resolucao,'/',dataset,'.shp',sep=''))

## Definindo numero de classes k, atributo classe deve ser preenchido com o 
## nome do atributo contendo a classe no shape

classe <- "class"

k <- length(unique(treinamento[[classe]]))

amostragem = data.frame(matrix(vector(), nrow = 0, ncol = length(names(imagem)) + 1)) 

tic('Extraindo amostras')
# Armazenando amostragem em um dataframe
for (i in 1:k){ # loop para coletar os dados de cada classe
  id <- unique(treinamento[[classe]])[i] # seleciona a id da classe a extrair
  mapa_id <- treinamento[treinamento[[classe]] == id,] # seleciona somente os shapes da categoria a ser extraida
  dados <- extract(imagem, mapa_id) # extrai os dados sobrepostos pelos shapes da categoria selecionada
  
  # remove os dados nulos da amostra
  dados <- dados[!unlist(lapply(dados, is.null))] 
  
  # seta o valor da coluna classe para o id da classe extraída
  dados <- lapply(dados, function(m){cbind(m, classe = as.numeric(rep(id, nrow(m))))}) 
  
  # transforma a lista de amostras para uma unica matriz
  df <- do.call("rbind",dados)
  
  # aloca os vetores da matriz no dataframe amostragem
  amostragem <- rbind(amostragem,df)
}
toc()

## Downsampling da amostragem (opcional, descomentar)
ds_amostragem <- amostragem[sample(1:nrow(amostragem), 0.1 * nrow(amostragem)),]

#ds_amostragem <- amostragem

## Selecionando 80% das amostras para treinamento
indice_amostra <- sample(1:nrow(ds_amostragem), 0.80 * nrow(ds_amostragem))
train_amostragem <- ds_amostragem[indice_amostra,]
test_amostragem <- ds_amostragem[-indice_amostra,]

### Utilizando o pacote randomForest

# importar o pacote RANDOM FOREST
library(randomForest)

formula = as.factor(classe) ~ .

tic('Iniciando treinamento do modelo')
rf <- randomForest(formula,
                   data = train_amostragem,
                   importance=TRUE, 
                   ntree=1000, 
                   do.trace=TRUE,
                   mtry=2)
toc()

# Predizendo pela randomForest

pred_rf <- predict(rf, test_amostragem)

cm <- confusionMatrix(pred_rf,as.factor(test_amostragem$classe))

sink(file = paste('./stats/',dataset,'_stats.txt',sep=''))
cm
sink(file = NULL)

cm$table

# Predizendo a imagem classificada

tic('Iniciando predição da imagem')
beginCluster()
pred <- clusterR(imagem, raster::predict, args = list(model = rf))
endCluster()
toc()

crs(pred) <- '+proj=utm +zone=24 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs '

plot(pred)

tic('Salvando imagem em disco')
raster::writeRaster(pred,paste('./classes/class_',dataset,'.tif',sep=''),overwrite=TRUE)
toc()
