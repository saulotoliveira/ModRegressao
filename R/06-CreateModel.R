# Cria um modelo preditivo usando randomForest

# Este código foi criado para executar tanto no Azure, quanto no RStudio.
# Para executar no Azure, altere o valor da variavel Azure para TRUE. 
# Se o valor for FALSE, o codigo será executado no RStudio

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
# setwd("C:/FCD/BigDataRAzure/Cap14/Projeto")
# getwd()

# Função para tratar as datas
set.asPOSIXct <- function(inFrame) { 
  dteday <- as.POSIXct(
    as.integer(inFrame$dteday), 
    origin = "1970-01-01")
  
  as.POSIXct(strptime(
    paste(as.character(dteday), 
          " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S"))
}

char.toPOSIXct <-   function(inFrame) {
  as.POSIXct(strptime(
    paste(inFrame$dteday, " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S")) }


# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  dataset$dteday <- set.asPOSIXct(dataset)
}else{
  bikes <- bikes
}

require(randomForest)
model <- randomForest(cnt ~ xformWorkHr + dteday + temp + hum, 
                      data = bikes, # altere o nome do objeto data para "dataset" de estiver trabalhando no Azure ML
                      ntree = 40, 
                      nodesize = 5)
print(model)
