# Análise de Série Temporal
# Será utilizado quando os dados estiverem distribuídos ao longo de períodos de datas: em minutos, horas, dias, semanas, anos....

# Este código contém comandos para análise de série temporal

# Este código foi criado para executar tanto no Azure, quanto no RStudio.
# Para executar no Azure, altere o valor da variavel Azure para TRUE. 
# Se o valor for FALSE, o codigo será executado no RStudio

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/Users/saulo/Documents/Cursos/DSA/FCD/BigDataRAzure/Cap14/Projeto") # Define o diretório de trabalho.
getwd() # Exibe o diretório de trabalho.

# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  source("src/Tools.R")
  Bikes <- maml.mapInputPort(1)
  Bikes$dteday <- set.asPOSIXct(Bikes)
}else{
  bikes <- bikes
}

# Avaliando a demanda por aluguel de bikes ao longo do tempo
# Construindo um time series plot para alguns determinados horários 
# em dias úteis e dias de fim de semana.
times <- c(7, 9, 12, 15, 18, 20, 22) # criando um vetor que representa algumas horas do dia para verificar qual o período de maior demanda de alugel de bikes.

# Time Series Plot
tms.plot <- function(times){
  ggplot(bikes[bikes$workTime == times, ], aes(x = dteday, y = cnt)) + 
    geom_line() +
    ylab("Numero de Bikes") +
    labs(title = paste("Demanda de Bikes as ", as.character(times), ":00", sep = "")) +
    theme(text = element_text(size = 20))
}

require(ggplot2)
lapply(times, tms.plot)

# Gera saida no Azure ML
if(Azure) maml.mapOutputPort('bikes')


