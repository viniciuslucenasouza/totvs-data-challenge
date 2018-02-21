library(jsonlite)
library(tidyverse)
library(forecast)
library(ggplot2)

#No RStudio use:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


dataSample <- read_json("sample.txt")

##Criando um data frame

dataFrame <- data.frame(matrix(ncol = 4))
colnames(dataFrame) <- c("produto","valorUnitario","data")

#Explicando o for
head(dataSample)
for (n in c(1:as.numeric(length(dataSample)))){
  
  for (i in c(1:as.numeric(length(dataSample[[n]]$dets))))  {
    #Para cada dets, gravar o produto, valor e data
    produto <- dataSample[[n]]$dets[[i]]$prod$xProd
    valorunidade <- dataSample[[n]]$dets[[i]]$prod$vProd
    data <-  dataSample[[n]]$ide$dhEmi$`$date`
    
    dataFrame <- rbind(dataFrame,c(produto,valorunidade,data))
  }
}
##Editando dados
#Transformar cada classe para ser usado com os pacotes do R
dataFrame$produto <- as.factor(dataFrame$produto)
dataFrame$valorUnitario <- as.double(dataFrame$valorUnitario)
dataFrame$data <- as.Date(dataFrame$data)
#Removo primeira linha com NAs
dataFrame <-  dataFrame[-1,]

#Summarising the data
vendasPorData <- dataFrame %>%
  group_by(data) %>% 
  summarise(soma = sum(valorUnitario))
  
##Plotando
ggplot(vendasPorData,
       aes(x=data,
           y=soma))+
  geom_line()+
  #Podemos adicionar a tendencia linear do modelo, para curiosidade
  geom_smooth(method ="glm")
  
##TIME SERIES
plot(ts(vendasPorData))

timeSeriesVendas <- ts(vendasPorData$soma, frequency = 7)

HWModel <- HoltWinters(timeSeriesVendas)

plot(HWModel)

plot(forecast(HoltWinters(timeSeriesVendas), 7))

