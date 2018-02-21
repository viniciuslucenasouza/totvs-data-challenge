library(jsonlite)
library(tidyverse)
library(forecast)
library(ggplot2)

dataSample <- read_json("sample.txt")

flattenJson <- flatten(dataSample)

dataSample2 <- fromJSON("sample.txt")
##Criando um data frame

dataFrame <- data.frame(matrix(ncol = 4))
colnames(dataFrame) <- c("produto","valorUnitario","data","valorTotalCompra")

#Explicando o for
head(dataSample)
for (n in c(1:as.numeric(length(dataSample)))){
  
  for (i in c(1:as.numeric(length(dataSample[[n]]$dets))))  {
    
    produto <- dataSample[[n]]$dets[[i]]$prod$xProd
    valorunidade <- dataSample[[n]]$dets[[i]]$prod$vProd
    data <-  dataSample[[n]]$ide$dhEmi$`$date`
    valorTotalCompra <- dataSample[[n]]$complemento$valorTotalCompra
    
    dataFrame <- rbind(dataFrame,c(produto,valorunidade,data,valorTotalCompra))
  }
}
##Editando dados
dataFrame$produto <- as.factor(dataFrame$produto)
dataFrame$valorUnitario <- as.double(dataFrame$valorUnitario)
dataFrame$data <- as.Date(dataFrame$data)
dataFrame$valorTotalCompra <- as.double(dataFrame$valorTotalCompra)
dataFrame <-  dataFrame[-1,]


vendasPorData <- dataFrame %>%
  group_by(data) %>% 
  summarise(soma = sum(valorUnitario))
  

# ggplot(vendasPorProduto,
#        aes(x=) )
##Plotando
ggplot(vendasPorData,
       aes(x=data,
           y=soma))+
  geom_line()+
  geom_smooth(method ="glm")
  


##TIME SERIES
plot(ts(vendasPorData))

nts <- ts(vendasPorData$soma, frequency = 7)
fit <- HoltWinters(nts)
plot(fit)

plot(forecast(HoltWinters(nts), 21))



####Another perspective
#We can also use the function fromJSON, it may be better. Let's see what we get and do some analysis:

dataSample2 <- fromJSON("sample.txt")

#Lets see the if we have NAs:

colSums(is.na(dataSample2))

unique(dataSample2)

