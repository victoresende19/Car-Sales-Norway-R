#https://www.kaggle.com/dmi3kno/newcarsalesnorway/data
#Dicionario do dataset > 3) Summary stats for car sales in Norway by month - norwaynewcarsalesby_month.csv

#Importando as bibliotecas
library(readr)
library(dplyr)
library(corrplot)
library(forecast)
library(moments)

#Importando o dataset
norway_car_month <- read_csv("norway_new_car_sales_by_month.csv")
norway_car_model <- read_csv("norway_new_car_sales_by_model.csv")

#Visualizando os datasets
View(norway_car_month)
View(norway_car_model)
View(head(norway_car_month))
View(head(norway_car_model))

####################################################################################################################################
#Descobrindo se existem dados faltantes
any(is.na(norway_car_month))
any(is.na(norway_car_model))

#Funcao para mostrar a porcentagem faltantes em colunas que possuem mais de 0% de dados faltantes
NAS <- round(colSums(is.na(norway_car_month))*100/ nrow(norway_car_month), 2)
View(NAS[NAS>0])

#Excluindo colunas com mais de 60% de dados faltantes
norway_car_month$Used_YoY <- NULL
norway_car_month$Used <- NULL
norway_car_month$Import_Electric <- NULL
norway_car_month$Quantity_Hybrid <- NULL
norway_car_month$Quantity_Electric <- NULL

#Novamente a funcao para mostrar a porcentagem porem agora sem as colunas que foram excluidas
NAS <- round(colSums(is.na(norway_car_month))*100/ nrow(norway_car_month), 2)
View(NAS[NAS>0])
View(norway_car_month)



####################################################################################################################################
#Plotagem graficos sobre Quantidade de carros vendidos por mes (Com um pseudo modelo linear)
plot(norway_car_month$Quantity ~ norway_car_month$Month, xlab = "Mes", ylab = "Quantidade Vendida")
abline(lm(norway_car_month$Quantity ~ norway_car_month$Month))
lm(norway_car_month$Quantity ~ norway_car_month$Month)

#Plotagem graficos sobre Quantidade de carros vendidos por ano (Com um pseudo modelo linear)
plot(norway_car_month$Quantity ~ norway_car_month$Year, xlab = "Ano", ylab = "Quantidade Vendida")
abline(lm(norway_car_month$Quantity ~ norway_car_month$Year))
lm(norway_car_month$Quantity ~ norway_car_month$Year)

#Plotagem graficos sobre Importacoes de carros por mes (Com um pseudo modelo linear)
plot(norway_car_month$Import ~ norway_car_month$Month, xlab = "Mes", ylab = "Quantidade Vendida")
abline(lm(norway_car_month$Import ~ norway_car_month$Month))
lm(norway_car_month$Import ~ norway_car_month$Month)

#Plotagem graficos sobre Importacoes de carros por ano (Com um pseudo modelo linear)
plot(norway_car_month$Import ~ norway_car_month$Year, xlab = "Ano", ylab = "Quantidade ImportaÃ§Ã£o")
abline(lm(norway_car_month$Import ~ norway_car_month$Year))
lm(norway_car_month$Import ~ norway_car_month$Year)

#Plotagem graficos sobre Quantidade de vendas em relação a quantidade de vendas movidos a diesel
plot(norway_car_month$Quantity ~ norway_car_month$Quantity_Diesel, xlab="Quantidade carros diesel vendidos", ylab = "Quantidade carros vendidos")
abline(lm(norway_car_month$Quantity ~ norway_car_month$Quantity_Diesel))
lm(norway_car_month$Quantity ~ norway_car_month$Quantity_Diesel)

#Diesel CO2 por ano
dus2 <- subset(norway_car_month,
               complete.cases(cbind(Year, Quantity)),
               select = c(Quantity_Diesel, Year))
plot(norway_car_month$Quantity_Diesel ~ norway_car_month$Year,
     data = dus2,
     xlab = "Ano",
     ylab = "Quantidade carros CO2 vendidos")
with(dus2, {
  lines(lowess(x = norway_car_month$Year, y = norway_car_month$Quantity_Diesel),
        lwd = 2,
        col = "#ff0050")
})


#Quantidade por ano
dus2 <- subset(norway_car_month,
               complete.cases(cbind(Year, Quantity)),
               select = c(Quantity, Year))
plot(norway_car_month$Quantity ~ norway_car_month$Year,
     data = dus2,
     xlab = "Ano",
     ylab = "Quantidade carros geral vendidos")
with(dus2, {
  lines(lowess(x = norway_car_month$Year, y = norway_car_month$Quantity),
        lwd = 2,
        col = "#ff0050")
})

####################################################################################################################################
#Medidas de tendencia central quantidade de veiculos vendidos
mean(norway_car_month$Quantity)
View(max(table(norway_car_month$Quantity)))
median(norway_car_month$Quantity)

#Medidas de dispersão quantidade de veiculos vendidos
var(norway_car_month$Quantity)
sd(norway_car_month$Quantity)

#Boxplot Quantidade de veiculos vendidos
boxplot(norway_car_month$Quantity)

#Grafico da densidade dos dados em relacao a quantidade vendida:
den <- density(norway_car_month$Quantity)
plot(den)
str(den)

x <- eval(parse(text = den$data.name))
ma <- mean(x)             # Média da amostra.
md <- median(x)           # Mediana da amostra.

modal <- which.max(den$y)
modal <- list(x = den$x[modal], y = den$y[modal])

plot(den,
     type = "n",
     xlab = "Quantidade de veículos Vendidos",
     ylab = "Densidade",
     ylim = c(0, modal$y + strheight("1")),
     main = "",
     sub = paste("Bandwidth:", round(den$bw,3)))
with(den, polygon(x, y, col = "gray90"))
with(modal, {
  segments(x, 0, x, y, col = 2)
  text(x, y, labels = sprintf("Moda: %0.2f", x), pos = 3)
})
arrows(ma, 0, ma, modal$y/3, code = 1, length = 0.15)
text(ma, modal$y/3, labels = sprintf("Média: %0.2f", ma), pos = 3)
arrows(md, 0, md, modal$y/6, code = 1, length = 0.15)
text(ma, modal$y/6, labels = sprintf("Mediana: %0.2f", md),
     pos = ifelse(md<ma, 2, 4))
rug(norway_car_month$Quantity)

#Boxplot quantidade de veiculos vendidos
boxplot(norway_car_month$Quantity)

#indice de assimetria e curtose
skewness(norway_car_month$Quantity)
kurtosis(norway_car_month$Quantity)


####################################################################################################################################
#Medidas de tendencia central quantidade de veiculos importados
mean(norway_car_month$Import)
View(max(table(norway_car_month$Import)))
median(norway_car_month$Import)

#Medidas de dispersao quantidade de veiculos importados
var(norway_car_month$Import)
sd(norway_car_month$Import)

#Grafico da densidade dos dados em relacao a quantidade vendida:
den <- density(norway_car_month$Import)
plot(den)
str(den)

x <- eval(parse(text = den$data.name))
ma <- mean(x)             # Média da amostra.
md <- median(x)           # Mediana da amostra.

modal <- which.max(den$y)
modal <- list(x = den$x[modal], y = den$y[modal])

plot(den,
     type = "n",
     xlab = "Quantidade de veículos importados",
     ylab = "Densidade",
     ylim = c(0, modal$y + strheight("1")),
     main = "",
     sub = paste("Bandwidth:", round(den$bw,3)))
with(den, polygon(x, y, col = "gray90"))
with(modal, {
  segments(x, 0, x, y, col = 2)
  text(x, y, labels = sprintf("Moda: %0.2f", x), pos = 3)
})
arrows(ma, 0, ma, modal$y/3, code = 1, length = 0.15)
text(ma, modal$y/3, labels = sprintf("Média: %0.2f", ma), pos = 3)
arrows(md, 0, md, modal$y/6, code = 1, length = 0.15)
text(ma, modal$y/6, labels = sprintf("Mediana: %0.2f", md),
     pos = ifelse(md<ma, 2, 4))
rug(norway_car_month$Import)

#Boxplot quantidade de veiculos importados
boxplot(norway_car_month$Import)

#indice de assimetria e curtose
skewness(norway_car_month$Import)
kurtosis(norway_car_month$Import)

####################################################################################################################################
#Medidas de tendencia central quantidade de vendas da ford
mean(norway_car_model$Quantity[norway_car_model$Make == 'Ford'])
table(norway_car_model$Quantity[norway_car_model$Make == 'Ford'])
median(norway_car_model$Quantity[norway_car_model$Make == 'Ford'])


#Medidas de dispersao quantidade de vendas da ford
var(norway_car_model$Quantity[norway_car_model$Make == 'Ford'])
sd(norway_car_model$Quantity[norway_car_model$Make == 'Ford'])

#Grafico de densidade dos dados: Quantidade de vendas da Ford
den <- density(norway_car_model$Quantity[norway_car_model$Make == 'Ford'])
str(den)

x <- eval(parse(text = den$data.name))
ma <- mean(x)             # Média da amostra.
md <- median(x)           # Mediana da amostra.

modal <- which.max(den$y)
modal <- list(x = den$x[modal], y = den$y[modal])

plot(den,
     type = "n",
     xlab = "Quantidade de vendas FORD",
     ylab = "Densidade",
     ylim = c(0, modal$y + strheight("1")),
     main = "",
     sub = paste("Bandwidth:", round(den$bw,3)))
with(den, polygon(x, y, col = "gray90"))
with(modal, {
  segments(x, 0, x, y, col = 2)
  text(x, y, labels = sprintf("Moda: %0.2f", x), pos = 3)
})
arrows(ma, 0, ma, modal$y/3, code = 1, length = 0.15)
text(ma, modal$y/3, labels = sprintf("Média: %0.2f", ma), pos = 3)
arrows(md, 0, md, modal$y/6, code = 1, length = 0.15)
text(ma, modal$y/6, labels = sprintf("Mediana: %0.2f", md),
     pos = ifelse(md<ma, 2, 4))
rug(norway_car_model$Quantity[norway_car_model$Make == 'Ford'])

#Box plot quantidade de carros vendidos da marca Ford
boxplot(norway_car_model$Quantity[norway_car_model$Make == 'Ford'])

#indice de assimetria e curtose
skewness(norway_car_model$Quantity[norway_car_model$Make == 'Ford'])
kurtosis(norway_car_model$Quantity[norway_car_model$Make == 'Ford'])

####################################################################################################################################
#Medidas de tendencia central quantidade de vendas da toyota
mean(norway_car_model$Quantity[norway_car_model$Make == 'Toyota'])
View(max(table(norway_car_model$Quantity[norway_car_model$Make == 'Toyota'])))
median(norway_car_model$Quantity[norway_car_model$Make == 'Toyota'])

#Medidas de dispersao quantidade de vendas da ford
var(norway_car_model$Quantity[norway_car_model$Make == 'Toyota'])
sd(norway_car_model$Quantity[norway_car_model$Make == 'Toyota'])

#Grafico de densidade dos dados: Quantidade de vendas da Ford
den <- density(norway_car_model$Quantity[norway_car_model$Make == 'Toyota'])
str(den)

x <- eval(parse(text = den$data.name))
ma <- mean(x)             # Média da amostra.
md <- median(x)           # Mediana da amostra.

modal <- which.max(den$y)
modal <- list(x = den$x[modal], y = den$y[modal])

plot(den,
     type = "n",
     xlab = "Quantidade de vendas TOYOTA",
     ylab = "Densidade",
     ylim = c(0, modal$y + strheight("1")),
     main = "",
     sub = paste("Bandwidth:", round(den$bw,3)))
with(den, polygon(x, y, col = "gray90"))
with(modal, {
  segments(x, 0, x, y, col = 2)
  text(x, y, labels = sprintf("Moda: %0.2f", x), pos = 3)
})
arrows(ma, 0, ma, modal$y/3, code = 1, length = 0.15)
text(ma, modal$y/3, labels = sprintf("Média: %0.2f", ma), pos = 3)
arrows(md, 0, md, modal$y/6, code = 1, length = 0.15)
text(ma, modal$y/6, labels = sprintf("Mediana: %0.2f", md),
     pos = ifelse(md<ma, 2, 4))
rug(norway_car_model$Quantity[norway_car_model$Make == 'Toyota'])

#Box plot quantidade de carros vendidos da marca Toyota
boxplot(norway_car_model$Quantity[norway_car_model$Make == 'Toyota'])

#indice de assimetria e curtose
skewness(norway_car_model$Quantity[norway_car_model$Make == 'Toyota'])
kurtosis(norway_car_model$Quantity[norway_car_model$Make == 'Toyota'])

####################################################################################################################################
#Correlacao variaveis quantitativas
#IMAGENS ESTARAO PRESENTES NA PASTA

#Correlacao imagem circulo 
M<-cor(norway_car_month)
head(round(M,2))
png("correlacao_circulo.PNG")
corrplot(M, method= "circle")
dev.off()

#Correlacao imagem numero
N<-cor(norway_car_month)
head(round(N,2))
png("correlacao_numero.PNG")
corrplot(N, method= "number")
dev.off()

#Correlacao imagem cluster das variaveis que tem mais similaridade
P<-cor(norway_car_month)
head(round(P,2))
png("correlacao_cluster.PNG")
col3 <- colorRampPalette(c("red", "white", "blue"))
corrplot(P, order = "hclust", addrect = 2, col = col3(20))
dev.off()

#Correlacao tabela numero
cor1 <- round(cor(norway_car_month), 2)
View(cor1)


####################################################################################################################################
#Serie Temporal (QUANTIDADE DE CARROS VENDIDOS NOS ANOS)
vetor <- as.numeric(norway_car_month$Quantity)
z <- ts(vetor, frequency = 12, start = c(2007,1), end = c(2016,12))

ts.plot(z)
seasonplot(z, col = rainbow(12), border = c("royalblue"), year.labels = TRUE, Type = "o", pch= 16, main = "Quantidade de vendas Mês e Ano")
#Nessa serie temporal podemos ver o reflexo da crise de 2008.
#O grafico começa descendo na epoca da crise (SET 2008)
#Comecando la embaixo em 2009 e subindo pro normal
#Mostrando que a crise podia esta sendo superada


####################################################################################################################################
#Previsao com Holt-Winters com Tendencia e Ajuste sazonal
ajuste_HW_CT_CS <- HoltWinters(z)
ajuste_HW_CT_CS
plot(ajuste_HW_CT_CS)

previsao <- forecast(ajuste_HW_CT_CS, h=36)
plot(previsao, main = "Previsão Venda de Carros Noruega 2017 - 2019", xlab = "Anos", ylab = "Quantidade de Carros Vendidos")
previsao

