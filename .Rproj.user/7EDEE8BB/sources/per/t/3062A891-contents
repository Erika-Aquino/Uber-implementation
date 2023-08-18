# Brasília, março de 2023

# Análise impacto da implantação do app Uber na mortalidade por acidentes de transporte terrestre no Brasil
# Érika Carvalho de Aquino

# https://otexts.com/fpp3/AR.html
# https://otexts.com/fpp2/arima-r.html
# https://otexts.com/fpp2/accuracy.html
# https://stackoverflow.com/questions/24390859/why-does-auto-arima-drop-my-seasonality-component-when-stepwise-false-and-approx
# https://robjhyndman.com/hyndsight/arimaconstants/
# https://otexts.com/fpp2/non-seasonal-arima.html



# Carregamento de pacotes a serem utilizados
library(fpp3)
library (tidyverse)
library(magrittr)
library(ggfortify)
library(forecast)
library(TSA)
library(feasts)
library(ggplot2)
library(astsa)
library(dplyr)
library(zoo)
library(readxl)
library (tidyverse)

library(maptools)     
library(spdep)          
library(cartography)  
library(mapsf)
library(tmap)           
library(leaflet)        
library(rgdal)
library(RColorBrewer) 


## TRABALHANDO COM MORTALIDADE POR ATT

# 1. Importação e formatação dos dados
setwd("E:/doutorado/ATT Uber/Documentos submissão AJE/Revisão rodada 2/ATTUber_aug2023")
mort_att = read.table("mort_att.csv",sep=";", header = TRUE,dec=",")  
glimpse(mort_att)


# 2. Estruturação da série temporal
## Formatação da variável de tempo mensal
## Armazenamento como um objeto tsibble, definindo a variável "month" como índice
mort_att <- mort_att %>%
        mutate(month= yearmonth(month)) %>%
        as_tsibble(index = month, key = UF)
mort_att


# 3. Plotagem dos gráficos para análise visual da série temporal


## Gráfico da série temporal completa 
tiff(filename = "Figura 1.tiff")
autoplot(mort_att, mort_pad_att) +
        labs(title = "Mortality from traffic injuries, Brazil (Jan 2000 to Dec 2020)", 
             x = "Month of death", 
             y = "Standardized  mortality (100 thousand inhabitants)") + 
        theme_minimal()+
        facet_wrap(vars(regiao, capital), ncol = 4L)
dev.off()



# Análise do impacto da implantação do Uber na mortalidade por ATT segundo por UF




# 1. Rondônia

# Preparação dos dados "Porto Velho - RO"

mort_att_RO <- mort_att %>%
        filter(UF == "RO")

mort_att_RO

## Início das atividades Uber
uber_portovelho <-as.Date("2017-05-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Porto Velho - RO")
## Gráfico da série temporal completa "AC"

tiff(filename = "Série RO.tiff")
autoplot(mort_att_RO, mort_pad_att) +
        labs(title = "Mortality from traffic injuries, Porto Velho - RO (Jan 2000 a Dec 2020)", 
             x = "Month of death", 
             y = "Standardized mortality (100 thousand inhabitants)") + 
        geom_vline(aes(xintercept=as.Date(uber_portovelho), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()


## Média e desvio padrão da mortalidade por ATT período total   "Porto Velho - RO"
mort_att_RO <- mort_att_RO %>%
        filter(month < yearmonth("2018 May"))

mort_att_RO

mort_att_RO %>% features(mort_pad_att, list(media_pre_RO = mean))

mort_att_RO %>% features(mort_pad_att, list(desv_pad_pre_RO = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Porto Velho - RO"
mort_pre_RO <- mort_att_RO %>%
        filter(month < yearmonth("2017 May"))

mort_pre_RO

mort_pre_RO %>% features(mort_pad_att, list(media_pre_RO = mean))

mort_pre_RO %>% features(mort_pad_att, list(desv_pad_pre_RO = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Porto Velho - RO"
mort_pos_RO <- mort_att_RO %>% filter(month >= yearmonth("2017 May"), month < yearmonth("2018 May"))

mort_pos_RO

mort_pos_RO %>% features(mort_pad_att, list(media_pos_RO = mean))

mort_pos_RO %>% features(mort_pad_att, list(desv_pad_pos_RO = sd))


## Preparação dados do período pré-Uber "Porto Velho - RO"
mort_pre_RO <- mort_att_RO %>%
        filter(UF == "RO", month < yearmonth("2017 May"))

mort_pre_RO


# Conversão de dados para time series object "Porto Velho - RO"

## Período total
mort_att_RO <- ts(mort_att_RO[,2], frequency=12, start=c(2000,1), 
                  end = c(2018,4))

## Pré-Uber 
mort_pre_RO <- ts(mort_pre_RO[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,4))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade RO.tiff")
mort_pre_RO %>% ggseasonplot()
dev.off()

# Visualização de gráficos ACF/PACF de dados não diferenciados "Porto Velho - RO"
pdf("ACF PACF undifferenced RO.pdf")
acf(as.numeric(mort_pre_RO), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Porto Velho - RO"
pdf("ACF PACF dif_seasonally dif RO.pdf")
acf(diff(as.numeric(mort_pre_RO),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Porto Velho - RO"
# Especificações: Número máximo de diferenciações sazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_RO, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,1,2)

# Checagem dos resíduos do modelo "Porto Velho - RO"
pdf("Resíduos modelo RO .pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Porto Velho - RO"
summary(model)
confint(model)


##Análise de Intervenção "Porto Velho - RO"


# Criação e visualização de variável que representa mudança em degrau "Porto Velho - RO"
step <- as.numeric(as.yearmon(time(mort_att_RO))>='Mai 2017')
step

# Criação e visualização de variável que representa mudança em rampa "Porto Velho - RO"
ramp <- append(rep(0,208), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Porto Velho - RO"

# Modelo 1 - sem função de transferência "Porto Velho - RO"
model1 = Arima(mort_att_RO, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2))
# Checagem dos resíduos do modelo 1 "Porto Velho - RO"
pdf("Resíduos modelo 1 RO.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Porto Velho - RO"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Porto Velho - RO"
model2 = Arima(mort_att_RO, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2), 
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Porto Velho - RO"
pdf("Resíduos modelo 2 RO.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Porto Velho - RO"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Porto Velho - RO"

model3 = Arima(mort_att_RO, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2), 
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Porto Velho - RO"
pdf("Resíduos modelo 3 RO.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Porto Velho - RO"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Porto Velho - RO"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)


# 2. Rio Branco - Acre

# Preparação dos dados "Rio Branco - AC"

mort_att_AC <- mort_att %>%
        filter(UF == "AC")

mort_att_AC

## Início das atividades Uber
uber_riobranco <-as.Date("2017-06-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Rio Branco - AC")
## Gráfico da série temporal completa "AC"

tiff(filename = "Série AC.tiff")
autoplot(mort_att_AC, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Rio Branco - AC (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized  mortality (100 thousand inhabitants)") +
        geom_vline(aes(xintercept=as.Date(uber_riobranco), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Rio Branco - AC"
mort_att_AC <- mort_att_AC %>%
        filter(month < yearmonth("2018 Jun"))

mort_att_AC

mort_att_AC %>% features(mort_pad_att, list(media_pre_AC = mean))

mort_att_AC %>% features(mort_pad_att, list(desv_pad_pre_AC = sd))

# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Rio Branco - AC"
mort_pre_AC <- mort_att_AC %>%
        filter(month < yearmonth("2017 Jun"))

mort_pre_AC

mort_pre_AC %>% features(mort_pad_att, list(media_pre_AC = mean))

mort_pre_AC %>% features(mort_pad_att, list(desv_pad_pre_AC = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Rio Branco - AC"
mort_pos_AC <- mort_att_AC %>% filter(month >= yearmonth("2017 Jun"), month < yearmonth("2018 Jun"))

mort_pos_AC

mort_pos_AC %>% features(mort_pad_att, list(media_pos_AC = mean))

mort_pos_AC %>% features(mort_pad_att, list(desv_pad_pos_AC = sd))


## Preparação dados do período pré-Uber "Rio Branco - AC"
mort_pre_AC <- mort_att_AC %>%
        filter(UF == "AC", month < yearmonth("2017 Jun"))

mort_pre_AC


# Conversão de dados para time series object "Rio Branco - AC"

## Período total
mort_att_AC <- ts(mort_att_AC[,2], frequency=12, start=c(2000,1), 
                  end = c(2018,5))

## Pré-uber 
mort_pre_AC <- ts(mort_pre_AC[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,5))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade AC.tiff")
mort_pre_AC %>% ggseasonplot()
dev.off()

# Visualização de gráficos ACF/PACF de dados não diferenciados "Rio Branco - AC"
pdf("ACF PACF undifferenced AC.pdf")
acf(as.numeric(mort_pre_AC), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Rio Branco - AC"
pdf("ACF PACF dif_seasonally dif AC.pdf")
acf(diff(as.numeric(mort_pre_AC),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Rio Branco - AC"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_AC, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,1,2)with drift

# Checagem dos resíduos do modelo "Rio Branco - AC"
pdf("Resíduos modelo AC.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Rio Branco - AC"
summary(model)
confint(model)




##Análise de Intervenção "Rio Branco - AC"


# Criação e visualização de variável que representa mudança em degrau "Rio Branco - AC"
step <- as.numeric(as.yearmon(time(mort_att_AC))>='Jun 2017')
step

# Criação e visualização de variável que representa mudança em rampa "Rio Branco - AC"
ramp <- append(rep(0,209), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Rio Branco - AC"

# Modelo 1 - sem função de transferência "Rio Branco - AC"
model1 = Arima(mort_att_AC, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(1,1,2))
# Checagem dos resíduos do modelo 1 "Rio Branco - AC"
pdf("Resíduos modelo 1 AC.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 1 "Rio Branco - AC"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Rio Branco - AC"
model2 = Arima(mort_att_AC, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(1,1,2),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Rio Branco - AC"
pdf("Resíduos modelo 2 AC.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Rio Branco - AC"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Rio Branco - AC"

model3 = Arima(mort_att_AC, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(1,1,2),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Rio Branco - AC"
pdf("Resíduos modelo 3 AC.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Rio Branco - AC"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Rio Branco - AC"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)



# 3. Amazonas

# Preparação dos dados "Manaus - AM"

mort_att_AM <- mort_att %>%
        filter(UF == "AM")

mort_att_AM

## Início das atividades Uber
uber_manaus <-as.Date("2017-04-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Manaus - AM")
## Gráfico da série temporal completa "AM"

tiff(filename = "Série AM.tiff")
autoplot(mort_att_AM, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Manaus - AM (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)") +
        geom_vline(aes(xintercept=as.Date(uber_manaus), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Manaus - AM"
mort_att_AM <- mort_att_AM %>%
        filter(month < yearmonth("2018 Apr"))

mort_att_AM

mort_att_AM %>% features(mort_pad_att, list(media_pre_AM = mean))

mort_att_AM %>% features(mort_pad_att, list(desv_pad_pre_AM = sd))

# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Manaus - AM"
mort_pre_AM <- mort_att_AM %>%
        filter(month < yearmonth("2017 Apr"))

mort_pre_AM

mort_pre_AM %>% features(mort_pad_att, list(media_pre_AM = mean))

mort_pre_AM %>% features(mort_pad_att, list(desv_pad_pre_AM = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Manaus - AM"
mort_pos_AM <- mort_att_AM %>% filter(month >= yearmonth("2017 Apr"))

mort_pos_AM

mort_pos_AM %>% features(mort_pad_att, list(media_pos_AM = mean))

mort_pos_AM %>% features(mort_pad_att, list(desv_pad_pos_AM = sd))


## Preparação dados do período pré-Uber "Manaus - AM"
mort_pre_AM <- mort_att_AM %>%
        filter(UF == "AM", month < yearmonth("2017 Apr"))

mort_pre_AM


# Conversão de dados para time series object "Manaus - AM"

## Período total
mort_att_AM <- ts(mort_att_AM[,2], frequency=12, start=c(2000,1), 
                  end = c(2018,3))

## Pré-uber 
mort_pre_AM <- ts(mort_pre_AM[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,3))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade AM.tiff")
mort_pre_AM %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Manaus - AM"
pdf("ACF PACF undifferenced AM.pdf")
acf(as.numeric(mort_pre_AM), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Manaus - AM"
pdf("ACF PACF dif_seasonally dif AM.pdf")
acf(diff(as.numeric(mort_pre_AM),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Manaus - AM"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_AM, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(0,1,1)

# Checagem dos resíduos do modelo "Manaus - AM"
pdf("Resíduos modelo AM.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Manaus - AM"
summary(model)
confint(model)




##Análise de Intervenção "Manaus - AM"


# Criação e visualização de variável que representa mudança em degrau "Manaus - AM"
step <- as.numeric(as.yearmon(time(mort_att_AM))>='Abr 2017')
step

# Criação e visualização de variável que representa mudança em rampa "Manaus - AM"
ramp <- append(rep(0,207), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Manaus - AM"

# Modelo 1 - sem função de transferência "Manaus - AM"
model1 = Arima(mort_att_AM, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1))
# Checagem dos resíduos do modelo 1 "Manaus - AM"
pdf("Resíduos modelo 1 AM.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Manaus - AM"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Manaus - AM"
model2 = Arima(mort_att_AM, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Manaus - AM"
pdf("Resíduos modelo 2 AM.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Manaus - AM"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Manaus - AM"

model3 = Arima(mort_att_AM, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Manaus - AM"
pdf("Resíduos modelo 3 AM.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Manaus - AM"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Manaus - AM"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)



# 4. Roraima
# Preparação dos dados "Boa Vista - RR"

mort_att_RR <- mort_att %>%
        filter(UF == "RR")

mort_att_RR

## Início das atividades Uber
uber_boavista <-as.Date("2017-06-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Boa Vista - RR")
## Gráfico da série temporal completa “RR”

tiff(filename = "Série RR.tiff")
autoplot(mort_att_RR, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Boa Vista - RR (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)") +
        geom_vline(aes(xintercept=as.Date(uber_boavista), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Boa Vista - RR"
mort_att_RR <- mort_att_RR %>%
        filter(month < yearmonth("2018 Jun"))

mort_att_RR

mort_att_RR %>% features(mort_pad_att, list(media_pre_RR = mean))

mort_att_RR %>% features(mort_pad_att, list(desv_pad_pre_RR = sd))

# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Boa Vista - RR"
mort_pre_RR <- mort_att_RR %>%
        filter(month < yearmonth("2017 Jun"))

mort_pre_RR

mort_pre_RR %>% features(mort_pad_att, list(media_pre_RR = mean))

mort_pre_RR %>% features(mort_pad_att, list(desv_pad_pre_RR = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Boa Vista - RR"
mort_pos_RR <- mort_att_RR %>% filter(month >= yearmonth("2017 Jun"), month < yearmonth("2018 Jun"))

mort_pos_RR

mort_pos_RR %>% features(mort_pad_att, list(media_pos_RR = mean))

mort_pos_RR %>% features(mort_pad_att, list(desv_pad_pos_RR = sd))


## Preparação dados do período pré-Uber "Boa Vista - RR"
mort_pre_RR <- mort_att_RR %>%
        filter(UF == "RR", month < yearmonth("2017 Jun"))

mort_pre_RR


# Conversão de dados para time series object "Boa Vista - RR"

## Período total
mort_att_RR <- ts(mort_att_RR[,2], frequency=12, start=c(2000,1), 
                  end = c(2018,5))

## Pré-uber 
mort_pre_RR <- ts(mort_pre_RR[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,5))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade RR.tiff")
mort_pre_RR %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Boa Vista - RR"
pdf("ACF PACF undifferenced RR.pdf")
acf(as.numeric(mort_pre_RR), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Boa Vista - RR"
pdf("ACF PACF dif_seasonally dif RR.pdf")
acf(diff(as.numeric(mort_pre_RR),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Boa Vista - RR"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_RR, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(0,1,1) with drift 

# Checagem dos resíduos do modelo "Boa Vista - RR"
pdf("Resíduos modelo RR.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Boa Vista - RR"
summary(model)
confint(model)




##Análise de Intervenção "Boa Vista - RR"


# Criação e visualização de variável que representa mudança em degrau "Boa Vista - RR"
step <- as.numeric(as.yearmon(time(mort_att_RR))>='Jun 2017')
step

# Criação e visualização de variável que representa mudança em rampa "Boa Vista - RR"
ramp <- append(rep(0,209), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Boa Vista - RR"

# Modelo 1 - sem função de transferência "Boa Vista - RR"
model1 = Arima(mort_att_RR, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(0,1,1))
# Checagem dos resíduos do modelo 1 "Boa Vista - RR"
pdf("Resíduos modelo 1 RR.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Boa Vista - RR"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Boa Vista - RR"
model2 = Arima(mort_att_RR, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Boa Vista - RR"
pdf("Resíduos modelo 2 RR.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Boa Vista - RR"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Boa Vista - RR"

model3 = Arima(mort_att_RR, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Boa Vista - RR"
pdf("Resíduos modelo 3 RR.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Boa Vista - RR"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Boa Vista - RR"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)



# 5. Pará

# Preparação dos dados "Belém - PA"

mort_att_PA <- mort_att %>%
        filter(UF == "PA")

mort_att_PA

## Início das atividades Uber
uber_belem <-as.Date("2017-02-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Belém - PA")
## Gráfico da série temporal completa “PA”

tiff(filename = "Série PA.tiff")
autoplot(mort_att_PA, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Belém - PA (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)") +
        geom_vline(aes(xintercept=as.Date(uber_belem), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Belém - PA"
mort_att_PA <- mort_att_PA %>%
        filter(month < yearmonth("2018 Feb"))

mort_att_PA

mort_att_PA %>% features(mort_pad_att, list(media_pre_PA = mean))

mort_att_PA %>% features(mort_pad_att, list(desv_pad_pre_PA = sd))

# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Belém - PA"
mort_pre_PA <- mort_att_PA %>%
        filter(month < yearmonth("2017 Feb"))

mort_pre_PA

mort_pre_PA %>% features(mort_pad_att, list(media_pre_PA = mean))

mort_pre_PA %>% features(mort_pad_att, list(desv_pad_pre_PA = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Belém - PA"
mort_pos_PA <- mort_att_PA %>% filter(month >= yearmonth("2017 Feb"), month < yearmonth("2018 Feb"))

mort_pos_PA

mort_pos_PA %>% features(mort_pad_att, list(media_pos_PA = mean))

mort_pos_PA %>% features(mort_pad_att, list(desv_pad_pos_PA = sd))


## Preparação dados do período pré-Uber "Belém - PA"
mort_pre_PA <- mort_att_PA %>%
        filter(UF == "PA", month < yearmonth("2017 Feb"))

mort_pre_PA


# Conversão de dados para time series object "Belém - PA"

## Período total
mort_att_PA <- ts(mort_att_PA[,2], frequency=12, start=c(2000,1), 
                  end = c(2018,1))

## Pré-uber 
mort_pre_PA <- ts(mort_pre_PA[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,1))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade PA.tiff")
mort_pre_PA %>% ggseasonplot()
dev.off()

# Visualização de gráficos ACF/PACF de dados não diferenciados "Belém - PA"
pdf("ACF PACF undifferenced PA.pdf")
acf(as.numeric(mort_pre_PA), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Belém - PA"
pdf("ACF PACF dif_seasonally dif PA.pdf")
acf(diff(as.numeric(mort_pre_PA),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Belém - PA"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_PA, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,1,4) 

# Checagem dos resíduos do modelo "Belém - PA"
pdf("Resíduos modelo PA.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Belém - PA"
summary(model)
confint(model)




##Análise de Intervenção "Belém - PA"


# Criação e visualização de variável que representa mudança em degrau "Belém - PA"
step <- as.numeric(as.yearmon(time(mort_att_PA))>='Fev 2017')
step

# Criação e visualização de variável que representa mudança em rampa "Belém - PA"
ramp <- append(rep(0,205), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Belém - PA"

# Modelo 1 - sem função de transferência "Belém - PA"
model1 = Arima(mort_att_PA, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,4))
# Checagem dos resíduos do modelo 1 "Belém - PA"
pdf("Resíduos modelo 1 PA.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Belém - PA"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Belém - PA"
model2 = Arima(mort_att_PA, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,4),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Belém - PA"
pdf("Resíduos modelo 2 PA.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Belém - PA"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Belém - PA"

model3 = Arima(mort_att_PA, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,4),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Belém - PA"
pdf("Resíduos modelo 3 PA.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Belém - PA"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Belém - PA"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)


# 6. Amapá

# Preparação dos dados "Macapá - AP"

mort_att_AP <- mort_att %>%
        filter(UF == "AP")

mort_att_AP

## Início das atividades Uber
uber_macapa <-as.Date("2017-06-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Macapá - AP")
## Gráfico da série temporal completa “AP”

tiff(filename = "Série AP.tiff")
autoplot(mort_att_AP, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Macapá -AP (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)") +
        geom_vline(aes(xintercept=as.Date(uber_macapa), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Macapá - AP"
mort_att_AP <- mort_att_AP %>%
        filter(month < yearmonth("2018 Jun"))

mort_att_AP

mort_att_AP %>% features(mort_pad_att, list(media_pre_AP = mean))

mort_att_AP %>% features(mort_pad_att, list(desv_pad_pre_AP = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Macapá - AP"
mort_pre_AP <- mort_att_AP %>%
        filter(month < yearmonth("2017 Jun"))

mort_pre_AP

mort_pre_AP %>% features(mort_pad_att, list(media_pre_AP = mean))

mort_pre_AP %>% features(mort_pad_att, list(desv_pad_pre_AP = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Macapá - AP"
mort_pos_AP <- mort_att_AP %>% filter(month >= yearmonth("2017 Jun"), month < yearmonth("2018 Jun"))

mort_pos_AP

mort_pos_AP %>% features(mort_pad_att, list(media_pos_AP = mean))

mort_pos_AP %>% features(mort_pad_att, list(desv_pad_pos_AP = sd))


## Preparação dados do período pré-Uber "Macapá - AP"
mort_pre_AP <- mort_att_AP %>%
        filter(UF == "AP", month < yearmonth("2017 Jun"))

mort_pre_AP


# Conversão de dados para time series object "Macapá - AP"

## Período total
mort_att_AP <- ts(mort_att_AP[,2], frequency=12, start=c(2000,1), 
                  end = c(2018,5))

## Pré-uber 
mort_pre_AP <- ts(mort_pre_AP[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,5))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade AP.tiff")
mort_pre_AP %>% ggseasonplot()
dev.off()

# Visualização de gráficos ACF/PACF de dados não diferenciados "Macapá - AP"
pdf("ACF PACF undifferenced AP.pdf")
acf(as.numeric(mort_pre_AP), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Macapá - AP"
pdf("ACF PACF dif_seasonally dif AP.pdf")
acf(diff(as.numeric(mort_pre_AP),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Macapá - AP"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_AP, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,1,1) 

# Checagem dos resíduos do modelo "Macapá - AP"
pdf("Resíduos modelo AP.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Macapá - AP"
summary(model)
confint(model)




##Análise de Intervenção "Macapá - AP"


# Criação e visualização de variável que representa mudança em degrau "Macapá - AP"
step <- as.numeric(as.yearmon(time(mort_att_AP))>='Jun 2017')
step

# Criação e visualização de variável que representa mudança em rampa "Macapá - AP"
ramp <- append(rep(0,209), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Macapá - AP"

# Modelo 1 - sem função de transferência "Macapá - AP"
model1 = Arima(mort_att_AP, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,1))
# Checagem dos resíduos do modelo 1 "Macapá - AP"
pdf("Resíduos modelo 1 AP.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Macapá - AP"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Macapá - AP"
model2 = Arima(mort_att_AP, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Macapá - AP"
pdf("Resíduos modelo 2 AP.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Macapá - AP"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Macapá - AP"

model3 = Arima(mort_att_AP, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Macapá - AP"
pdf("Resíduos modelo 3 AP.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Macapá - AP"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Macapá - AP"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)


# 7. Tocantins
# Preparação dos dados "Palmas - TO"

mort_att_TO <- mort_att %>%
        filter(UF == "TO")

mort_att_TO

## Início das atividades Uber
uber_palmas <-as.Date("2017-03-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Palmas - TO")
## Gráfico da série temporal completa “TO”

tiff(filename = "Série TO.tiff")
autoplot(mort_att_TO, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Palmas - TO (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)") +
        geom_vline(aes(xintercept=as.Date(uber_palmas), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Palmas - TO"
mort_att_TO <- mort_att_TO %>%
        filter(month < yearmonth("2018 Mar"))

mort_att_TO

mort_att_TO %>% features(mort_pad_att, list(media_pre_TO = mean))

mort_att_TO %>% features(mort_pad_att, list(desv_pad_pre_TO = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Palmas - TO"
mort_pre_TO <- mort_att_TO %>%
        filter(month < yearmonth("2017 Mar"))

mort_pre_TO

mort_pre_TO %>% features(mort_pad_att, list(media_pre_TO = mean))

mort_pre_TO %>% features(mort_pad_att, list(desv_pad_pre_TO = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Palmas - TO"
mort_pos_TO <- mort_att_TO %>% filter(month >= yearmonth("2017 Mar"), month < yearmonth("2018 Mar"))

mort_pos_TO

mort_pos_TO %>% features(mort_pad_att, list(media_pos_TO = mean))

mort_pos_TO %>% features(mort_pad_att, list(desv_pad_pos_TO = sd))


## Preparação dados do período pré-Uber "Palmas - TO"
mort_pre_TO <- mort_att_TO %>%
        filter(UF == "TO", month < yearmonth("2017 Mar"))

mort_pre_TO


# Conversão de dados para time series object "Palmas - TO"

## Período total
mort_att_TO <- ts(mort_att_TO[,2], frequency=12, start=c(2000,1), 
                  end = c(2018,2))

## Pré-uber 
mort_pre_TO <- ts(mort_pre_TO[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,2))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade TO.tiff")
mort_pre_TO %>% ggseasonplot()
dev.off()

# Visualização de gráficos ACF/PACF de dados não diferenciados "Palmas - TO"
pdf("ACF PACF undifferenced TO.pdf")
acf(as.numeric(mort_pre_TO), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Palmas - TO"
pdf("ACF PACF dif_seasonally dif TO.pdf")
acf(diff(as.numeric(mort_pre_TO),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Palmas - TO"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_TO, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(0,1,2) 

# Checagem dos resíduos do modelo "Palmas - TO"
pdf("Resíduos modelo TO.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Palmas - TO"
summary(model)
confint(model)




##Análise de Intervenção "Palmas - TO"


# Criação e visualização de variável que representa mudança em degrau "Palmas - TO"
step <- as.numeric(as.yearmon(time(mort_att_TO))>='Mar 2017')
step

# Criação e visualização de variável que representa mudança em rampa "Palmas - TO"
ramp <- append(rep(0,206), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Palmas - TO"

# Modelo 1 - sem função de transferência "Palmas - TO"
model1 = Arima(mort_att_TO, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,2))
# Checagem dos resíduos do modelo 1 "Palmas - TO"
pdf("Resíduos modelo 1 TO.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Palmas - TO"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Palmas - TO"
model2 = Arima(mort_att_TO, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,2),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Palmas - TO"
pdf("Resíduos modelo 2 TO.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Palmas - TO"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Palmas - TO"

model3 = Arima(mort_att_TO, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,2),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Palmas - TO"
pdf("Resíduos modelo 3 TO.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Palmas - TO"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Palmas - TO"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 8. Maranhão
# Preparação dos dados "São Luís - MA"

mort_att_MA <- mort_att %>%
        filter(UF == "MA")

mort_att_MA

## Início das atividades Uber
uber_saoluis <-as.Date("2017-04-01") 

# Plotagem dos gráficos para análise visual da série temporal ("São Luís - MA")
## Gráfico da série temporal completa “MA”

tiff(filename = "Série MA.tiff")
autoplot(mort_att_MA, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, São Luís - MA (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_saoluis), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "São Luís - MA"
mort_att_MA <- mort_att_MA %>% filter(month < yearmonth("2018 Apr"))

mort_att_MA

mort_att_MA %>% features(mort_pad_att, list(media_pos_MA = mean))

mort_att_MA %>% features(mort_pad_att, list(desv_pad_pos_MA = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "São Luís - MA"
mort_pre_MA <- mort_att_MA %>% filter(month < yearmonth("2017 Apr"))

mort_pre_MA

mort_pre_MA %>% features(mort_pad_att, list(media_pre_MA = mean))

mort_pre_MA %>% features(mort_pad_att, list(desv_pad_pre_MA = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "São Luís - MA"
mort_pos_MA <- mort_att_MA %>% filter(month >= yearmonth("2017 Apr"), month < yearmonth("2018 Apr"))

mort_pos_MA

mort_pos_MA %>% features(mort_pad_att, list(media_pos_MA = mean))

mort_pos_MA %>% features(mort_pad_att, list(desv_pad_pos_MA = sd))


## Preparação dados do período pré-Uber "São Luís - MA"
mort_pre_MA <- mort_att_MA %>%
        filter(UF == "MA", month < yearmonth("2017 Apr"))

mort_pre_MA


# Conversão de dados para time series object "São Luís - MA"

## Período total
mort_att_MA <- ts(mort_att_MA[,2], frequency=12, start=c(2000,1), 
                  end = c(2018,3))

## Pré-uber 
mort_pre_MA <- ts(mort_pre_MA[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,3))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade MA.tiff")
mort_pre_MA %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "São Luís - MA"
pdf("ACF PACF undifferenced MA.pdf")
acf(as.numeric(mort_pre_MA), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "São Luís - MA"
pdf("ACF PACF dif_seasonally dif MA.pdf")
acf(diff(as.numeric(mort_pre_MA),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "São Luís - MA"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_MA, seasonal=FALSE, max.d=2,
                    stepwise=FALSE, trace=TRUE,max.order=10)
model

# Modelo selecionado: ARIMA(1,0,1) with non-zero mean

# Checagem dos resíduos do modelo "São Luís - MA"
pdf("Resíduos modelo MA.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "São Luís - MA"
summary(model)
confint(model)




##Análise de Intervenção "São Luís - MA"


# Criação e visualização de variável que representa mudança em degrau "São Luís - MA"
step <- as.numeric(as.yearmon(time(mort_att_MA))>='Abr 2017')
step

# Criação e visualização de variável que representa mudança em rampa "São Luís - MA"
ramp <- append(rep(0,207), seq(1,12,1))
ramp  


## Criação e testagem de modelos "São Luís - MA"

# Modelo 1 - sem função de transferência "São Luís - MA"
model1 = Arima(mort_att_MA, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(1,0,1))
# Checagem dos resíduos do modelo 1 "São Luís - MA"
pdf("Resíduos modelo 1 MA.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "São Luís - MA"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "São Luís - MA"
model2 = Arima(mort_att_MA, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(1,0,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "São Luís - MA"
pdf("Resíduos modelo 2 MA.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "São Luís - MA"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "São Luís - MA"

model3 = Arima(mort_att_MA, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(1,0,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "São Luís - MA"
pdf("Resíduos modelo 3 MA.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "São Luís - MA"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "São Luís - MA"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model3$residuals)





# 9. Piauí
# Preparação dos dados "Teresina - PI"

mort_att_PI <- mort_att %>%
  filter(UF == "PI")

mort_att_PI

## Início das atividades Uber
uber_teresina <-as.Date("2016-11-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Teresina - PI")
## Gráfico da série temporal completa “PI”

tiff(filename = "Série PI.tiff")
autoplot(mort_att_PI, mort_pad_att) +
  labs(title = "Mortality from traffic injuries, Teresina - PI (Jan 2000 to Dec 2020)", 
       x = "Month of death", 
       y = "Redistributed and standardized mortality (100 thousand inhabitants)")+
  geom_vline(aes(xintercept=as.Date(uber_teresina), 
                 col="Start of Uber activities"))+
  theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Teresina - PI"
mort_att_PI <- mort_att_PI %>%
  filter(month < yearmonth("2017 Nov"))

mort_att_PI

mort_att_PI %>% features(mort_pad_att, list(media_pre_PI = mean))

mort_att_PI %>% features(mort_pad_att, list(desv_pad_pre_PI = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Teresina - PI"
mort_pre_PI <- mort_att_PI %>%
  filter(month < yearmonth("2016 Nov"))

mort_pre_PI

mort_pre_PI %>% features(mort_pad_att, list(media_pre_PI = mean))

mort_pre_PI %>% features(mort_pad_att, list(desv_pad_pre_PI = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Teresina - PI"
mort_pos_PI <- mort_att_PI %>% filter(month >= yearmonth("2016 Nov"), month < yearmonth("2017 Nov"))

mort_pos_PI

mort_pos_PI %>% features(mort_pad_att, list(media_pos_PI = mean))

mort_pos_PI %>% features(mort_pad_att, list(desv_pad_pos_PI = sd))


## Preparação dados do período pré-Uber "Teresina - PI"
mort_pre_PI <- mort_att_PI %>%
  filter(UF == "PI", month < yearmonth("2016 Nov"))

mort_pre_PI


# Conversão de dados para time series object "Teresina - PI"

## Período total
mort_att_PI <- ts(mort_att_PI[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,10))

## Pré-uber 
mort_pre_PI <- ts(mort_pre_PI[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,10))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade PI.tiff")
mort_pre_PI %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Teresina - PI"
pdf("ACF PACF undifferenced PI.pdf")
acf(as.numeric(mort_pre_PI), lag.max=48)
dev.off()

#Visualiazacão da PACF
pdf("ACF PACF undifferenced PI.pdf")
pacf(as.numeric(mort_pre_PI), lag.max=48)
dev.off()


#Teste de Ruído Branco


mort_pre_PI.iid <- whiteNoiseTest(autocorrelations(as.numeric(mort_pre_PI)), 
                                  h0 = "iid", nlags = c(5,10,20), 
                                  x = as.numeric(mort_pre_PI),
                                  method = "LjungBox")

mort_pre_PI.iid$test 

# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Teresina - PI"
pdf("ACF PACF dif_seasonally dif PI.pdf")
acf(diff(as.numeric(mort_pre_PI),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Teresina - PI"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_PI, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: (0,0,0) with non-zero mean 

# Checagem dos resíduos do modelo "Teresina - PI"
pdf("Resíduos modelo PI.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Teresina - PI"
summary(model)
confint(model)




##Análise de Intervenção "Teresina - PI"


# Criação e visualização de variável que representa mudança em degrau "Teresina - PI"
step <- as.numeric(as.yearmon(time(mort_att_PI))>='Nov 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Teresina - PI"
ramp <- append(rep(0,202), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Teresina - PI"

# Modelo 1 - sem função de transferência "Teresina - PI"
model1 = Arima(mort_att_PI, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(0,0,0))
# Checagem dos resíduos do modelo 1 "Teresina - PI"
pdf("Resíduos modelo 1 PI.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Teresina - PI"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Teresina - PI"
model2 = Arima(mort_att_PI, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(0,0,0),
               xreg = cbind(step),
               method="ML")
# Checagem dos resíduos do modelo 2 "Teresina - PI"
pdf("Resíduos modelo 2 PI.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Teresina - PI"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Teresina - PI"

model3 = Arima(mort_att_PI, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(0,0,0),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Teresina - PI"
pdf("Resíduos modelo 3 PI.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Teresina - PI"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Teresina - PI"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model2$residuals)






# 10. Ceará
# Preparação dos dados "Fortaleza - CE"

mort_att_CE <- mort_att %>%
        filter(UF == "CE")

mort_att_CE

## Início das atividades Uber
uber_fortaleza <-as.Date("2016-04-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Fortaleza - CE")
## Gráfico da série temporal completa “CE”

tiff(filename = "Série CE.tiff")
autoplot(mort_att_CE, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Fortaleza - CE (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_fortaleza), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Fortaleza - CE"
mort_att_CE <- mort_att_CE %>%
        filter(month < yearmonth("2017 Apr"))

mort_att_CE

mort_att_CE %>% features(mort_pad_att, list(media_pre_CE = mean))

mort_att_CE %>% features(mort_pad_att, list(desv_pad_pre_CE = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Fortaleza - CE"
mort_pre_CE <- mort_att_CE %>%
        filter(month < yearmonth("2016 Apr"))

mort_pre_CE

mort_pre_CE %>% features(mort_pad_att, list(media_pre_CE = mean))

mort_pre_CE %>% features(mort_pad_att, list(desv_pad_pre_CE = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Fortaleza - CE"
mort_pos_CE <- mort_att_CE %>% filter(month >= yearmonth("2016 Apr"), month < yearmonth("2017 Apr"))

mort_pos_CE

mort_pos_CE %>% features(mort_pad_att, list(media_pos_CE = mean))

mort_pos_CE %>% features(mort_pad_att, list(desv_pad_pos_CE = sd))


## Preparação dados do período pré-Uber "Fortaleza - CE"
mort_pre_CE <- mort_att_CE %>%
        filter(UF == "CE", month < yearmonth("2016 Apr"))

mort_pre_CE


# Conversão de dados para time series object "Fortaleza - CE"

## Período total
mort_att_CE <- ts(mort_att_CE[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,3))

## Pré-uber 
mort_pre_CE <- ts(mort_pre_CE[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,3))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade CE.tiff")
mort_pre_CE %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Fortaleza - CE"
pdf("ACF PACF undifferenced CE.pdf")
acf(as.numeric(mort_pre_CE), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Fortaleza - CE"
pdf("ACF PACF dif_seasonally dif CE.pdf")
acf(diff(as.numeric(mort_pre_CE),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Fortaleza - CE"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_CE, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,1,2) 

# Checagem dos resíduos do modelo "Fortaleza - CE"
pdf("Resíduos modelo CE.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Fortaleza - CE"
summary(model)
confint(model)




##Análise de Intervenção "Fortaleza - CE"


# Criação e visualização de variável que representa mudança em degrau "Fortaleza - CE"
step <- as.numeric(as.yearmon(time(mort_att_CE))>='Abr 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Fortaleza - CE"
ramp <- append(rep(0,195), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Fortaleza - CE"

# Modelo 1 - sem função de transferência "Fortaleza - CE"
model1 = Arima(mort_att_CE, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2))
# Checagem dos resíduos do modelo 1 "Fortaleza - CE"
pdf("Resíduos modelo 1 CE.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Fortaleza - CE"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Fortaleza - CE"
model2 = Arima(mort_att_CE, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Fortaleza - CE"
pdf("Resíduos modelo 2 CE.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Fortaleza - CE"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Fortaleza - CE"

model3 = Arima(mort_att_CE, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Fortaleza - CE"
pdf("Resíduos modelo 3 CE.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Fortaleza - CE"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Fortaleza - CE"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model2$residuals)




# 11. Rio Grande do Norte
# Preparação dos dados "Natal - RN"

mort_att_RN <- mort_att %>%
        filter(UF == "RN")

mort_att_RN

## Início das atividades Uber
uber_natal <-as.Date("2016-08-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Natal - RN")
## Gráfico da série temporal completa “RN”

tiff(filename = "Série RN.tiff")
autoplot(mort_att_RN, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Natal - RN (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_natal), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Natal - RN"
mort_att_RN <- mort_att_RN %>%
        filter(month < yearmonth("2017 Aug"))

mort_att_RN

mort_att_RN %>% features(mort_pad_att, list(media_pre_RN = mean))

mort_att_RN %>% features(mort_pad_att, list(desv_pad_pre_RN = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Natal - RN"
mort_pre_RN <- mort_att_RN %>%
        filter(month < yearmonth("2016 Aug"))

mort_pre_RN

mort_pre_RN %>% features(mort_pad_att, list(media_pre_RN = mean))

mort_pre_RN %>% features(mort_pad_att, list(desv_pad_pre_RN = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Natal - RN"
mort_pos_RN <- mort_att_RN %>% filter(month >= yearmonth("2016 Aug"), month < yearmonth("2017 Aug"))

mort_pos_RN

mort_pos_RN %>% features(mort_pad_att, list(media_pos_RN = mean))

mort_pos_RN %>% features(mort_pad_att, list(desv_pad_pos_RN = sd))


## Preparação dados do período pré-Uber "Natal - RN"
mort_pre_RN <- mort_att_RN %>%
        filter(UF == "RN", month < yearmonth("2016 Aug"))

mort_pre_RN


# Conversão de dados para time series object "Natal - RN"

## Período total
mort_att_RN <- ts(mort_att_RN[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,7))

## Pré-uber 
mort_pre_RN <- ts(mort_pre_RN[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,7))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade RN.tiff")
mort_pre_RN %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Natal - RN"
pdf("ACF PACF undifferenced RN.pdf")
acf(as.numeric(mort_pre_RN), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Natal - RN"
pdf("ACF PACF dif_seasonally dif RN.pdf")
acf(diff(as.numeric(mort_pre_RN),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Natal - RN"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_RN, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: (1,0,0) with non-zero mean  

# Checagem dos resíduos do modelo "Natal - RN"
pdf("Resíduos modelo RN.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Natal - RN"
summary(model)
confint(model)




##Análise de Intervenção "Natal - RN"


# Criação e visualização de variável que representa mudança em degrau "Natal - RN"
step <- as.numeric(as.yearmon(time(mort_att_RN))>='Ago 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Natal - RN"
ramp <- append(rep(0,199), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Natal - RN"

# Modelo 1 - sem função de transferência "Natal - RN"
model1 = Arima(mort_att_RN, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(1,0,0))
# Checagem dos resíduos do modelo 1 "Natal - RN"
pdf("Resíduos modelo 1 RN.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Natal - RN"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Natal - RN"
model2 = Arima(mort_att_RN, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(1,0,0),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Natal - RN"
pdf("Resíduos modelo 2 RN.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Natal - RN"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Natal - RN"

model3 = Arima(mort_att_RN, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(1,0,0),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Natal - RN"
pdf("Resíduos modelo 3 RN.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Natal - RN"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Natal - RN"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)







# 12. Paraíba

# Preparação dos dados "João Pessoa - PB"

mort_att_PB <- mort_att %>%
        filter(UF == "PB")

mort_att_PB

## Início das atividades Uber
uber_joaopessoa <-as.Date("2016-09-01") 

# Plotagem dos gráficos para análise visual da série temporal ("João Pessoa - PB")
## Gráfico da série temporal completa “PB”

tiff(filename = "Série PB.tiff")
autoplot(mort_att_PB, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, João Pessoa - PB (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_joaopessoa), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "João Pessoa - PB"
mort_att_PB <- mort_att_PB %>%
        filter(month < yearmonth("2017 Sep"))

mort_att_PB

mort_att_PB %>% features(mort_pad_att, list(media_pre_PB = mean))

mort_att_PB %>% features(mort_pad_att, list(desv_pad_pre_PB = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "João Pessoa - PB"
mort_pre_PB <- mort_att_PB %>%
        filter(month < yearmonth("2016 Sep"))

mort_pre_PB

mort_pre_PB %>% features(mort_pad_att, list(media_pre_PB = mean))

mort_pre_PB %>% features(mort_pad_att, list(desv_pad_pre_PB = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "João Pessoa - PB"
mort_pos_PB <- mort_att_PB %>% filter(month >= yearmonth("2016 Sep"), month < yearmonth("2017 Sep"))

mort_pos_PB

mort_pos_PB %>% features(mort_pad_att, list(media_pos_PB = mean))

mort_pos_PB %>% features(mort_pad_att, list(desv_pad_pos_PB = sd))


## Preparação dados do período pré-Uber "João Pessoa - PB"
mort_pre_PB <- mort_att_PB %>%
        filter(UF == "PB", month < yearmonth("2016 Sep"))

mort_pre_PB


# Conversão de dados para time series object "João Pessoa - PB"

## Período total
mort_att_PB <- ts(mort_att_PB[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,8))

## Pré-uber 
mort_pre_PB <- ts(mort_pre_PB[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,8))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade PB.tiff")
mort_pre_PB %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "João Pessoa - PB"
pdf("ACF PACF undifferenced PB.pdf")
acf(as.numeric(mort_pre_PB), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "João Pessoa - PB"
pdf("ACF PACF dif_seasonally dif PB.pdf")
acf(diff(as.numeric(mort_pre_PB),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "João Pessoa - PB"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_PB, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: (0,1,1)

# Checagem dos resíduos do modelo "João Pessoa - PB"
pdf("Resíduos modelo PB.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "João Pessoa - PB"
summary(model)
confint(model)




##Análise de Intervenção "João Pessoa - PB"


# Criação e visualização de variável que representa mudança em degrau "João Pessoa - PB"
step <- as.numeric(as.yearmon(time(mort_att_PB))>='Set 2016')
step

# Criação e visualização de variável que representa mudança em rampa "João Pessoa - PB"
ramp <- append(rep(0,200), seq(1,12,1))
ramp  


## Criação e testagem de modelos "João Pessoa - PB"

# Modelo 1 - sem função de transferência "João Pessoa - PB"
model1 = Arima(mort_att_PB, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1))
# Checagem dos resíduos do modelo 1 "João Pessoa - PB"
pdf("Resíduos modelo 1 PB.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "João Pessoa - PB"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "João Pessoa - PB"
model2 = Arima(mort_att_PB, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "João Pessoa - PB"
pdf("Resíduos modelo 2 PB.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "João Pessoa - PB"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "João Pessoa - PB"

model3 = Arima(mort_att_PB, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "João Pessoa - PB"
pdf("Resíduos modelo 3 PB.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "João Pessoa - PB"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "João Pessoa - PB"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 13. Pernambuco

# Preparação dos dados "Recife - PE"

mort_att_PE <- mort_att %>%
        filter(UF == "PE")

mort_att_PE

## Início das atividades Uber
uber_recife <-as.Date("2016-03-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Recife - PE")
## Gráfico da série temporal completa “PE”

tiff(filename = "Série PE.tiff")
autoplot(mort_att_PE, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Recife - PE (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_recife), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total "Recife - PE"
mort_att_PE <- mort_att_PE %>%
        filter(month < yearmonth("2017 Mar"))

mort_att_PE

mort_att_PE %>% features(mort_pad_att, list(media_pre_PE = mean))

mort_att_PE %>% features(mort_pad_att, list(desv_pad_pre_PE = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Recife - PE"
mort_pre_PE <- mort_att_PE %>%
        filter(month < yearmonth("2016 Mar"))

mort_pre_PE

mort_pre_PE %>% features(mort_pad_att, list(media_pre_PE = mean))

mort_pre_PE %>% features(mort_pad_att, list(desv_pad_pre_PE = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Recife - PE"
mort_pos_PE <- mort_att_PE %>% filter(month >= yearmonth("2016 Mar"), month < yearmonth("2017 Mar"))

mort_pos_PE

mort_pos_PE %>% features(mort_pad_att, list(media_pos_PE = mean))

mort_pos_PE %>% features(mort_pad_att, list(desv_pad_pos_PE = sd))


## Preparação dados do período pré-Uber "Recife - PE"
mort_pre_PE <- mort_att_PE %>%
        filter(UF == "PE", month < yearmonth("2016 Mar"))

mort_pre_PE


# Conversão de dados para time series object "Recife - PE"

## Período total
mort_att_PE <- ts(mort_att_PE[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,2))

## Pré-uber 
mort_pre_PE <- ts(mort_pre_PE[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,2))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade PE.tiff")
mort_pre_PE %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Recife - PE"
pdf("ACF PACF undifferenced PE.pdf")
acf(as.numeric(mort_pre_PE), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Recife - PE"
pdf("ACF PACF dif_seasonally dif PE.pdf")
acf(diff(as.numeric(mort_pre_PE),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Recife - PE"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_PE, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(2,1,2) with drift 

# Checagem dos resíduos do modelo "Recife - PE"
pdf("Resíduos modelo PE.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Recife - PE"
summary(model)
confint(model)




##Análise de Intervenção "Recife - PE"


# Criação e visualização de variável que representa mudança em degrau "Recife - PE"
step <- as.numeric(as.yearmon(time(mort_att_PE))>='Mar 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Recife - PE"
ramp <- append(rep(0,194), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Recife - PE"

# Modelo 1 - sem função de transferência "Recife - PE"
model1 = Arima(mort_att_PE, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(2,1,2))
# Checagem dos resíduos do modelo 1 "Recife - PE"
pdf("Resíduos modelo 1 PE.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Recife - PE"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Recife - PE"
model2 = Arima(mort_att_PE, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(2,1,2),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Recife - PE"
pdf("Resíduos modelo 2 PE.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Recife - PE"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Recife - PE"

model3 = Arima(mort_att_PE, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(2,1,2),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Recife - PE"
pdf("Resíduos modelo 3 PE.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Recife - PE"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Recife - PE"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)





# 14. Alagoas

# Preparação dos dados "Maceió - AL"

mort_att_AL <- mort_att %>%
        filter(UF == "AL")

mort_att_AL

## Início das atividades Uber
uber_maceio <-as.Date("2016-10-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Maceió - AL")
## Gráfico da série temporal completa “AL”

tiff(filename = "Série AL.tiff")
autoplot(mort_att_AL, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Maceió - AL (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_maceio), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total "Maceió - AL"
mort_att_AL <- mort_att_AL %>%
        filter(month < yearmonth("2017 Oct"))

mort_att_AL

mort_att_AL %>% features(mort_pad_att, list(media_pre_AL = mean))

mort_att_AL %>% features(mort_pad_att, list(desv_pad_pre_AL = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Maceió - AL"
mort_pre_AL <- mort_att_AL %>%
        filter(month < yearmonth("2016 Oct"))

mort_pre_AL

mort_pre_AL %>% features(mort_pad_att, list(media_pre_AL = mean))

mort_pre_AL %>% features(mort_pad_att, list(desv_pad_pre_AL = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Maceió - AL"
mort_pos_AL <- mort_att_AL %>% filter(month >= yearmonth("2016 Oct"), month < yearmonth("2017 Oct"))

mort_pos_AL

mort_pos_AL %>% features(mort_pad_att, list(media_pos_AL = mean))

mort_pos_AL %>% features(mort_pad_att, list(desv_pad_pos_AL = sd))


## Preparação dados do período pré-Uber "Maceió - AL"
mort_pre_AL <- mort_att_AL %>%
        filter(UF == "AL", month < yearmonth("2016 Oct"))

mort_pre_AL


# Conversão de dados para time series object "Maceió - AL"

## Período total
mort_att_AL <- ts(mort_att_AL[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,9))

## Pré-uber 
mort_pre_AL <- ts(mort_pre_AL[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,9))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade AL.tiff")
mort_pre_AL %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Maceió - AL"
pdf("ACF PACF undifferenced AL.pdf")
acf(as.numeric(mort_pre_AL), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Maceió - AL"
pdf("ACF PACF dif_seasonally dif AL.pdf")
acf(diff(as.numeric(mort_pre_AL),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Maceió - AL"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_AL, seasonal=FALSE, max.d=2,
                    stepwise=FALSE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(0,1,4)

# Checagem dos resíduos do modelo "Maceió - AL"
pdf("Resíduos modelo AL.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Maceió - AL"
summary(model)
confint(model)




##Análise de Intervenção "Maceió - AL"


# Criação e visualização de variável que representa mudança em degrau "Maceió - AL"
step <- as.numeric(as.yearmon(time(mort_att_AL))>='Out 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Maceió - AL"
ramp <- append(rep(0,201), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Maceió - AL"

# Modelo 1 - sem função de transferência "Maceió - AL"
model1 = Arima(mort_att_AL, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,4))
# Checagem dos resíduos do modelo 1 "Maceió - AL"
pdf("Resíduos modelo 1 AL.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Maceió - AL"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Maceió - AL"
model2 = Arima(mort_att_AL, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,4),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Maceió - AL"
pdf("Resíduos modelo 2 AL.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Maceió - AL"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Maceió - AL"

model3 = Arima(mort_att_AL, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,4),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Maceió - AL"
pdf("Resíduos modelo 3 AL.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Maceió - AL"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Maceió - AL"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 15. Sergipe

# Preparação dos dados "Aracaju - SE"

mort_att_SE <- mort_att %>%
        filter(UF == "SE")

mort_att_SE

## Início das atividades Uber
uber_aracaju <-as.Date("2016-12-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Aracaju - SE")
## Gráfico da série temporal completa “SE”

tiff(filename = "Série SE.tiff")
autoplot(mort_att_SE, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Aracaju - SE (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_aracaju), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total "Aracaju - SE"
mort_att_SE <- mort_att_SE %>%
        filter(month < yearmonth("2017 Dec"))

mort_att_SE

mort_att_SE %>% features(mort_pad_att, list(media_pre_SE = mean))

mort_att_SE %>% features(mort_pad_att, list(desv_pad_pre_SE = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Aracaju - SE"
mort_pre_SE <- mort_att_SE %>%
        filter(month < yearmonth("2016 Dec"))

mort_pre_SE

mort_pre_SE %>% features(mort_pad_att, list(media_pre_SE = mean))

mort_pre_SE %>% features(mort_pad_att, list(desv_pad_pre_SE = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Aracaju - SE"
mort_pos_SE <- mort_att_SE %>% filter(month >= yearmonth("2016 Dec"), month < yearmonth("2017 Dec"))

mort_pos_SE

mort_pos_SE %>% features(mort_pad_att, list(media_pos_SE = mean))

mort_pos_SE %>% features(mort_pad_att, list(desv_pad_pos_SE = sd))


## Preparação dados do período pré-Uber "Aracaju - SE"
mort_pre_SE <- mort_att_SE %>%
        filter(UF == "SE", month < yearmonth("2016 Dec"))

mort_pre_SE


# Conversão de dados para time series object "Aracaju - SE"

## Período total
mort_att_SE <- ts(mort_att_SE[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,11))

## Pré-uber 
mort_pre_SE <- ts(mort_pre_SE[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,11))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade SE.tiff")
mort_pre_SE %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Aracaju - SE"
pdf("ACF PACF undifferenced SE.pdf")
acf(as.numeric(mort_pre_SE), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Aracaju - SE"
pdf("ACF PACF dif_seasonally dif SE.pdf")
acf(diff(as.numeric(mort_pre_SE),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Aracaju - SE"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_SE, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(2,1,1) 

# Checagem dos resíduos do modelo "Aracaju - SE"
pdf("Resíduos modelo SE.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Aracaju - SE"
summary(model)
confint(model)




##Análise de Intervenção "Aracaju - SE"


# Criação e visualização de variável que representa mudança em degrau "Aracaju - SE"
step <- as.numeric(as.yearmon(time(mort_att_SE))>='Dez 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Aracaju - SE"
ramp <- append(rep(0,203), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Aracaju - SE"

# Modelo 1 - sem função de transferência "Aracaju - SE"
model1 = Arima(mort_att_SE, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(2,1,1))
# Checagem dos resíduos do modelo 1 "Aracaju - SE"
pdf("Resíduos modelo 1 SE.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Aracaju - SE"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Aracaju - SE"
model2 = Arima(mort_att_SE, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(2,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Aracaju - SE"
pdf("Resíduos modelo 2 SE.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Aracaju - SE"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Aracaju - SE"

model3 = Arima(mort_att_SE, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(2,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Aracaju - SE"
pdf("Resíduos modelo 3 SE.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Aracaju - SE"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Aracaju - SE"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 16. Bahia

# Preparação dos dados "Salvador - BA"

mort_att_BA <- mort_att %>%
        filter(UF == "BA")

mort_att_BA

## Início das atividades Uber
uber_salvador <-as.Date("2016-04-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Salvador - BA")
## Gráfico da série temporal completa “BA”

tiff(filename = "Série BA.tiff")
autoplot(mort_att_BA, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Salvador - BA (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_salvador), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total "Salvador - BA"
mort_att_BA <- mort_att_BA %>%
        filter(month < yearmonth("2017 Apr"))

mort_att_BA

mort_att_BA %>% features(mort_pad_att, list(media_pre_BA = mean))

mort_att_BA %>% features(mort_pad_att, list(desv_pad_pre_BA = sd))

# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Salvador - BA"
mort_pre_BA <- mort_att_BA %>%
        filter(month < yearmonth("2016 Apr"))

mort_pre_BA

mort_pre_BA %>% features(mort_pad_att, list(media_pre_BA = mean))

mort_pre_BA %>% features(mort_pad_att, list(desv_pad_pre_BA = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Salvador - BA"
mort_pos_BA <- mort_att_BA %>% filter(month >= yearmonth("2016 Apr"), month < yearmonth("2017 Apr"))

mort_pos_BA

mort_pos_BA %>% features(mort_pad_att, list(media_pos_BA = mean))

mort_pos_BA %>% features(mort_pad_att, list(desv_pad_pos_BA = sd))


## Preparação dados do período pré-Uber "Salvador - BA"
mort_pre_BA <- mort_att_BA %>%
        filter(UF == "BA", month < yearmonth("2016 Apr"))

mort_pre_BA


# Conversão de dados para time series object "Salvador - BA"

## Período total
mort_att_BA <- ts(mort_att_BA[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,3))

## Pré-uber 
mort_pre_BA <- ts(mort_pre_BA[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,3))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade BA.tiff")
mort_pre_BA %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Salvador - BA"
pdf("ACF PACF undifferenced BA.pdf")
acf(as.numeric(mort_pre_BA), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Salvador - BA"
pdf("ACF PACF dif_seasonally dif BA.pdf")
acf(diff(as.numeric(mort_pre_BA),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Salvador - BA"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_BA, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(0,1,1) 

# Checagem dos resíduos do modelo "Salvador - BA"
pdf("Resíduos modelo BA.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Salvador - BA"
summary(model)
confint(model)




##Análise de Intervenção "Salvador - BA"


# Criação e visualização de variável que representa mudança em degrau "Salvador - BA"
step <- as.numeric(as.yearmon(time(mort_att_BA))>='Abr 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Salvador - BA"
ramp <- append(rep(0,195), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Salvador - BA"

# Modelo 1 - sem função de transferência "Salvador - BA"
model1 = Arima(mort_att_BA, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1))
# Checagem dos resíduos do modelo 1 "Salvador - BA"
pdf("Resíduos modelo 1 BA.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Salvador - BA"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Salvador - BA"
model2 = Arima(mort_att_BA, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Salvador - BA"
pdf("Resíduos modelo 2 BA.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Salvador - BA"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Salvador - BA"

model3 = Arima(mort_att_BA, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Salvador - BA"
pdf("Resíduos modelo 3 BA.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Salvador - BA"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Salvador - BA"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 17. Minas Gerais

# Preparação dos dados "Belo Horizonte - MG"

mort_att_MG <- mort_att %>%
        filter(UF == "MG")

mort_att_MG

## Início das atividades Uber
uber_belohorizonte <-as.Date("2014-09-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Belo Horizonte - MG")
## Gráfico da série temporal completa “MG”

tiff(filename = "Série MG.tiff")
autoplot(mort_att_MG, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Belo Horizonte - MG (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_belohorizonte), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total "Belo Horizonte - MG"
mort_att_MG <- mort_att_MG %>%
        filter(month < yearmonth("2015 Sep"))

mort_att_MG

mort_att_MG %>% features(mort_pad_att, list(media_pre_MG = mean))

mort_att_MG %>% features(mort_pad_att, list(desv_pad_pre_MG = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Belo Horizonte - MG"
mort_pre_MG <- mort_att_MG %>%
        filter(month < yearmonth("2014 Sep"))

mort_pre_MG

mort_pre_MG %>% features(mort_pad_att, list(media_pre_MG = mean))

mort_pre_MG %>% features(mort_pad_att, list(desv_pad_pre_MG = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Belo Horizonte - MG"
mort_pos_MG <- mort_att_MG %>% filter(month >= yearmonth("2014 Sep"), month < yearmonth("2015 Sep"))

mort_pos_MG

mort_pos_MG %>% features(mort_pad_att, list(media_pos_MG = mean))

mort_pos_MG %>% features(mort_pad_att, list(desv_pad_pos_MG = sd))


## Preparação dados do período pré-Uber "Belo Horizonte - MG"
mort_pre_MG <- mort_att_MG %>%
        filter(UF == "MG", month < yearmonth("2014 Sep"))

mort_pre_MG


# Conversão de dados para time series object "Belo Horizonte - MG"

## Período total
mort_att_MG <- ts(mort_att_MG[,2], frequency=12, start=c(2000,1), 
                  end = c(2015,8))

## Pré-uber 
mort_pre_MG <- ts(mort_pre_MG[,2], frequency=12, start=c(2000,1), 
                  end = c(2014,8))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade MG.tiff")
mort_pre_MG %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Belo Horizonte - MG"
pdf("ACF PACF undifferenced MG.pdf")
acf(as.numeric(mort_pre_MG), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Belo Horizonte - MG"
pdf("ACF PACF dif_seasonally dif MG.pdf")
acf(diff(as.numeric(mort_pre_MG),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Belo Horizonte - MG"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_MG, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(0,1,1)  

# Checagem dos resíduos do modelo "Belo Horizonte - MG"
pdf("Resíduos modelo MG.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Belo Horizonte - MG"
summary(model)
confint(model)




##Análise de Intervenção "Belo Horizonte - MG"


# Criação e visualização de variável que representa mudança em degrau "Belo Horizonte - MG"
step <- as.numeric(as.yearmon(time(mort_att_MG))>='Set 2014')
step

# Criação e visualização de variável que representa mudança em rampa "Belo Horizonte - MG"
ramp <- append(rep(0,176), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Belo Horizonte - MG"

# Modelo 1 - sem função de transferência "Belo Horizonte - MG"
model1 = Arima(mort_att_MG, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1))
# Checagem dos resíduos do modelo 1 "Belo Horizonte - MG"
pdf("Resíduos modelo 1 MG.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Belo Horizonte - MG"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Belo Horizonte - MG"
model2 = Arima(mort_att_MG, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Belo Horizonte - MG"
pdf("Resíduos modelo 2 MG.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Belo Horizonte - MG"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Belo Horizonte - MG"

model3 = Arima(mort_att_MG, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Belo Horizonte - MG"
pdf("Resíduos modelo 3 MG.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Belo Horizonte - MG"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Belo Horizonte - MG"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model3$residuals)





# 18. Espírito Santo
# Preparação dos dados "Vitória - ES"

mort_att_ES <- mort_att %>%
        filter(UF == "ES")

mort_att_ES

## Início das atividades Uber
uber_vitoria <-as.Date("2016-09-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Vitória - ES")
## Gráfico da série temporal completa “ES”

tiff(filename = "Série ES.tiff")
autoplot(mort_att_ES, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Vitória - ES (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_vitoria), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total "Vitória - ES"
mort_att_ES <- mort_att_ES %>%
        filter(month < yearmonth("2017 Sep"))

mort_att_ES

mort_att_ES %>% features(mort_pad_att, list(media_pre_ES = mean))

mort_att_ES %>% features(mort_pad_att, list(desv_pad_pre_ES = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Vitória - ES"
mort_pre_ES <- mort_att_ES %>%
        filter(month < yearmonth("2016 Sep"))

mort_pre_ES

mort_pre_ES %>% features(mort_pad_att, list(media_pre_ES = mean))

mort_pre_ES %>% features(mort_pad_att, list(desv_pad_pre_ES = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Vitória - ES"
mort_pos_ES <- mort_att_ES %>% filter(month >= yearmonth("2016 Sep"), month < yearmonth("2017 Sep"))

mort_pos_ES

mort_pos_ES %>% features(mort_pad_att, list(media_pos_ES = mean))

mort_pos_ES %>% features(mort_pad_att, list(desv_pad_pos_ES = sd))


## Preparação dados do período pré-Uber "Vitória - ES"
mort_pre_ES <- mort_att_ES %>%
        filter(UF == "ES", month < yearmonth("2016 Sep"))

mort_pre_ES


# Conversão de dados para time series object "Vitória - ES"

## Período total
mort_att_ES <- ts(mort_att_ES[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,8))

## Pré-uber 
mort_pre_ES <- ts(mort_pre_ES[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,8))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade ES.tiff")
mort_pre_ES %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Vitória - ES"
pdf("ACF PACF undifferenced ES.pdf")
acf(as.numeric(mort_pre_ES), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Vitória - ES"
pdf("ACF PACF dif_seasonally dif ES.pdf")
acf(diff(as.numeric(mort_pre_ES),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Vitória - ES"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_ES, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(2,1,1)

# Checagem dos resíduos do modelo "Vitória - ES"
pdf("Resíduos modelo ES.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Vitória - ES"
summary(model)
confint(model)




##Análise de Intervenção "Vitória - ES"


# Criação e visualização de variável que representa mudança em degrau "Vitória - ES"
step <- as.numeric(as.yearmon(time(mort_att_ES))>='Set 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Vitória - ES"
ramp <- append(rep(0,200), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Vitória - ES"

# Modelo 1 - sem função de transferência "Vitória - ES"
model1 = Arima(mort_att_ES, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(2,1,1))
# Checagem dos resíduos do modelo 1 "Vitória - ES"
pdf("Resíduos modelo 1 ES.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Vitória - ES"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Vitória - ES"
model2 = Arima(mort_att_ES, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(2,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Vitória - ES"
pdf("Resíduos modelo 2 ES.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Vitória - ES"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Vitória - ES"

model3 = Arima(mort_att_ES, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(2,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Vitória - ES"
pdf("Resíduos modelo 3 ES.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Vitória - ES"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Vitória - ES"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 19. Rio de Janeiro

# Preparação dos dados "Rio de Janeiro - RJ"

mort_att_RJ <- mort_att %>%
        filter(UF == "RJ")

mort_att_RJ

## Início das atividades Uber
uber_riodejaneiro <-as.Date("2014-05-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Rio de Janeiro - RJ")
## Gráfico da série temporal completa “RJ”

tiff(filename = "Série RJ.tiff")
autoplot(mort_att_RJ, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Rio de Janeiro - RJ (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_riodejaneiro), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total "Rio de Janeiro - RJ"
mort_att_RJ <- mort_att_RJ %>%
        filter(month < yearmonth("2015 May"))

mort_att_RJ

mort_att_RJ %>% features(mort_pad_att, list(media_pre_RJ = mean))

mort_att_RJ %>% features(mort_pad_att, list(desv_pad_pre_RJ = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Rio de Janeiro - RJ"
mort_pre_RJ <- mort_att_RJ %>%
        filter(month < yearmonth("2014 May"))

mort_pre_RJ

mort_pre_RJ %>% features(mort_pad_att, list(media_pre_RJ = mean))

mort_pre_RJ %>% features(mort_pad_att, list(desv_pad_pre_RJ = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Rio de Janeiro - RJ"
mort_pos_RJ <- mort_att_RJ %>% filter(month >= yearmonth("2014 May"), month < yearmonth("2015 May"))

mort_pos_RJ

mort_pos_RJ %>% features(mort_pad_att, list(media_pos_RJ = mean))

mort_pos_RJ %>% features(mort_pad_att, list(desv_pad_pos_RJ = sd))


## Preparação dados do período pré-Uber "Rio de Janeiro - RJ"
mort_pre_RJ <- mort_att_RJ %>%
        filter(UF == "RJ", month < yearmonth("2014 May"))

mort_pre_RJ


# Conversão de dados para time series object "Rio de Janeiro - RJ"

## Período total
mort_att_RJ <- ts(mort_att_RJ[,2], frequency=12, start=c(2000,1), 
                  end = c(2015,4))

## Pré-uber 
mort_pre_RJ <- ts(mort_pre_RJ[,2], frequency=12, start=c(2000,1), 
                  end = c(2014,4))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade RJ.tiff")
mort_pre_RJ %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Rio de Janeiro - RJ"
pdf("ACF PACF undifferenced RJ.pdf")
acf(as.numeric(mort_pre_RJ), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Rio de Janeiro - RJ"
pdf("ACF PACF dif_seasonally dif RJ.pdf")
acf(diff(as.numeric(mort_pre_RJ),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Rio de Janeiro - RJ"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_RJ, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,1,2)

# Checagem dos resíduos do modelo "Rio de Janeiro - RJ"
pdf("Resíduos modelo RJ.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Rio de Janeiro - RJ"
summary(model)
confint(model)




##Análise de Intervenção "Rio de Janeiro - RJ"


# Criação e visualização de variável que representa mudança em degrau "Rio de Janeiro - RJ"
step <- as.numeric(as.yearmon(time(mort_att_RJ))>='Mai 2014')
step

# Criação e visualização de variável que representa mudança em rampa "Rio de Janeiro - RJ"
ramp <- append(rep(0,172), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Rio de Janeiro - RJ"

# Modelo 1 - sem função de transferência "Rio de Janeiro - RJ"
model1 = Arima(mort_att_RJ, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2))
# Checagem dos resíduos do modelo 1 "Rio de Janeiro - RJ"
pdf("Resíduos modelo 1 RJ.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Rio de Janeiro - RJ"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Rio de Janeiro - RJ"
model2 = Arima(mort_att_RJ, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Rio de Janeiro - RJ"
pdf("Resíduos modelo 2 RJ.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Rio de Janeiro - RJ"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Rio de Janeiro - RJ"

model3 = Arima(mort_att_RJ, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Rio de Janeiro - RJ"
pdf("Resíduos modelo 3 RJ.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Rio de Janeiro - RJ"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Rio de Janeiro - RJ"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model3$residuals)




# 20. São Paulo

# Preparação dos dados "São Paulo - SP"

mort_att_SP <- mort_att %>%
        filter(UF == "SP")

mort_att_SP

## Início das atividades Uber
uber_saopaulo <-as.Date("2014-06-01") 

# Plotagem dos gráficos para análise visual da série temporal ("São Paulo - SP")
## Gráfico da série temporal completa “SP”

tiff(filename = "Série SP.tiff")
autoplot(mort_att_SP, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, São Paulo - SP (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_saopaulo), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total "São Paulo - SP"
mort_att_SP <- mort_att_SP %>%
        filter(month < yearmonth("2015 Jun"))

mort_att_SP

mort_att_SP %>% features(mort_pad_att, list(media_pre_SP = mean))

mort_att_SP %>% features(mort_pad_att, list(desv_pad_pre_SP = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "São Paulo - SP"
mort_pre_SP <- mort_att_SP %>%
        filter(month < yearmonth("2014 Jun"))

mort_pre_SP

mort_pre_SP %>% features(mort_pad_att, list(media_pre_SP = mean))

mort_pre_SP %>% features(mort_pad_att, list(desv_pad_pre_SP = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "São Paulo - SP"
mort_pos_SP <- mort_att_SP %>% filter(month >= yearmonth("2014 Jun"), month < yearmonth("2015 Jun"))

mort_pos_SP

mort_pos_SP %>% features(mort_pad_att, list(media_pos_SP = mean))

mort_pos_SP %>% features(mort_pad_att, list(desv_pad_pos_SP = sd))


## Preparação dados do período pré-Uber "São Paulo - SP"
mort_pre_SP <- mort_att_SP %>%
        filter(UF == "SP", month < yearmonth("2014 Jun"))

mort_pre_SP


# Conversão de dados para time series object "São Paulo - SP"

## Período total
mort_att_SP <- ts(mort_att_SP[,2], frequency=12, start=c(2000,1), 
                  end = c(2015,5))

## Pré-uber 
mort_pre_SP <- ts(mort_pre_SP[,2], frequency=12, start=c(2000,1), 
                  end = c(2014,5))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade SP.tiff")
mort_pre_SP %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "São Paulo - SP"
pdf("ACF PACF undifferenced SP.pdf")
acf(as.numeric(mort_pre_SP), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "São Paulo - SP"
pdf("ACF PACF dif_seasonally dif SP.pdf")
acf(diff(as.numeric(mort_pre_SP),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "São Paulo - SP"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_SP, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,0,1)    

# Checagem dos resíduos do modelo "São Paulo - SP"
pdf("Resíduos modelo SP.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "São Paulo - SP"
summary(model)
confint(model)




##Análise de Intervenção "São Paulo - SP"


# Criação e visualização de variável que representa mudança em degrau "São Paulo - SP"
step <- as.numeric(as.yearmon(time(mort_att_SP))>='Jun 2014')
step

# Criação e visualização de variável que representa mudança em rampa "São Paulo - SP"
ramp <- append(rep(0,173), seq(1,12,1))
ramp  


## Criação e testagem de modelos "São Paulo - SP"

# Modelo 1 - sem função de transferência "São Paulo - SP"
model1 = Arima(mort_att_SP, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,0,1))
# Checagem dos resíduos do modelo 1 "São Paulo - SP"
pdf("Resíduos modelo 1 SP.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "São Paulo - SP"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "São Paulo - SP"
model2 = Arima(mort_att_SP, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,0,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "São Paulo - SP"
pdf("Resíduos modelo 2 SP.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "São Paulo - SP"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "São Paulo - SP"

model3 = Arima(mort_att_SP, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,0,1), 
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "São Paulo - SP"
pdf("Resíduos modelo 3 SP.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "São Paulo - SP"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "São Paulo - SP"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)





# 21. Paraná

# Preparação dos dados "Curitiba - PR"

mort_att_PR <- mort_att %>%
        filter(UF == "PR")

mort_att_PR

## Início das atividades Uber
uber_curitiba <-as.Date("2016-03-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Curitiba - PR")
## Gráfico da série temporal completa “PR”

tiff(filename = "Série PR.tiff")
autoplot(mort_att_PR, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Curitiba - PR (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_curitiba), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total "Curitiba - PR"
mort_att_PR <- mort_att_PR %>%
        filter(month < yearmonth("2017 Mar"))

mort_att_PR

mort_att_PR %>% features(mort_pad_att, list(media_pre_PR = mean))

mort_att_PR %>% features(mort_pad_att, list(desv_pad_pre_PR = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Curitiba - PR"
mort_pre_PR <- mort_att_PR %>%
        filter(month < yearmonth("2016 Mar"))

mort_pre_PR

mort_pre_PR %>% features(mort_pad_att, list(media_pre_PR = mean))

mort_pre_PR %>% features(mort_pad_att, list(desv_pad_pre_PR = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Curitiba - PR"
mort_pos_PR <- mort_att_PR %>% filter(month >= yearmonth("2016 Mar"), month < yearmonth("2017 Mar"))

mort_pos_PR

mort_pos_PR %>% features(mort_pad_att, list(media_pos_PR = mean))

mort_pos_PR %>% features(mort_pad_att, list(desv_pad_pos_PR = sd))


## Preparação dados do período pré-Uber "Curitiba - PR"
mort_pre_PR <- mort_att_PR %>%
        filter(UF == "PR", month < yearmonth("2016 Mar"))

mort_pre_PR


# Conversão de dados para time series object "Curitiba - PR"

## Período total
mort_att_PR <- ts(mort_att_PR[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,2))

## Pré-uber 
mort_pre_PR <- ts(mort_pre_PR[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,2))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade PR.tiff")
mort_pre_PR %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Curitiba - PR"
pdf("ACF PACF undifferenced PR.pdf")
acf(as.numeric(mort_pre_PR), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Curitiba - PR"
pdf("ACF PACF dif_seasonally dif PR.pdf")
acf(diff(as.numeric(mort_pre_PR),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Curitiba - PR"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_PR, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(2,1,1)with drift   

# Checagem dos resíduos do modelo "Curitiba - PR"
pdf("Resíduos modelo PR.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Curitiba - PR"
summary(model)
confint(model)




##Análise de Intervenção "Curitiba - PR"


# Criação e visualização de variável que representa mudança em degrau "Curitiba - PR"
step <- as.numeric(as.yearmon(time(mort_att_PR))>='Mar 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Curitiba - PR"
ramp <- append(rep(0,194), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Curitiba - PR"

# Modelo 1 - sem função de transferência "Curitiba - PR"
model1 = Arima(mort_att_PR, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(2,1,1))
# Checagem dos resíduos do modelo 1 "Curitiba - PR"
pdf("Resíduos modelo 1 PR.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Curitiba - PR"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Curitiba - PR"
model2 = Arima(mort_att_PR, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(2,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Curitiba - PR"
pdf("Resíduos modelo 2 PR.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Curitiba - PR"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Curitiba - PR"

model3 = Arima(mort_att_PR, 
               include.drift = TRUE,
               include.mean = FALSE,
               order = c(2,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Curitiba - PR"
pdf("Resíduos modelo 3 PR.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Curitiba - PR"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Curitiba - PR"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 22. Santa Catarina


# Preparação dos dados "Florianópolis - SC"

mort_att_SC <- mort_att %>%
        filter(UF == "SC")

mort_att_SC

## Início das atividades Uber
uber_florianopolis <-as.Date("2016-09-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Florianópolis - SC")
## Gráfico da série temporal completa “SC”

tiff(filename = "Série SC.tiff")
autoplot(mort_att_SC, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Florianópolis - SC (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_florianopolis), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total "Florianópolis - SC"
mort_att_SC <- mort_att_SC %>%
        filter(month < yearmonth("2017 Sep"))

mort_att_SC

mort_att_SC %>% features(mort_pad_att, list(media_pre_SC = mean))

mort_att_SC %>% features(mort_pad_att, list(desv_pad_pre_SC = sd))

# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Florianópolis - SC"
mort_pre_SC <- mort_att_SC %>%
        filter(month < yearmonth("2016 Sep"))

mort_pre_SC

mort_pre_SC %>% features(mort_pad_att, list(media_pre_SC = mean))

mort_pre_SC %>% features(mort_pad_att, list(desv_pad_pre_SC = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Florianópolis - SC"
mort_pos_SC <- mort_att_SC %>% filter(month >= yearmonth("2016 Sep"), month < yearmonth("2017 Sep"))

mort_pos_SC

mort_pos_SC %>% features(mort_pad_att, list(media_pos_SC = mean))

mort_pos_SC %>% features(mort_pad_att, list(desv_pad_pos_SC = sd))


## Preparação dados do período pré-Uber "Florianópolis - SC"
mort_pre_SC <- mort_att_SC %>%
        filter(UF == "SC", month < yearmonth("2016 Sep"))

mort_pre_SC


# Conversão de dados para time series object "Florianópolis - SC"

## Período total
mort_att_SC <- ts(mort_att_SC[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,8))

## Pré-uber 
mort_pre_SC <- ts(mort_pre_SC[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,8))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade SC.tiff")
mort_pre_SC %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Florianópolis - SC"
pdf("ACF PACF undifferenced SC.pdf")
acf(as.numeric(mort_pre_SC), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Florianópolis - SC"
pdf("ACF PACF dif_seasonally dif SC.pdf")
acf(diff(as.numeric(mort_pre_SC),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Florianópolis - SC"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_SC, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(2,1,4)   

# Checagem dos resíduos do modelo "Florianópolis - SC"
pdf("Resíduos modelo SC.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Florianópolis - SC"
summary(model)
confint(model)




##Análise de Intervenção "Florianópolis - SC"


# Criação e visualização de variável que representa mudança em degrau "Florianópolis - SC"
step <- as.numeric(as.yearmon(time(mort_att_SC))>='Set 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Florianópolis - SC"
ramp <- append(rep(0,200), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Florianópolis - SC"

# Modelo 1 - sem função de transferência "Florianópolis - SC"
model1 = Arima(mort_att_SC, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(2,1,4))
# Checagem dos resíduos do modelo 1 "Florianópolis - SC"
pdf("Resíduos modelo 1 SC.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Florianópolis - SC"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Florianópolis - SC"
model2 = Arima(mort_att_SC, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(2,1,4),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Florianópolis - SC"
pdf("Resíduos modelo 2 SC.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Florianópolis - SC"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Florianópolis - SC"

model3 = Arima(mort_att_SC, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(2,1,4),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Florianópolis - SC"
pdf("Resíduos modelo 3 SC.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Florianópolis - SC"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Florianópolis - SC"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 23. Rio Grande do Sul

# Preparação dos dados "Porto Alegre - RS"

mort_att_RS <- mort_att %>%
        filter(UF == "RS")

mort_att_RS

## Início das atividades Uber
uber_portoalegre <-as.Date("2015-11-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Porto Alegre - RS")
## Gráfico da série temporal completa “RS”

tiff(filename = "Série RS.tiff")
autoplot(mort_att_RS, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Porto Alegre - RS (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_portoalegre), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Porto Alegre - RS"
mort_att_RS <- mort_att_RS %>%
        filter(month < yearmonth("2016 Nov"))

mort_att_RS

mort_att_RS %>% features(mort_pad_att, list(media_pre_RS = mean))

mort_att_RS %>% features(mort_pad_att, list(desv_pad_pre_RS = sd))

# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Porto Alegre - RS"
mort_pre_RS <- mort_att_RS %>%
        filter(month < yearmonth("2015 Nov"))

mort_pre_RS

mort_pre_RS %>% features(mort_pad_att, list(media_pre_RS = mean))

mort_pre_RS %>% features(mort_pad_att, list(desv_pad_pre_RS = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Porto Alegre - RS"
mort_pos_RS <- mort_att_RS %>% filter(month >= yearmonth("2015 Nov"), month < yearmonth("2016 Nov"))

mort_pos_RS

mort_pos_RS %>% features(mort_pad_att, list(media_pos_RS = mean))

mort_pos_RS %>% features(mort_pad_att, list(desv_pad_pos_RS = sd))


## Preparação dados do período pré-Uber "Porto Alegre - RS"
mort_pre_RS <- mort_att_RS %>%
        filter(UF == "RS", month < yearmonth("2015 Nov"))

mort_pre_RS


# Conversão de dados para time series object "Porto Alegre - RS"

## Período total
mort_att_RS <- ts(mort_att_RS[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,10))

## Pré-uber 
mort_pre_RS <- ts(mort_pre_RS[,2], frequency=12, start=c(2000,1), 
                  end = c(2015,10))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade RS.tiff")
mort_pre_RS %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Porto Alegre - RS"
pdf("ACF PACF undifferenced RS.pdf")
acf(as.numeric(mort_pre_RS), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Porto Alegre - RS"
pdf("ACF PACF dif_seasonally dif RS.pdf")
acf(diff(as.numeric(mort_pre_RS),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Porto Alegre - RS"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_RS, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,1,5)   

# Checagem dos resíduos do modelo "Porto Alegre - RS"
pdf("Resíduos modelo RS.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Porto Alegre - RS"
summary(model)
confint(model)




##Análise de Intervenção "Porto Alegre - RS"


# Criação e visualização de variável que representa mudança em degrau "Porto Alegre - RS"
step <- as.numeric(as.yearmon(time(mort_att_RS))>='Nov 2015')
step

# Criação e visualização de variável que representa mudança em rampa "Porto Alegre - RS"
ramp <- append(rep(0,190), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Porto Alegre - RS"

# Modelo 1 - sem função de transferência "Porto Alegre - RS"
model1 = Arima(mort_att_RS, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(3,1,1))
# Checagem dos resíduos do modelo 1 "Porto Alegre - RS"
pdf("Resíduos modelo 1 RS.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Porto Alegre - RS"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Porto Alegre - RS"
model2 = Arima(mort_att_RS, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(3,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Porto Alegre - RS"
pdf("Resíduos modelo 2 RS.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Porto Alegre - RS"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Porto Alegre - RS"

model3 = Arima(mort_att_RS, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(3,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Porto Alegre - RS"
pdf("Resíduos modelo 3 RS.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Porto Alegre - RS"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Porto Alegre - RS"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)



# 24. Mato Grosso do Sul



# Preparação dos dados "Campo Grande - MS"

mort_att_MS <- mort_att %>%
        filter(UF == "MS")

mort_att_MS

## Início das atividades Uber
uber_campogrande <-as.Date("2016-09-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Campo Grande - MS")
## Gráfico da série temporal completa “MS”

tiff(filename = "Série MS.tiff")
autoplot(mort_att_MS, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Campo Grande - MS (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_campogrande), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Campo Grande - MS"
mort_att_MS <- mort_att_MS %>%
        filter(month < yearmonth("2017 Sep"))

mort_att_MS

mort_att_MS %>% features(mort_pad_att, list(media_pre_MS = mean))

mort_att_MS %>% features(mort_pad_att, list(desv_pad_pre_MS = sd))

# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Campo Grande - MS"
mort_pre_MS <- mort_att_MS %>%
        filter(month < yearmonth("2016 Sep"))

mort_pre_MS

mort_pre_MS %>% features(mort_pad_att, list(media_pre_MS = mean))

mort_pre_MS %>% features(mort_pad_att, list(desv_pad_pre_MS = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Campo Grande - MS"
mort_pos_MS <- mort_att_MS %>% filter(month >= yearmonth("2016 Sep"), month < yearmonth("2017 Sep"))

mort_pos_MS

mort_pos_MS %>% features(mort_pad_att, list(media_pos_MS = mean))

mort_pos_MS %>% features(mort_pad_att, list(desv_pad_pos_MS = sd))


## Preparação dados do período pré-Uber "Campo Grande - MS"
mort_pre_MS <- mort_att_MS %>%
        filter(UF == "MS", month < yearmonth("2016 Sep"))

mort_pre_MS


# Conversão de dados para time series object "Campo Grande - MS"

## Período total
mort_att_MS <- ts(mort_att_MS[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,8))

## Pré-uber 
mort_pre_MS <- ts(mort_pre_MS[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,8))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade MS.tiff")
mort_pre_MS %>% ggseasonplot()
dev.off()



# Visualização de gráficos ACF/PACF de dados não diferenciados "Campo Grande - MS"
pdf("ACF PACF undifferenced MS.pdf")
acf(as.numeric(mort_pre_MS), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Campo Grande - MS"
pdf("ACF PACF dif_seasonally dif MS.pdf")
acf(diff(as.numeric(mort_pre_MS),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Campo Grande - MS"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_MS, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,1,2)     

# Checagem dos resíduos do modelo "Campo Grande - MS"
pdf("Resíduos modelo MS.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Campo Grande - MS"
summary(model)
confint(model)




##Análise de Intervenção "Campo Grande - MS"


# Criação e visualização de variável que representa mudança em degrau "Campo Grande - MS"
step <- as.numeric(as.yearmon(time(mort_att_MS))>='Set 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Campo Grande - MS"
ramp <- append(rep(0,200), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Campo Grande - MS"

# Modelo 1 - sem função de transferência "Campo Grande - MS"
model1 = Arima(mort_att_MS, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2))
# Checagem dos resíduos do modelo 1 "Campo Grande - MS"
pdf("Resíduos modelo 1 MS.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Campo Grande - MS"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Campo Grande - MS"
model2 = Arima(mort_att_MS, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Campo Grande - MS"
pdf("Resíduos modelo 2 MS.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Campo Grande - MS"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Campo Grande - MS"

model3 = Arima(mort_att_MS, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Campo Grande - MS"
pdf("Resíduos modelo 3 MS.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Campo Grande - MS"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Campo Grande - MS"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 25. Mato Grosso



# Preparação dos dados "Cuiabá - MT"

mort_att_MT <- mort_att %>%
        filter(UF == "MT")

mort_att_MT

## Início das atividades Uber
uber_cuiaba <-as.Date("2016-11-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Cuiabá - MT")
## Gráfico da série temporal completa “MT”

tiff(filename = "Série MT.tiff")
autoplot(mort_att_MT, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Cuiabá - MT (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_cuiaba), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Cuiabá - MT"
mort_att_MT <- mort_att_MT %>%
        filter(month < yearmonth("2017 Nov"))

mort_att_MT

mort_att_MT %>% features(mort_pad_att, list(media_pre_MT = mean))

mort_att_MT %>% features(mort_pad_att, list(desv_pad_pre_MT = sd))

# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Cuiabá - MT"
mort_pre_MT <- mort_att_MT %>%
        filter(month < yearmonth("2016 Nov"))

mort_pre_MT

mort_pre_MT %>% features(mort_pad_att, list(media_pre_MT = mean))

mort_pre_MT %>% features(mort_pad_att, list(desv_pad_pre_MT = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Cuiabá - MT"
mort_pos_MT <- mort_att_MT %>% filter(month >= yearmonth("2016 Nov"), month < yearmonth("2017 Nov"))

mort_pos_MT

mort_pos_MT %>% features(mort_pad_att, list(media_pos_MT = mean))

mort_pos_MT %>% features(mort_pad_att, list(desv_pad_pos_MT = sd))


## Preparação dados do período pré-Uber "Cuiabá - MT"
mort_pre_MT <- mort_att_MT %>%
        filter(UF == "MT", month < yearmonth("2016 Nov"))

mort_pre_MT


# Conversão de dados para time series object "Cuiabá - MT"

## Período total
mort_att_MT <- ts(mort_att_MT[,2], frequency=12, start=c(2000,1), 
                  end = c(2017,10))

## Pré-uber 
mort_pre_MT <- ts(mort_pre_MT[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,10))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade MT.tiff")
mort_pre_MT %>% ggseasonplot()
dev.off()



# Visualização de gráficos ACF/PACF de dados não diferenciados "Cuiabá - MT"
pdf("ACF PACF undifferenced MT.pdf")
acf(as.numeric(mort_pre_MT), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Cuiabá - MT"
pdf("ACF PACF dif_seasonally dif MT.pdf")
acf(diff(as.numeric(mort_pre_MT),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Cuiabá - MT"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_MT, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,0,1) with non-zero mean 


# Checagem dos resíduos do modelo "Cuiabá - MT"
pdf("Resíduos modelo MT.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Cuiabá - MT"
summary(model)
confint(model)




##Análise de Intervenção "Cuiabá - MT"


# Criação e visualização de variável que representa mudança em degrau "Cuiabá - MT"
step <- as.numeric(as.yearmon(time(mort_att_MT))>='Nov 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Cuiabá - MT"
ramp <- append(rep(0,202), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Cuiabá - MT"

# Modelo 1 - sem função de transferência "Cuiabá - MT"
model1 = Arima(mort_att_MT, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(1,0,1),
               method = c("ML"))
# Checagem dos resíduos do modelo 1 "Cuiabá - MT"
pdf("Resíduos modelo 1 MT.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Cuiabá - MT"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Cuiabá - MT"
model2 = Arima(mort_att_MT, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(1,0,1),
               xreg = cbind(step),
               method = c("ML"))
# Checagem dos resíduos do modelo 2 "Cuiabá - MT"
pdf("Resíduos modelo 2 MT.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Cuiabá - MT"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Cuiabá - MT"

model3 = Arima(mort_att_MT, 
               include.drift = FALSE,
               include.mean = TRUE,
               order = c(1,0,1),
               xreg = cbind(ramp),
               method = c("ML"))
# Checagem dos resíduos do modelo 3 "Cuiabá - MT"
pdf("Resíduos modelo 3 MT.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Cuiabá - MT"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Cuiabá - MT"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)


# 26. Goiás



# Preparação dos dados "Goiânia - GO"

mort_att_GO <- mort_att %>%
        filter(UF == "GO")

mort_att_GO

## Início das atividades Uber
uber_goiania <-as.Date("2016-01-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Goiânia - GO")
## Gráfico da série temporal completa “GO”

tiff(filename = "Série GO.tiff")
autoplot(mort_att_GO, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Goiânia - GO (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_goiania), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Goiânia - GO"
mort_att_GO <- mort_att_GO %>%
        filter(month < yearmonth("2017 Jan"))

mort_att_GO

mort_att_GO %>% features(mort_pad_att, list(media_pre_GO = mean))

mort_att_GO %>% features(mort_pad_att, list(desv_pad_pre_GO = sd))



# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Goiânia - GO"
mort_pre_GO <- mort_att_GO %>%
        filter(month < yearmonth("2016 Jan"))

mort_pre_GO

mort_pre_GO %>% features(mort_pad_att, list(media_pre_GO = mean))

mort_pre_GO %>% features(mort_pad_att, list(desv_pad_pre_GO = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Goiânia - GO"
mort_pos_GO <- mort_att_GO %>% filter(month >= yearmonth("2016 Jan"), month < yearmonth("2017 Jan"))

mort_pos_GO

mort_pos_GO %>% features(mort_pad_att, list(media_pos_GO = mean))

mort_pos_GO %>% features(mort_pad_att, list(desv_pad_pos_GO = sd))


## Preparação dados do período pré-Uber "Goiânia - GO"
mort_pre_GO <- mort_att_GO %>%
        filter(UF == "GO", month < yearmonth("2016 Jan"))

mort_pre_GO


# Conversão de dados para time series object "Goiânia - GO"

## Período total
mort_att_GO <- ts(mort_att_GO[,2], frequency=12, start=c(2000,1), 
                  end = c(2016,12))

## Pré-uber 
mort_pre_GO <- ts(mort_pre_GO[,2], frequency=12, start=c(2000,1), 
                  end = c(2015,12))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade GO.tiff")
mort_pre_GO %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Goiânia - GO"
pdf("ACF PACF undifferenced GO.pdf")
acf(as.numeric(mort_pre_GO), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Goiânia - GO"
pdf("ACF PACF dif_seasonally dif GO.pdf")
acf(diff(as.numeric(mort_pre_GO),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Goiânia - GO"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_GO, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(0,1,1) 

# Checagem dos resíduos do modelo "Goiânia - GO"
pdf("Resíduos modelo GO.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Goiânia - GO"
summary(model)
confint(model)




##Análise de Intervenção "Goiânia - GO"


# Criação e visualização de variável que representa mudança em degrau "Goiânia - GO"
step <- as.numeric(as.yearmon(time(mort_att_GO))>='Jan 2016')
step

# Criação e visualização de variável que representa mudança em rampa "Goiânia - GO"
ramp <- append(rep(0,192), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Goiânia - GO"

# Modelo 1 - sem função de transferência "Goiânia - GO"
model1 = Arima(mort_att_GO, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1))
# Checagem dos resíduos do modelo 1 "Goiânia - GO"
pdf("Resíduos modelo 1 GO.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Goiânia - GO"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Goiânia - GO"
model2 = Arima(mort_att_GO, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Goiânia - GO"
pdf("Resíduos modelo 2 GO.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Goiânia - GO"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Goiânia - GO"

model3 = Arima(mort_att_GO, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(0,1,1),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Goiânia - GO"
pdf("Resíduos modelo 3 GO.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Goiânia - GO"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Goiânia - GO"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 27. Distrito Federal


# Preparação dos dados "Brasília - DF"

mort_att_DF <- mort_att %>%
        filter(UF == "DF")

mort_att_DF

## Início das atividades Uber
uber_brasilia <-as.Date("2014-11-01") 

# Plotagem dos gráficos para análise visual da série temporal ("Brasília - DF")
## Gráfico da série temporal completa “DF”

tiff(filename = "Série DF.tiff")
autoplot(mort_att_DF, mort_pad_att) +
             labs(title = "Mortality from traffic injuries, Brasília - DF (Jan 2000 to Dec 2020)", 
                  x = "Month of death", 
                  y = "Standardized mortality (100 thousand inhabitants)")+
        geom_vline(aes(xintercept=as.Date(uber_brasilia), 
                       col="Start of Uber activities"))+
        theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Brasília - DF"
mort_att_DF <- mort_att_DF %>%
        filter(month < yearmonth("2015 Nov"))

mort_att_DF

mort_att_DF %>% features(mort_pad_att, list(media_pre_DF = mean))

mort_att_DF %>% features(mort_pad_att, list(desv_pad_pre_DF = sd))


# 2. Média e desvio padrão da mortalidade por ATT período pré-Uber "Brasília - DF"
mort_pre_DF <- mort_att_DF %>%
        filter(month < yearmonth("2014 Nov"))

mort_pre_DF

mort_pre_DF %>% features(mort_pad_att, list(media_pre_DF = mean))

mort_pre_DF %>% features(mort_pad_att, list(desv_pad_pre_DF = sd))



# 3. Média e desvio padrão da mortalidade por ATT período pós-Uber "Brasília - DF"
mort_pos_DF <- mort_att_DF %>% filter(month >= yearmonth("2014 Nov"), month < yearmonth("2015 Nov"))

mort_pos_DF

mort_pos_DF %>% features(mort_pad_att, list(media_pos_DF = mean))

mort_pos_DF %>% features(mort_pad_att, list(desv_pad_pos_DF = sd))


## Preparação dados do período pré-Uber "Brasília - DF"
mort_pre_DF <- mort_att_DF %>%
        filter(UF == "DF", month < yearmonth("2014 Nov"))

mort_pre_DF


# Conversão de dados para time series object "Brasília - DF"

## Período total
mort_att_DF <- ts(mort_att_DF[,2], frequency=12, start=c(2000,1), 
                  end = c(2015,10))

## Pré-uber 
mort_pre_DF <- ts(mort_pre_DF[,2], frequency=12, start=c(2000,1), 
                  end = c(2014,10))

## Gráfico para verificação visual de sazonalidade
tiff(filename = "Sazonalidade DF.tiff")
mort_pre_DF %>% ggseasonplot()
dev.off()


# Visualização de gráficos ACF/PACF de dados não diferenciados "Brasília - DF"
pdf("ACF PACF undifferenced DF.pdf")
acf(as.numeric(mort_pre_DF), lag.max=48)
dev.off()


# Visualização de gráficos ACF/PACF de dados diferenciados/sazonalmente diferenciados "Brasília - DF"
pdf("ACF PACF dif_seasonally dif DF.pdf")
acf(diff(as.numeric(mort_pre_DF),12), lag.max=48)
dev.off()


# Uso de algoritmo automatizado para identificar parâmetros p/q  "Brasília - DF"
# Especiaficações: Número máximo de diferenciações soazonais = 2 ; Número máximo de diferenciações não sazonais = 2
model <- auto.arima(mort_pre_DF, seasonal=FALSE, max.d=2, max.D=2,
                    stepwise=TRUE, trace=TRUE,max.order=7)
model

# Modelo selecionado: ARIMA(1,1,2)

# Checagem dos resíduos do modelo "Brasília - DF"
pdf("Resíduos modelo DF.pdf")
checkresiduals(model, test=FALSE)
dev.off()

Box.test(model$residuals)

# Estimativa dos parâmetros e intervalo de confiança do modelo "Brasília - DF"
summary(model)
confint(model)




##Análise de Intervenção "Brasília - DF"


# Criação e visualização de variável que representa mudança em degrau "Brasília - DF"
step <- as.numeric(as.yearmon(time(mort_att_DF))>='Nov 2014')
step

# Criação e visualização de variável que representa mudança em rampa "Brasília - DF"
ramp <- append(rep(0,178), seq(1,12,1))
ramp  


## Criação e testagem de modelos "Brasília - DF"

# Modelo 1 - sem função de transferência "Brasília - DF"
model1 = Arima(mort_att_DF, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2))
# Checagem dos resíduos do modelo 1 "Brasília - DF"
pdf("Resíduos modelo 1 DF.pdf")
checkresiduals(model1, test=FALSE)
dev.off()

Box.test(model1$residuals)

# Estimativa dos parâmetros do modelo 1 "Brasília - DF"
summary(model1)
confint(model1)

# Modelo 2 - função de transferência em degrau "Brasília - DF"
model2 = Arima(mort_att_DF, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2),
               xreg = cbind(step))
# Checagem dos resíduos do modelo 2 "Brasília - DF"
pdf("Resíduos modelo 2 DF.pdf")
checkresiduals(model2, test=FALSE)
dev.off()

Box.test(model2$residuals)

# Estimativa dos parâmetros do modelo 2 "Brasília - DF"
summary(model2)
confint(model2)


# Modelo 3 - função de transferência em rampa "Brasília - DF"

model3 = Arima(mort_att_DF, 
               include.drift = FALSE,
               include.mean = FALSE,
               order = c(1,1,2),
               xreg = cbind(ramp))
# Checagem dos resíduos do modelo 3 "Brasília - DF"
pdf("Resíduos modelo 3 DF.pdf")
checkresiduals(model3, test=FALSE)
dev.off()

Box.test(model3$residuals)

# Estimativa dos parâmetros do modelo 3 "Brasília - DF"
summary(model3)
confint(model3)


# Comparação AIC dos 3 modelos "Brasília - DF"
AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

Box.test(model1$residuals)




# 28. Todas as capitais (apenas média e desvio padrão totais)


# Preparação dos dados "Todas as capitais"

mort_att_BR <- mort_att %>%
  filter(UF == "BR")

mort_att_BR

# Plotagem dos gráficos para análise visual da série temporal ("Todas as capitais")
## Gráfico da série temporal completa “BR”

tiff(filename = "Série Todas as capitais.tiff")
autoplot(mort_att_BR, mort_pad_att) +
  labs(title = "Mortality from traffic injuries, Todas as capitais (Jan 2000 to Dec 2020)", 
       x = "Month of death", 
       y = "Standardized mortality (100 thousand inhabitants)")+
  theme_minimal()
dev.off()

## Média e desvio padrão da mortalidade por ATT período total   "Todas as capitais"
mort_att_BR <- mort_att_BR %>%
  filter(month < yearmonth("2018 Dec"))

mort_att_BR

mort_att_BR %>% features(mort_pad_att, list(media_pre_BR = mean))

mort_att_BR %>% features(mort_pad_att, list(desv_pad_pre_BR = sd))







# Valores previstos x observados da taxa de mortalidade por ATT das UFs em que foram selecionados modelos com função de transferência


## 1. Belo Horizonte (MG)	ARIMA(0,1,1)

# Modelagem dos dados excluindo o período pós-uber
model2_s <- Arima(window(mort_pre_MG, end=c(2014,8)), order=c(0,1,1))


# Previsão de 10 meses após a intervenção e conversão para objeto de série temporal
fc_s <- forecast(model2_s, h=12, level=c(95))
fc_s.ts <- ts(as.numeric(fc_s$mean), start=c(2014,9), frequency=12)

# Combinação com dados observados
ts.comb_s <- ts.union(mort_att_MG, fc_s.ts)
ts.comb_s

# Plotagem (previsto x observado)
tiff(filename = "Prev x Obs MG.tiff")
plot(ts.comb_s, type="l", plot.type="s", col=c('black','red'), ylim=c(0,6), main = "Belo Horizonte (MG)", xlab="Month of death", ylab="Standardized mortality (100,000 inhab.)")
abline(v=2014.55, lty="dashed", col="blue")
dev.off()

help("plot")




## 2. Rio de Janeiro (RJ)	ARIMA(1,1,2)

# Modelagem dos dados excluindo o período pós-uber
model2_s <- Arima(window(mort_pre_RJ, end=c(2014,4)), order=c(1,1,2))


# Previsão de 10 meses após a intervenção e conversão para objeto de série temporal
fc_s <- forecast(model2_s, h=12, level=c(95))
fc_s.ts <- ts(as.numeric(fc_s$mean), start=c(2014,5), frequency=12)

# Combinação com dados observados
ts.comb_s <- ts.union(mort_att_RJ, fc_s.ts)
ts.comb_s

# Plotagem (previsto x observado)
tiff(filename = "Prev x Obs RJ.tiff")
plot(ts.comb_s, type="l", plot.type="s", col=c('black','red'), ylim=c(0,6), main = "Rio de Janeiro (RJ)", xlab="Month of death", ylab="Standardized mortality (100,000 inhab.)")
abline(v=2014.35, lty="dashed", col="blue")
dev.off()


