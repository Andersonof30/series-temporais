# leitura dados 
require(tidyverse)
require(forecast)
dados = read_csv2('https://raw.githubusercontent.com/Andersonof30/series-temporais/main/var/dados.csv')

head(dados)
# descritivas

par(mfrow = c(1,2))
apply(dados[,24], 2, plot, type = 'l')

summary(dados[, 2:4])
# estacionaridade

#primeira diferença, tem q ter o mesmo tamanho 
#dados.dif = apply(dados[2:4], 2, diff)
apply(dados[3:4], 2, tseries::adf.test)

# var

require(vars)

VARselect(dados[1:324, 3:4])
m1 = VAR(dados[1:312,3:4], p = 1)
summary(m1)

# interpretação

vars::causality(m1, cause = 'Selic')$Granger
vars::causality(m1, cause = 'inflacao')$Granger


# diagnostico 
par(mfrow = c(2, 1))

plot(as.numeric(residuals(m1)[,1]), type = 'l', 
     main = 'Resíduos do modelo da Selic', ylab = 'Resíduos')
plot(as.numeric(residuals(m1)[,2]), type = 'l',
     main = 'Resíduos do modelo da inflação', ylab = 'Resíduos')

acf(as.numeric(residuals(m1)[,1]), type = 'correlation', 
    main = 'Autocorrelação resíduos do modelo da Selic')
acf(as.numeric(residuals(m1)[,2]), type = 'correlation', 
    main = 'Autocorrelação resíduos do modelo da inflação')

Box.test(as.numeric(residuals(m1)[,1]), lag = 1, type = "Ljung")
Box.test(as.numeric(residuals(m1)[,2]), lag = 1, type = "Ljung")

dw.T = function(resi){
  tam = length(resi)
  num = NULL
  for (i in 2:tam){
    num[i] = (resi[i] - resi[i-1])^2
  }
  num2 = sum(num, na.rm = T)
  dem = sum(resi^2)
  test = num2/dem
  round(test,2)
}


dw.T(resi = as.numeric(residuals(m1)[,1]))
dw.T(resi = as.numeric(residuals(m1)[,2]))

# previsão 

pred = predict(m1, n.ahead = 12)
pred$fcst$Selic[,1]
dados$Selic[313:322]

Metrics::rmse(dados$Selic[313:324], pred$fcst$Selic[,1])
Metrics::mape(dados$Selic[313:324], pred$fcst$Selic[,1])

m2s = forecast::auto.arima(dados$Selic[1:312])
pred.as = forecast::forecast(m2s, h = 12)
pred.as$mean

Metrics::rmse(dados$Selic[313:324], pred.as$mean)
Metrics::mape(dados$Selic[313:324], pred.as$mean)

#melhor que o auto.arima 2(2,1,0) - selic 
pred$fcst$inflacao[,1]

Metrics::rmse(dados$inflacao[313:324], pred$fcst$inflacao[,1])
Metrics::mape(dados$inflacao[313:324], pred$fcst$inflacao[,1])

m2i = forecast::auto.arima(dados$inflacao[1:312])
pred.is = forecast::forecast(m2i, h = 12)
pred.is$mean

Metrics::rmse(dados$inflacao[313:324], pred.is$mean)
Metrics::mape(dados$inflacao[313:324], pred.is$mean)



#pior mas muito mais simples 

predict(m1) |> plot()

