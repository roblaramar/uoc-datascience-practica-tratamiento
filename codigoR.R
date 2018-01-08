# Cargamos los datos
bitcoin_data = read.csv("bitcoin_price.csv", header = TRUE)
litecoin_data = read.csv("litecoin_price.csv", header = TRUE)
ripple_data = read.csv("ripple_price.csv", header = TRUE)
# Escojemos las columnas deseadas
bitcoin_data_clean <- bitcoin_data[,c("Date","Close")]
litecoin_data_clean <- litecoin_data[,c("Date","Close")]
ripple_data_clean <- ripple_data[,c("Date","Close")]
# Cambiamos el formato de fecha de las columna "Date" a uno que sea más cómodo, en este caso el de 
# serie en R, con formato "YYYY-MM-DD"
# NOTA: Si as.Date devuelve NA, ejecutar la siguiente línea:
# lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
bitcoin_data_clean$Date <- as.Date(bitcoin_data_clean$Date, format = "%B %d, %Y")
litecoin_data_clean$Date <- as.Date(litecoin_data_clean$Date, format = "%B %d, %Y")
ripple_data_clean$Date <- as.Date(ripple_data_clean$Date, format = "%B %d, %Y")
# Comprobamos que no haya elementos vacíos o nulos. 
table(is.na(bitcoin_data_clean))
table(is.na(litecoin_data_clean))
table(is.na(ripple_data_clean))
# Una parte de la comprobación de que no hayan elementos vacíos consiste en ver que se encuentran todos los días
# desde la fecha inicial hasta la final. Para ello observamos que la cantidad de elementos de la lista de las fechas
# de los dataframe coincide con la cantidad de elementos que hay en un rango de fechas creado manualmente.
days1 <- seq(as.Date("2013/4/28"), as.Date("2017/11/7"), "days")
days2 <- seq(as.Date("2013/8/4"), as.Date("2017/11/7"), "days")
length(days1) == length(bitcoin_data_clean$Date)
length(days1) == length(litecoin_data_clean$Date)
length(days2) == length(ripple_data_clean$Date)
# Buscamos ceros
sum(bitcoin_data_clean == 0)
sum(litecoin_data_clean == 0)
sum(ripple_data_clean == 0)
# Vamos a dibujar el historico de los datos para buscar outliers
plot(bitcoin_data_clean, type="l")
plot(litecoin_data_clean, type="l")
plot(ripple_data_clean, type="l")
# Procedemos al análisis de los datos
# Hacemos una análisis descriptivo en forma de tabla
summary(bitcoin_data_clean$Close)
summary(litecoin_data_clean$Close)
summary(ripple_data_clean$Close)
# Vamos a dibujar un histograma con los precios de cada criptomoneda para visualizar la normalidad
hist(bitcoin_data_clean$Close)
hist(litecoin_data_clean$Close)
hist(ripple_data_clean$Close)
# Normalizamos los datos
bitcoin_data_clean_norm <- bitcoin_data_clean
bitcoin_data_clean_norm$Close <- scale(bitcoin_data_clean$Close)
litecoin_data_clean_norm <- litecoin_data_clean
litecoin_data_clean_norm$Close <- scale(litecoin_data_clean$Close)
ripple_data_clean_norm <- ripple_data_clean
ripple_data_clean_norm$Close <- scale(ripple_data_clean$Close)
# Hacemos test de homogeneidad de la varianza
var.test(x=bitcoin_data_clean_norm$Close, y=litecoin_data_clean_norm$Close)
var.test(x=bitcoin_data_clean_norm$Close, y=ripple_data_clean_norm$Close)
var.test(x=litecoin_data_clean_norm$Close, y=ripple_data_clean_norm$Close)
# Calculamos la volatilidad de cada mercado como su desviación estándar
volBitcoin <- sd(bitcoin_data_clean$Close)
volLitecoin <- sd(litecoin_data_clean$Close)
volRipple <- sd(ripple_data_clean$Close)
# Calculamos la volatilidad de cada mercado durante los períodos de noticias y sin noticias
bitcoin_period1 <- subset(bitcoin_data_clean, Date>="2013-10-01" & Date<="2013-12-01")
volBitcoin_period1 <- sd(bitcoin_period1$Close)
litecoin_period1 <- subset(litecoin_data_clean, Date>="2013-10-01" & Date<="2013-12-01")
vollitecoin_period1 <- sd(litecoin_period1$Close)
ripple_period1 <- subset(ripple_data_clean, Date>="2013-10-01" & Date<="2013-12-01")
volripple_period1 <- sd(ripple_period1$Close)
bitcoin_period2 <- subset(bitcoin_data_clean, Date>="2015-10-01" & Date<="2015-12-01")
volBitcoin_period2 <- sd(bitcoin_period2$Close)
litecoin_period2 <- subset(litecoin_data_clean, Date>="2015-10-01" & Date<="2015-12-01")
vollitecoin_period2 <- sd(litecoin_period2$Close)
ripple_period2 <- subset(ripple_data_clean, Date>="2015-10-01" & Date<="2015-12-01")
volripple_period2 <- sd(ripple_period2$Close)
bitcoin_period3 <- subset(bitcoin_data_clean, Date>="2017-10-01" & Date<="2017-11-07")
volBitcoin_period3 <- sd(bitcoin_period3$Close)
litecoin_period3 <- subset(litecoin_data_clean, Date>="2017-10-01" & Date<="2017-11-07")
vollitecoin_period3 <- sd(litecoin_period3$Close)
ripple_period3 <- subset(ripple_data_clean, Date>="2017-10-01" & Date<="2017-11-07")
volripple_period3 <- sd(ripple_period3$Close)
# Dibujamos la volatilidad para Bitcoin
plot(bitcoin_period1, type="l")
plot(bitcoin_period2, type="l")
plot(bitcoin_period3, type="l")
# Calculamos la volatilidad anual para Bitcoin
bitcoin_vol_anual <- sd(subset(bitcoin_data_clean, Date>="2013-04-28" & Date<="2013-12-31")$Close)
bitcoin_vol_anual <- append(bitcoin_vol_anual,sd(subset(bitcoin_data_clean, Date>="2014-01-01" & Date<="2014-12-31")$Close))
bitcoin_vol_anual <- append(bitcoin_vol_anual,sd(subset(bitcoin_data_clean, Date>="2015-01-01" & Date<="2015-12-31")$Close))
bitcoin_vol_anual <- append(bitcoin_vol_anual,sd(subset(bitcoin_data_clean, Date>="2016-01-01" & Date<="2016-12-31")$Close))
bitcoin_vol_anual <- append(bitcoin_vol_anual,sd(subset(bitcoin_data_clean, Date>="2017-01-01" & Date<="2017-11-07")$Close))
litecoin_vol_anual <- sd(subset(litecoin_data_clean, Date>="2013-04-28" & Date<="2013-12-31")$Close)
litecoin_vol_anual <- append(litecoin_vol_anual,sd(subset(litecoin_data_clean, Date>="2014-01-01" & Date<="2014-12-31")$Close))
litecoin_vol_anual <- append(litecoin_vol_anual,sd(subset(litecoin_data_clean, Date>="2015-01-01" & Date<="2015-12-31")$Close))
litecoin_vol_anual <- append(litecoin_vol_anual,sd(subset(litecoin_data_clean, Date>="2016-01-01" & Date<="2016-12-31")$Close))
litecoin_vol_anual <- append(litecoin_vol_anual,sd(subset(litecoin_data_clean, Date>="2017-01-01" & Date<="2017-11-07")$Close))
ripple_vol_anual <- sd(subset(ripple_data_clean, Date>="2013-08-04" & Date<="2013-12-31")$Close)
ripple_vol_anual <- append(ripple_vol_anual,sd(subset(ripple_data_clean, Date>="2014-01-01" & Date<="2014-12-31")$Close))
ripple_vol_anual <- append(ripple_vol_anual,sd(subset(ripple_data_clean, Date>="2015-01-01" & Date<="2015-12-31")$Close))
ripple_vol_anual <- append(ripple_vol_anual,sd(subset(ripple_data_clean, Date>="2016-01-01" & Date<="2016-12-31")$Close))
ripple_vol_anual <- append(ripple_vol_anual,sd(subset(ripple_data_clean, Date>="2017-01-01" & Date<="2017-11-07")$Close))
# Vamos a hacer un test de Pearson para ver la correlación entre votilidades
cor.test(bitcoin_vol_anual,litecoin_vol_anual)
cor.test(bitcoin_vol_anual,ripple_vol_anual)
cor.test(litecoin_vol_anual,ripple_vol_anual)
# Vamos a hacer un test de Pearson para ver la correlación entre precios
cor.test(bitcoin_data_clean$Close,litecoin_data_clean$Close)
cor.test(bitcoin_data_clean$Close[1:1557],ripple_data_clean$Close)
cor.test(litecoin_data_clean$Close[1:1557],ripple_data_clean$Close)
# Exportamos a unos CSV los datos obtenidos
write.csv(bitcoin_data_clean, file = "bitcoin.csv", row.names = FALSE)
write.csv(litecoin_data_clean, file = "litecoin.csv", row.names = FALSE)
write.csv(ripple_data_clean, file = "ripple.csv", row.names = FALSE)