if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse,skimr,readxl,mice, visdat,
               outliers, janitor)



#######

#Trafico

library(fpp2)

movility_gijon <- arrange(movility_gijon, movility_gijon$date)
media_coches_dia <- rowMeans(movility_gijon[-c(1)])


serie_trafico_gijon <- ts(media_coches_dia, start = 2019, frequency = 365)
autoplot(serie_trafico_gijon) + theme_bw() #2019-2024

serie_trafico_gijon_covid <- window(serie_trafico_gijon, start = c(2020, 74), end = c(2020, 298))
autoplot(serie_trafico_gijon_covid) +theme_bw() #covid

serie_trafico_gijon_precovid <- window(serie_trafico_gijon, start = 2019, end = c(2020, 73))
autoplot(serie_trafico_gijon_precovid) + theme_bw() #precovid

serie_trafico_gijon_postcovid <- window(serie_trafico_gijon, start = c(2020, 299), end = 2024)
autoplot(serie_trafico_gijon_postcovid) + theme_bw() #postcovid

valores_atipicos_trafico <- which(media_coches_dia < 500)
valores_atipicos_1 <- movility_gijon$date[outliers_trafico]
valores_atipicos_1
#Vemos que estos outliers coinciden en su mayoría con la época covid, y los que no,  por ejemplo los de 2022 muchos de ellos
#se dan en julio y principios de septimebre época en la que la gente tiende a irse de vacaciones

#Vemos claramente una gran reducción del tráfico en el periodo Covid, que se vuelve a recuperar con la conclusión del mismo,
#sin embargo el tráfico medio sigue siendo inferior al observado en la época pre-covid.


#Contaminación

skim(air_quality_gijon)

air_quality_gijon <- air_quality_gijon[c(1,14,16,17,18,19)]

air_quality_gijon <- arrange(air_quality_gijon, air_quality_gijon$date)
media_contaminacion <- rowSums(air_quality_gijon[-c(1)])

serie_contaminacion_gijon <- ts(media_contaminacion, start = 2019, frequency = 365)
autoplot(serie_contaminacion_gijon) + theme_bw() #2019-2024

serie_contaminacion_gijon_covid <- window(serie_contaminacion_gijon, start = c(2020, 74), end = c(2020, 298))
autoplot(serie_contaminacion_gijon_covid) +theme_bw() #covid

serie_contaminacion_gijon_precovid <- window(serie_contaminacion_gijon, start = 2019, end = c(2020, 73))
autoplot(serie_contaminacion_gijon_precovid) + theme_bw() #precovid

serie_contaminacion_gijon_postcovid <- window(serie_contaminacion_gijon, start = c(2020, 299), end = 2024)
autoplot(serie_contaminacion_gijon_postcovid) + theme_bw() #postcovid

valores_atipicos_contaminacion <- which(media_contaminacion < 120)
valores_atipicos_2 <- air_quality_gijon$date[valores_atipicos_contaminacion]
valores_atipicos_2

#La contaminación durante la epoca covid disminuyó de manera no demasiado notable pero si que se pueden apreciar valores
#más bajos respecto a los periodos precovid y postcovid