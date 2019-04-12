library("tidyverse")
library("nycflights13") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 

setwd("D:/YandexDisk/BorodaR/Zad 2_mod2")
# Бородина Кира – создайте модель множественной линейной регрессии 
# дневных потоков углекислого газа за осенний период 2013 года по данным 
# измерений методом турбулентной пульсации
# файл с данными загружен и считывается из рабочей директории
data = read.csv("eddypro.csv", skip = 1, na = c("","NA","-9999", "-9999.0"), comment = c("[")) 
data = data[-1,] # удаляем 1 пустую строку

data=data[data$DOY > 244 & data$DOY < 335,] # отбираем данные за осенний период
data=data[data$daytime == TRUE,] # отбираем данные дневных потоков
glimpse(data) #показывает данные таблицы

data = select(data, -(roll)) # удаляем столбец с пустыми ячейками
data = data %>% mutate_if(is.character, factor) #Преобразуем столбы с даннми типа character (символ) в факторы (factor) 

sapply(data,is.numeric) 
#Выбираем числовые данные из таблицы. Будем работать с таблицей числовых значений
data_numeric = data[,sapply(data,is.numeric)] 

#выбираем строки исключае пропущенные данные и строим формулу для модели 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))

#строим первоначальную модель используя все имеющиеся независимые переменные (числовые)
mod = lm(formula, data = data_numeric) 
summary(mod)
anova(mod)
#необходимо подобрать модель, отсеев наименее значимые переменные, получив оптимальную модель, отражающую зависимость искомой переменный от нескольких наиболее значимых показателей
formula1 = co2_flux ~ (rand_err_H + LE + rand_err_co2_flux + h2o_flux + 
                         co2_molar_density + co2_mole_fraction + co2_mixing_ratio + 
                         sonic_temperature + air_temperature + es + T. + un_LE + un_co2_flux + 
                         un_h2o_flux + ts_var + co2_var + w.co2_cov + w.h2o_cov + 
                         co2 + co2.1)
mod1 = lm(formula1, data = data_numeric)
anova(mod1)
summary(mod1)
#повторяем моделирование отсеевая наименее значимые переменные
formula2 = co2_flux ~ (rand_err_H +  rand_err_co2_flux + 
                          T. + un_co2_flux + 
                        ts_var + co2_var)

mod2 = lm(formula2, data = data_numeric)
anova(mod2)
summary(mod2)
anova(mod, mod1)

#Выберем из таблицы только участвующие у линейной моделе переменные
cor_tbl = select(data_numeric, rand_err_H, rand_err_co2_flux,  T., un_co2_flux, ts_var, co2_var)
#Получаем таблицу коэффициентов корреляций.
cor_td = cor(cor_tbl) %>% as.data.frame
cor_td
#в данном случае взаимосвязи переменных мы не наблюдаем и можем принять последнюю модель (mod2)
