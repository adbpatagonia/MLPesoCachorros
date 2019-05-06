
## ADB
## Feb
## Pasantia Malena Pfoh en el IAA, a cargo del Dr Javier Negrete
## Recibí datos de peso al destete de Mirounga leonina en la isla 25 de Mayo
## El objetivo de este script es analizar si existe relacion entre el peso al destete y variables oceánicas (ENSO)

# Data sources ----
# 1. Datos enviados por Malena Pfoh el 20 de febrero de 2018 'data-raw/Tabla para Ale.xlsx'
# 2. Antarctic Oscillation https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/aao/aao.shtml 'data-raw/monthly.aao.index.b79.current.ascii'
# 3. SAM. Downloaded from https://legacy.bas.ac.uk/met/gjma/sam.html  'data-raw/newsam.1957.2007.txt'

# cargar funciones ----
source('R/compile_rmd.R')
source('R/standardise_variables.r')

# cargar paquetes ----
library('tidyverse')
library('cowplot')
library('ggsidekick')
library('tidybayes')
library(yarrr)
#library('plotly')
library('here')
library('kableExtra')
library(plotly)

# cargar datos ----
ml <- read.csv('data/MLpesos.csv', header = T, as.is = T)
mlnew <- read.csv('data/MLpesos_Apr2019.csv', header = T, as.is = T)
aao <- read.table('data-raw/monthly.aao.index.b79.current.ascii')
sam <- read.table('data-raw/newsam.1957.2007.txt')

# buscar errores en los datos ----
ml$pesos <- as.numeric(ml$peso)
which(is.na(ml$pesos)) # esto se corresponde con la celda B168 del archivo que envio Malena, eliminarlo por ahora

# limpiar datos ----
#crear un nuevo data.frame eliminando los valores NA de peso
mldat <- ml[which(!is.na(ml$pesos)),]
# hallar valores de sexo que no son unicos
mldat <- mldat %>%
  mutate(sexo = replace(sexo, sexo == 'MACHO', 'Macho'))
# encontrar outliers
which.max(mldat$pesos) # hay un peso que es un outlier, peso = 1332.5, celda B64 en el archivo que envio malena
#mldat <- mldat[-which.max(mldat$pesos),]
# convertir sexo y evento a factores
mldat$sexo <- as.factor(mldat$sexo)
mldat$evento <- as.factor(mldat$evento)


# calcular datos ----
# * eventos basados en Jul-Sep y Mar-Agos ----
mls <- mldat %>%
  group_by(year) %>%
  select(Jul.Ago.Sep) %>%
  unique() %>%
  na.omit() %>%
  mutate(evento.js = ifelse(Jul.Ago.Sep >= 0.5, 'Niño',
                            ifelse(Jul.Ago.Sep <= -0.5, 'Niña',
                                   'Neutro'))) %>%
  right_join(mlnew, by = 'year') %>%
  mutate(evento.ma = ifelse(potencia.mar.agos >= 0.5, 'Niño',
                            ifelse(potencia.mar.agos <= -0.5, 'Niña',
                                   'Neutro')))

mls <- mls %>%
  mutate(evento.js = factor(evento.js, levels = c('Niña', 'Neutro', 'Niño')),
         evento.ma = factor(evento.ma, levels = c('Niña', 'Neutro', 'Niño')),
         sexo = factor(sexo, levels = c('Hembra', 'Macho'))
         )


# pirate plots ----
# * potencia Marzo-Agosto ----
# ** plot ----
plotdat <- mls

names(plotdat) <- c('year', 'jas', 'evento,js', 'marca', 'mag', 'Sexo', 'Peso', 'ENSO')


png(filename = 'output/Peso_Sexo_Evento.png', width = 8, height = 5, units = 'in', res = 400)
 pirateplot(formula = Peso ~  ENSO + Sexo,
           data = plotdat,
           theme = 2,
           ylab = 'Peso (Kg)',
         #  yaxp = c(50, 300, 6),
           inf.method = 'hdi',
           pal = 'black',
           inf.disp = 'bean')
dev.off()
# ** datos ----
dat <- pirateplot(formula = peso ~  evento.ma + sexo,
           data = as.data.frame(mls),
           theme = 2,
           inf.disp = 'bean',
           inf.method = 'hdi',
           plot = FALSE)

# * potencia Julio-Sept ----
pirateplot(formula = peso ~  evento.js + sexo,
           data = as.data.frame(mls),
           theme = 2,
           inf.disp = 'bean')

# funcion continua ----
ancova.int <- lm(peso ~ potencia.mar.agos * sexo, data = mls)
ancova <- lm(peso ~ potencia.mar.agos + sexo, data = mls)


  pred <- as.data.frame(predict(ancova, interval = 'confidence', type = 'response'))
  mls <- data.frame(mls, pred)

ancovaplot <- ggplot(data = mls, aes(x = potencia.mar.agos, y = peso, color = sexo)) +
  geom_point(alpha = 0.4, position = position_jitterdodge(dodge.width = 0.3, jitter.width = 0.1))  +
   geom_line(data = unique(mls[,c('potencia.mar.agos', 'sexo', 'fit')]), aes(y = fit), position = position_jitterdodge(dodge.width = 0.3, jitter.width = 0)) +
  geom_ribbon(aes(ymin = mls$lwr, ymax = mls$upr, fill = sexo, color = NA), alpha = 0.4, position = position_jitterdodge(dodge.width = 0.3, jitter.width = 0)) +
  theme_sleek() +
  theme(legend.position = c(0.9, 0.95)) +
  scale_color_manual(name = '', values = c('red', 'blue')) +
  scale_fill_manual(name = '', values = c('red', 'blue')) +
  ylab('Peso (Kg)') + xlab('Potencia') #+
 #geom_smooth(method = "lm", se = TRUE, fullrange = TRUE)





# modelo ----
# * anova con interaccion ----
lm1 <-lm(peso ~ evento.ma * sexo, data = mls)
# * anova sin interaccion ----
lm2 <-lm(peso ~ evento.ma + sexo, data = mls)
# * chequear supuestos ----
par(mfrow = c(2,2))
plot(lm1)   # sin problemas con los supuestos
plot(lm2)   # sin problemas con los supuestos

# * tabla ANOVA modelo con interaccion ----
anova (lm1) # interaccion no es significativa

# * tabla ANOVA modelo sin interaccion ----
anout <- anova(lm2)

# * parametros ----
coefficients(lm2)
confint(lm2)

# output ----
write.csv(dat, 'output/pirate.csv', row.names=F)
write.csv(anout, 'output/anova.csv', row.names=T)
ggsave(filename = 'output/ancovaplot.png', plot = ancovaplot, width = 8, height = 5, units = 'in' )

# compile report ----
compile_rmd('MLPesoCachorros')
