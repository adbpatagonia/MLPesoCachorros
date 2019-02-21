
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
#source('R/compile_rmd.R')
source('R/standardise_variables.r')

# cargar paquetes ----
library('tidyverse')
library('cowplot')
library('ggsidekick')
library('tidybayes')
library('plotly')

# cargar datos ----
ml <- read.csv('data/MLpesos.csv', header = T, as.is = T)
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
mldat <- mldat[-which.max(mldat$pesos),]
# convertir sexo y evento a factores
mldat$sexo <- as.factor(mldat$sexo)
mldat$evento <- as.factor(mldat$evento)

# daots de ENSO ----
ENSO <- mldat %>%
  select(year, potencia, evento) %>%
  unique()

# plotear datos ----
p <- ggplot(mldat, aes(x = year, y = pesos, color = sexo , group = sexo)) +
  geom_eye(alpha = 0.5, position = position_dodge(width = .8)) +
  theme_sleek() +
  xlab('Año') +
  ylab('Peso (Kg)') +
  theme(legend.position = c(0.1,0.9))

p2 <- ggplot(ENSO, aes(x = year, y = potencia)) +
  geom_line() + theme_sleek()

cowplot::plot_grid(p, p2, ncol = 1)

# estandarizar variables ----
mldat$pesos.std <- std(mldat$pesos)
ENSO$potencia.std <- std(ENSO$potencia)


# plotear datos estandarizados ----
a <- mldat %>%
  select(year, sexo, pesos.std)
b <- ENSO %>%
  select(year, potencia.std, evento)
df <-  merge(a, b, by = 'year')
p3 <- ggplot(df, aes(x = year, y = pesos.std, color = sexo , group = sexo)) +
  geom_eye(alpha = 0.5, position = position_dodge(width = .8)) +
  geom_line(data = subset(df, sexo == 'Hembra'), aes(x = year, y = potencia.std), color = 'black') +
  theme_sleek() +
  xlab('Año') +
#  ylab('Peso (Kg)') +
  theme(legend.position = c(0.1,0.9))

p4 <- ggplot(df, aes(x = potencia.std, y = pesos.std, color = sexo , group = sexo)) +
  #geom_eye(alpha = 0.5, position = position_dodge(width = .8)) +
  geom_point(alpha = 0.5, position = position_jitterdodge(dodge.width = 1)) +
#  geom_line(data = subset(df, sexo == 'Hembra'), aes(x = year, y = potencia.std), color = 'black') +
  theme_sleek() +
#  xlab('Año') +
  #  ylab('Peso (Kg)') +
  theme(legend.position = c(0.1,0.9))

# plotear con evento como variable explicativa ----
mldat$evs <- interaction(mldat$evento, mldat$sexo)
mldat <-mldat %>%
  mutate(evento = factor(evento, levels = c('Niña', 'Neutro', 'Niño')))

p5 <- ggplot(mldat, aes(x = evento, y = pesos, color = sexo , group = evs)) +
  geom_boxplot(outlier.size = 0) +
    #geom_eye(alpha = 0.5, position = position_dodge(width = .8)) +
  geom_point(alpha = 0.5, position = position_jitterdodge()) +
  #  geom_line(data = subset(df, sexo == 'Hembra'), aes(x = year, y = potencia.std), color = 'black') +
  theme_sleek() +
    xlab('Evento') +
    ylab('Peso (Kg)') +
  theme(legend.position = c(0.1,0.9))

# ajustar modelo lineal ----
m1 <- lm(data = df, pesos.std ~ potencia.std + sexo + year)

# datos atmosfericos ----
# * aao ----
names(aao) <- c('year', 'month', 'aao')
avaao <- aao %>%
  group_by(year) %>%
  summarise(av.aao = mean(aao)) %>%
  filter(year > 1994) %>%
  filter(year < 2019) %>%
  mutate(av.aao.std = std(av.aao))

dat <- merge(avaao, mldat)


p6 <- ggplot(dat, aes(x = year, y = pesos.std, color = sexo , group = sexo)) +
  geom_eye(alpha = 0.5, position = position_dodge(width = .8)) +
  geom_line(data = subset(dat, sexo == 'Hembra'), aes(x = year, y = av.aao.std), color = 'black') +
  theme_sleek() +
  xlab('Año') +
  #  ylab('Peso (Kg)') +
  theme(legend.position = c(0.1,0.9))

p7 <- ggplot(dat, aes(x = av.aao.std, y = pesos.std, color = sexo , group = sexo)) +
  #geom_eye(alpha = 0.5, position = position_dodge(width = .8)) +
  geom_point(alpha = 0.5, position = position_jitterdodge(dodge.width = 1)) +
  #  geom_line(data = subset(df, sexo == 'Hembra'), aes(x = year, y = potencia.std), color = 'black') +
  theme_sleek() +
  #  xlab('Año') +
  #  ylab('Peso (Kg)') +
  theme(legend.position = c(0.1,0.9))

maao <- lm(data = dat, pesos.std ~ av.aao.std + sexo + year)

# * sam ----
avsam <- data.frame(year = as.numeric(rownames(sam)), avsam = rowMeans(sam)) %>%
  filter(year > 1994) %>%
  filter(year < 2019) %>%
  mutate(av.sam.std = std(avsam))

dat <- merge(avsam, mldat)


p8 <- ggplot(dat, aes(x = year, y = pesos.std, color = sexo , group = sexo)) +
  geom_eye(alpha = 0.5, position = position_dodge(width = .8)) +
  geom_line(data = subset(dat, sexo == 'Hembra'), aes(x = year, y = av.sam.std), color = 'black') +
  theme_sleek() +
  xlab('Año') +
  #  ylab('Peso (Kg)') +
  theme(legend.position = c(0.1,0.9))

p9 <- ggplot(dat, aes(x = av.sam.std, y = pesos.std, color = sexo , group = sexo)) +
  #geom_eye(alpha = 0.5, position = position_dodge(width = .8)) +
  geom_point(alpha = 0.5, position = position_jitterdodge(dodge.width = 1)) +
  #  geom_line(data = subset(df, sexo == 'Hembra'), aes(x = year, y = potencia.std), color = 'black') +
  theme_sleek() +
  #  xlab('Año') +
  #  ylab('Peso (Kg)') +
  theme(legend.position = c(0.1,0.9))

msam <- lm(data = dat, pesos.std ~ av.sam.std + sexo + year)

# salvar graficos ----
cowplot::ggsave(plot = p5, filename = 'output/PesoXEvento.png')
