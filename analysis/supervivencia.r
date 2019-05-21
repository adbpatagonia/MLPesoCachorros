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
library('pgirmess')   # for permutation tests
library('here')
library('kableExtra')

# cargar datos ----
sup <- read.csv('data/supervivencia.csv', header = T, as.is = T)

# limpiar datos ----
sup <- sup[,c(2:7)]
names(sup) <- c('marcado', 'sexo', 'peso', 'n.resight', 'age.first.resight', 'age.last.resight')



sup$resight <- ifelse(sup$n.resight == 0, 0, 1)
sup$sexo <- as.factor(sup$sexo)
#sup$resight <- as.factor(sup$resight)


sup <- sup %>%
  transform(resight = factor(resight,levels=c(
    0,
    1),
    labels = c(
      "No Visto ",
      "Visto")))

sup <- sup %>%
  subset(sexo != "") %>%
  droplevels()

# plotear datos ----
# * ca;cular medias ----
mafr <- sup %>%
  group_by(sexo) %>%
  summarise(meanafr = mean(age.first.resight, na.rm = TRUE))

presight <- ggplot(data = sup, aes(x = age.first.resight, fill = sexo)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = mafr, aes(xintercept = meanafr, color = sexo)) +
  theme(legend.position =  c(0.8, 0.9), legend.title = element_blank()) +
  xlab('Edad.1ra.vez.volvió')

presight2 <- ggplot(data = sup, aes(x = n.resight, fill = sexo)) +
  geom_density(alpha = 0.4) +
#  geom_vline(data = mafr, aes(xintercept = meanafr, color = sexo)) +
  theme(legend.position =  c(0.8, 0.9), legend.title = element_blank()) +
  xlab('Nro.años.visto.volver')


# media de edad al reavistamiento 5 anos, por ende usar la serie de datos desde el 2013
sup <- sup %>% filter(marcado < 2014)


# explorar datos ----
par(mar = c(5,5,4,2))
plot(x = sup$peso,
     y = 1:nrow(sup),
     pch = 16,
     cex = 0.5,
     xlab = "Range of the data",
     ylab = "Order of the data",
     cex.lab = 1.5,
     main = "")
#That is ok

dotchart(sup$peso)


table(sup$resight)
table(sup$sexo)
table(sup$peso)

# * pirate plot ----
plotdat <- sup[,c('peso', 'resight', 'sexo')]
names(plotdat) <- c('peso', 'Reavistamiento', 'Sexo')
png(filename = 'output/Peso_Reavistamiento_Sexo.png', width = 8, height = 5, units = 'in', res = 400)
pirateplot(formula = peso ~  Reavistamiento + Sexo,
           data = plotdat,
           theme = 2,
           ylab = 'Peso (Kg)',
           #  yaxp = c(50, 300, 6),
           inf.method = 'hdi',
           pal = 'black',
           inf.disp = 'bean',
           cex.names = .7,
           cex.lab = .7,
           cex.axis = .65)
dev.off()
# * datos ----
# ** datos ----
dat <- pirateplot(formula = peso ~  Reavistamiento + Sexo,
                  data = plotdat,
                  theme = 2,
                  ylab = 'Peso (Kg)',
                  #  yaxp = c(50, 300, 6),
                  inf.method = 'hdi',
                  pal = 'black',
                  inf.disp = 'bean',
                  plot = FALSE)


# modelos ----
# * con interaccion ----
glm1 <- glm(resight ~ sexo * peso, family = 'binomial', data = sup)
# * sin interaccion ----
glm2 <- glm(resight ~ sexo + peso, family = 'binomial', data = sup)

# * anova tables ----
drop1(glm1, test = "LRT")
car::Anova(glm1, type = 'III')

# * errors ----
E1 <- resid(glm1, type = "pearson")
plot(x = sup$peso,
     y = E1)

# * overdispersion ----
sum(residuals(glm1, type = "deviance")^2)/glm1$df.residual
# no overdispersion
quasiglm1 <- glm(resight ~ sexo * peso, family = 'quasibinomial', data = sup)

# * parameter values ----
data_frame(MLE = coef(glm1), confint(glm1))
# ** hembras ----
inth <- coef(glm1)[1]
intm <- coef(glm1)[1]+coef(glm1)[2]
# ** machos ----
slh <- coef(glm1)[3]
slm <- coef(glm1)[3]+coef(glm1)[4]

pred <- predict(glm1, se.fit=TRUE, type = 'response') # type = 'link' by default


preds <- with(pred, data.frame(fit = fit,
                 low = fit - 1.96*se.fit,
                 up = fit + 1.96*se.fit)
)

sup$fit <- preds$fit
sup$low <- preds$low
sup$up <- preds$up


binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}


plog <- ggplot(data = sup, aes(x = peso, y = as.numeric(resight) - 1, color = sexo)) +
  geom_point(alpha = 0.3, position = position_jitter(height = 0.05)) +
  #geom_jitter(height = 0.05) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = sexo), color = 'transparent', alpha = 0.3) +
  geom_line(aes(y = fit)) +
  scale_y_continuous("Probabilidad de reavistamiento",  breaks = c(0, 0.5, 1)) +
  xlab('Peso (kg)') +
  theme(legend.position =  c(0.9, 0.9), legend.title = element_blank())



# salvar graficos ----
cowplot::ggsave(plot = plog, filename = 'output/ProbAvist_Sexo_Peso.png', width = 10, height = 6)
cowplot::ggsave(plot = presight, filename = 'output/Distribucion_Edad_Primer_Reavistamiento.png', width = 10, height = 6)
cowplot::ggsave(plot = presight2, filename = 'output/Distribucion_N_Reavistamiento.png', width = 10, height = 6)

# compilar reporte ----
compile_rmd('Supervivencia')

