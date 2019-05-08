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
  summarise(meanafr = median(age.first.resight, na.rm = TRUE))

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


glm1 <- glm(resight ~ sexo * peso, family = 'binomial', data = sup)
glm2 <- glm(resight ~ sexo + peso, family = 'binomial', data = sup)


anova(glm1)


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
  geom_point() +
  geom_jitter(height = 0.05) +
  geom_ribbon(aes(ymin = low, ymax = up, fill = sexo), color = 'transparent', alpha = 0.3) +
  geom_line(aes(y = fit)) +
  scale_y_continuous("Probabilidad de reavistamiento",  breaks = c(0, 0.5, 1)) +
  xlab('Peso (kg)') +
  theme(legend.position =  c(0.8, 0.75), legend.title = element_blank())



# salvar graficos ----
cowplot::ggsave(plot = plog, filename = 'output/ProbAvist_Sexo_Peso.png', width = 6, height = 4)
cowplot::ggsave(plot = presight, filename = 'output/Distribucion_Edad_Primer_Reavistamiento.png', width = 6, height = 4)
cowplot::ggsave(plot = presight2, filename = 'output/Distribucion_N_Reavistamiento.png', width = 6, height = 4)



