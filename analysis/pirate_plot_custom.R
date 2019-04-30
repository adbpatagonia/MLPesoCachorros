library(ggplot2)
library(yarrr)

plottheme = function() {
  theme(
    legend.key.size = unit(0.75, "lines"),
    legend.key = element_blank(),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 9),
    plot.title = element_text(size = 8, face = "bold"),
    axis.line = element_line(colour = "grey", size = 1),
    panel.grid.major.y = element_line(colour = "gray", linetype = "dashed"),
    panel.grid.major.x = element_line(colour = "gray"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "gray40", fill = NA),
    panel.background = element_rect(fill = NA, colour = "gray40"),
    axis.title = element_text(face = "bold", size = 9),
    axis.text = element_text(size = 8, colour = "black"),
    axis.ticks.x = element_blank(),
    plot.margin = unit(c(2, 5, 2, 2), "mm"),
    strip.background = element_blank(),
    strip.text = element_text(size = 8, face = "bold")
  )
}


 
dat <- subset(movies, 
       genre %in% c("Action", "Adventure", "Comedy", "Horror") &
         rating %in% c("G", "PG", "PG-13", "R") & time > 0)
dat$inter <- interaction( dat$sequel, dat$genre)

times.pp <- pirateplot(formula = time ~ sequel + genre,
                           data = dat, 
                           plot = FALSE)
hdidat <- times.pp$summary

hdidat$inter <- interaction( hdidat$sequel,hdidat$genre) 


data_summary <- function(x) {
 dat <-  x %>% 
    group_by(inter) %>%
    summarise(m = avg, ymin = inf.lb, ymax = inf.ub)
  # m <- mean(x)
  # ymin <- m-sd(x)
  # ymax <- m+sd(x)
#  return(c(y=m,ymin=ymin,ymax=ymax))
 return(dat)
}

p <- ggplot(data = dat, aes(x = inter, y = time, fill = as.factor(sequel))) +
  geom_violin(alpha = 0.5) + 
  #geom_jitter(height = 0, width = 0.1, pch = 1, alpha = 0.5) +
  geom_point(position = position_jitter(width = 0.2), col = 'grey30', alpha = 0.5, pch = 1) +
  geom_point(data = hdidat, aes(x = inter, y = avg), col = 'black') +
#  geom_ribbon(data = hdidat, aes(x = inter,  ymin = inf.lb, ymax = inf.ub)) +
#  geom_polygon(data = hdidat, aes(x = inter, )) +
#  stat_summary(fun.data=data_summary)
#  geom_boxplot() +
  geom_linerange(data = hdidat, aes(x = inter, y = avg, ymin = inf.lb, ymax = inf.ub), col = 'black') +
  plottheme() +

  
#ggplot(data = as.data.frame(hdidat), aes (x = inter))   +
geom_boxplot(data = as.data.frame(hdidat),
  aes (x = inter, ymin = inf.lb, lower = inf.lb, middle = avg, upper = inf.ub, ymax = inf.ub),
  stat = "identity"
)


pirateplot(formula = time ~ sequel + genre,
           data = subset(movies, 
                         genre %in% c("Action", "Adventure", "Comedy", "Horror") &
                           rating %in% c("G", "PG", "PG-13", "R") &
                           time > 0),
           plot = TRUE)






# ggplot(data = dat, aes(x = inter, y = time)) +
#   geom_violin() + 
#   geom_jitter(height = 0, width = 0.1, pch = 1, alpha = 0.5) +
#   geom_point(data = hdidat, aes(x = inter, y = avg), col = 'red') +
#   geom_linerange(data = hdidat, aes(x = inter, y = avg, ymin = inf.lb, ymax = inf.ub), col = 'red') +
#   plottheme()