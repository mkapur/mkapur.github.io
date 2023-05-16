require(ggplot2)
require(hrbrthemes)
require(tibble)
require(patchwork)
require(showtext)


## S1 GGPLOT DEFAULTS ----
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot()

ggsave(last_plot(),
     file = "notheme.png")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  theme_bw()

ggsave(last_plot(),
       file = "theme_bw.png")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  theme_minimal()

ggsave(last_plot(), file = "theme_minimal.png")

## S2 PACKAGES ON THE WEB ----
remotes::install_github("seananderson/ggsidekick") ## for later R versions
library(ggsidekick)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  ggsidekick::theme_sleek()
ggsave(last_plot(), file = "theme_sleek.png")


# remotes::install_git("https://git.rud.is/hrbrmstr/hrbrthemes.git")
extrafont::font_import() ## takes a long time to run

import_roboto_condensed()
extrafont::loadfonts(device= 'win' ) ## must be run once each session
## restart R
library(hrbrthemes)

p2 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  labs(title = 'Lorem Ipsum') +
  scale_fill_ipsum() +
  theme_ipsum()

p1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  labs(title = 'Lorem Ipsum') +
  theme_ft_rc()

ggsave(Rmisc::multiplot(p1,p2), file = "hrbrthemes.png")

p1 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  ggthemes::theme_solarized()

p2 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  ggthemes::theme_solarized_2()

p3 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  ggthemes::theme_solarized(light = FALSE)

p4 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  ggthemes::theme_solarized_2(light = FALSE)
ggsave((p2)/(p4), file = "solarized.png")

ggplot(mtcars, aes(x = mpg, y = qsec)) +
  geom_line(lwd = 1.1) +
  ggthemes::theme_solid()
 ggsave(last_plot(), file = "solid.png")

## S3 LIGHT TWEAKS TO DEFAULTS ----
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  theme_bw() +
  RColorBrewer::scale_fill_brewer(palette = 'Spectral')

ggsave(last_plot(),
       file = "bw_spectral.png")

# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mtcars$make <- as.factor(row.names(mtcars))
ggplot(mtcars, aes(x = mpg, y = hp, color = factor(gear))) +
  geom_point() +
  theme_bw() +
 scale_color_manual(values = cbbPalette)

ggsave(last_plot(),
       file = "bw_cbb.png")

## S4 YOUR OWN CUSTOM WORLD ---

# Requires tibble
remotes::install_github("mkapur/kaplot")
library(kaplot)

p1 <- ggplot(mtcars, aes(x = mpg, y = hp, color = factor(gear))) +
  geom_boxplot() +
  theme_solarized_mk(base_size = 14, light = TRUE)
p2 <- ggplot(mtcars, aes(x = mpg, y = hp, color = factor(gear))) +
  geom_boxplot() +
  theme_solarized_mk(base_size = 14, light = FALSE)

p1 | p2

ggsave(last_plot(),
       file = "C:/Users/mkapur/Dropbox/blog/ggthemes/theme_solarized_mk.png")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() + kaplot::theme_mk()
ggsave(last_plot(),
       file = "C:/Users/mkapur/Dropbox/blog/ggthemes/theme_mk.png")



p1 <- ggplot(ToothGrowth, aes(x = len, y = dose, fill = supp)) +
  geom_boxplot() + kaplot::theme_mk() + kaplot::scale_fill_starfish()

p2 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, fill = Species)) +
  geom_boxplot() + kaplot::theme_mk() + kaplot::scale_fill_sunset()

p3 <- ggplot(mtcars[1:9,], aes(x = mpg, y = qsec, fill = rownames(mtcars[1:9,]))) +
  geom_bar(stat='identity') + kaplot::theme_mk() + kaplot::scale_fill_ipsum()

p4 <- ggplot(USArrests, aes(x = UrbanPop, y = Murder, fill = UrbanPop)) +
  geom_bar(stat='identity') + kaplot::theme_mk() + scale_fill_viridis_c()
ggsave((p1  |p2) /(p3  |p4),
       file = "C:/Users/mkapur/Dropbox/blog/ggthemes/starfish_mk.png")

font_add(family = "amaticsc",
         regular = "C:/Users/mkapur/Downloads/amatic-sc/amaticsc-regular.ttf")
# https://www.r-bloggers.com/adding-custom-fonts-to-ggplot-in-r/
## special fonts only work in a new window
font_add_google(name = "Amatic SC", family = "amatic-sc")
font_add_google(name = "Patrick Hand SC", family = "PatrickHand-sc")
font_add_google(name = "Oxygen", family = "Oxygen")
font_add_google(name = "Tajawal", family = "Tajawal")
font_add_google(name = "Staatliches", family = "Staatliches")
font_add_google(name = "lacquer")

font_add_google(name = "Archivo", family = "Archivo")

windows()
showtext_auto()

ggplot(ToothGrowth, aes(x = len, y = dose, fill = supp)) +
  geom_boxplot() + ggsidekick::theme_sleek(base_size = 25) + labs(x = 'LENGTH', y = 'DOSE') +
  theme(text = element_text(family = "Staatliches", size =16))
