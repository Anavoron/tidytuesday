library(tidyverse)
library(here)
library(png)
library(magick)

astronauts_orig <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

astronauts <- astronauts_orig

# uniting European countries into the "Europe" category for a clearer visualisation

astronauts$nationality <- as.factor(astronauts$nationality)

astronauts <- astronauts %>% mutate(nationality1 = fct_collapse(nationality,
                                                  Europe = c("Austria","Belgium",
                                                             "Bulgaria","Czechoslovakia",
                                                             "Denmark","France",
                                                             "Germany","Italy",
                                                             "Netherland",
                                                             "Poland","Romania",
                                                             "Slovakia","Spain",
                                                             "Sweden","Switzerland"
                                                             )))

# there was one outlier in eva_hrs_mission, checking who it was
astronauts %>% 
  filter(eva_hrs_mission > 50) %>% select(name)


# making the graph

astronauts_plot <- astronauts %>% 
  filter(eva_hrs_mission > 0 & eva_hrs_mission < 50 & nationality1 != "U.K./U.S.") %>%
  ggplot(aes(x = year_of_mission, y = eva_hrs_mission, colour = nationality1)) +
  # add scatter points
  geom_point(alpha = 0.8,size = 3.5, position = "jitter", shape = 18) +
  # change labels
  labs(title = "Humans walking in space (hours)",
       color = "Country:") +
  # change x scale breaks
  scale_x_continuous(limits = c(1960,2020),
                     breaks = c(1960,1970,1980,1990,2000,2010,2020)) +
  theme(panel.background = element_rect(fill = "#000015"),
        plot.background = element_rect(fill = "#000015"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour = "white"),
        axis.text.x = element_text(colour = "white",
                                   size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(colour = "white",
                                   size = 12),
        axis.ticks.y = element_line(colour = "white"),
        plot.title = element_text(colour = "white",
                                  size = 25,
                                  hjust = .5,
                                  face = "bold"),
        legend.background = element_rect(fill="#000015", 
                                         size=0.5, linetype="solid",
                                         colour = "#000015"),
        legend.key = element_rect(fill="#000015"),
        legend.text = element_text(colour = "white",
                                   size = 12),
        legend.title = element_text(colour = "white",
                                    size = 13),
        legend.position = "top",
        text = element_text(family = "mono")
        ) +
  scale_colour_manual(values = c('#3cb44b', '#e6194b', '#ffe119', 
                                 '#4363d8', '#800000', '#f032e6', 
                                 '#46f0f0', '#f58231')) +
  # making the square with spacewalking record
  annotate(geom = "text", x = 1965, y = 33, label = "World record of spacewalking:", 
           hjust = "left", colour = "#ffffff", size = 4.5, family = "mono") +
  annotate(geom = "text", x = 1965, y = 31.5, label = "Anatoly Solovyev (USSR/Russia)", 
           hjust = "left", colour = "#f58231", size = 4.5, family = "mono") +
  annotate(geom = "text", x = 1965, y = 29, label = "78 hours", 
           hjust = "left", colour = "#f58231", size = 8.5, family = "mono") +
  annotate(geom = "text", x = 1965, y = 27, label = "28 minutes", 
            hjust = "left", colour = "#f58231", size = 8.5, family = "mono")+ 
  annotate(geom = "rect", xmin = 1960, xmax = 1977, ymin = 25, ymax = 35, colour = "white", alpha = 0)+
  # Aleksei Leonov
  annotate(geom = "text", x = 1960, y = 3, label = "Aleksei Leonov",
           hjust = "left", colour = "#f58231", size = 4.5, family = "mono") +
  # Ed White
  annotate(geom = "text", x = 1960, y = 0, label = "Ed White",
           hjust = "left", colour = "#46f0f0", size = 4.5, family = "mono") +
  # arrow for White
  annotate(geom = "curve", x = 1963.5, y = 0, xend = 1964.5, yend = 0,7, 
    curvature = 0.1, arrow = arrow(length = unit(2, "mm")), colour = "#46f0f0", size = 0.5) +
  # arrow for Leonov
  annotate(geom = "curve", x = 1965.4, y = 2.65, xend = 1965.15, yend = 1.15, 
           curvature = -0.1, arrow = arrow(length = unit(2, "mm")), colour = "#f58231", size = 0.5)+
  # Svetlana Savitskaya
  annotate(geom = "text", x = 1985, y = 1, label = "Svetlana Savitskaya",
           hjust = "left", colour = "#f58231", size = 4.5, family = "mono") +
  # who is Svetlana
  annotate(geom = "text", x = 1985, y = 0, label = "First woman to spacewalk",
           hjust = "left", colour = "#f58231", size = 4.5, family = "mono") +
  # arrow for Savitskaya
  annotate(geom = "curve", x = 1985, y = 1, xend = 1984.1, yend = 3, 
           curvature = -0.1, arrow = arrow(length = unit(2, "mm")), colour = "#f58231", size = 0.5) +
  # total hours in space (decided not to include in the end)
  # annotate(geom = "text", x = 2006, y = 38.3, label = "Total hours outside",
  #         hjust = "left", colour = "#ffffff", size = 4.5, family = "mono",
  #         angle = 18) +
  #
  # women hours in space
  annotate(geom = "text", x = 2012.8, y = 38.6, label = "Women: 289 hours total",
           hjust = "left", colour = "green", size = 4, family = "mono") +
  # men hours in space
  annotate(geom = "text", x = 2013.2, y = 35, label = "Men: 4238 hours total",
           hjust = "left", colour = "#FF00FF", size = 4, family = "mono")


# adding the rocket picture

# loading the image of a rocket
img <- readPNG(here::here("rocket3.png"))

# putting it into the correct format (magick package)
rocket <- image_read(img)
rocket1 <- image_fill(rocket, 'none')
rocket2 <- as.raster(rocket1)


# total spacewalking by sex (to know which numbers to put in)

astronauts %>% filter(eva_hrs_mission > 0 & eva_hrs_mission < 50 & nationality1 != "U.K./U.S.") %>%
  group_by(sex) %>%
  summarise(n = sum(eva_hrs_mission))

# making astronaut picture

img1 <- readPNG(here::here("astronaut.png"))

# putting it into the correct format (magick package)
astronaut <- image_read(img1)
astronaut1 <- image_fill(astronaut, 'none')

astronaut1 <- image_flop(astronaut1)

# making the top part

astronaut1_top <- image_crop(astronaut1,"280x100+100") %>% 
  image_modulate(saturation = 150,hue = 150)

astronaut1_top_an <- as.raster(astronaut1_top)


# making the bottom part

astronaut1_bottom <- image_crop(astronaut1,"280x370+100+110") %>%
  image_modulate(saturation = 150, hue = 80)

astronaut1_bottom_an <- as.raster(astronaut1_bottom)


# uniting all into one picture

astronauts_plot + annotation_raster(rocket2, 1955,1970,25,35) + 
  annotation_raster(astronaut1_top_an,2009.1,2013.1,38,41) +
  annotation_raster(astronaut1_bottom_an,2009,2013,26.5,37.95)

# saving it as an object

astronauts_visual <- astronauts_plot + annotation_raster(rocket2, 1955,1970,25,35) + 
  annotation_raster(astronaut1_top_an,2009.1,2013.1,38,41) +
  annotation_raster(astronaut1_bottom_an,2009,2013,26.5,37.95)

