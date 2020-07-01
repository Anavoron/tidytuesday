x<-c("readr","tidyverse")
lapply(x,require,character.only = TRUE)
rm(x)

# Reading the data in and making a pain (alas) dataset:

characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')

# I read a recommendation somewhere to save the original dataset separately
# and I find it really useful

pain_orig <- characters %>% select(character, rendered_unconcious,captured,declared_dead,depowered,subject_to_torture)

pain <- pain_orig

# Putting the data into the long format and only extracting the characters we want to look at:

pattern = c("Professor X","Wolverine = Logan","Cyclops = Scott Summers", "Marvel Girl/Phoenix = Jean Grey",
            "Storm = Ororo Munroe","Nightcrawler = Kurt Wagner","Gambit = Name Unknown", "Rogue = Name Unknown")

pain <- pain %>% gather(event,occasion,2:6) %>% filter(str_detect(character, paste(pattern, collapse = "|")))

# Plot time:

polar_x <- ggplot(pain, aes(x = factor(character),y = occasion,fill = event)) + 
  geom_col(width = 1) + 
  coord_polar() +
  geom_hline(yintercept = seq(0, 80, by = 10), #making circles to mark every ten hardships
             color = "#CFB53B", size = 0.5) + 
  geom_vline(xintercept = seq(.5, 16.5, by = 1), #making radial lines 
             color = "#CFB53B", size = 0.8) +
  scale_y_continuous(breaks = c(0,40,80))+
  labs(title = "Hardships endured by the lead X-men",
       fill = "Hardships:") +
  scale_fill_manual(values = c("#201520","#505268","#4C3A30","#C3C9BD","#A29D4C"),
                    labels = c("Captured","Declared dead","Depowered","Rendered unconcious",
                               "Subject to torture"))+
  theme(axis.text.x = element_text(size=10,colour = "#0082A8" ,
                                   angle = seq(-25,-335,length.out = 8),vjust = 90), #putting the character names around the polar chart
        axis.ticks.y = element_line(colour = "#CFB53B"),
        axis.text.y = element_text(colour = "#CFB53B"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(colour = "#E9463F", hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(colour = "#E9463F")
  )

polar_x

