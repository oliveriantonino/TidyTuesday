library(data.table)
library(magrittr)
library(stringr)
library(tidyverse)
library(tidytuesdayR)
library(gganimate)
library(ggthemes)
library(countrycode)
library(ggimage)

# Start of the script -----------------------------------------------------

tuesdata <- tt_load(2023, week = 49)

life_expectancy <- 
    tuesdata$life_expectancy %>% 
    mutate(
        Code_iso2c = countrycode(Entity, origin = 'country.name', destination = 'iso2c'),
        Continent = countrycode(Code_iso2c, "iso2c", "continent")
    ) %>% 
    relocate(Code_iso2c, .after = Code) %>% 
    relocate(Continent, .after = Code_iso2c) %>% 
    filter(
        !is.na(Code_iso2c),
        !is.na(LifeExpectancy),
        Year > 1950
    )

life_expectancy_ranking <- 
    life_expectancy %>%
    group_by(Year) %>% 
    mutate(
        rank = rank(-LifeExpectancy),
        Value_rel = LifeExpectancy/LifeExpectancy[rank==1],
        Value_lbl = paste0(" ",round(LifeExpectancy,1))
    ) %>% 
    group_by(Entity) %>% 
    filter(rank <= 10) %>% 
    ungroup() %>% 
    arrange(Year, rank)

# Lollipop chart---- 

life_expectancy_ranking %>%
    filter(
        Year == 1951
    ) %>% 
    ggplot(
        aes(rank, group=Entity)
    ) +
    geom_segment( aes(x=rank, xend=rank, y=60, yend=LifeExpectancy), color="black") +
    geom_point( aes(x=rank, y=LifeExpectancy, color=Continent), size = 22 ) +
    #geom_flag(y = 60, aes(image = Code_iso2c)) +
    geom_text(aes(y = LifeExpectancy, label = Value_lbl), color = "white", size = 7) +
    geom_text(aes(y = 57, label = paste(Entity, " ")), vjust = 0.2, hjust = 1, size = 10) +
    #geom_text(aes(y = LifeExpectancy, label = Value_lbl, hjust=0)) +
    xlab("Country") +
    ylab("Life Expectancy (years)") +
    scale_x_reverse() +
    theme(axis.line=element_blank(),
          axis.text.x=element_text(size = 15),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.ticks.length.x = unit(1, "cm"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="bottom",
          legend.text = element_text(size =  15),
          legend.title = element_text(size = 15),
          legend.background = element_rect(fill = "aliceblue"),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x = element_line(linewidth=.1, color="grey" ),
          panel.grid.minor.x = element_blank(),
          #panel.grid.minor.x = element_line(linewidth=.1, color="grey" ),
          plot.title=element_text(size=45, hjust=0.5, face="bold", colour="grey20"),
          plot.subtitle=element_text(size=35, hjust=0.5, color="grey20"),
          plot.caption =element_text(size=13, hjust=0.5, face="italic", color="grey20"),
          plot.background=element_rect(fill = "aliceblue"),
          plot.margin = margin(2, 2, 2, 10, "cm")) +
    guides(colour = guide_legend(override.aes = list(size=15))) +
    scale_color_brewer(palette="Dark2", name = "Continent") +
    ylim(53,95) +
    #scale_y_continuous(limits = c(53,90), expand = expansion(mult = c(50, 50))) +
    coord_flip(clip = "off", expand = FALSE) +
    labs(title = 'Life Expectancy per Year : 1951',  
         subtitle  =  "Top 10 Countries",
         caption  = "Life Expectancy in Years | Data Source: TidyTuesday")

staticplot <- 
    life_expectancy_ranking %>%
    ggplot(
        aes(rank, group=Entity)
    ) +
    geom_segment( aes(x=rank, xend=rank, y=60, yend=LifeExpectancy), color="black") +
    geom_point( aes(x=rank, y=LifeExpectancy, color=Continent), size = 22 ) +
    geom_flag(y = 60, aes(image = Code_iso2c)) +
    geom_text(aes(y = LifeExpectancy, label = Value_lbl), color = "white", size = 7) +
    geom_text(aes(y = 57, label = paste(Entity, " ")), vjust = 0.2, hjust = 1, size = 10) +
    #geom_text(aes(y = LifeExpectancy, label = Value_lbl, hjust=0)) +
    xlab("Country") +
    ylab("Life Expectancy (years)") +
    scale_x_reverse() +
    theme(
        axis.line=element_blank(),
        axis.text.x=element_text(size = 15),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.ticks.length.x = unit(1, "cm"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        legend.text = element_text(size =  15),
        legend.title = element_text(size = 15),
        legend.background = element_rect(fill = "aliceblue"),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line(linewidth=.1, color="grey" ),
        panel.grid.minor.x = element_blank(),
        plot.title=element_text(size=45, hjust=0.5, face="bold", colour="grey20"),
        plot.subtitle=element_text(size=35, hjust=0.5, color="grey20"),
        plot.caption =element_text(size=13, hjust=0.5, face="italic", color="grey20"),
        plot.background=element_rect(fill = "aliceblue"),
        plot.margin = margin(2, 2, 2, 10, "cm")
    ) +
    guides(colour = guide_legend(override.aes = list(size=15))) +
    scale_color_brewer(palette="Dark2", name = "Continent") +
    ylim(53,95) +
    coord_flip(clip = "off", expand = FALSE)

anim = 
    staticplot + 
    transition_states(Year, transition_length = 4, state_length = 1) +
    view_follow(fixed_x= TRUE) +
    labs(
        title = 'Life Expectancy per Year : {closest_state}\n',  
        subtitle  =  "Top 10 Countries",
        caption  = "Life Expectancy in Years | Data Source: https://ourworldindata.org/life-expectancy"
    )

animate(
    anim,
    4000, fps = 60,
    width = 1200, height = 1000, 
    renderer = gifski_renderer("animated_chart_life_expectancy.gif")
)
