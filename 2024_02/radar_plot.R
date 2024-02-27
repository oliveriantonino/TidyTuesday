library(data.table)
library(magrittr)
library(stringr)
library(tidyverse)
library(tidytuesdayR)
library(countrycode)
#devtools::install_github("ricardo-bion/ggradar")
library(ggradar)
library(camcorder)


# Input and output --------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, week = 2)
device <- "png"
output_folder <- "2024_02"


# Start of the script -----------------------------------------------------

if(!dir.exists(output_folder)) {dir.create(output_folder)}

nhl_player_births <-
    tuesdata$nhl_player_births %>%
    mutate(
        birth_month_letters = 
            factor(
                birth_month,
                levels=1:12,
                labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                ordered = T
            )
    ) %>% 
    select(player_id, birth_year, birth_month_letters) %>% 
    distinct() %>% 
    arrange(birth_month_letters, birth_year)


nhl_player_births_by_month <- 
    nhl_player_births %>% 
    summarize(
        N = n(),
        .by = birth_month_letters
    ) 

summary_nhl_player_births_by_month <-
    nhl_player_births_by_month %>%
    summarize(
        min = 0,
        avg = as.integer(round(max(N)/2), digits = 0),
        max = max(N)
    ) %>% 
    unlist()

nhl_player_births_by_month_wide <-
    nhl_player_births_by_month %>% 
    pivot_wider(names_from = birth_month_letters, values_from = N) %>% 
    bind_cols(
        Group = "Overall"
    ) %>% 
    select(Group, everything())

gg_record(
    dir = output_folder,
    device = device,
    width = 16,
    height = 9,
    units = 'in',
    dpi = 300,
    bg = "white"
)

ggradar(
    nhl_player_births_by_month_wide,
    values.radar = summary_nhl_player_births_by_month,
    grid.min = summary_nhl_player_births_by_month["min"],
    grid.mid = summary_nhl_player_births_by_month["avg"],
    grid.max = summary_nhl_player_births_by_month["max"],
    plot.title = "More NHL Players are born in the first half of the year",
    background.circle.colour = "white",
    gridline.mid.colour = "grey80"
)

gg_stop_recording()

img_file <- dir(output_folder, pattern = device) 

file.rename(
    from = paste0(output_folder, "/", img_file),
    to = paste0(output_folder, "/", "radar_plot.", device)
)