library(data.table)
library(magrittr)
library(stringr)
library(tidyverse)
library(tidytuesdayR)
library(glue)
library(ggtext)

# Input and output --------------------------------------------------------

#tuesdata <- tidytuesdayR::tt_load('2025-01-14')

output_folder <- "2025_02"

color_blind_friendly_pal <- 
    c(
        "#006ddb","#920000","#ff6db6","#db6d00","#490092",
        "#009292","#b66dff","#1c8943","#004949","#000000",
        "#ffb6db","#6db6ff","#b6dbff","#ffff6d", "#924900"
    )

# Start of the script -----------------------------------------------------


conf2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2023.csv')
conf2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2024.csv')

conf2023 %>% 
    distinct(speaker_affiliation) %>% 
    arrange(speaker_affiliation) %>% 
    View()

mod_conf2023 <- 
    conf2023 %>% 
    mutate(
        speaker_affiliation = speaker_affiliation %>% str_replace_all(", Inc.|, Inc|, LLC", ""),
        speaker_affiliation = speaker_affiliation %>% str_replace_all("/| /| and ", ", "),
        speaker_affiliation = speaker_affiliation %>% str_replace_all("  ", " "),
        speaker_affiliation = 
            case_when(
                speaker_affiliation %>% str_to_lower() %>% str_detect("a plus associates") ~ "A Plus Associates",
                speaker_affiliation %>% str_to_lower() %>% str_detect("posit") ~ "Posit",
                speaker_affiliation %>% str_to_lower() %>% str_detect("jumping rivers") ~ "Jumping Rivers",
                speaker_affiliation %>% str_to_lower() %>% str_detect("university|universidad|universiteit|uc santa barbara") ~ "University",
                speaker_affiliation %>% str_to_lower() %>% str_detect("pfizer") ~ "Pfizer",
                speaker_affiliation %>% str_to_lower() %>% str_detect("prostate cancer clinical trials consortium") ~ "PCCTC",
                speaker_affiliation %>% str_to_lower() %>% str_detect("freelance") ~ "Freelance",
                TRUE ~ speaker_affiliation
            )
    )

mod_conf2023 %>% 
    distinct(speaker_affiliation) %>% 
    arrange(speaker_affiliation) %>% 
    View()

summary_mod_conf2023 <-
    mod_conf2023 %>% 
    summarize(
        n = n(),
        .by = speaker_affiliation
    ) %>% 
    filter(
        n > 1
    ) %>% 
    arrange(
        -n
    ) %>% 
    mutate(
        speaker_affiliation = fct(speaker_affiliation)
    ) %>% 
    bind_cols(
        field = 
            c(
                "Posit & Appsilon", "Academia", "Posit & Appsilon",
                "Pharma", "Pharma", "Materials Science",
                "Data Science", "Data Science", "Assistance", "Data Science", "Sports")
    )

pal <- color_blind_friendly_pal[1:length(unique(summary_mod_conf2023$field))]
names(pal) <- unique(summary_mod_conf2023$field)

title_text <- glue::glue(
    'The companies with more than one speaker at posit::conf(2023) were primarily ',
    '<span style="color:{pal["Posit & Appsilon"]};">**Posit**</span>',
    ' and ',
    '<span style="color:{pal["Posit & Appsilon"]};">**Appsilon**</span>', ", ",
    'the hosts of the event.'
)

subtitle_text <- glue::glue(
    'This was followed by ',
    '<span style="color:{pal["Academia"]};">**academia**</span>',', ',
    'and other companies related to ',
    '<span style="color:{pal["Pharma"]};">**pharma**</span>',', ',
    '<span style="color:{pal["Data Science"]};">**data science**</span>',', ',
    '<span style="color:{pal["Materials Science"]};">**materials science**</span>',', ',
    '<span style="color:{pal["Assistance"]};">**assistance**</span>',
    ' and ',
    '<span style="color:{pal["Sports"]};">**sports**</span>','.'
)

caption_text <- "Source: TidyTuesday (2025-01-14)"

p <- 
    summary_mod_conf2023 %>% 
    ggplot(
        aes(speaker_affiliation, n, fill = field)
    ) +
    geom_col() +
    theme_minimal(base_size = 15) +
    theme(
        legend.position = "none",
        plot.title.position = 'plot',
        plot.title = ggtext::element_markdown(size = rel(1.3), face="bold"),
        plot.subtitle = ggtext::element_markdown(
            margin = margin(t = 0, b = 6, unit='mm')
        ),
        plot.caption = element_text(size = rel(0.6)),
    ) +
    labs(
       x = "Speaker affiliation",
       y = "Number of speakers",
       fill = "Field",
       title = title_text,
       subtitle = subtitle_text,
       caption = caption_text
    ) +
    scale_x_discrete(limits = rev) +
    scale_fill_manual(values = pal) +
    coord_flip()

ggsave(
    paste0(output_folder,"/","bar_plot_companies_with_more_than_one_speaker.jpeg"),
    width = 16,
    height = 9,
    dpi = 300
)
