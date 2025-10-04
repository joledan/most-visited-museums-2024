rm(list=ls())
library(tidyverse)
library(rvest)
library(janitor)
library(stringr)
library(tidygeocoder)
library(countrycode)
library(ellmer)

# 2019
url2019 <- "https://web.archive.org/web/20201206221159/https://en.wikipedia.org/wiki/List_of_most-visited_art_museums"

# most recent article for 2024
url2024 <- "https://en.wikipedia.org/wiki/List_of_most-visited_art_museums"

# rename columns
new_names <- c(rank = "art_museums",
    rank = "n",
    rank = "rank_in_2024_1",
    museum = "art_museums_2",
    city = "art_museums_3",
    city = "country_and_city",
    visitors = "art_museums_4",
    visitors = "annual_visitation_figures_for_2019_as_reported_by_the_art_newspaper",
    visitors = "visitors_annualy_a",
    visitors = "visitors_annually_a",
    visitors = "visitors_annually")

# function to scrape tables
scrape_table <- . %>%
    read_html() %>%
    html_node("table.wikitable") %>%
    html_table(fill = TRUE) %>%
    clean_names() %>%
    rename(any_of(new_names)) %>%
    select(rank, museum, city, visitors) %>%
    mutate(visitors = str_extract(visitors, "^[^\\s]+") %>% str_replace_all(., c("," = "", ", " = ",")) %>% as.numeric(.),
        address = paste(museum, city, sep = ", ")) %>%
    filter(!is.na(visitors))

#### get data from 2019, 2024 ####
df2019 <- url2019 %>%
    scrape_table() %>%
    mutate(year = 2019)

df2024 <- url2024 %>%
    scrape_table() %>%
    mutate(year = 2024,
        visitors = case_when(
            museum == "National Portrait Gallery" ~ 1578065,
            TRUE ~ visitors),
        rank = rank(-visitors))

#### combine both years ####
df <- bind_rows(df2019, df2024) %>%
    mutate(address = paste(museum, city, sep = ", ")) %>%
    group_by(address) %>%
    mutate(n = n())

write_delim(df, paste0("./data/out/museums_raw_2019_2024.csv"))


#### set up for llm ####
# use llm to standardize museum names across both years 
unique_names <- read_delim(paste0("./data/out/museums_raw_2019_2024.csv")) %>% 
    select(address) %>% 
    unique() %>% 
    arrange(address)
names_vector <- unique_names$address
museum_list_text <- paste(seq_along(names_vector), names_vector, sep = ". ", collapse = "\n")

#### set up chat and prompt ####

# define output structure
type_museum <- type_array(
    type_object(
        original_name = type_string("The original, uncleaned input name of the museum"),
        museum_name = type_string("Standardized, canonical name of the museum"),
        city = type_string("Standardized name of the city where the museum is located"),
        country = type_string("Country where the museum is located")
    )
)

chat <- chat_openai(model = "gpt-5-nano")

prompt <- paste(
      "Here is a list of museum names (one per line). Group names based on similarity into a single canonical institution.",
      "Return the output, with one row per original museum name. Make sure all original museum names are present. If you could not find a standard name, city, or country, make sure these columns are NA. For each original museum name, return the following variables:  - `original_name`: the original input name, - `museum_name`: the canonical institution name used, `city`: the canonical city of the museum, `country`: country of the museum.\n\n",
      museum_list_text
    )

# get results, output 
results <- chat$chat_structured(prompt, type = type_museum, echo = "text")

write_delim(results, paste0("./data/out/museums_standardized.csv"))


#### read standardized names, and clean up results ####
df <- read_delim(paste0("./data/out/museums_raw_2019_2024.csv"))
museums_std <- read_delim(paste0("./data/out/museums_standardized.csv")) %>%
    rename(museum = original_name)

# merge standardized museum names
# quality check
# National Gallery of Victoria, Melbourne
    # 2024, NGV International and Ian Potter Centre are two buildings, same system -> collapse into one
# Centro Cultural Banco do Brasil
    # appears in 3 cities, Rio de Janeiro, Belo Horizonte, and 
# National Museum of Modern and Contemporary Art and National Museum of Modern and Contemporary Art, Seoul -> same thing, collapse into 1
# In 2024, Shanghai Museum opened a new branch, Shanghai Museum East, so appears twice on the 2024 list -> consider as one
# Belvedere and Upper Belvedere include Lower, Upper, and Belvedere museums -> consider as one 
# National Portrait Gallery, DC and Smithsonian American Art Museum are in the same building, and report only one number for visitor count -> consider as one 

df1 <- df %>%
    select(museum = address, visitors, year) %>%
    left_join(museums_std) %>%
    mutate(
        museum_std = case_when(
            museum_name == "Centro Cultural Banco do Brasil" ~ paste(museum_name, city, sep = ", "), 
            str_detect(museum_name, "Belvedere") ~ "Belvedere Museum",
            museum_name %in% c("National Museum of Modern and Contemporary Art, Seoul", "National Museum of Modern and Contemporary Art") ~ "National Museum of Modern and Contemporary Art, Seoul",
            str_detect(museum, "Donald W. Reynolds Center") ~ "Smithsonian American Art Museum and National Portrait Gallery",
            museum_name == "Shanghai Museum East" ~ "Shanghai Museum",
            TRUE ~ museum_name),
        city = case_when(str_detect(city, "New York") ~ "New York City", TRUE ~ city)
    ) %>%
    group_by(museum = museum_std, city, country, year) %>%
    summarise(visitors = sum(visitors)) %>%
    mutate(continent = countrycode(sourcevar = country,
                        origin = "country.name", # Or the relevant origin code
                        destination = "continent")) %>%
    group_by(year) %>%
    mutate(rank = rank(-visitors)) %>%
    filter(rank <= 100) %>%
    group_by(museum, city, country, continent) %>%
    mutate(n = n())


write_delim(df1, paste0("./data/out/museums_cleaned_2019_2024.csv"))


# top 300
url_top300 <- "https://culturalindex.substack.com/p/most-recognizable-paintings-revisited"

# extract table
top300 <- read_html(url_top300)
df <- top300 %>%
    html_nodes("p") %>%
    html_text()

# combine into one string
all_text <- paste(df, collapse = "\n")

# extract lines that start with a digit
p <- str_extract_all(all_text, "\\d+\\.\\s.*")[[1]]

# convert to df
dfp <- tibble(rank = seq_along(p), painting = p) %>%
    filter(rank != 1) %>%
    separate_wider_delim(painting, delim = "; ",  names = c("title", "artist", "museum", "extra"), too_few = "debug") %>%
    mutate(title = if_else(str_detect(title, "244. Paris Street"), paste(title, artist, sep = "; "), title),
        artist = if_else(str_detect(title, "244. Paris Street"), museum, artist),
        museum = if_else(str_detect(title, "244. Paris Street"), extra, museum)) %>%
    select(-c(extra, starts_with("painting_"), rank)) %>%
    separate_wider_regex(title, c(rank = "\\d+\\.", title = ".*"))
