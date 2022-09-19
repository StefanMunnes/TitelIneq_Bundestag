
# https://opendiscourse.de/daten-und-methodik
# https://offenesparlament.de/daten/

library(dplyr)
library(tidyr)
library(stringr)
library(XML)
library(purrr)
library(lubridate)
library(rvest)
library(xml2)


try(setwd("scripts/"), silent = TRUE)


source("../scripts/01_members_1_mbs.R")



url_wiki <- "https://de.wikipedia.org"


# define year intervall for WPs
wp_years <- c(
  "1" = "1949-1953", "2" = "1953-1957", "3" = "1957-1961", "4" = "1961-1965",
  "5" = "1965-1969", "6" = "1969-1972", "7" = "1972-1976", "8" = "1976-1980",
  "9" = "1980-1983", "10" = "1983-1987", "11" = "1987-1990", "12" = "1990-1994",
  "13" = "1994-1998", "14" = "1998-2002", "15" = "2002-2005",
  "16" = "2005-2009", "17" = "2009-2013", "18" = "2013-2017",
  "19" = "2017-2021", "20" = "2021-2025"
)

# create function to get Wahlperiode from years -> rowwise data.frame for join
get_wp <- function(jahr_von, jahr_bis) {
  lapply(seq_along(wp_years), function(index) {
    wp_von <- str_extract(wp_years[index], "^[0-9]{4,4}") |> as.numeric()
    wp_bis <- str_extract(wp_years[index], "[0-9]{4,4}$") |> as.numeric()

    if (wp_bis < min(jahr_von)) {
      return(NULL)
    }

    rows_von <- which(between(jahr_von, wp_von, wp_von + 3))
    rows_bis <- which(between(jahr_bis, wp_bis - 3, wp_bis))

    data.frame(row = unique(append(rows_von, rows_bis)), wp = index)
  }) |>
    bind_rows() |>
    group_by(row) |>
    summarize(wp = paste(wp, collapse = ";")) |> # nolint
    mutate(
      wp = str_extract_all(wp, "[0-9]{1,2}")
    ) |>
    rowwise() |>
    transmute(
      wp = list(as.numeric(wp)),
      wp = list(seq.int(min(wp), max(wp))),
      wp = paste0(wp, collapse = ";")
    )
}


source("../scripts/01_members_2_minister.R")

source("../scripts/01_members_3_statesec.R")



# ---- 4. combine the two data.frames of MDBs and ministers ----
mdbs <- readRDS("../data/mdbs.RDS")
minister <- readRDS("../data/minister.RDS")
statesec <- readRDS("../data/statesec.RDS")

members <- mdbs |>
  mutate(geburtsjahr = str_extract(geburtsdatum, "[0-9]{4,4}")) |>
  bind_rows(minister) |>
  bind_rows(statesec) |>
  select(
    nachname, vorname, anrede_titel, adel, praefix, geburtsjahr, geschlecht,
    partei_kurz, partei_wiki, wp, type
  ) |>
  distinct(nachname, vorname, wp, .keep_all = TRUE)


saveRDS(members, file = "../data/members.RDS")
