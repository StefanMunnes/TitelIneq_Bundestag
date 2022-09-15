
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
    summarize(wp = paste(wp, collapse = ";")) |>
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





# ---- 1. load and prepare MDB data from XML file ----
mdb1 <- XML::xmlParse("../data/MDB_STAMMDATEN.XML")
mdb2 <- xmlToList(mdb1)

mdb3 <- mdb2[2:length(mdb2)]

mdb4 <- lapply(mdb3, function(mdb) {
  id <- mdb$ID
  name <- map_df(mdb$NAMEN, ~.x)
  bio <- map_df(mdb$BIOGRAFISCHE_ANGABEN, ~.x)
  wp <- map_df(mdb$WAHLPERIODEN, ~.x)

  tidyr::crossing(id, name, bio, wp)
})

mdbs <- bind_rows(mdb4) |>
  rename_with(tolower) |>
  select(
    id, nachname, vorname, anrede_titel, adel, praefix,
    geburtsdatum, geburtsort, geburtsland, sterbedatum, geschlecht,
    familienstand, religion, beruf,
    partei_kurz, wp, mdbwp_von, mdbwp_bis, wkr_nummer, wkr_land,
    historie_von, historie_bis
  ) |>
  mutate(
    id = str_sub(id, -4, -1) |> as.numeric(),
    wp = as.numeric(wp)
  ) |>
  group_by(id) |>
  distinct() |>
  ungroup() |>
  # remove obs with informations from time, not overlap with wahlperiode
  mutate(across(starts_with(c("mdbwp", "historie")), dmy),
    across(ends_with("bis"), ~ if_else(is.na(.x), today(), .x)),
    mdbwp    = interval(mdbwp_von, mdbwp_bis),
    historie = interval(historie_von, historie_bis)
  ) |>
  filter(int_overlaps(mdbwp, historie)) |>
  # remove dublicates
  group_by(id, wp) |>
  distinct(across(id:wp), .keep_all = TRUE) |>
  # remove title if inconsistent information (observation with and without Dr.)
  mutate(
    anrede_titel = ifelse(is.na(anrede_titel), "", anrede_titel),
    temp = sum(str_detect(anrede_titel, "Dr")),
    anrede_titel = ifelse((temp > 0 & temp < n()) | (temp == 0), NA, "Dr."),
    type = "mp"
  ) |>
  distinct(across(id:wp), .keep_all = TRUE) |>
  select(!temp) |>
  ungroup()


saveRDS(mdbs, file = "../data/mdbs.RDS")



# ---- 2. additional extract ministers from list of wikipedia ----
# https://github.com/open-discourse/open-discourse/blob/develop/python/src/
# -> od_lib/03_politicians/02_scrape_mgs.py
# -> README.md
url_minister <- "/wiki/Liste_der_deutschen_Regierungsmitglieder_seit_1949"


minister_nowp <- rvest::read_html(paste0(url_wiki, url_minister)) |>
  rvest::html_nodes("h3+ ul li") |>
  rvest::html_text() |>
  str_subset("^[A-ZÄÖÜ]") |>
  as.data.frame() |>
  rename(name = 1) |>
  # clean entrys
  separate(name,
    into = c("name", "ministerium"),
    sep = "\\n", extra = "merge", fill = "right"
  ) |>
  separate(name,
    into = c("name", "partei_wiki"),
    sep = ", ", extra = "merge", fill = "right"
  ) |>
  separate(name,
    into = c("name", "life"),
    sep = "\\s\\(", extra = "drop", fill = "right"
  ) |>
  mutate(
    praefix = str_extract(name, "\\s((von und zu)|(von der)|von|zu|van|de)\\s"),
    name = str_remove_all(name, "\\s((von und zu)|(von der)|von|zu|van|de)"),
    adel = str_extract(name, "Graf"),
    name = str_remove(name, "Graf"),
    name = str_replace(name, " ([A-ZÄÖÜa-zäöüßè-]*?)$", ";\\1"),
    life = str_remove_all(life, "\\)|(\\*\\s)"),
  ) |>
  separate(life,
    into = c("geburtsjahr", "sterbejahr"),
    sep = "[\U{2013}]", fill = "right"
  ) |>
  separate(name, into = c("vorname", "nachname"), sep = ";") |>
  # correct names for matching process
  mutate(vorname = case_when(
    vorname == "Gerhart" ~ "Gerhart Rudolf",
    vorname == "Gustav" ~ "Gustav W.",
    vorname == "Ursula" & nachname == "Lehr" ~ "Ursula Maria",
    nachname == "Möllemann" ~ "Jürgen W.",
    vorname == "Theo" ~ "Theodor",
    TRUE ~ vorname
  )) |>
  separate_rows(ministerium, sep = "\\n") |>
  mutate(
    ministerium = str_replace(ministerium, "seit ([0-9]{4,4})", "\\1;2022") |>
      str_replace_all("([0-9]{4,4})(, )([0-9]{4,4})", "\\1;\\3")
  ) |>
  separate(ministerium,
    into = c("minister_von_bis", "ministerium"),
    sep = " ", extra = "merge"
  ) |>
  separate_rows(minister_von_bis, sep = ";") |>
  separate(minister_von_bis,
    into = c("minister_von", "minister_bis"), sep = "[\U{2013}]",
    fill = "right", convert = TRUE
  ) |>
  mutate(
    minister_bis = ifelse(is.na(minister_bis), minister_von, minister_bis),
    type = "minister"
  )


# extract WP rowwise from years in intervall
minister_wp_rows <- get_wp(
  minister_nowp$minister_von,
  minister_nowp$minister_bis
)


minister <- cbind(minister_nowp, minister_wp_rows) |>
  separate_rows(wp, sep = ";", convert = TRUE) |>
  distinct()


saveRDS(minister, file = "../data/minister.RDS")



# ---- 3. get State Secretaries from Wikipedia ----
url_cabinets <- "/wiki/Kategorie:Bundeskabinett_(Deutschland)"

urls_cabinets <- read_html(paste0(url_wiki, url_cabinets)) |>
  html_nodes(".mw-category-group+ .mw-category-group a") |>
  html_attr("href")

urls_cabinets <- urls_cabinets[!grepl("Regierungsmitglieder", urls_cabinets)]


statesec_tables_ls <- lapply(urls_cabinets, function(url) {
  Sys.sleep(1)

  html <- read_html(paste0(url_wiki, url))

  # replace br linebreak with split element for later separation
  xml_find_all(html, ".//br") |> xml_add_sibling("p", ";")
  xml_find_all(html, ".//br") |> xml_remove()

  # extract valid tables and captions
  tables_raw <- html_nodes(html, "table.wikitable") |> html_table()
  captions <- html_nodes(html, "caption") |> html_text()

  # break loop & return NULL if no valid table
  if (length(tables_raw) == 0) {
    return(NULL)
  }

  # loop over tabs & prep var names; if no Staatssekretär -> NULL; add caption
  tables_all <- lapply(seq_along(tables_raw), function(index) {
    tab <- tables_raw[[index]]

    names(tab) <- make.names(names(tab), unique = TRUE)

    if (all(!grepl("Staatssekretär", names(tab)))) {
      return(NULL)
    }

    tab <- mutate(tab, caption = captions[index])

    names(tab)[grepl("Staatssekretär", names(tab))] <- "Staatssek"
    names(tab)[grepl("^Amt", names(tab))] <- "Amt"

    return(tab)
  })

  tables <- tables_all[lengths(tables_all) != 0]
  table <- bind_rows(tables)

  message(
    url, " - ",
    length(tables_all), ",", length(captions), " - ",
    length(tables), " - ", ncol(table)
  )

  return(table)
})

names(statesec_tables_ls) <- urls_cabinets

statesec_nowp <- statesec_tables_ls[lengths(statesec_tables_ls) != 0] |>
  bind_rows(.id = "url") |>
  select(url, Amt, Name, Staatssek, caption) |>
  # prepare further for split -> linebreak to split
  mutate(Staatssek = str_replace_all(Staatssek, "\\n", ";")) |>
  separate_rows(Staatssek, sep = ";") |>
  # prep necessary variables
  transmute(
    Staatssek = str_remove_all(Staatssek, "\\[.*?\\]"),
    Staatssek = case_when(
      str_detect(Staatssek, "Friedrich Vogel") ~ "Friedrich Vogel",
      str_detect(Staatssek, "Marco Wanderwitz") ~ "Marco Wanderwitz",
      str_detect(Staatssek, " und (?!zu)|,") ~ "",
      str_detect(Staatssek, "Bundesregierung|Staatsminister") ~ "",
      Staatssek %in% c(
        "Internationale Kulturpolitik",
        "Maritime Wirtschaft"
      ) ~ "",
      TRUE ~ Staatssek
    ),
    caption = ifelse(url == "/wiki/Kabinett_Scholz", "2021, 2022", caption)
  ) |>
  rowwise() |>
  mutate(
    wp_von = str_extract_all(caption, "[0-9]{4,4}") |> unlist() |>
      as.numeric() |>
      min(),
    wp_bis = str_extract_all(caption, "[0-9]{4,4}") |> unlist() |>
      as.numeric() |>
      max()
  ) |>
  filter(!str_detect(Staatssek, "[0-9]+") & str_count(Staatssek, "\\w+") > 1) |>
  select(!caption) |>
  distinct() |>
  mutate(
    Staatssek = str_squish(Staatssek),
    nachname = str_extract(
      Staatssek,
      "[A-ZÄÖÜ][a-zäöüßğ]+(-[A-ZÄÖÜ][a-zäöüßğ]+)*$"
    ),
    vorname = str_extract(Staatssek, paste0(".*(?= ", nachname, ")")),
    praefix = str_extract(vorname, "de|vom|dos|zu|van|von( der| und zu)*$"),
    vorname = str_remove(vorname, " de|vom|dos|zu|van|von( der| und zu)*$"),
    adel = str_extract(vorname, "Freiherr"),
    vorname = str_remove(vorname, " Freiherr"),
    type = "state secretary"
  )



statesec_wp_rows <- get_wp(statesec_nowp$wp_von, statesec_nowp$wp_bis)


statesec_wiki <- cbind(statesec_nowp, statesec_wp_rows) |>
  separate_rows(wp, sep = ";", convert = TRUE) |>
  select(nachname, vorname, adel, praefix, wp, type) |>
  distinct()



statesec_1_5 <- read.csv2("../data/state_secretary_1-5.csv",
  encoding = "UTF-8"
) |>
  rename(name = 1) |>
  mutate(
    name = str_squish(name),
    name = str_replace_all(name, "- ", "-"),
    nachname = str_extract(
      name,
      "[A-ZÄÖÜ][a-zäöüßğ]+(-[A-ZÄÖÜ][a-zäöüßğ]+)*$"
    ),
    vorname = str_extract(name, paste0(".*(?= ", nachname, ")")),
    praefix = str_extract(vorname, "de|vom|dos|zu|van|von( der| und zu)*$"),
    vorname = str_remove(vorname, " de|vom|dos|zu|van|von( der| und zu)*$"),
    adel = str_extract(vorname, "Freiherr"),
    vorname = str_remove(vorname, " Freiherr"),
    type = "state secretary"
  ) |>
  select(nachname, vorname, adel, praefix, wp, type)


statesec <- bind_rows(statesec_wiki, statesec_1_5)


saveRDS(statesec, file = "../data/statesec.RDS")



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
