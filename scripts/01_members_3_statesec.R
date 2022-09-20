
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



statesec_missing <- data.frame(
  nachname = c("Manger-Koenig", "Manger-Koenig"),
  vorname = c("Ludwig", "Ludwig"),
  praefix = c("von", "von"),
  wp = c(6, 7),
  type = c("state secretary", "state secretary")
)


statesec <- bind_rows(statesec_wiki, statesec_1_5, statesec_missing)


saveRDS(statesec, file = "../data/statesec.RDS")
