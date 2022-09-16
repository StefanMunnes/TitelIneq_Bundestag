
# ---- 1. additional extract ministers from list of wikipedia ----
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
