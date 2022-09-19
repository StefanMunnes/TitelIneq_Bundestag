
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
