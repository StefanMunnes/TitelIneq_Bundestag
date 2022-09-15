library(dplyr)
library(tidyr)
library(stringr)

setwd("scripts/")


# df <- readRDS("C:/Users/munnes/Data/plenarprotokolle.RDS")
# # df <- readRDS("M:/group/DSI/AF/_Data/BT_protokolle/plenarprotokolle.RDS")

# pt_smpl <- group_by(pt, wahlperiode) |>
#   slice_sample(n = 10)

# saveRDS(pt_smpl, file = "../data/pt_smpl.RDS")


pt_smpl <- readRDS("../data/pt_smpl.RDS")

pt_smpl <- group_by(pt_smpl, wahlperiode) |>
  slice_sample(n = 2)

df_notext <- select(pt_smpl, !text)


members <- readRDS("../data/members.RDS")


get_regex_names <- function(w) {
  filter(members, wp == w) |>
    mutate(nachname = case_when(
      .data$nachname == "Grund" ~ "Grund(?>!lage)",
      .data$nachname == "Volk" ~ "Volk(?>!er|mar)",
      .data$nachname == "Heil" ~ "Heil(?>!mittel)",
      TRUE ~ .data$nachname
    )) |>
    pull(nachname) |>
    unique() |>
    str_sort(decreasing = TRUE) |> # long version of name in front (Klein-ert)
    paste0(collapse = "|")
}


remove_header <- function(text_raw = text, rex_name = regex_n) {

  # remove main part of header (Deutscher Bundestag, session, place, date)
  regex_header_base <- regex("
    ([0-9]{1,5}\\.*?)?        # number of Wahlperiode
    \\s*Deutscher\\sBundestag # base name
    .*?Sitzung                # number of session
    .*?[0-9]{4,4}             # date till year
    ", comments = TRUE)

  text_raw <- str_replace_all(text_raw, regex_header_base, "\nHEADER\n")


  # remove page numbering around header (front or back)
  regex_header_page <- regex("
    ([0-9]{1,6}[.,\\s]{1,7})? # page num & space/dot/pagebreak before header
    HEADER
    ([.,\\s]{1,7}[0-9]{1,7})? # page num & space/dot/pagebreak behind header
    ", comments = TRUE)

  text_raw <- str_replace_all(text_raw, regex_header_page, "\nHEADER\n")


  # sub-page marking under header later WPs)
  regex_header_subh <- regex("
    (\\([A-D]\\) # one of first four uppercase letters in parantheses
    \\s*)        # additional space/linebreak
    ", comments = TRUE)

  text_raw <- str_remove_all(text_raw, regex_header_subh)


  # remove name of speaker under/above header if splited speech

  # names behind header: e.g.HEADER\n Pau; (Pau); (Pau [Berlin]);  Pau (Berlin)
  text_raw <- gsub(
    paste0(
      "\\n+HEADER\\s+.{0,30}?",
      "(", rex_name, ")",
      "(?!.*:)",
      "((\\s(\\[|\\().*?(\\]|\\)))*\\)*\\s*)*"
    ),
    "\nHEADER\n",
    text_raw,
    perl = TRUE
  )

  # names in front of header: e.g. \nDr. Anton Hofreiter\nHEADER
  text_raw <- gsub(
    paste0("\\n.{0,30}(", rex_name, ").{0,20}\\n+HEADER\\n+"),
    "\nHEADER\n",
    text_raw,
    perl = TRUE
  )

  # remove HEADER
  text_nohead <- str_remove_all(text_raw, "HEADER")

  return(text_nohead)
}




regex_1 <- "^.*?((Die Sitzung wird.+?eröffnet\\.)|(Beginn:.*?Uhr))\\s{0,2}(\\n)*(Vizepräsident|Präsident)"


# init correct Wahlperiode and names
wp <- 1
regex_n <- get_regex_names(wp)

test <- lapply(seq_len(nrow(pt_smpl)), function(row) { # seq_len(nrow(pt_smpl))

  # check wahlperiode -> prepare names for clean-up
  if (pt_smpl$wahlperiode[row] > wp) {
    wp <<- pt_smpl$wahlperiode[row]
    regex_n <<- get_regex_names(wp)

    message("Neue Wahlperiode -> neue Namen zu entfernen")
  }

  message("Wahlperiode: ", wp, ", Text: ", row)

  text <- pt_smpl$text[row]

  # remove all stuff befor begin (e.g table of content)
  if (grepl(regex_1, text)) {
    text <- gsub(regex_1, "\\5", text)
  } else { # WP 14: zT ist Beginn verutscht auf die 2. Seite, mitten im Text

    text <- gsub(
      paste0(
        "^.*?Deutscher\\sBundestag.*?Sitzung.*?[0-9]{4,4}",
        ".*?\\n*?(Präsident|Vizepräsident)"
      ),
      "\\1", text
    )

    text <- gsub("\\nBeginn:.{,6}Uhr\\n", "\n", text)
  }


  # remove end of session and everythin behind (e.g. appendix)
  # (Schlu� der 'Sitzung: 21.04 Uhr.)
  text <- gsub(
    "\\n\\(Schlu(ss|ß)( der [']*Sitzung)*:*.{,4}[0-9]+\\s*Uhr.{,20}\\).*?$",
    "", text
  )


  # footenotes, e.g. *) Siehe Anlage 8.
  text <- gsub("\\n\\*+\\)\\sSiehe Anlage\\s[0-9]+\\.*\\s*", "", text)


  # remove header (with pages, names, and sub-page-markers)
  text <- remove_header(text)


  # # remove unecessary linebreaks and space
  text <- str_remove_all(text, "-\\n+(?=[:lower:])") |> # word split
    str_replace_all(c(" *\\n+ *\\(" = "\n(", "\\) *\\n+ *" = ")\n")) #|>
  # str_replace_all("(?<=[^:)])\\n+(?=[^(])", " ") |> # keep around paranteheses
  # str_replace_all(" +", " ") # trim whitespace to single one


  # # remove comments (in text)
  text <- str_replace_all(
    text,
    regex("\\n\\(.*?\\)\\n", dotall = TRUE),
    "\nZWISCHENRUF\n"
  )


  #  speaker: begin of speech: from newline to : with name (keywords/paranth.)
  regex_speaker <- stringr::regex(
    paste0(
      "\\n(", # begin with linebreak and first group
      "[^\\n]{0,30}?", # take everything before, without linebreak
      "(", regex_n, ")", # member names of Wahlperiode
      ".{0,10}?", # space/comma after name, before additional words
      "(((Minister|Bundesminister|Staatssekretär).{0,45}?)|", # longer keywords
      "(Antragsteller|Anfragender|Berichterstatter|Schriftführer|", # keywords
      "(\\(.{0,35}\\))).{0,5})?", # filled parantheses (city/party)
      "):" # end first group before :
    ),
    dotall = TRUE, comments = TRUE
  )

  text <- str_replace_all(text, regex_speaker, "\nSPLIT1\\1SPLIT2\n") |>
    str_replace("(^.*?:)", "\\1SPLIT2") # first speaker

  return(text)
})


saveRDS(test, file = "../data/tmp_smpl.RDS")
test <- readRDS("../data/tmp_smpl.RDS")

c <- test[[1]]
View(c)

fileConn <- file("output.txt")
writeLines(test[[10]], fileConn) # test[[1]]  df_smpl$text[15]
close(fileConn)


Text 10: Dr. von Manger-Koenig, Staatssekret�r im Bundesministerium f�r Gesundheitswesen


library(quanteda)

test2 <- tokens(corpus(unlist(test[11:21])))
test3 <- kwic(test2, ":", window = 10)
test5 <- kwic(tokens(corpus(unlist(test))), "Staatssek*", window = 15)







# Nach HEADER: 1, 2
# Vor HEADER:

# WP 1 - immer nach HEADER
# (Dr. Menzel)
# (Präsident Dr. Ehlers)
# (Bundesinnenminister Dr. Dr. h. c. Lehr)
# (Dr. Etzel [Bamberg])
# (Rademacher)
# (Vizepräsident Dr. Schmid)
# (Bundesminister Storch)

# WP 2
# (Präsident D. Dr. Ehlers)
# (Staatssekretär Dr. Westrick)
# (Kühn [Köln])
# (Bundesminister Dr. Wuermeling)

# WP 7
# Vizepräsident von Hassel
# Leicht
# Dr. h. c. Dr.-Ing. E. h. Möller
# Wittmann (Straubing)
# Vizepräsident Dr. Jaeger

# WP 11
# Frau Vennegerts
# Staatsminister Krollmann (Hessen)
# Bundesminister Dr. Wallmann
# Kleinert (Marburg)
# Mischnick
# Dr. Hauff

# WP 14
# Staatsminister Dr. Ludger Volmer
# Dr. Peter Struck
# Ministerpräsident Dr. Bernhard Vogel (Thüringen)

# WP 19
# Präsident Dr. Wolfgang Schäuble
# Beatrix von Storch
# Dr. Silke Launert
# Sonja Amalie Steffen
# Vizepräsident Thomas Oppermann
# Sandra Weeser
# Dr. Rainer Kraft
# Parl. Staatssekretärin Rita Schwarzelühr-Sutter
# Parl Staatssekretärin bei der Bundesministerin für Umwelt, Naturschutz, Bau und Reaktorsicherheit



# in Text

# WP 4
# Staatssekretär Dr. Lippe rt (ANSTELLE VON LIPPERT)

# WP 14

# Frage stellen: Wie muss die Energiepolitik im 21. Jahr-
#
#  Präsident Wolfgang Thierse
#
# Deutscher Bundestag – 14. Wahlperiode – 95. Sitzung. Berlin, Donnerstag, den 23. März 2000 8735
#
# (A)

# https://dserver.bundestag.de/btp/14/14057.pdf
# Vizepräsident Dr. Hermann Otto Solms



# WP 19

# 3964 	Deutscher Bundestag — 4. Wahlperiode — 82. Sitzung. Bonn, Donnerstag, den 27. Juni 1963
# Staatssekretär Dr. Seiermann



a <- df_smpl$text[7]

# WP 1:

# remove linebreaks cutting words
b <- gsub("-\\n", "", a)

# remove begin and end phrases
b <- gsub("^.*?((Die Sitzung wird.+?eröffnet\\.)|(Beginn:.*?Uhr))\\s{,2}(\\n)*", "\n", b)
b <- gsub("\\n(((\\(Schlu(ß|ss)(\\sder\\sSitzung):))|(\\(Ende der Sitzung:.*?Uhr\\))).*$", "", b)

"\\n(Schlu(ss|ß)|Ende( der Sitzung)*:.*?$)"


# "Beginn?:?\s?(\d){1,2}(\s?[.,]\s?(\d){1,2})?\s?Uhr"
# "\(Schlu(ß|ss)\s?:?(.*?)\d{1,2}\D+(\d{1,2})?(.*?)\)?|\(Ende der Sitzung: \d{1,2}\D+(\d{1,2}) Uhr\.?\)"



# remove header
# b <- gsub("\nDeutscher\\sBundestag.+?\\(.+?\\)\\s\\n", "\n", b)
# 516 \tDeutscher Bundestag — 18. Sitzung. Bonn, den 24. und 25. November 1949
# 512 \tDeutscher Bundestag — 18. Sitzung. Bonn, den 24. und 25. November 1949

# remove comments
b <- gsub("\\n\\(.+?\\)\\s*\\n", "\n", b)


# add seperator between speeches
regex1 <- "((Vizepräsident|Präsident|Frau)(in)*\\s)*" # Leitung
regex2 <- "(\\s\\(.*?\\)\\s*)*" # Partei
regex3 <- "(,.{0,1}((Schriftführer)|(Bundeskanzler)|(Bundespräsident)|(Bundesminister.*?))\\s*)*" # Funktion

c <- gsub(paste0("\\n(", regex1, "(D\\.\\s)*(Dr\\.\\s){0,1}(", mdbs_1, ")", regex2, regex3, ":)"), ":_:\\1", b)

d <- data.frame(text = c) %>%
  mutate(text = str_remove_all(text, "\\n")) %>%
  separate_rows(text, sep = ":_:") %>%
  separate(text, into = c("speaker", "text"), sep = ":", extra = "merge")


# Die Sitzung wird um 6 Uhr 11 Minuten durch
# den Präsidenten Dr. Köhler wieder aufgenommen.



# remove noise: header, page, (A) ...
b <- gsub("Deutscher.Bundestag.+?[0-9].+?Wahlperiode.+?Sitzung.+?Berlin.+?\\(D\\)", "", a)
b <- gsub("-\\n", "", b)
b <- gsub("\\n\\(.*?\\)\\n", " ", b) # remove comments


b <- gsub(".*Beginn:.[0-9]+.[0-9]+.Uhr", "", a)
b <- gsub("\\(Schluss:.[0-9]+.[0-9]+.Uhr.*", "", b)

# add seperator between speeches
c <- gsub(paste0("(((Vizepräsident|Präsident|Dr\\.)(in)*\\s)*(", mdbs_20, ")[A-Za-zÄÖÜäöü0-9()\\/ ]*?(,.{1,30})*:)(\\n)"), ":_:\\1", b)


d <- data.frame(text = c) %>%
  mutate(text = str_remove_all(text, "\\n")) %>%
  separate_rows(text, sep = ":_:") %>%
  separate(text, into = c("speaker", "text"), sep = ":", extra = "merge") %>%
  filter(speaker != "") %>%
  separate(speaker, into = c("speaker", "party"), sep = " \\(", fill = "right") %>%
  mutate(
    text = str_squish(text),
    chair = ifelse(str_detect(speaker, "räsident"), TRUE, FALSE),
    speaker = str_remove(speaker, "(Vizepräsident|Präsident)(in)*\\s"),
    speaker = str_remove(speaker, ",.*"),
    party = str_remove(party, "\\)"),
    speechnum = row_number()
  ) %>%
  separate_rows(text, sep = "(?<!Dr)[.!?](?!$)") %>%
  mutate(
    text = str_squish(text),
    row = row_number()
  )
