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
    ([0-9]{1,5}\\.{1,5})*?        # number of Wahlperiode
    \\s*Deutscher\\sBundestag     # base name
    .{1,30}?Sitzung               # number of session
    .{0,50}?[0-9]{4,4}            # date till year
    ", comments = TRUE, dotall = TRUE)

  # remove page numbering around header (front or back)
  regex_header_page <- regex("
    ([0-9]{1,6}[.,\\s]{1,8})? # page num & space/dot/pagebreak before header
    HEADER
    ([.,\\s]{1,7}[0-9]{1,8})? # page num & space/dot/pagebreak behind header
    ", comments = TRUE)

  text_raw <- str_replace_all(text_raw, regex_header_page, "\nHEADER\n")


  # sub-page marking under header later WPs)
  regex_header_subh <- regex("
    (\\([A-D]\\) # one of first four uppercase letters in parantheses
    \\s*)        # additional space/linebreak
    ", comments = TRUE)

  text_raw <- str_remove_all(text_raw, regex_header_subh)


  # remove name of speaker under/above header if splited speech

  if (wp < 18) { # WP 1- WP 17: names behind header:
    # e.g. HEADER\n Pau; (Pau); (Pau [Berlin]);  Pau (Berlin)
    text_raw <- gsub(
      paste0(
        "\\n+HEADER\\s+.{0,30}?",
        "(", rex_name, ")",
        "(?!.*(:|Staatssekretär))", # anything behind name except speaker signs
        "((\\s(\\[|\\().*?(\\]|\\)))*\\)*\\s*)*"
      ),
      "\nHEADER\n",
      text_raw,
      perl = TRUE
    )
  } else { # names in front of header: e.g. \nDr. Anton Hofreiter\nHEADER
    text_raw <- gsub(
      paste0("\\n.{0,30}(", rex_name, ").{0,20}\\n+HEADER\\n+"),
      "\nHEADER\n",
      text_raw,
      perl = TRUE
    )
  }


  # remove HEADER
  text_nohead <- str_remove_all(text_raw, "HEADER")

  return(text_nohead)
}


regex_start <- regex(
  "^.*?
  (Die\\sSitzung\\swird.+?eröffnet\\.|Beginn:.*?Uhr\\.*)\\s{0,4}
  ((Vize)*[Pp]räsident)",
  comments = TRUE, dotall = TRUE
)


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
  if (str_detect(text, regex_start)) {
    text <- str_replace(text, regex_start, "\\2")
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
  regex_speaker <- regex(
    paste0(
      "\\n(", # begin with linebreak and first group
      "[^\\n]{0,30}?", # take everything before, without linebreak
      "(", regex_n, ")", # member names of Wahlperiode
      "[^\\n]{0,25}?", # space/comma after name,before additional words(?!.*\\2)
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

c <- test[[2]]
View(c)

fileConn <- file("../data/tmp_smpl.txt")
writeLines(test[[18]], fileConn) # test[[1]]  df_smpl$text[15]
#writeLines(pt_smpl$text[[3]], fileConn) # test[[1]]  df_smpl$text[15]
close(fileConn)


# ! check: text 38: Amira Mohamed Ali


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