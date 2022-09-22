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

set.seed(1245)

pt_smpl <- group_by(pt_smpl, wahlperiode) |>
  slice_sample(n = 2) |>
  ungroup()

df_notext <- select(pt_smpl, !text)


members <- readRDS("../data/members.RDS")

# members_correct <- filter(
#   members,
#   nachname %in% c(
#     "Löwenstein-Wertheim-Freudenberg", "Missmahl", "Gerstenmaier",
#     "Jaeger"
#   )
# ) |>
#   mutate(nachname = case_when(
#     nachname == "Löwenstein-Wertheim-Freudenberg" ~ "Löwenstein",
#     nachname == "Missmahl" ~ "M[ai]ßmahl", # wrong in text, a and ß
#     nachname == "Gerstenmaier" ~ "Gerstenmaler", # wrong in text
#     nachname == "Jaeger" ~ "jaeger" # wrong in text
#   ))

# members <- bind_rows(members, members_correct)



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

  text_raw <- str_replace_all(text_raw, regex_header_base, "\nHEADER\n")


  # remove page numbering around header (front or back) & space/dot/pagebreak
  regex_header_page <- regex("
    ([0-9]{1,7}[0-9A-Z ]{1,7}[.,\\s]{1,8})? # before header
    HEADER
    ([.,\\s]{1,8}[0-9A-Z ]{1,7}[0-9]{1,7})? # behind header
    ", comments = TRUE)


  text_raw <- str_replace_all(text_raw, regex_header_page, "\nHEADER\n")


  # sub-page marking under header later WPs)
  regex_header_subh <- regex("
    (\\([A-D]\\) # one of first four uppercase letters in parantheses
    [:blank:]*)  # additional space/tan, no linebreak
    ", comments = TRUE)

  text_raw <- str_remove_all(text_raw, regex_header_subh)


  # remove name of speaker under/above header if splited speech
  if (wp < 18) { # WP 1- WP 17: names behind header:
    # e.g. HEADER\n Pau; (Pau); (Pau [Berlin]);  Pau (Berlin)
    text_raw <- gsub(
      paste0(
        "\\n+HEADER\\s+.{0,42}?",
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
      paste0("\\n.{0,40}(", rex_name, ").{0,20}\\n+HEADER\\n+"),
      "\nHEADER\n",
      text_raw,
      perl = TRUE
    )
  }


  # remove HEADER
  text_nohead <- str_remove_all(text_raw, "-\\s+HEADER\\s+") |>
    str_replace_all("\\s+HEADER\\s+", "\n")


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

?pt_prep <- lapply(1:38, function(row) { # seq_len(nrow(pt_smpl))

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
  # (Schluß der 'Sitzung: 21.04 Uhr.)
  text <- gsub(
    "\\n\\(Schlu(ss|ß)( der [']*Sitzung)*:*.{,4}[0-9]+\\s*Uhr.{,20}\\).*?$",
    "", text
  )


  # footenotes, e.g. *) Siehe Anlage 8.
  text <- gsub("\\n\\*+\\)\\sSiehe Anlage\\s[0-9]+\\.*\\s*", "", text)


  # # remove unecessary linebreaks and space
  text <- str_remove_all(text, "-\\n+(?=[:lower:])") |> # word split in lower
    str_remove_all("(?<=[A-ZÄÖÜ])-\\n+(?=[A-ZÄÖÜ])") |> # word split UPPER
    # multiple spaces/linebreaks before/after parantheses to
    str_replace_all(c(
      " *\\n+ *\\(" = "\n(",
      "\\) *\\n+ *" = ")\n"
    )) |>
    str_remove_all(" (?=-[A-ZÖÄÜ])") |> # wrong space before -
    str_replace_all(" +", " ") # trim whitespace to single one


  # mark comments (including names of members); min 5; just last parantheses
  text <- str_replace_all(
    text,
    regex("\\n?\\([^\\(]*?\\)\\n|\\n\\([^\\(]{18,}?\\)", dotall = TRUE),
    "\nZWISCHENRUF\n"
  )

  # remove header (with pages, names, and sub-page-markers)
  text <- remove_header(text)


  #  speaker I: begin of speech: from newline to : with name (keywords/parant.)
  regex_speaker <- regex(
    paste0(
      "\\n(", # begin of 1 group to keep after linebreak
      # 1. max 15 signs before name OR
      "([^\\n:]{0,15}|",
      # 2. if one of the keywords -> 15 + kw + 22 before
      "([^\\n:]{0,15}",
      "((Vize)*[Pp]räsident|Herr|Frau|Dr|(Staats|Bundes)*[Mm]inister|Staatssekretär|Freiherr|Prinz|Graf)",
      "[^:]{0,22}))", # \\n removed: check
      # Abgeordnete and linkebreak not allowed to appear before name
      "(?<!Abgeordnete.{1,35}?[^\\n])",
      # all surnames of wp in or parantheses
      "(", regex_n, ")",
      # just max 3 signs between name and : or following optional keywords
      "[^\\n.]{0,3}?",
      # 1. if one of the keywords -> 20 + kw + 75 signs (also newline) OR
      "(([^:.]{0,20}?((Staats|Bundes)*[Mm]inister|(Parl\\.\\s*)Staatssekretär|Senator).{0,75}?)|",
      # 2. if one of the keywords -> max 3 additional signs OR
      "((Antragsteller|Anfragender|Berichterstatter|Schriftführer|Interpellant).{0,3})|",
      # 3. if parantheses behind name (party; city) -> both max 40 + add 3
      "(\\([^\\n:]{0,40}\\).{0,3}?|", # without : before and in ()
      ".{0,3}?\\([^\\n]{0,7}\\).{0,3}?)", # OR with : but max 3 before ()
      ")*", # close optional keyword/parantheses
      "):" # end main group to keep before :
    ),
    dotall = TRUE
  )

  text <- str_replace_all(text, regex_speaker, "\nSPLIT1\\1SPLIT2\n") |>
    str_replace("(^.*?):", "\\1SPLIT2") # first initial speaker (no linebreak)


  # speaker II: detect speakers that not appear with name from members list
  regex_speaker_noname <- regex(
    "\\n # begin of first group to keep after linebreak;
    # no ( in Beginning; noSPLIT, Abgeordnete, Drucksache or . before linebreak
    (?!\\s*\\(|.{1,35}SPLIT|.{1,35}Abgeordnete|.{1,35}Drucksache|.{1,35}\\.[:space:]*\\n)
    ( # start of 1 group to keep
    [^\\n:?]{0,25}? # max 15 signs before Keywords
    ((,\\s+((Staats|Bundes)*[Mm]inister|Staatssekretär|Senator))| # Keywords OR
    \\([^\\n:]{0,50}?\\)) # paranthese
    [^:?]{0,40}? # max 40 signs behind keyword/parantheses, without :
    )(?<!\\b[a-zäöüß]{1,10}): # end 1 group to keep before NO lowercase word :",
    dotall = TRUE, comments = TRUE
  )

  text <- str_replace_all(text, regex_speaker_noname, "\nSPLIT1XXX\\1SPLIT2\n")


  # split text in speeches and separate speaker in rows
  split_ls <- str_split(text, "SPLIT1")

  split_df <- as.data.frame(split_ls, col.names = "text") |>
    separate(text, into = c("speaker", "text"), sep = "SPLIT2") |>
    cbind(pt_smpl[row, c("id", "pdf_url", "wahlperiode", "datum", "titel")])


  return(split_df)
})


saveRDS(pt_prep, file = "../data/tmp_smpl.RDS")
pt_prep <- readRDS("../data/tmp_smpl.RDS")

# 8297 - 1065
# 8341 - 524
b <- bind_rows(pt_prep)


c <- pt_prep[[2]]
View(c)


textout <- function(num) {
  file <- file("../data/tmp_smpl.txt")
  writeLines(pt_prep[[num]], file) # pt_prep[[1]]  df_smpl$text[15]
  close(file)
}

textout(2)

a <- pt_smpl$text[pt_smpl$id == 3899]
View(a)


b <- str_detect(pt_smpl$text[4], "Kalinke")


# ! check: Vizepräsident etc. speaker wrong Name?
# ! check: text 38: Amira Mohamed Ali



library(quanteda)

test2 <- tokens(corpus(unlist(pt_prep)))
test3 <- kwic(test2, "SPLIT1", window = 10)

test2 <- tokens(corpus(b))
test3 <- kwic(test2, ":", window = 10)



# ! check Text 19: wrong Split just name
# Wir stellen uns hier Fehlentwicklungen und �ben
# auch Selbstkritik. Ich m�chte zu Ihnen sagen, Herr
# SPLIT1SchilySPLIT2
#  Die Kritik der GR�NEN am Verhalten der
# Koalitionsparteien im Au



# Wrong Names: Honig zu Harig
