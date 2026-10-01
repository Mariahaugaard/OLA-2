# Opgave 1.1
library(dkstat)

dst_search("byområder")

bymeta <- dst_meta("BY3")
bymeta$variables
str(bymeta$values)

myquery <- list(
  BYER = "*",
  FOLKARTAET = "Folketal",
  Tid = "2026"
)

dfBy <- dst_get_data("BY3", query = myquery)

# Ryd op i bynavnene
dfBy$by <- sub("^[^ ]+ [^ ]+ ", "", dfBy$BYER)
dfBy$by <- sub(" \\(del af.*\\)$", "", dfBy$by)

# Fjern områder som ikke er byer
dfBy <- dfBy[!dfBy$by %in% c(
  "Landdistrikter",
  "Uden fast bopæl",
  "Hovedstadsområdet"
), ]

# Behold kun by og indbyggertal
dfBy <- dfBy[, c("by", "value")]

# Saml byer der er opdelt på flere kommuner
dfBy <- aggregate(value ~ by, data = dfBy, FUN = sum)

names(dfBy)[2] <- "indbyggertal"

# Fjern byer uden indbyggere
dfBy <- dfBy[dfBy$indbyggertal > 0, ]

View(dfBy)


# Opgave 1.2

dfBy$bycat <- cut(
  dfBy$indbyggertal,
  breaks = c(0, 1000, 5000, 20000, 100000, Inf),
  labels = c("landsby", "lille by", "almindelig by", "større by", "storby"),
  right = FALSE
)

table(dfBy$bycat)
View(dfBy)


# Opgave 1.3

library(readr)

boligsiden <- read_csv(
  "https://raw.githubusercontent.com/Sylle1234/Test/main/boligsiden.csv",
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)

# Behold kun de variable vi skal bruge
boligsiden <- boligsiden[, c("by", "pris", "kvmpris")]
boligsiden <- boligsiden[!is.na(boligsiden$by), ]

# Lav en kopi af DST-data til merge
dfBy_merge <- dfBy[, c("by", "bycat")]

# Disse giver ellers samme navn som Faaborg og Haarby
dfBy_merge <- dfBy_merge[!dfBy_merge$by %in% c("Fåborg", "Hårby"), ]

# Lav en fælles nøgle til merge
dfBy_merge$by_key <- tolower(dfBy_merge$by)
dfBy_merge$by_key <- gsub("æ", "ae", dfBy_merge$by_key)
dfBy_merge$by_key <- gsub("ø", "oe", dfBy_merge$by_key)
dfBy_merge$by_key <- gsub("å", "aa", dfBy_merge$by_key)

boligsiden$by_key <- tolower(boligsiden$by)
boligsiden$by_key <- gsub("æ", "ae", boligsiden$by_key)
boligsiden$by_key <- gsub("ø", "oe", boligsiden$by_key)
boligsiden$by_key <- gsub("å", "aa", boligsiden$by_key)

# Kontroller at by_key er entydig
sum(duplicated(dfBy_merge$by_key))

# Merge bycat ind i boligdata
boligsiden_merge <- merge(
  boligsiden,
  dfBy_merge[, c("by_key", "bycat")],
  by = "by_key",
  all.x = TRUE
)

# Fjern den tekniske nøgle igen
boligsiden_merge$by_key <- NULL

boligsiden_merge <- boligsiden_merge[, c("by", "pris", "kvmpris", "bycat")]

View(boligsiden_merge)


# Opgave 1.4

library(ggplot2)

# Gennemsnitlig kvm-pris for hver bykategori
kvmpris_bycat <- aggregate(
  kvmpris ~ bycat,
  data = boligsiden_merge,
  FUN = mean
)

kvmpris_bycat

ggplot(kvmpris_bycat, aes(x = bycat, y = kvmpris, fill = bycat)) +
  geom_col() +
  scale_y_continuous(
    breaks = seq(0, 30000, by = 5000),
    limits = c(0, 30000),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title = "Gennemsnitlig kvm-pris efter bykategori",
    x = "Bykategori",
    y = "Gennemsnitlig kvm-pris",
    fill = "Bytype"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

