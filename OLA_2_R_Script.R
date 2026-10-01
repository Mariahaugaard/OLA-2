# ============================================================
# Opgave 1.1
# ============================================================
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

# ============================================================
# Opgave 1.2
# ============================================================

dfBy$bycat <- cut(
  dfBy$indbyggertal,
  breaks = c(0, 1000, 5000, 20000, 100000, Inf),
  labels = c("landsby", "lille by", "almindelig by", "større by", "storby"),
  right = FALSE
)

table(dfBy$bycat)
View(dfBy)

# ============================================================
# Opgave 1.3
# ============================================================

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

# ============================================================
# Opgave 1.4
# ============================================================

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

# ============================================================
#Besvarelse af 2.1
# ============================================================

library(dkstat)

#Privatforbrug fra NKH1
forbrugMeta <- dst_meta("NKH1")

forbrugMeta$variables
str(forbrugMeta$values)

queryForbrug <- list(
  "P.31 Husholdningernes forbrugsudgifter",
  "2020-priser, kædede værdier",
  "Sæsonkorrigeret",
  "*"
)

names(queryForbrug) <- forbrugMeta$variables$id

forbrugRaw <- dst_get_data(
  "NKH1",
  query = queryForbrug,
  parse_dst_tid = FALSE
)

#Behold perioden 1999K1-2026K2
forbrugRaw <- forbrugRaw[
  forbrugRaw$TID >= "1999K1" &
    forbrugRaw$TID <= "2026K2",
]

#Sorter kvartalerne
forbrugRaw <- forbrugRaw[order(forbrugRaw$TID), ]

#Lav det samme format som Excel-filen havde
forbrug <- data.frame(
  t(forbrugRaw$value),
  check.names = FALSE
)

colnames(forbrug) <- forbrugRaw$TID


#Forbrugerforventninger fra FORV1
FTIMeta <- dst_meta("FORV1")

FTIMeta$variables
str(FTIMeta$values)

DI_spoergsmaal <- c(
  "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
  "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
  "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
  "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."
)

queryFTI <- list(
  INDIKATOR = DI_spoergsmaal,
  Tid = "*"
)

FTIRaw <- dst_get_data(
  "FORV1",
  query = queryFTI,
  parse_dst_tid = FALSE
)

#Behold perioden 2000M01-2026M09
FTIRaw <- FTIRaw[
  FTIRaw$TID >= "2000M01" &
    FTIRaw$TID <= "2026M09",
]

#Lav data i samme brede format som Excel-filen
FTI <- reshape(
  FTIRaw[, c("INDIKATOR", "TID", "value")],
  idvar = "INDIKATOR",
  timevar = "TID",
  direction = "wide"
)

colnames(FTI) <- sub("value.", "", colnames(FTI), fixed = TRUE)
colnames(FTI)[1] <- "Variabel"

forbrugerforventninger_2026 <- FTI



#Danmarks Statistiks officielle FTI
queryDST <- list(
  INDIKATOR = "Forbrugertillidsindikatoren",
  Tid = "*"
)

DSTRaw <- dst_get_data(
  "FORV1",
  query = queryDST,
  parse_dst_tid = FALSE
)

DSTRaw <- DSTRaw[
  DSTRaw$TID >= "2000M01" &
    DSTRaw$TID <= "2026M09",
]

DST <- data.frame(
  Variabel = "Forbrugertillidsindikatoren",
  t(DSTRaw$value),
  check.names = FALSE
)

colnames(DST)[-1] <- DSTRaw$TID


# 1. Hent datoerne
datoer <- colnames(FTI)[-1]

# 2. Lav datasæt til kvartaler
FTIKvartal <- FTI

# 3. Giv første kolonne navnet Variabel
colnames(FTIKvartal)[1] <- "Variabel"

# 4. Lav månedskolonnerne om til numeric
FTIKvartal[, -1] <- lapply(FTIKvartal[, -1], as.numeric)

# 5. Find år
aar <- substr(datoer, 1, 4)

# 6. Find måned
maaned <- as.numeric(substr(datoer, 6, 7))

# 7. Lav kvartalsnummer
kvartal <- (maaned - 1) %/% 3 + 1

# 8. Lav kvartalsnavne
kvartalNavn <- paste0(aar, "Q", kvartal)

# 9. Beregn gennemsnittet for hvert kvartal
kvartalsData <- sapply(
  unique(kvartalNavn),
  function(x) {
    rowMeans(
      FTIKvartal[, -1][, kvartalNavn == x, drop = FALSE],
      na.rm = TRUE
    )
  }
)

# 10. Sæt variabelnavnene på igen
FTIKvartalsData <- data.frame(
  Variabel = FTIKvartal$Variabel,
  kvartalsData,
  check.names = FALSE
)

View(FTIKvartalsData)

#Lav selve DI's forbrugertillidsindikator
DI_FTI <- colMeans(
  FTIKvartalsData[, -1],
  na.rm = TRUE
)

DI_FTI_data <- data.frame(
  kvartal = colnames(FTIKvartalsData)[-1],
  DI_FTI = DI_FTI
)

View(DI_FTI_data)

#Forbrug
datoerForbrug <- colnames(forbrug)

#fordi FTI bruger Q, ændrer vi K til Q:
datoerForbrug <- gsub("K", "Q", datoerForbrug)

forbrugVærdi <- as.numeric(forbrug[1, ])

forbrugData <- data.frame(
  kvartal = datoerForbrug,
  forbrug = forbrugVærdi
)

View(forbrugData)

#Sammenligning
#Vi skal IKKE regressere på selve forbrugsniveauet
forbrugData$realvækst <- NA

forbrugData$realvækst[5:nrow(forbrugData)] <-
  (
    forbrugData$forbrug[5:nrow(forbrugData)] /
      forbrugData$forbrug[1:(nrow(forbrugData) - 4)]
    - 1
  ) * 100

#Fælles karakterer
fellesKvartaler <- intersect(
  DI_FTI_data$kvartal,
  forbrugData$kvartal
)

#regressionsdatasættet
regData_DI <- data.frame(
  kvartal = fellesKvartaler,
  DI_FTI = DI_FTI_data$DI_FTI[
    match(fellesKvartaler, DI_FTI_data$kvartal)
  ],
  realvækst = forbrugData$realvækst[
    match(fellesKvartaler, forbrugData$kvartal)
  ]
)

View(regData_DI)

#Linear regression
linearregressionDI <- lm(
  realvækst ~ DI_FTI,
  data = regData_DI
)

summary(linearregressionDI)

#Lav graf
# Datasæt til figuren
grafData <- regData_DI

# Find placeringer til årstal på x-aksen
aarPos <- seq(1, nrow(grafData), by = 4)
aarLabels <- substr(grafData$kvartal[aarPos], 3, 4)

# Gør plads til akser og forklaring
par(mar = c(7, 5, 4, 5), xpd = FALSE)

# 1. Lav søjler for realvækst
barMid <- barplot(
  grafData$realvækst,
  col = "deepskyblue3",
  border = NA,
  ylim = c(-10, 10),
  axes = FALSE,
  space = 0.15,
  main = "DI's forbrugertillidsindikator følger i højere grad privatforbruget"
)

# X-akse
axis(1, at = barMid[aarPos], labels = aarLabels, tick = FALSE)

# Højre y-akse til realvækst
axis(4, at = c(-10, -5, 0, 5, 10), las = 1)
mtext("Pct.", side = 4, line = 2)

# 2. Læg DI-FTI ovenpå
par(new = TRUE)

plot(
  x = barMid,
  y = grafData$DI_FTI,
  type = "n",
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(-45, 45),
  xlim = range(barMid)
)

# Venstre y-akse til DI-FTI
axis(2, at = c(-45, -30, -15, 0, 15, 30, 45), las = 1)
mtext("Nettotal", side = 2, line = 2)

# Hjælpelinjer
abline(h = c(-45, -30, -15, 0, 15, 30, 45),
       col = "grey85", lty = 1)

# DI-linjen
lines(barMid, grafData$DI_FTI, lwd = 2.5, col = "grey30")

box()

#Forklaring på hvilke linje viser hvad
legend(
  "bottom",
  inset = -0.28,
  legend = c("DI's forbrugertillidsindikator",
             "Årlig realvækst pr. kvartal i privatforbruget (højre akse)"),
  col = c("grey30", "deepskyblue3"),
  lty = c(1, NA),
  lwd = c(2.5, NA),
  pch = c(NA, 15),
  pt.cex = c(NA, 1.8),
  bty = "n",
  xpd = TRUE
)

# Korrelation mellem DI-FTI og realvækst
kor_DI <- cor(
  regData_DI$realvækst,
  regData_DI$DI_FTI,
  use = "complete.obs"
)

kor_DI

#Regression
model_DI <- lm(
  realvækst ~ DI_FTI,
  data = regData_DI
)

summary(model_DI)


#Sammenligning med forbrugertillidsindikatoren fra DANMARKS STATESTIK

# 1. Hent datoerne
datoerDST <- colnames(DST)[-1]

# 2. Lav datasæt til kvartaler
DSTKvartal <- DST

# 3. Giv første kolonne navnet Variabel
colnames(DSTKvartal)[1] <- "Variabel"

# 4. Lav månedskolonnerne om til numeric
DSTKvartal[, -1] <- lapply(DSTKvartal[, -1], as.numeric)

# 5. Find år
aarDST <- substr(datoerDST, 1, 4)

# 6. Find måned
maanedDST <- as.numeric(substr(datoerDST, 6, 7))

# 7. Lav kvartalsnummer
kvartalDST <- (maanedDST - 1) %/% 3 + 1

# 8. Lav kvartalsnavne
kvartalNavnDST <- paste0(aarDST, "Q", kvartalDST)

# Beregn gennemsnittet for hvert kvartal
DSTkvartalsData <- sapply(
  unique(kvartalNavnDST),
  function(x) {
    rowMeans(
      DSTKvartal[, -1][, kvartalNavnDST == x, drop = FALSE],
      na.rm = TRUE
    )
  }
)

#Lav datasæt med kvartal og DST-FTI
DSTKvartalsData <- data.frame(
  kvartal = unique(kvartalNavnDST),
  DST_FTI = as.numeric(DSTkvartalsData)
)

View(DSTKvartalsData)

#Fælles kvartaler med privatforbrug
fællesKvartalerDST <- intersect(
  DSTKvartalsData$kvartal,
  forbrugData$kvartal
)

#Lav regressionsdatasættet
regData_DST <- data.frame(
  kvartal = fællesKvartalerDST,
  
  DST_FTI = DSTKvartalsData$DST_FTI[
    match(
      fællesKvartalerDST,
      DSTKvartalsData$kvartal
    )
  ],
  
  realvækst = forbrugData$realvækst[
    match(
      fællesKvartalerDST,
      forbrugData$kvartal
    )
  ]
)

View(regData_DST)
# Korrelation
kor_DST <- cor(
  regData_DST$realvækst,
  regData_DST$DST_FTI,
  use = "complete.obs"
)

kor_DST

# Regression
model_DST <- lm(
  realvækst ~ DST_FTI,
  data = regData_DST
)

regData_DST$realvækst
summary(model_DST)

#Lav R^2 
#X = Forbrugertillid
#Y = Realvækst 

regData_DST$realvækst 

regData_DST$DST_FTI

library(dplyr)


#Find perioden og beregn fejlled
periode1 <- regData_DST %>%
  filter(kvartal >= "2000Q1" & kvartal <= "2026Q2")

periode1$forudsagt <- predict(model_DST, newdata = periode1)

periode1$residual <- periode1$realvækst - periode1$forudsagt

RSS_periode1 <- sum(periode1$residual^2)

RSS_periode1

sum(residuals(model_DST)^2)

sum(residuals(model_DI)^2)

View(periode1)

nrow(periode1)


#Plot fejlled 

periode1$aar <- as.numeric(substr(periode1$kvartal, 1, 4))

farver <- ifelse(
  periode1$aar < 2010, "steelblue",
  ifelse(
    periode1$aar < 2018, "darkorange",
    ifelse(
      periode1$aar < 2026, "seagreen",
      "purple"
    )
  )
)

plot(
  periode1$residual,
  type = "h",
  lwd = 2,
  col = farver,
  xaxt = "n",
  xlab = "År",
  ylab = "Fejlled",
  main = "Fejlled 2000Q1-2026Q2"
)


abline(h = 0, lty = 2)

axis(
  1,
  at = c(
    which(periode1$kvartal == "2000Q1"),
    which(periode1$kvartal == "2010Q1"),
    which(periode1$kvartal == "2018Q1"),
    which(periode1$kvartal == "2026Q1")
  ),
  labels = c("2000", "2010", "2018", "2026")
)

par(xpd = NA)

legend(
  "bottomleft",
  inset = c(0, -0.45),
  legend = c(
    "2000-2009",
    "2010-2017",
    "2018-2025",
    "2026"
  ),
  col = c(
    "steelblue",
    "darkorange",
    "seagreen",
    "purple"
  ),
  lwd = 2,
  bty = "n"
)

# ============================================================
#Besvarelse af 2.2
# ============================================================

#Dansk Industri
q3_2026_DI <- DI_FTI_data[
  DI_FTI_data$kvartal == "2026Q3",
]

q3_2026_DI

forudsigelse_DI_2026Q3 <- predict(
  model_DI,
  newdata = q3_2026_DI
)

forudsigelse_DI_2026Q3


#Danmarks Statistik
q3_2026_DST <- DSTKvartalsData[
  DSTKvartalsData$kvartal == "2026Q3",
]

q3_2026_DST

forudsigelse_DST_2026Q3 <- predict(
  model_DST,
  newdata = q3_2026_DST
)

forudsigelse_DST_2026Q3


#Samlet
forudsigelser_2026Q3 <- data.frame(
  Indikator = c("DI-FTI", "DST-FTI"),
  Forudsagt_realvækst = c(
    forudsigelse_DI_2026Q3,
    forudsigelse_DST_2026Q3
  )
)

forudsigelser_2026Q3

# ============================================================
# OPGAVE 2.4
# ============================================================

# Nationalbankens forventede vækst i privatforbruget i 2026
NB_vaekst <- 1.8


# Faktiske forbrugstal
Q1_2026 <- forbrugData$forbrug[
  forbrugData$kvartal == "2026Q1"
]

Q2_2026 <- forbrugData$forbrug[
  forbrugData$kvartal == "2026Q2"
]

Q3_2025 <- forbrugData$forbrug[
  forbrugData$kvartal == "2025Q3"
]

Q4_2025 <- forbrugData$forbrug[
  forbrugData$kvartal == "2025Q4"
]


# ------------------------------------------------------------
# 1. Beregn vores forudsagte forbrug i Q3 2026
# ------------------------------------------------------------

Q3_2026 <- Q3_2025 *
  (1 + as.numeric(forudsigelse_DI_2026Q3) / 100)


# ------------------------------------------------------------
# 2. Samlet forbrug i de første 3 kvartaler af 2026
# ------------------------------------------------------------

Q1_Q3_2026 <- Q1_2026 + Q2_2026 + Q3_2026


# ------------------------------------------------------------
# 3. Samlet privatforbrug i 2025
# ------------------------------------------------------------

forbrug_2025 <- sum(
  forbrugData$forbrug[
    forbrugData$kvartal >= "2025Q1" &
      forbrugData$kvartal <= "2025Q4"
  ]
)


# ------------------------------------------------------------
# 4. Nationalbankens forventede forbrug for hele 2026
# ------------------------------------------------------------

forbrug_2026_NB <- forbrug_2025 *
  (1 + NB_vaekst / 100)


# ------------------------------------------------------------
# 5. Hvor stort skal Q4 være?
# ------------------------------------------------------------

Q4_2026 <- forbrug_2026_NB - Q1_Q3_2026


# ------------------------------------------------------------
# Resultater
# ------------------------------------------------------------

round(Q3_2026, 2)
round(Q1_Q3_2026, 2)
round(forbrug_2026_NB, 2)
round(Q4_2026, 2)
round(Q4_2025 / 1000, 2)

Q4_vaekst <- (Q4_2026 / Q4_2025 - 1) * 100
round(Q4_vaekst, 2)

# ============================================================
# OPGAVE 3.1 - MODELLENS FORUDSIGELSER
# ============================================================

# Koefficienter fra DI-modellen
b0_DI <- coef(model_DI)[1]
b1_DI <- coef(model_DI)[2]

# Koefficienter fra DST-modellen
b0_DST <- coef(model_DST)[1]
b1_DST <- coef(model_DST)[2]


# Beregn de forudsagte værdier manuelt
regData_DI$forudsagt <- b0_DI +
  b1_DI * regData_DI$DI_FTI

regData_DST$forudsagt <- b0_DST +
  b1_DST * regData_DST$DST_FTI


# Se resultaterne
head(regData_DI)
head(regData_DST)


# ============================================================
# OPGAVE 3.2 - RESIDUALPLOT FOR DST-FTI
# ============================================================
# ============================================================
# OPGAVE 3.2 - RESIDUALER
# ============================================================

# Residual = faktisk realvækst - forudsagt realvækst


# ------------------------------------------------------------
# DI-FTI
# ------------------------------------------------------------

# Beregn residualer
regData_DI$residual <- regData_DI$realvækst - regData_DI$forudsagt

# Farver
farver_DI <- ifelse(
  regData_DI$residual >= 0,
  "steelblue",
  "tomato"
)

# Plot
plot(
  regData_DI$forudsagt,
  regData_DI$residual,
  pch = 19,
  col = farver_DI,
  cex = 1,
  xlab = "Forudsagt realvækst (%)",
  ylab = "Residual",
  main = "Residualplot for DI-FTI"
)

# 0-linje
abline(
  h = 0,
  lty = 2,
  lwd = 2
)

# Forklaring
legend(
  "bottomleft",
  legend = c(
    "Faktisk vækst højere end forudsagt",
    "Faktisk vækst lavere end forudsagt"
  ),
  col = c("steelblue", "tomato"),
  pch = 19,
  bty = "n"
)


# ------------------------------------------------------------
# DST-FTI
# ------------------------------------------------------------

# Beregn residualer
regData_DST$residual <- regData_DST$realvækst - regData_DST$forudsagt

# Farver
farver_DST <- ifelse(
  regData_DST$residual >= 0,
  "steelblue",
  "tomato"
)

# Plot
plot(
  regData_DST$forudsagt,
  regData_DST$residual,
  pch = 19,
  col = farver_DST,
  cex = 1,
  xlab = "Forudsagt realvækst (%)",
  ylab = "Residual",
  main = "Residualplot for DST-FTI"
)

# 0-linje
abline(
  h = 0,
  lty = 2,
  lwd = 2
)

# Forklaring
legend(
  "bottomleft",
  legend = c(
    "Faktisk vækst højere end forudsagt",
    "Faktisk vækst lavere end forudsagt"
  ),
  col = c("steelblue", "tomato"),
  pch = 19,
  bty = "n"
)
)
# ============================================================
# OPGAVE 3.3 - RSS OG TSS
# ============================================================

# RSS = summen af residualerne i anden

RSS_DI <- sum(
  regData_DI$residual^2,
  na.rm = TRUE
)

RSS_DST <- sum(
  regData_DST$residual^2,
  na.rm = TRUE
)


# TSS = samlet variation omkring gennemsnittet

TSS_DI <- sum(
  (regData_DI$realvækst -
     mean(regData_DI$realvækst, na.rm = TRUE))^2,
  na.rm = TRUE
)

TSS_DST <- sum(
  (regData_DST$realvækst -
     mean(regData_DST$realvækst, na.rm = TRUE))^2,
  na.rm = TRUE
)


# Se resultater
round(RSS_DI, 2)
round(TSS_DI, 2)

round(RSS_DST, 2)
round(TSS_DST, 2)


# ============================================================
# OPGAVE 3.4 - FORKLARINGSGRAD R^2
# ============================================================

R2_DI <- 1 - RSS_DI / TSS_DI
R2_DST <- 1 - RSS_DST / TSS_DST


round(R2_DI, 3)
round(R2_DST, 3)


# Samlet resultat
resultat_opgave3 <- data.frame(
  Model = c("DI-FTI", "DST-FTI"),
  RSS = round(c(RSS_DI, RSS_DST), 2),
  TSS = round(c(TSS_DI, TSS_DST), 2),
  R2 = round(c(R2_DI, R2_DST), 3)
)

resultat_opgave3


