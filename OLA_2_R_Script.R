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

# ============================================================
#OPGAVE 4
# ============================================================
#Opgave 4.1 Illustration af forbrugertillid
# ============================================================

queryDST_1996 <- list(
  INDIKATOR = "Forbrugertillidsindikatoren",
  Tid = "*"
)

DSTRaw_1996 <- dst_get_data(
  "FORV1",
  query = queryDST_1996,
  parse_dst_tid = FALSE
)

# Behold perioden fra januar 1996 til i dag
DSTRaw_1996 <- DSTRaw_1996[
  DSTRaw_1996$TID >= "1996M01",
]

# Lav data i bredt format
DST_1996 <- data.frame(
  Variabel = "Forbrugertillidsindikatoren",
  t(DSTRaw_1996$value),
  check.names = FALSE
)

colnames(DST_1996)[-1] <- DSTRaw_1996$TID


# Omregn til kvartaler

# Hent datoerne
datoerDST_1996 <- colnames(DST_1996)[-1]

# Lav datasæt til kvartaler
DSTKvartal_1996 <- DST_1996

# Lav månedskolonnerne om til numeric
DSTKvartal_1996[, -1] <- lapply(
  DSTKvartal_1996[, -1],
  as.numeric
)

# Find år
aarDST_1996 <- substr(datoerDST_1996, 1, 4)

# Find måned
maanedDST_1996 <- as.numeric(
  substr(datoerDST_1996, 6, 7)
)

# Lav kvartalsnummer
kvartalDST_1996 <- (maanedDST_1996 - 1) %/% 3 + 1

# Lav kvartalsnavne
kvartalNavnDST_1996 <- paste0(
  aarDST_1996,
  "Q",
  kvartalDST_1996
)

# Beregn gennemsnittet for hvert kvartal
DSTkvartalsData_1996 <- sapply(
  unique(kvartalNavnDST_1996),
  function(x) {
    rowMeans(
      DSTKvartal_1996[, -1][
        , kvartalNavnDST_1996 == x,
        drop = FALSE
      ],
      na.rm = TRUE
    )
  }
)

# Lav det endelige kvartalsdatasæt
DSTKvartalsData_1996 <- data.frame(
  kvartal = unique(kvartalNavnDST_1996),
  DST_FTI = as.numeric(DSTkvartalsData_1996)
)

View(DSTKvartalsData_1996)

#Plot
par(xpd = FALSE)

plot(
  DSTKvartalsData_1996$DST_FTI,
  type = "l",
  lwd = 2,
  xaxt = "n",
  xlab = "År",
  ylab = "Forbrugertillidsindikator",
  main = "DST's forbrugertillidsindikator 1996 til i dag"
)

abline(h = 0, lty = 2)

aarPosDST_1996 <- seq(
  1,
  nrow(DSTKvartalsData_1996),
  by = 4
)

axis(
  1,
  at = aarPosDST_1996,
  labels = substr(
    DSTKvartalsData_1996$kvartal[aarPosDST_1996],
    1,
    4
  ),
  las = 2
)

#Find det højeste og laveste kvartal
DSTKvartalsData_1996[
  which.max(DSTKvartalsData_1996$DST_FTI),
]

DSTKvartalsData_1996[
  which.min(DSTKvartalsData_1996$DST_FTI),
]

# ============================================================
# Opgave 4.2
# ============================================================
# Gennemsnit af underspørgsmålet:
# "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"

# Hent det præcise underspørgsmål fra FORV1
FTIMeta$values

querySpg4_2 <- list(
  INDIKATOR = "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
  Tid = "*"
)

spg4_2Raw <- dst_get_data(
  "FORV1",
  query = querySpg4_2,
  parse_dst_tid = FALSE
)

# Lav data i bredt format
spg4_2 <- data.frame(
  Variabel = "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
  t(spg4_2Raw$value),
  check.names = FALSE
)

colnames(spg4_2)[-1] <- spg4_2Raw$TID

# Hent datoerne
datoerSpg4_2 <- colnames(spg4_2)[-1]

# Lav datasæt til kvartaler
spg4_2Kvartal <- spg4_2

# Lav månedskolonnerne om til numeric
spg4_2Kvartal[, -1] <- lapply(
  spg4_2Kvartal[, -1],
  as.numeric
)

# Find år
aarSpg4_2 <- substr(
  datoerSpg4_2,
  1,
  4
)

# Find måned
maanedSpg4_2 <- as.numeric(
  substr(
    datoerSpg4_2,
    6,
    7
  )
)

# Lav kvartalsnummer
kvartalSpg4_2 <- (maanedSpg4_2 - 1) %/% 3 + 1

# Lav kvartalsnavne
kvartalNavnSpg4_2 <- paste0(
  aarSpg4_2,
  "Q",
  kvartalSpg4_2
)

# Beregn gennemsnittet for hvert kvartal
spg4_2KvartalsData <- sapply(
  unique(kvartalNavnSpg4_2),
  function(x) {
    rowMeans(
      spg4_2Kvartal[, -1][
        , kvartalNavnSpg4_2 == x,
        drop = FALSE
      ],
      na.rm = TRUE
    )
  }
)

# Lav datasæt med kvartal og værdi
spg4_2Data <- data.frame(
  kvartal = unique(kvartalNavnSpg4_2),
  Spg4_2 = as.numeric(spg4_2KvartalsData)
)

# Behold perioden fra 2000Q1 til seneste mulige kvartal
spg4_2Data <- spg4_2Data[
  spg4_2Data$kvartal >= "2000Q1",
]

# Se datasættet
View(spg4_2Data)

# Beregn gennemsnittet for hele perioden
gennemsnitSpg4_2 <- mean(
  spg4_2Data$Spg4_2,
  na.rm = TRUE
)

gennemsnitSpg4_2

# ============================================================
#Opgave 4.3
# ============================================================
#Hent data "Husholdningers forbrug på dansk område (11 gruppering) efter formål, prisenhed og tid"

library(dkstat)

dst_search("Husholdningers forbrug på dansk område")

#Vi vælger NAHC21

forbrug11Meta <- dst_meta("NAHC21")

forbrug11Meta$variables
str(forbrug11Meta$values)

# Hent de 11 forbrugsgrupper
queryForbrug11 <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  Tid = c("2020", "2022", "2023")
)

forbrug11Raw <- dst_get_data(
  "NAHC21",
  query = queryForbrug11,
  parse_dst_tid = FALSE
)

# Fjern kategorien "I alt"
forbrug11 <- forbrug11Raw[
  forbrug11Raw$FORMAAAL != "CPT I alt",
]

# Fjern koderne foran kategorierne
forbrug11$FORMAAAL <- sub(
  "^[A-Z]{3} ",
  "",
  forbrug11$FORMAAAL
)

View(forbrug11)

# Find den største forbrugsgruppe i 2022
forbrug2022 <- forbrug11[
  forbrug11$TID == "2022",
]

forbrug2022[
  which.max(forbrug2022$value),
]

#Find, hvilken gruppe der steg mest fra 2020 til 2023
#Lav 2 datasæt 
forbrug4_3_2020 <- forbrug11[
  forbrug11$TID == "2020",
]

forbrug4_3_2023 <- forbrug11[
  forbrug11$TID == "2023",
]

#Match grupperne på FORMAAAL og beregn forskellen
forbrug4_3_forskel <- data.frame(
  FORMAAAL = forbrug4_3_2020$FORMAAAL,
  forbrug2020 = forbrug4_3_2020$value,
  forbrug2023 = forbrug4_3_2023$value
)

forbrug4_3_forskel$forskel <-
  forbrug4_3_forskel$forbrug2023 -
  forbrug4_3_forskel$forbrug2020

View(forbrug4_3_forskel)

forbrug4_3_forskel[
  which.max(forbrug4_3_forskel$forskel),
]

# ============================================================
# Opgave 4.4
# ============================================================
# Metadata for kvartalsvise 11 forbrugsgrupper

library(dkstat)

forbrug11KvartalMeta <- dst_meta("NKHC21")

forbrug11KvartalMeta$variables

str(forbrug11KvartalMeta$values)

# Hent kvartalsdata for de 11 forbrugsgrupper
valgForbrug11Kvartal <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

forbrug11KvartalRaw <- dst_get_data(
  "NKHC21",
  query = valgForbrug11Kvartal,
  parse_dst_tid = FALSE
)

# Behold perioden 1999K1 til 2023K2
forbrug11KvartalRaw <- forbrug11KvartalRaw[
  forbrug11KvartalRaw$TID >= "1999K1" &
    forbrug11KvartalRaw$TID <= "2023K2",
]

# Fjern kategorien "I alt"
forbrug11Kvartal <- forbrug11KvartalRaw[
  forbrug11KvartalRaw$FORMAAAL != "CPT I alt",
]

# Fjern koderne foran forbrugsgrupperne
forbrug11Kvartal$FORMAAAL <- sub(
  "^[A-Z]{3} ",
  "",
  forbrug11Kvartal$FORMAAAL
)

View(forbrug11Kvartal)

nrow(forbrug11Kvartal)

#Beregne årlig realvækst for hver af de 11 forbrugsgrupper

# Sorter efter forbrugsgruppe og tid
forbrug11Kvartal <- forbrug11Kvartal[
  order(forbrug11Kvartal$FORMAAAL, forbrug11Kvartal$TID),
]

# Lav kolonne til årlig realvækst
forbrug11Kvartal$realvaekst <- NA

# Lav kolonne til realvækst
forbrug11Kvartal$realvaekst <- NA

# Find de 11 forbrugsgrupper
forbrugsgrupper <- unique(forbrug11Kvartal$FORMAAAL)

for (gruppe in forbrugsgrupper) {
  
  dataGruppe <- forbrug11Kvartal[
    forbrug11Kvartal$FORMAAAL == gruppe,
  ]
  
  dataGruppe$realvaekst[5:nrow(dataGruppe)] <-
    (
      dataGruppe$value[5:nrow(dataGruppe)] /
        dataGruppe$value[1:(nrow(dataGruppe) - 4)]
      - 1
    ) * 100
  
  forbrug11Kvartal$realvaekst[
    forbrug11Kvartal$FORMAAAL == gruppe
  ] <- dataGruppe$realvaekst
}

View(forbrug11Kvartal)

# Behold kun perioden til regressionerne
forbrug11Regression <- forbrug11Kvartal[
  forbrug11Kvartal$TID >= "2000K1" &
    forbrug11Kvartal$TID <= "2023K2",
]

#Ændre K til Q, så kvartalsnavnene matcher de eksisterende i DI- og DST-data
forbrug11Regression$kvartal <- gsub(
  "K",
  "Q",
  forbrug11Regression$TID
)

# Tilføj DI's forbrugertillidsindikator
forbrug11Regression$DI_FTI <- DI_FTI_data$DI_FTI[
  match(
    forbrug11Regression$kvartal,
    DI_FTI_data$kvartal
  )
]

# Tilføj DST's forbrugertillidsindikator
forbrug11Regression$DST_FTI <- DSTKvartalsData$DST_FTI[
  match(
    forbrug11Regression$kvartal,
    DSTKvartalsData$kvartal
  )
]

View(forbrug11Regression)

#22 simple lineære regressioner med loop
# Find de 11 forbrugsgrupper
forbrugsgrupper <- unique(forbrug11Regression$FORMAAAL)

# Lav tomme lister til resultaterne
summary_DI <- list()
summary_DST <- list()

#Loopet
for (i in 1:length(forbrugsgrupper)) {
  
  gruppe <- forbrugsgrupper[i]
  
  dataGruppe <- forbrug11Regression[
    forbrug11Regression$FORMAAAL == gruppe,
  ]
  
  model_DI_4_4 <- lm(
    realvaekst ~ DI_FTI,
    data = dataGruppe
  )
  
  model_DST_4_4 <- lm(
    realvaekst ~ DST_FTI,
    data = dataGruppe
  )
  
  summary_DI[[i]] <- summary(model_DI_4_4)
  
  summary_DST[[i]] <- summary(model_DST_4_4)
}

names(summary_DI) <- forbrugsgrupper
names(summary_DST) <- forbrugsgrupper

names(summary_DI)
names(summary_DST)

#Resultater
# Lav tom dataframe til resultaterne
resultater4_4 <- data.frame(
  FORMAAAL = forbrugsgrupper,
  DI_koefficient = NA,
  DI_pvaerdi = NA,
  DI_R2 = NA,
  DST_koefficient = NA,
  DST_pvaerdi = NA,
  DST_R2 = NA
)

#Udfyld dataframe med loop
for (i in 1:length(forbrugsgrupper)) {
  
  # DI
  resultater4_4$DI_koefficient[i] <-
    summary_DI[[i]]$coefficients["DI_FTI", "Estimate"]
  
  resultater4_4$DI_pvaerdi[i] <-
    summary_DI[[i]]$coefficients["DI_FTI", "Pr(>|t|)"]
  
  resultater4_4$DI_R2[i] <-
    summary_DI[[i]]$r.squared
  
  
  # DST
  resultater4_4$DST_koefficient[i] <-
    summary_DST[[i]]$coefficients["DST_FTI", "Estimate"]
  
  resultater4_4$DST_pvaerdi[i] <-
    summary_DST[[i]]$coefficients["DST_FTI", "Pr(>|t|)"]
  
  resultater4_4$DST_R2[i] <-
    summary_DST[[i]]$r.squared
}

View(resultater4_4)

#Sortere tabellen
resultater4_4[
  order(resultater4_4$DI_R2, decreasing = TRUE),
]

resultater4_4[
  order(resultater4_4$DST_R2, decreasing = TRUE),
]

View(resultater4_4)


# ============================================================
OPGAVE 5
# ============================================================

library(stringr)
library(dplyr)
library(eurostat)
library(ggplot2)
library(restatapi)

allTabs <- get_eurostat_toc()

# søg efter husholdningernes forbrug
forbrugTabs <- allTabs |> filter(str_detect(title,regex("household final consumption expenditure",ignore_case=T)))

forbrugTabs

# undersøg relevant tabel
tab1="namq_10_fcs"
meta1 <- get_eurostat_dsd(tab1)

unique(meta1$concept)
names(meta1)

freqM <- meta1 |> filter(concept=="freq")
freqM

unitM <- meta1 |> filter(concept=="unit")
unitM

itemM <- meta1 |> filter(concept=="na_item")
itemM

sadjM <- meta1 |> filter(concept=="s_adj")
sadjM

geoM <- meta1 |> filter(concept=="geo")
geoM

# ============================================================
# Opgave 5.1
# ============================================================

lande <- c("DK","BE","NL","SE","AT","DE","FR","IT","ES")

df51 <- get_eurostat("namq_10_fcs",time_format="num")

df51 <- df51 |> filter(geo %in% lande,freq=="Q",na_item=="P31_S14", unit=="CLV_PCH_SM", s_adj=="SCA",TIME_PERIOD>=2000)

View(df51)

# kontroller data
unique(df51$geo)
min(df51$TIME_PERIOD)
max(df51$TIME_PERIOD)
sum(is.na(df51$values))
table(df51$geo)

# landenavne
df51 <- df51 |> mutate(land=case_when(
  geo=="DK" ~ "Danmark",
  geo=="BE" ~ "Belgien",
  geo=="NL" ~ "Holland",
  geo=="SE" ~ "Sverige",
  geo=="AT" ~ "Østrig",
  geo=="DE" ~ "Tyskland",
  geo=="FR" ~ "Frankrig",
  geo=="IT" ~ "Italien",
  geo=="ES" ~ "Spanien"))

# lav kvartal
df51 <- df51 |> mutate(aar=floor(TIME_PERIOD),
                       kvartal_nr=round((TIME_PERIOD-aar)*4)+1,
                       kvartal=paste0(aar,"Q",kvartal_nr))

# endeligt datasæt
opgave51 <- df51 |> select(land,kvartal,realvaekst=values)

View(opgave51)

# lav rækkefølge på kvartaler
kvartaler <- unique(opgave51$kvartal)

opgave51$kvartal <- factor(opgave51$kvartal, levels = kvartaler)

# vis kun nogle kvartaler på x-aksen
x_labels <- kvartaler[
  seq(
    1,
    length(kvartaler),
    by = 8
  )
]

# lav graf
ggplot(
  opgave51,
  aes(
    x = kvartal,
    y = realvaekst,
    group = land,
    color = land
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line() +
  scale_x_discrete(
    breaks = x_labels
  ) +
  labs(
    title = "Kvartalsvis årlig realvækst i husholdningernes forbrug",
    subtitle = "2000Q1 - 2026Q2",
    x = "Kvartal",
    y = "Årlig realvækst (%)",
    color = "Land",
    caption = "Kilde: Eurostat, namq_10_fcs"
  ) +
  theme_minimal()

# ============================================================
#Opgave 5.2 – Højeste kvartalsvise årlige realvækst
# ============================================================

#Beregn gennemsnitlig realvækst pr kland
gennemsnit52 <- opgave51 |> group_by(land) |> summarise(gennemsnit=mean(realvaekst))

gennemsnit52 <- gennemsnit52 |> arrange(desc(gennemsnit))

gennemsnit52$gennemsnit <- round(gennemsnit52$gennemsnit,2)

gennemsnit52

# landet med højest gennemsnit
gennemsnit52[1,]

#Lave til en DF
tabel52 <- gennemsnit52

names(tabel52) <- c("Land","Gennemsnitlig realvækst")

tabel52

#Simpel graf

ggplot(gennemsnit52,aes(x=reorder(land,gennemsnit),y=gennemsnit))+
  geom_col()+
  geom_text(aes(label=round(gennemsnit,2)),
            hjust=-0.1,
            size=4)+
  coord_flip()+
  labs(
    title="Gennemsnitlig kvartalsvis årlig realvækst",
    x="Land",
    y="Gennemsnitlig realvækst (%)",
    caption="Kilde: Eurostat, namq_10_fcs"
  )+
  ylim(0,max(gennemsnit52$gennemsnit)+0.3)+
  theme_minimal()

# ============================================================
# Opgave 5.3
# ============================================================

# vi definerer coronaperioden
corona <- c("2020Q1","2020Q2","2020Q3","2020Q4","2021Q1","2021Q2","2021Q3","2021Q4")

# fjern coronaperioden
uden_corona <- opgave51 |> filter(!kvartal %in% corona)

# gennemsnit med corona
med_corona <- opgave51 |> group_by(land) |> summarise(med_corona=mean(realvaekst))

# gennemsnit uden corona
uden_corona_gns <- uden_corona |> group_by(land) |> summarise(uden_corona=mean(realvaekst))

# saml resultater
corona_effekt <- merge(med_corona,uden_corona_gns,by="land")

# beregn forskellen
corona_effekt <- corona_effekt |> mutate(forskel=uden_corona-med_corona, absolut_forskel=abs(forskel))

# sorter efter størst effekt
corona_effekt <- corona_effekt |> arrange(desc(absolut_forskel))

# afrund
corona_effekt$med_corona <- round(corona_effekt$med_corona,2)
corona_effekt$uden_corona <- round(corona_effekt$uden_corona,2)
corona_effekt$forskel <- round(corona_effekt$forskel,2)
corona_effekt$absolut_forskel <- round(corona_effekt$absolut_forskel,2)

corona_effekt

# landet med størst effekt
corona_effekt[1,]


#Graf

ggplot(corona_effekt, aes(x = reorder(land, absolut_forskel), y = forskel)) +
  geom_col() +
  geom_text(aes(label = round(forskel, 2)), hjust = -0.1) +
  coord_flip() +
  ylim(min(corona_effekt$forskel) - 0.1, max(corona_effekt$forskel) + 0.1) +
  labs(
    title = "Coronakrisens effekt på gennemsnitlig årlig realvækst",
    subtitle = "Forskel mellem gennemsnit med og uden coronaperioden",
    x = "Land",
    y = "Forskel i procentpoint",
    caption = "Kilde: Eurostat, namq_10_fcs"
  ) +
  theme_minimal()

# ============================================================
# Opgave 5.4
# ============================================================

# lav liste med landekoder
europa <- geoM |>
  filter(!code %in% c("EU27_2020","EA","EA21","EA20","EA19","EA12")) |> pull(code)

europa

# hent data 
df54 <- get_eurostat("namq_10_fcs",time_format="num")

df54 <- df54 |>
  filter(geo %in% europa,
         freq=="Q",
         na_item=="P31_S14",
         unit=="CLV_PCH_SM",
         s_adj=="SCA",
         TIME_PERIOD>=2020,
         TIME_PERIOD<=2023.25)

View(df54)

# kontroller data
unique(df54$geo)
table(df54$geo)
sum(is.na(df54$values))
min(df54$TIME_PERIOD)
max(df54$TIME_PERIOD)

# hvilke lande mangler data?
setdiff(europa,unique(df54$geo))

# gennemsnit pr. land
gennemsnit54 <- df54 |> group_by(geo) |> summarise(gennemsnit=mean(values))

# få landenavne fra metadata
landenavne <- geoM |> select(code,name)

gennemsnit54 <- gennemsnit54 |> left_join(landenavne,by=c("geo"="code"))

# sorter fra laveste til højeste
gennemsnit54 <- gennemsnit54 |> arrange(gennemsnit)

gennemsnit54$gennemsnit <- round(gennemsnit54$gennemsnit,2)

gennemsnit54

# de 10 lande med lavest gennemsnit
laveste10 <- gennemsnit54 |> slice(1:10)

ggplot(laveste10,aes(x=reorder(name,gennemsnit),y=gennemsnit))+
  geom_col()+
  coord_flip()+
  geom_hline(yintercept=0,linetype="dashed")+
  geom_text(aes(label=gennemsnit),hjust=-0.1)+
  labs(
    title="Laveste gennemsnitlige realvækst i husholdningernes forbrug",
    subtitle="2020Q1 - 2023Q2",
    x="Land",
    y="Gennemsnitlig årlig realvækst (%)",
    caption="Kilde: Eurostat, namq_10_fcs")+
  theme_minimal()

