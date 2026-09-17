library(readxl)

forbrug <- read_excel("Forbrug.xlsx")

FTI <- read_excel("forbrugforventninger-excel.xlsx")

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
par(mar = c(7, 5, 4, 5))

# 1. Lav søjler for realvækst
barMid <- barplot(
  grafData$realvækst,
  col = "deepskyblue3",
  border = NA,
  ylim = c(-8, 8),
  axes = FALSE,
  space = 0.15,
  main = "DI's forbrugertillidsindikator følger i højere grad privatforbruget"
)

# X-akse
axis(1, at = barMid[aarPos], labels = aarLabels, tick = FALSE)

# Højre y-akse til realvækst
axis(4, at = c(-8, -5, -3, 0, 3, 5, 8), las = 1)
mtext("Pct.", side = 4, line = 2)

# 2. Læg linjen ovenpå
par(new = TRUE)

plot(
  x = barMid,
  y = grafData$DI_FTI,
  type = "n",
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(-25, 25),
  xlim = range(barMid)
)

# Venstre y-akse til DI-FTI
axis(2, at = c(-25, -17, -8, 0, 8, 17, 25), las = 1)
mtext("Nettotal", side = 2, line = 2)

# Hjælpelinjer
abline(h = c(-25, -17, -8, 0, 8, 17, 25), col = "grey85", lty = 1)

# Selve linjen
lines(barMid, grafData$DI_FTI, lwd = 2.5, col = "grey30")

# Ramme
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
