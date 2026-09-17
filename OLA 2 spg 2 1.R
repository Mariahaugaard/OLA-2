#Importer datasæt 
library(readxl)

forbrug <- read_excel("Forbrug.xlsx")

FTI <- read_excel("forbrugforventninger-excel.xlsx")

#forbrugertillid påvirker forbruget


# 1. Hent datoerne fra første række

datoer <- colnames(FTI)[-1]

# 2. Fjern rækken med datoer
FTIKvartal <- FTI

# 3. Giv kolonnerne de rigtige navne
colnames(FTIKvartal)[1] <- "Variabel"

# 4. Lav alle månedskolonner om til numeric
FTIKvartal[, -1] <- lapply(FTIKvartal[, -1], as.numeric)

# 5. Find år
aar <- substr(datoer, 1, 4)

# 6. Find måned
maaned <- as.numeric(substr(datoer, 6, 7))

# 7. Lav kvartalsnummer
kvartal <- (maaned - 1) %/% 3 + 1

# 8. Lav navne som 2006Q1, 2006Q2 osv.
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


# Se resultatet
View(kvartalsData)

