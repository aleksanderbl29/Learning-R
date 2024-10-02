# Installer og indlæs nødvendige pakker
install.packages("readxl")
install.packages("haven")

library(readxl)
library(haven)

# Læs Excel-filen ind i en data frame
df <- read_excel("Data/import.xlsx")

# Skriv data frame til en Stata-fil
write_dta(df, "Data/data.dta")
