library(manifestoR)
mp_setapikey("manifesto_apikey.txt")

my_corpus <- mp_corpus(countryname == "Austria")

warnings()

View(my_corpus)

head(my_corpus)
