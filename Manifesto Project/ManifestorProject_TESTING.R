library(manifestoR)
library(manifestoEnhanceR)
mp_setapikey("manifesto_apikey.txt")
mp_cite()


mp_corp <- mp_corpus(countryname == "Austria")
mp_corp

mp <-  as_tibble(mp_corp)
mp
head(mp)
