library(tidyverse)
library(httr)
library(XML)
library(xml2)

XML_hammerum <- read_xml(GET("https://www.dst.dk/valg/Valg1968094/xml/fintal_1973350.xml"))

xml_structure(XML_hammerum)

XML_parsed_hammerum <- xmlParse(XML_hammerum)

xml_structure(XML_hammerum)

# hammerum <- xmlToDataFrame(XML_hammerum)

hammerum <- xmlToDataFrame(nodes = getNodeSet(XML_hammerum, "//Stemmer"))
