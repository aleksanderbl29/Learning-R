library(tidyverse)
library(jsonlite)
library(splitstackshape)

import_json <- as.data.frame(fromJSON("API/response.json"))

temp_api_data <- import_json %>% 
  select(-starts_with("lin"), -timeStamp, -numberReturned, -type) %>% 
  as_tibble() %>% 
  concat.split(split.col = 1) %>% 
  mutate(features.geometry.coordinates_1 = substr(features.geometry.coordinates_1, 3, length(features.geometry.coordinates_1)),
         features.geometry.coordinates_2 = substr(features.geometry.coordinates_2, 1, 7))

rm(import_json)

head(temp_api_data)
colnames(temp_api_data)

api_data <- temp_api_data %>% 
  rename(coordinates = features.geometry.coordinates,
         coordinate_1 = features.geometry.coordinates_1,
         coordinate_2 = features.geometry.coordinates_2,
         parameter = features.properties.parameterId,
         value = features.properties.value,
         station = features.properties.stationId,
         created = features.properties.created,
         observed = features.properties.observed) %>% 
  select(-features.type, -features.geometry.type)

rm(temp_api_data)

colnames(api_data)
head(api_data)


