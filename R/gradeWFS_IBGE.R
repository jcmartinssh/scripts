

library(sf) # simple features packages for handling vector GIS data
library(httr) # generic webservice package
library(tidyverse) # a suite of packages for data wrangling, transformation, plotting, ...
library(ows4R) # interface for OGC webservices

install.packages("ows4R")
install.packages("httr")


Sys.setenv(LANG = "English")



wfsIBGE = "https://geoservicos.ibge.gov.br/geoserver/ows"

#url <-f