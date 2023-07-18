library(sf) # simple features packages for handling vector GIS data
library(httr) # generic webservice package
library(tidyverse) # a suite of packages for data wrangling, transformation, plotting, ...
library(ows4R) # interface for OGC webservices

wfs_bwk <- "https://geoservicos.ibge.gov.br/geoserver/ows"

url <- parse_url(wfs_bwk)

url$query <- list(service = "wfs",
                  version = "2.0.0",
                  request = "GetCapabilities"
                  )

request <- build_url(url)

request

bwk_client <- WFSClient$new(wfs_bwk,
                            serviceVersion = "2.0.0")

