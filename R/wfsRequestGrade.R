library(sf) # simple features packages for handling vector GIS data
library(httr) # generic webservice package
library(tidyverse) # a suite of packages for data wrangling, transformation, plotting, ...
library(ows4R) # interface for OGC webservices

pr_albers_br <- 'PROJCS["Conica_Equivalente_de_Albers_Brasil",
                         GEOGCS["GCS_SIRGAS2000",
                                DATUM["D_SIRGAS2000",
                                      SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221009113]],
                                PRIMEM["Greenwich",0],
                                UNIT["Degree",0.017453292519943295]],
                         PROJECTION["Albers"],
                         PARAMETER["standard_parallel_1",-2],
                         PARAMETER["standard_parallel_2",-22],
                         PARAMETER["latitude_of_origin",-12],
                         PARAMETER["central_meridian",-54],
                         PARAMETER["false_easting",5000000],
                         PARAMETER["false_northing",10000000],
                         UNIT["Meter",1]]'

wfs_bwk <- "https://geoservicos.ibge.gov.br/geoserver/ows"

url <- parse_url(wfs_bwk)

url$query <- list(service = "wfs",
                  version = "2.0.0",
                  request = "GetCapabilities"
                  )

request <- build_url(url)

request

bwk_client <- WFSCapabilities$new(wfs_bwk,
                            version = "1.1.0")

