

#  ------------------------------------------------------------------------
#
# Title : cities-1000
#    By : Victor
#  Date : 2018-07-02
#
#  ------------------------------------------------------------------------


# Source: http://download.geonames.org/export/


# Packages ----------------------------------------------------------------

library( data.table )
library( ISOcodes )
library( stringr )



# Data --------------------------------------------------------------------

tmp <- tempdir()
download.file(
  url = "http://download.geonames.org/export/dump/cities1000.zip",
  destfile = file.path(tmp, "cities1000.zip")
)

unzip(
  zipfile = file.path(tmp, "cities1000.zip"),
  exdir = tmp
)
list.files(path = tmp)

cities1000 <- fread(file = file.path(tmp, "cities1000.txt"), encoding = "UTF-8")
cities1000


cities1000 <- cities1000[, list(
  name = stringi::stri_trans_general(str = V2, id = "ASCII-Latin"),
  asciiname = V3,
  latitude = V5, longitude = V6,
  country_code = V9,
  county_code = paste(V9, V11, sep = "."),
  population =  V15,
  elevation = V16,
  dem = V17
)]

cities1000





# Countries code ----------------------------------------------------------

code_countries <- as.data.table(ISOcodes::ISO_3166_1)
code_countries <- code_countries[, list(country_code = Alpha_2, country_num = Numeric, country_name = Name)]

cities1000 <- merge(x = cities1000, y = code_countries, by = "country_code")
cities1000


# View(cities1000[, .N, by = country_name])




# Counties code -----------------------------------------------------------

admin1CodesASCII <- fread("http://download.geonames.org/export/dump/admin1CodesASCII.txt", encoding = "UTF-8")
admin1CodesASCII <- admin1CodesASCII[, list(county_code = V1, county_name = stringi::stri_trans_general(str = V2, id = "ASCII-Latin"))]

cities1000 <- merge(x = cities1000, y = admin1CodesASCII, by = "county_code")

cities1000



# Continent ---------------------------------------------------------------

View(UN_M.49_Regions)
head(UN_M.49_Countries)

regions <- as.data.table(UN_M.49_Regions)
regions <- regions[as.character(Type) == "Region"]
regions[, Type := NULL]
regions


max_child <- max(str_count(string = regions$Children, pattern = ", ")) + 1
cols <- paste0("child", seq_len(max_child))

regions[, (cols) := tstrsplit(Children, split = ", ")]

regions <- melt(
  data = regions,
  id.vars = c("Code", "Name", "Parent"),
  measure.vars = cols,
  variable.name = "ChildrenNum",
  value.name = "Children",
  na.rm = TRUE
)

continent <- regions[Parent == "001", list(continent_name = Name, region_code = Children)]
continent <- continent[order(continent_name)]
continent

continent <- merge(
  x = continent,
  y = regions[, list(region_code = Code, region_name = Name, country_num = Children)],
  by = "region_code", all.y = FALSE
)
continent <- continent[order(continent_name, region_name, country_num)]
continent

View(continent)



# merge avec cities

cities1000_ <- merge(
  x = cities1000,
  y = continent[, list(continent_name, region_name, country_num)],
  by = "country_num",
  all.x = FALSE, all.y = FALSE
)

cities1000_
cities1000_[, .N, by = continent_name]


# order columns
setcolorder(cities1000_, c( "name", "asciiname", "latitude",
                            "longitude", "population", "elevation", "dem",
                            "country_num", "country_code", "country_name",
                            "continent_name", "region_name"))


# save rds
saveRDS(cities1000_, file = "data-raw/cities1000_county.rds")





# use data in package -----------------------------------------------------

cities1000 <- copy(cities1000_)
usethis::use_data(cities1000, internal = TRUE, overwrite = TRUE)




