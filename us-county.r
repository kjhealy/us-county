library(tidyverse)
library(broom)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(stringr)
library(scales)

###--------------------------------------------------
### Set up the Maps.
### Code borrows heavily from work by Bob Rudis:
### https://github.com/hrbrmstr/rd3albers
###--------------------------------------------------

theme_set(theme_minimal())

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)


## for theme_map
## devtools::source_gist("33baa3a79c5cfef0f6df")

theme_map <- function(base_size=9, base_family="") {
    require(grid)
    theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
          )
}

## US Census Shapefiles
## https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
## See also http://eric.clst.org/Stuff/USGeoJSON

## Converted to geojson format
## http://eric.clst.org/Stuff/USGeoJSON
## Read U.S. counties moderately-simplified GeoJSON file
us_counties <- readOGR(dsn="data/geojson/gz_2010_us_050_00_5m.json",
                       layer="OGRGeoJSON")

# Convert it to Albers equal area
us_counties_aea <- spTransform(us_counties,
                               CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

us_counties_aea@data$id <- rownames(us_counties_aea@data)

# Extract, then rotate, shrink & move alaska (and reset projection)
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- us_counties_aea[us_counties_aea$STATE=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_counties_aea)

# extract, then rotate & shift hawaii
hawaii <- us_counties_aea[us_counties_aea$STATE=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_counties_aea)

# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used
us_counties_aea <- us_counties_aea[!us_counties_aea$STATE %in% c("02", "15", "72"),]
us_counties_aea <- rbind(us_counties_aea, alaska, hawaii)



## p <- ggplot(data=co.map, aes(x=long, y=lat, group=group))
## p1 <- p + geom_map(data = co.map,
##                    map = co.map,
##                    aes(map_id=id,
##                        x=long,
##                        y=lat,
##                        group=group,
##                        fill=pop.dens),
##                    color="white",
##                    size=0.2)


###--------------------------------------------------
### Merge census county-level dataset with map data
###--------------------------------------------------

state_data <- read_csv("data/census/state-data-statabs-2012.csv")

county_names <- read_csv("data/census/fips-by-state.csv")
county_names$name <- stringr::str_replace_all(county_names$name, "ñ", "n")

county_data <- read_csv("data/census/DataSet.txt")
county_data$id <- as.character(county_data$fips)
ind <- county_data$fips<10000
county_data$id[ind] <- paste("0", county_data$id[ind], sep="")
county_data$id[county_data$id=="00"] <- "0"

ind <- match(county_data$fips, county_names$fips)
county_data$name <- county_names$name[ind]
county_data$state <- county_names$state[ind]

ind <- match(state_data$fips, county_data$fips)
county_data$state[ind] <- state_data$State.Abbr

## Add state names as levels of county name, so states have FIPS too
levels(county_data$name) <- c(levels(county_data$name), levels(state_data$State))
county_data$name[ind] <- state_data$State


### Add census region. Don't call the variable "region" because that's
### already reserved by the map object
ind <- match(county_data$state, state_data$State.Abbr)
county_data$census.region <- state_data$Region[ind]



county_data$pop_dens <- with(county_data, PST045214/LND110210)
county_data$pop_dens <- Hmisc::cut2(county_data$pop_dens,
                                    cuts = c(0, 10, 50, 100, 500, 1000, 5000))
county_data$pop_dens4 <- with(county_data, Hmisc::cut2(PST045214/LND110210, g = 4, digits = 1))
county_data$pop_dens6 <- with(county_data, Hmisc::cut2(PST045214/LND110210, g = 6, digits = 1))

county_data$pct_black <- Hmisc::cut2(county_data$RHI225213,
                              cuts = c(0, 2, 5, 10, 15, 25, 50))

### Add data on suicides by firearm
county_su <- read_csv("data/census/gun-suicide-allyrs.csv")
county_su <- county_su %>% mutate(raw_man = ((raw/pop)*100000),
                                  su_gun4 = Hmisc::cut2(raw_man, g = 4, digits = 1),
                                  su_gun6 = Hmisc::cut2(raw_man, g = 6, digits = 1))
county_su <- county_su %>% select(id, su_gun4, su_gun6)
county_su$fips <- county_su$id

ind <- county_su$id<10000
county_su$id[ind] <- paste("0", county_su$id[ind], sep="")



## merge county_data and county_su
county_data$id <- county_data$fips
county_data <- left_join(county_data, county_su, "id")

## Tidy the spatial object into a data fram ggplot can use
co_map <- tidy(us_counties_aea, region="GEO_ID")

## Clean up the id label by stripping out the 'This is a map of the
## US' prefix
co_map$id <- str_replace(co_map$id, "0500000US", "")

county_map <- co_map %>% janitor::clean_names()
save(county_map, file = "~/Source/socviz/data/county_map.rda", compress = "xz")


## Merge the map data frame with the county-level census data, by id.
co_map <- left_join(co_map, county_data, by="id")

county_data <- county_data %>% janitor::clean_names()

co2 <- county_data %>% dplyr::select(id, name, state, census_region,
                             pop_dens, pop_dens4, pop_dens6, pct_black, pst045214,
                             sex255213, rhi125213, rhi225213,
                             lfe305213, lnd110210, inc110213, su_gun4, su_gun6)

colnames(co2) <- c("id", "name", "state", "census_region", "pop_dens", "pop_dens4",
                   "pop_dens6", "pct_black", "pop", "female", "white", "black",
                   "travel_time", "land_area", "hh_income", "su_gun4", "su_gun6")

county_data <- co2

save(county_data, file = "~/Source/socviz/data/county_data.rda")


### Documentation helper for the package file
out <- lapply(county_data, attributes)

for(i in 1: length(out)) {
    name <- colnames(county_data)[i]
    info <- out[[i]]$label
    var_str <- paste0("\\item ", trimws(name), ". ",
                     trimws(info), ".\n")
    cat(casefold(var_str))
}

out <- lapply(county_map, attributes)

for(i in 1: length(out)) {
    name <- colnames(county_map)[i]
    info <- out[[i]]$label
    var_str <- paste0("\\item ", trimws(name), ". ",
                     trimws(info), ".\n")
    cat(casefold(var_str))
}


###--------------------------------------------------
### Make some maps
###--------------------------------------------------

p <- ggplot(data=na.omit(co_map), aes(x=long, y=lat, group=group))

p1 <- p + geom_map(data = na.omit(co_map),
                   map = na.omit(co_map),
                   aes(map_id=id,
                       group=group,
                       fill=su_gun6),
                   color="gray80",
                   size=0.2)

p2 <- p1 + scale_fill_brewer(palette="Oranges")

p2 <- p2 + coord_equal()
p2 <- p2 + theme_map()
p2 <- p2 + theme(legend.position="right") + labs(fill="Rate per 100,000 population")
p2 <- p2 + ggtitle("Gun-related suicides, 1999-2015")
p2



### Population Density
p <- ggplot(data=co_map, aes(x=long, y=lat, group=group))

p1 <- p + geom_map(data = co_map,
                   map = co_map,
                   aes(map_id=id,
                       x=long,
                       y=lat,
                       group=group,
                       fill=pop_dens),
                   color="white",
                   size=0.2)

p2 <- p1 + scale_fill_brewer(palette="PuBu",
                             labels = c("0-10", "10-50", "50-100",
                                        "100-500", "500-1,000", "1,000-5,000", ">5,000"))

p2 <- p2 + coord_equal()
p2 <- p2 + theme_map()
p2 <- p2 + theme(legend.position="right") + labs(fill="Population per\nsquare mile")
p2 <- p2 + ggtitle("Population Density, 2014")
p2

ggsave("figures/us-pop-density-2014.png",
       p2,
       height=8,
       width=12,
       dpi=300)


### Percent Black
p <- ggplot(data=co_map, aes(x=long, y=lat, group=group))

p1 <- p + geom_map(data=co_map,
                   map = co_map,
                   aes(map_id=id,
                       x=long,
                       y=lat,
                       group=group,
                       fill=pct_black),
                   color="white",
                   size=0.2)

p2 <- p1 + scale_fill_brewer(palette="Oranges",
                             labels = c("<2", "2-5", "5-10",
                                        "10-15", "15-25", "25-50", ">50"))
p2 <- p2 + coord_equal()
p2 <- p2 + theme_map()
p2 <- p2 + theme(legend.position="right") + labs(fill="Percent of\nPopulation, 2013")
p2 <- p2 + ggtitle("US Population, Percent Black")
p2

ggsave("figures/us-pct-black-2013.png",
       p2,
       height=8,
       width=12,
       dpi=300)


### --------------------------------------------------
### Scatter plot
### --------------------------------------------------

p <- ggplot(county_data, aes(x=pop/land_area,
                             y=black,
                             label=state,
                             color=census_region))
p2 <- p + geom_text(size=1.4) +
    scale_x_continuous(trans=asinh_trans(),
                       breaks=c(0, 10, 100,
                                1000, 10000, 100000),
                       labels=c("0", "10", "100",
                                "1,000", "10,000", "100,000")) +
    scale_color_manual(values=my.colors("rcb")) +
    labs(x="County Population Density (People per Square Mile)",
         y="Percent Black", color="Region") +
    theme(legend.position="bottom") +
    guides(color = guide_legend(override.aes = list(size=6))) +
    ggtitle("Population Density and Percent Black by County, showing Region")

p2

ggsave("figures/us-density-v-pctblack-lab.png",
       p2,
       height=8,
       width=12,
       dpi=300)
