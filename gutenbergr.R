library(tibble)
library(tidyr)
library(dplyr)
library(tidytext)
library(gutenbergr)
library(stringr)
library(ggplot2)
# library(maps)
# library(ggmap)

daystext <- "(monday|tuesday|wednesday|thursday|friday|saturday|sunday)"
monthstext <- "(january|february|march|april|may|june|july|august|september|october|november|december)"

## james cook gutenberg_author_id == 2644

# jc <- filter(gutenberg_metadata, gutenberg_author_id == 2644 & has_text == TRUE) %>% select(gutenberg_id, title, has_text)

## first voyage id == 8106
## second voyage   == 15777 & 15869

## get text from saved RDS or from Gutenberg
if (file.exists("jc_v1.rds")) {
  jc_v1 <- readRDS("jc_v1.rds")
  as_tibble(jc_v1)
} else {
  jc_v1 <- gutenberg_download(8106, strip = T)[,2]
  saveRDS(jc_v1, "jc_v1.rds")
}

## trim to include only body text
jc_v1 <- jc_v1[2244:18618,]

## split into paragraphs
jc_v1_paragraphs <- jc_v1 %>% unnest_tokens(output = paragraph, input = text, token = "paragraphs", to_lower = T)


jc_v1_dated <- jc_v1_paragraphs$paragraph[grepl(pattern = paste(daystext, monthstext, sep = "|"),x = jc_v1_paragraphs$paragraph, perl = T)]

jc_v1_dated2 <- lapply(jc_v1_paragraphs$paragraph,function(x){
  c(str_extract(x, paste0(daystext,", [a-z ]*[0-9]{1,2}(st|nd|rd|th){1}[, ]*[0-9]{0,4}")),
    str_extract(x, monthstext),
    str_extract(x, "17(68|69|70|71|72){1}"),
    str_extract(x, "(latitude) [0-9]{1,} degrees [0-9]{1,} minutes (north|south), (longitude) [0-9]{1,} degrees [0-9]{1,} minutes (west|east)")
  )
  }
)
jc_v1_dated2 <- as_tibble(do.call(rbind,jc_v1_dated2))
names(jc_v1_dated2) <- c("daylong", "monthlong", "year", "latlon_text")

jc_v1_dated3_latlon <- lapply(
  str_split(jc_v1_dated2$latlon_text, "[, ]+"),
  function(x) {
    if(sum(is.na(x)) == 0) {
    c(
      switch(
        str_extract(x[6], pattern = "(south|north)"),
        "north" =  as.numeric(x[2]) + as.numeric(x[4])/60 ,
        "south" = (as.numeric(x[2]) + as.numeric(x[4])/60)*-1
      ),
      switch(
        str_extract(x[12], pattern = "(east|west)"),
        "west" = (as.numeric(x[8]) + as.numeric(x[10])/60) *-1,
        "east" = (as.numeric(x[8]) + as.numeric(x[10])/60)
      )
    )
    } else c(NA,NA)
    }
  )
jc_v1_dated3_latlon <- as_tibble(do.call(rbind,jc_v1_dated3_latlon))
names(jc_v1_dated3_latlon) <- c("lat", "long")

jc_v1_dated3_date <- as_tibble(select(jc_v1_dated2, daylong, monthlong, year))
jc_v1_dated3_date <- jc_v1_dated3_date %>%
  # mutate(year  = str_extract(jc_v1_dated3_date$date,"[0-9]{4}")) %>%
  mutate(month = as.numeric(factor(jc_v1_dated3_date$monthlong, levels = tolower(month.name)))) %>%
  mutate(day   = str_extract(jc_v1_dated3_date$daylong, "[0-9]{1,2}"))
# jc_v1_dated3_date$year[1] <- 1768
# jc_v1_dated3_date$month[1] <- "august"
jc_v1_dated3_date <- jc_v1_dated3_date %>% fill(year, month) %>% mutate(fulldate = lubridate::ymd(paste(year, month, day, sep = "-")))

jc_v1_dated4 <- bind_cols(jc_v1_dated3_date,jc_v1_dated3_latlon) %>% select(fulldate,lat,long)

pacific <- map_data("world2") %>% group_by(group) # %>% filter(min(lat) <= 20)
worldmap <- ggplot(jc_v1_dated4, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group), data = pacific, colour = "grey", fill = "gainsboro") +
  coord_map(projection = "sinusoidal")
print(worldmap + geom_point(size = 1, shape = 19, colour = "red")#+
 # scale_x_continuous("longitude", breaks = (-4:4) * 45) +
 # scale_y_continuous("latitude",  breaks = (-2:3) * 30)
)