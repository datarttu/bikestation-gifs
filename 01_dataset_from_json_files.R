#' # Prepare HSL citybike station dataset for visualization
#' 
#' - Read JSON datasets of a given date
#' - Use static properties (coords, capacity) of the first JSON
#' - Save static properties and time series as csv files
#' 
#' *&copy; Arttu Kosonen 8/2019*

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(jsonlite)

raw_data_dir <- "" # your data dir here
csv_data_dir <- "" # output dir of your choice here

#' We choose to use Monday 29th July.
#' Let us find the relevant JSON files per minute.
date_part <- "20190729"
jsons_in <- list.files(path = raw_data_dir,
                       pattern = date_part,
                       full.names = TRUE)
head(jsons_in)

#' Seems about right. First, we extract the static attributes
#' from the first entry, assuming that it is not broken
#' and the attributes do not change during the day.
#' In this case, the data inside the JSON lives under a node
#' named `result`, and `jsonlite` can convert it directly to a
#' data frame.
st_attrs <- fromJSON(jsons_in[1])[["result"]]
head(st_attrs)

#' We want to treat station id and name separately,
#' so we can just save the integer id in the time series dataset.
#' Moreover, we have to do some string wrangling to extract the coordinates.
#' I am not sure whether `operative` and `style` have different meanings here,
#' but let us just pick the string attribute `style`.
#' 
#' Some stations may not have coordinates; they are just dropped here
#' since the idea is to visualize the stations on a map (hence `filter()`).
st_attrs_tidy <- st_attrs %>%
  mutate(stid = as.integer(substr(name, 1, 3)),
         stname = substr(name, 5, nchar(name))) %>%
  filter(grepl(",", coordinates)) %>%
  separate(coordinates, c("lon", "lat"), sep = ",") %>%
  select(stid, stname, lat, lon, total_slots, style)
head(st_attrs_tidy)

#' I am using `data.table`'s `fwrite` to write out the csv,
#' it does not matter what writer function to use here.
#' However, `fread` shows better performance with large datasets
#' such as the following time series dataset.
data.table::fwrite(st_attrs_tidy,
                   file = sprintf("%s/station_attrs.csv", csv_data_dir))

#' Next we want to iterate over the JSON files
#' to build a single time series dataset.
#' There is a file for almost every minute,
#' but that is too much for a visualization over one day:
#' we would rather want 15-minute samples, for example.
#' On the other hand, we cannot guarantee that there is a file
#' available for *every* "even" 15-minute period, such as "T131500...".
#' So we take the input file names, extract the time part
#' and find the first "representative" files for each 15-minute chunk.
fn_w_quarters <- data.frame(fpath = jsons_in) %>%
  mutate(
    ts = str_match(
      fpath, "(?<=_)[0-9]+T[0-9]+(?=Z)") %>%
      as.POSIXct(format = "%Y%m%dT%H%M%S", tz = "UTC"),
    tsq = floor_date(ts, unit = "15 minutes"))
head(fn_w_quarters)
fn_sample <- fn_w_quarters %>%
  group_by(tsq) %>%
  summarize(fpath = first(fpath)) %>%
  mutate(fpath = as.character(fpath))

#' As a result, we have `24 * 4` rows as we wanted:
nrow(fn_sample)
head(fn_sample)
tail(fn_sample)

#' This is a reasonable amount of data we can read into a list
#' of data frames. This list can then be made into a single table
#' using `data.table`'s `rbindlist` function.
dt_ls <- list()
for (i in 1:nrow(fn_sample)) {
  dt <- fromJSON(fn_sample[i,]$fpath)[["result"]] %>%
    mutate(stid = as.integer(substr(name, 1, 3)),
           ts = with_tz(fn_sample[i,]$tsq, "UTC")) %>%
    filter(grepl(",", coordinates)) %>%
    select(ts, stid, avl_bikes)
  dt_ls[[i]] <- dt
}
dt_out <- data.table::rbindlist(dt_ls)

fn_out <- sprintf("%s/series_%s.csv", csv_data_dir, date_part)
data.table::fwrite(dt_out, file = fn_out)
