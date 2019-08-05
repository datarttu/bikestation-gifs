#' # Make an animated map of HSL citybike station states
#' 
#' Given the datasets prepared by the previous script,
#' make a GIF animation of the 15-minute sampled
#' citybike station time series on a map.
#' 
#' NOTE: timestamps are in UTC time zone,
#' therefore the day begins at 21:00 previous day in
#' Finnish time.
#' 
#' *&copy; Arttu Kosonen 8/2019*

library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)

st_attrs <- data.table::fread("station_attrs.csv") %>%
  st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
  st_transform(crs = 3879) %>%
  cbind(st_coordinates(.))
bb <- st_bbox(st_buffer(st_attrs, 500))
st_series <- data.table::fread("series_20190729.csv") %>%
  left_join(st_attrs, by = "stid") %>%
  mutate(avl_prop = avl_bikes / total_slots) %>%
  mutate(ts = with_tz(ymd_hms(ts), tz = "Europe/Helsinki")) %>%
  mutate(state = case_when(avl_bikes <= 0 ~ "empty",
                           # avl_bikes == 1 ~ "almost",
                           # avl_bikes > total_slots ~ "over",
                           TRUE ~ "available"))

#' Sea area was extracted from HSY WFS:
#' [](https://kartta.hsy.fi/geoserver/wfs)
waters <- st_read("hsy-sea.geojson") %>%
  st_crop(bb)

ani <- ggplot() +
  geom_sf(data = waters, color = NA, fill = "gray15") +
  geom_point(data = st_series,
             aes(x = X, y = Y, 
                 alpha = avl_bikes,
                 fill = state),
             size = 4,
             shape = 21) +
  scale_alpha_continuous(range = c(0.4, 1)) +
  scale_fill_manual(values = c("empty"="indianred1", "almost"="#c580c5", 
                               "available"="darkseagreen1", "over"="lightskyblue")) +
  labs(title = "{current_frame}") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "black"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, color = "white", size = 20),
        axis.text = element_blank(),
        axis.ticks.length = unit(0, "mm")) +
  transition_manual(ts)

#' Check out the gif
animate(ani, nframes = 96, fps = 8, end_pause = 4, width = 800)

#' Save as file
anim_save("anim_state.gif", animation = ani, nframes = 96, fps = 8, end_pause = 4, width = 800)
