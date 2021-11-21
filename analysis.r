library(tidyverse)

# load dataset
trend <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Q1: Across the country, how is the proportion between black jailed population
# and total jailed population changed from 1985 to 2010? Analyze the change rate
# by comparing 1985 data and 2010 data. Store this in `black_jp_change`.
black_1985 <- trend %>%
  group_by(year) %>%
  summarise(bj_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
  filter(year == 1985) %>%
  pull(bj_pop)
total_1985 <- trend %>%
  group_by(year) %>%
  summarise(tj_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(year == 1985) %>%
  pull(tj_pop)
black_jp_1985 <- black_1985 / total_1985

black_2010 <- trend %>%
  group_by(year) %>%
  summarise(bj_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
  filter(year == 2010) %>%
  pull(bj_pop)
total_2010 <- trend %>%
  group_by(year) %>%
  summarise(tj_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(year == 2010) %>%
  pull(tj_pop)
black_jp_2010 <- black_2010 / total_2010

black_jp_change <- (black_jp_2010 - black_jp_1985) / black_jp_1985


# Q2: Across the country, how is the proportion between black population and total
# population changed from 1990 to 2010? Analyze the change rate by comparing 1990
# data and 2010 data. Store this in `black_tp_change`.
black_pop_1990 <- trend %>%
  group_by(year) %>%
  summarise(bp_pop = sum(black_pop_15to64, na.rm = TRUE)) %>%
  filter(year == 1990) %>%
  pull(bp_pop)
total_pop_1990 <- trend %>%
  group_by(year) %>%
  summarise(to_pop = sum(total_pop_15to64, na.rm = TRUE)) %>%
  filter(year == 1990) %>%
  pull(to_pop)
black_tp_1990 <- black_pop_1990 / total_pop_1990

black_pop_2010 <- trend %>%
  group_by(year) %>%
  summarise(bp_pop = sum(black_pop_15to64, na.rm = TRUE)) %>%
  filter(year == 2010) %>%
  pull(bp_pop)
total_pop_2010 <- trend %>%
  group_by(year) %>%
  summarise(to_pop = sum(total_pop_15to64, na.rm = TRUE)) %>%
  filter(year == 2010) %>%
  pull(to_pop)
black_tp_2010 <- black_pop_2010 / total_pop_2010

black_tp_change <- (black_tp_2010 - black_tp_1990) / black_tp_1990


# Q3: Across the country, how is the proportion between white jailed population
# and total jailed population changed from 1985 to 2010? Analyze the change rate
# by comparing 1985 data and 2010 data. Store this in `white_jp_change`.
white_1985 <- trend %>%
  group_by(year) %>%
  summarise(wj_pop = sum(white_jail_pop, na.rm = TRUE)) %>%
  filter(year == 1985) %>%
  pull(wj_pop)
white_jp_1985 <- white_1985 / total_1985

white_2010 <- trend %>%
  group_by(year) %>%
  summarise(wj_pop = sum(white_jail_pop, na.rm = TRUE)) %>%
  filter(year == 2010) %>%
  pull(wj_pop)
white_jp_2010 <- white_2010 / total_2010

white_jp_change <- (white_jp_2010 - white_jp_1985) / white_jp_1985


# Q4: Across the country, how is the proportion between white population and total
# population changed from 1990 to 2010? Analyze the change rate by comparing 1990
# data and 2010 data. Store this in `white_tp_change`.
white_pop_1990 <- trend %>%
  group_by(year) %>%
  summarise(wp_pop = sum(white_pop_15to64, na.rm = TRUE)) %>%
  filter(year == 1990) %>%
  pull(wp_pop)
white_tp_1990 <- white_pop_1990 / total_pop_1990

white_pop_2010 <- trend %>%
  group_by(year) %>%
  summarise(wp_pop = sum(white_pop_15to64, na.rm = TRUE)) %>%
  filter(year == 2010) %>%
  pull(wp_pop)
white_tp_2010 <- white_pop_2010 / total_pop_2010

white_tp_change <- (white_tp_2010 - white_tp_1990) / white_tp_1990


# Q5: Recently (in 2010), among these four regions ("Midwest", "Northeast", "South",
# "West"), which region has the highest black jailed population over total jailed
# population proportion? Store this in `high_region`
high_region <- trend %>%
  filter(year == 2010) %>%
  group_by(region) %>%
  summarise(bt = sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(bt == max(bt)) %>%
  pull(region)


# time trend chart: Plot trend of jailed population proportion in south region over
# past 25 years (1985-2010), for both black and white. Store this in `prop_trend`.
prop_change <- trend %>%
  filter(region == "South") %>%
  filter(year >= 1985 & year <= 2010) %>%
  group_by(year) %>%
  summarise(
    bl_p = sum(black_jail_pop, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE),
    wh_p = sum(white_jail_pop, na.rm = TRUE) / sum(total_pop_15to64, na.rm = TRUE)
  ) %>%
  select(year, bl_p, wh_p)
prop_trend <- ggplot(data = prop_change) +
  geom_line(aes(y = bl_p, x = year, colour = "Black")) +
  geom_line(aes(y = wh_p, x = year, colour = "White")) +
  labs(x = "Year", y = "Proportion", title = "Black vs. White Jailed Proportion, South Region") +
  scale_color_manual(
    name = "line of",
    values = c("Black" = "darkred", "White" = "steelblue")
  )


# variable comparison chart: Plot comparison chart of white jailed population
# (y-axis) vs. total white population (x-axis) at each county. Store this in
# `comparison`.
comparison_data <- trend %>%
  filter(!is.na(white_pop_15to64)) %>%
  filter(!is.na(white_jail_pop)) %>%
  select(white_jail_pop, white_pop_15to64)
comparison <- ggplot(data = comparison_data) +
  aes(y = white_jail_pop, x = white_pop_15to64) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    x = "White Total Population", y = "White Jailed Population",
    title = "Jailed Population vs. Total Population, White"
  )

# map: Plot a map to visualize the recent(2010) black jailed population vs.
# total black population proportion in each state; Store this in `prop_map`.
state_info <- map_data("state")
state_info <- mutate(state_info, region = state.abb[match(region, tolower(state.name))])
prop_for_map <- trend %>%
  filter(year == 2010) %>%
  group_by(state) %>%
  summarise(bl_prop = sum(black_jail_pop, na.rm = TRUE) / sum(black_pop_15to64, na.rm = TRUE)) %>%
  select(state, bl_prop)
state_bl_prop <- inner_join(state_info, prop_for_map, by = c("region" = "state"))
prop_map <- ggplot() +
  geom_polygon(
    data = state_bl_prop,
    aes(x = long, y = lat, group = group, fill = bl_prop),
    color = "white"
  ) +
  scale_fill_continuous(
    name = "proportion",
    breaks = seq(0, max(prop_for_map$bl_prop), 0.004)
  ) +
  labs(
    x = "longtitude", y = "latitude",
    title = "jailed population / total population in each state, black"
  ) +
  theme_minimal()
