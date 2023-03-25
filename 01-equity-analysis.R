library(sf)
library(tigris)
library(tidycensus)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lehdr)

# LRT stations
# Note: Dr. Karner created these service areas in ArcGIS Pro using manually
# drawn station locations based on published maps. Actual station locations 
# are not known with certainty at this stage. Changes to the locations could 
# affect the results although impacts are likely to be small. 

lrt_sa <- 
  st_read("lrtServiceAreas.geojson") %>%
  st_transform("+init=epsg:3664")

### Pull demographic data ------------------------------------------------------
povvars <- c("C17002_002", # up to 50% of poverty
             "C17002_003") # 50%-99% poverty

hlstatusvars <- c("B03002_003", # white alone
                  "B03002_004", # black alone
                  "B03002_005", # american indian/alaska native
                  "B03002_006", # Asian
                  "B03002_007", # NHPI
                  "B03002_008", # other
                  "B03002_009", # Two or more
                  "B03002_012") # Hispanic or Latino

travis_bgs <- 
  block_groups(state = "TX", county = "Travis", year = 2021) %>%
  st_transform("+init=epsg:3664") %>%
  mutate(orig_area = units::drop_units(st_area(.)))

travis_acs <-
  get_acs("block group",
          c("B01001_001", # total population
          povvars,
          hlstatusvars),
          year = 2021,
          state = "TX",
          county = "Travis") %>%
  select(GEOID, variable, estimate)

jobs <-
  grab_lodes(state = "TX",
             year = 2019,
             lodes_type = "wac",
             job_type = "JT00",
             segment = "S000",
             agg_geo = "bg",
             state_part = "main",
             use_cache = TRUE) %>%
  pivot_longer(cols = c("C000", "CE01", "CE02", "CE03"),
               names_to = "variable") %>%
  select(GEOID = w_bg, variable, estimate = value)

travis_demogs <- left_join(travis_bgs, rbind(travis_acs, jobs))

### Identify alternatives and dissolve service areas to avoid double counting --

## Alternative 1: On-street: 38th to Oltorf to Yellow Jacket
# alt1a <- c("45th", "38th", "29th", "UT", "15th", "Republic Square", 
#            "Auditorium Shores", "Travis Heights", "Lakeshore", 
#            "Pleasant Valley", "Faro", "Montopolis", "Yellow Jacket", "Soco",
#            "Oltorf", "St. Edward's")

alt1a <- c("38th", "29th", "UT", "15th", "Republic Square",
           "Auditorium Shores", "Travis Heights", "Lakeshore",
           "Pleasant Valley", "Faro", "Montopolis", "Yellow Jacket", "Soco",
           "Oltorf")

# stopifnot(length(alt1a) == 16)

alt1a_geo <- 
  filter(lrt_sa, Name %in% alt1a) %>%
  st_union()

# alt1b <- c("45th", "38th", "29th", "UT", "15th", "Republic Square", 
#            "Congress", "Cesar Chavez", "Waterfront", "Travis Heights", 
#            "Lakeshore", "Pleasant Valley", "Faro", "Montopolis", 
#            "Yellow Jacket", "Soco", "Oltorf", "St. Edward's")

alt1b <- c("38th", "29th", "UT", "15th", "Republic Square", 
           "Congress", "Cesar Chavez", "Waterfront", "Travis Heights", 
           "Lakeshore", "Pleasant Valley", "Faro", "Montopolis", 
           "Yellow Jacket", "Soco", "Oltorf")

# stopifnot(length(alt1b) == 18)

alt1b_geo <- 
  filter(lrt_sa, Name %in% alt1b) %>%
  st_union()

## Alternative 2: On-street: North Lamar to Pleasant Valley
alt2a <- c("North Lamar Transit Center", "Crestview", "Koenig", "45th", "38th",
          "29th", "UT", "15th", "Republic Square", "Auditorium Shores", 
          "Travis Heights", "Lakeshore", "Pleasant Valley")

# stopifnot(length(alt2a) == 13)

alt2a_geo <- 
  filter(lrt_sa, Name %in% alt2a) %>%
  st_union()

alt2b <- c("North Lamar Transit Center", "Crestview", "Koenig", "45th", "38th",
          "29th", "UT", "15th", 
          "Congress", "Cesar Chavez", "Waterfront", 
          "Travis Heights", "Lakeshore", "Pleasant Valley")

# stopifnot(length(alt2b) == 14)

alt2b_geo <- 
  filter(lrt_sa, Name %in% alt2b) %>%
  st_union()

## Alternative 3: On-street: 29th to Airport
alt3 <- c("29th", "UT", "15th", 
          "Congress", "Cesar Chavez", "Waterfront", 
          "Travis Heights", "Lakeshore", "Pleasant Valley",
          "Faro", "Montopolis", "Yellow Jacket", "ABIA")

# stopifnot(length(alt3) == 13)

alt3_geo <- 
  filter(lrt_sa, Name %in% alt3) %>%
  st_union()

## Alternative 4: Partial elevated: 29th to Oltorf to Yellow Jacket
alt4 <- c("29th", "UT", "15th", 
          "Republic Square", "Auditorium Shores", "Soco", "Oltorf",
          "Travis Heights", "Lakeshore", "Pleasant Valley",
          "Faro", "Montopolis", "Yellow Jacket")

# stopifnot(length(alt4) == 13)

alt4_geo <- 
  filter(lrt_sa, Name %in% alt4) %>%
  st_union()

## Alternative 5: Partial underground: UT to Yellow Jacket
alt5 <- c("UT", "15th", "Republic Square", "Auditorium Shores",
          "Travis Heights", "Lakeshore", "Pleasant Valley",
          "Faro", "Montopolis", "Yellow Jacket")

# stopifnot(length(alt5) == 10)

alt5_geo <- 
  filter(lrt_sa, Name %in% alt5) %>%
  st_union()


alts <- 
  rbind(mutate(filter(lrt_sa, Name %in% alt1a), alt = "1a: 38th/Oltorf/Yellow Jacket 1"),
        mutate(filter(lrt_sa, Name %in% alt1b), alt = "1b: 38th/Oltorf/Yellow Jacket 2"),
        mutate(filter(lrt_sa, Name %in% alt2a), alt = "2a: N. Lamar/Pleasant Valley 1"),
        mutate(filter(lrt_sa, Name %in% alt2b), alt = "2b: N. Lamar/Pleasant Valley 2"),
        mutate(filter(lrt_sa, Name %in% alt3), alt = "3. 29th/ABIA"),
        mutate(filter(lrt_sa, Name %in% alt4), alt = "4. 29th/Oltorf/Yellow Jacket"),
        mutate(filter(lrt_sa, Name %in% alt5), alt = "5. UT/Yellow Jacket")) %>%
  group_by(alt) %>%
  summarize(geometry = st_union(geometry))

### Conduct areal interpolation and performance analysis -----------------------

station_performance <- 
  st_intersection(travis_demogs, lrt_sa) %>%
  mutate(new_area = units::drop_units(st_area(.)),
         estimate_share = new_area / orig_area * estimate,
         variable2 = 
           forcats::fct_collapse(
             variable,
             BIPOC = c("B03002_004", "B03002_006", "B03002_007", "B03002_008", 
                       "B03002_009", "B03002_012"),
             poverty = c("C17002_002", "C17002_003"),
             `total population` = "B01001_001",
             `total jobs` = "C000",
             `low-wage jobs` = "CE01")) %>%
  group_by(Name, variable2) %>%
  summarize(total = sum(estimate_share)) %>%
  mutate(variable2 = forcats::fct_reorder(variable2, total, .desc = TRUE),
         wrapping = ifelse(variable2 %in% c("total jobs", "low-wage jobs"), "jobs", "population"))

alt_performance_wide <- 
  st_intersection(travis_demogs, alts) %>%
  mutate(new_area = units::drop_units(st_area(.)),
         estimate_share = new_area / orig_area * estimate) %>%
  st_drop_geometry() %>%
  group_by(alt, variable) %>%
  summarize(total = sum(estimate_share)) %>%
  pivot_wider(names_from = "alt", values_from = "total")
  
alt_performance_long <- 
  st_intersection(travis_demogs, alts) %>%
  mutate(new_area = units::drop_units(st_area(.)),
         estimate_share = new_area / orig_area * estimate) %>%
  st_drop_geometry() %>%
  group_by(alt, variable) %>%
  summarize(total = sum(estimate_share))

alt_performance_final <- 
  mutate(alt_performance_long, 
         variable2 = 
           forcats::fct_collapse(
             variable,
              BIPOC = c("B03002_004", "B03002_006", "B03002_007", "B03002_008", 
                        "B03002_009", "B03002_012"),
              poverty = c("C17002_002", "C17002_003"),
              `total population` = "B01001_001",
              `total jobs` = "C000",
              `low-wage jobs` = "CE01")) %>%
  group_by(alt, variable2) %>%
  summarize(total = sum(total)) %>%
  mutate(variable2 = forcats::fct_reorder(variable2, total, .desc = TRUE),
         wrapping = ifelse(variable2 %in% c("total jobs", "low-wage jobs"), "jobs", "population"))

## Station area performance ----------------------------------------------------
station_performance$Name <-
  factor(station_performance$Name, levels = levels(forcats::fct_reorder(filter(station_performance, variable2 == "total jobs")$Name, 
                       filter(station_performance, variable2 == "total jobs")$total, 
                       .desc = TRUE)))

ggplot(filter(station_performance,
              variable2 %in% c("total population", "total jobs"))) + 
  geom_point(aes(y = forcats::fct_rev(Name), x = total)) +
  geom_segment(aes(y = forcats::fct_rev(Name), yend = forcats::fct_rev(Name), 
                   x = 0, xend = total)) +
  facet_wrap(~wrapping, scales = "free_x") + 
  ylab(NULL) + 
  xlab("count") + 
  theme_bw()
           


## Alternatives performance ----------------------------------------------------

ggplot(filter(alt_performance_final,
              variable2 %in% c("BIPOC", "poverty", "total population", "total jobs", "low-wage jobs"))) + 
  geom_bar(aes(x = variable2, y = total, col = NULL, fill = alt), 
           position = "dodge", stat = "identity") + 
  facet_wrap(~ forcats::fct_rev(wrapping), scales = "free_x") + 
  scale_fill_manual(values = c("#238b45", "#41ae76", "#d7301f", "#ef6548", "#7570b3", "#e6ab02", "#666666")) + 
  xlab(NULL) + 
  ylab("count") + 
  guides(fill = guide_legend(title = NULL)) + 
  labs(title = "Population and jobs within 1/2 mile of proposed light-rail stations",
       subtitle = "by scenario",
       caption = "Source: Alex Karner, UT-Austin Community & Regional Planning") +
  theme_bw()


ggplot() + 
  geom_sf(data = lrt_sa, fill = "red") + 
  ggthemes::theme_map()
