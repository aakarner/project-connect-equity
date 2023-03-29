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
  st_read("lrtServiceAreas_clean.geojson") %>%
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
  block_groups(state = "TX", county = "Travis", year = 2019) %>%
  st_transform("+init=epsg:3664") %>%
  mutate(orig_area = units::drop_units(st_area(.)))

travis_acs <-
  get_acs("block group",
          c("B01001_001", # total population
          povvars,
          hlstatusvars),
          year = 2019,
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

alt1 <- c("38th", "29th", "UT", "15th", 
          "Travis Heights", "Lakeshore",
          "Pleasant Valley", "Faro", "Montopolis", "Yellow Jacket", 
          "Soco", "Oltorf")

dt1 <- c("Republic Square", "Auditorium Shores")
dt2 <- c("Congress", "Cesar Chavez", "Waterfront")

alt1a <- c("38th", "29th", "UT", "15th", 
           "Republic Square", "Auditorium Shores", 
           "Travis Heights", "Lakeshore",
           "Pleasant Valley", "Faro", "Montopolis", "Yellow Jacket", 
           "Soco", "Oltorf")

# stopifnot(length(alt1a) == 14)

# alt1b <- c("45th", "38th", "29th", "UT", "15th",  
#            "Congress", "Cesar Chavez", "Waterfront", "Travis Heights", 
#            "Lakeshore", "Pleasant Valley", "Faro", "Montopolis", 
#            "Yellow Jacket", "Soco", "Oltorf", "St. Edward's")

alt1b <- c("38th", "29th", "UT", "15th", 
           "Congress", "Cesar Chavez", "Waterfront", 
           "Travis Heights", "Lakeshore", 
           "Pleasant Valley", "Faro", "Montopolis", "Yellow Jacket", 
           "Soco", "Oltorf")

# stopifnot(length(alt1b) == 15)

## Alternative 2: On-street: North Lamar to Pleasant Valley
alt2a <- c("North Lamar Transit Center", "Crestview", "Koenig", "45th", "38th",
          "29th", "UT", "15th", 
          "Republic Square", "Auditorium Shores", 
          "Travis Heights", "Lakeshore", "Pleasant Valley")

# stopifnot(length(alt2a) == 13)

alt2b <- c("North Lamar Transit Center", "Crestview", "Koenig", "45th", "38th",
          "29th", "UT", "15th", 
          "Congress", "Cesar Chavez", "Waterfront", 
          "Travis Heights", "Lakeshore", "Pleasant Valley")

# stopifnot(length(alt2b) == 14)

## Alternative 3: On-street: 29th to Airport
alt3 <- c("29th", "UT", "15th", 
          "Congress", "Cesar Chavez", "Waterfront", 
          "Travis Heights", "Lakeshore", "Pleasant Valley",
          "Faro", "Montopolis", "Yellow Jacket", "ABIA")

# stopifnot(length(alt3) == 13)

## Alternative 4: Partial elevated: 29th to Oltorf to Yellow Jacket
alt4 <- c("29th", "UT", "15th", 
          "Republic Square", "Auditorium Shores", "Soco", "Oltorf",
          "Travis Heights", "Lakeshore", "Pleasant Valley",
          "Faro", "Montopolis", "Yellow Jacket")

# stopifnot(length(alt4) == 13)

## Alternative 5: Partial underground: UT to Yellow Jacket
alt5 <- c("UT", "15th", "Republic Square", "Auditorium Shores",
          "Travis Heights", "Lakeshore", "Pleasant Valley",
          "Faro", "Montopolis", "Yellow Jacket")

# stopifnot(length(alt5) == 10)

alts <- 
  rbind(mutate(filter(lrt_sa, Name %in% alt1), alt = "1: Base"),
        mutate(filter(lrt_sa, Name %in% alt1a), alt = "1a: 38th/Oltorf/Yellow Jacket 1"),
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
  st_drop_geometry() %>%
  group_by(Name, variable2) %>%
  summarize(total = sum(estimate_share)) %>%
  mutate(variable2 = forcats::fct_reorder(variable2, total, .desc = TRUE),
         wrapping = ifelse(variable2 %in% c("total jobs", "low-wage jobs"), "jobs", "population"))

# Add all of ABIA's jobs back manually
station_performance$total[station_performance$Name == "ABIA" & station_performance$variable2 == "total jobs"] <- 2570
station_performance$total[station_performance$Name == "ABIA" & station_performance$variable2 == "low-wage jobs"] <- 436
station_performance$total[station_performance$Name == "ABIA" & station_performance$variable2 == "CE02"] <- 816
station_performance$total[station_performance$Name == "ABIA" & station_performance$variable2 == "CE03"] <- 1318

# Spot check
# filter(station_performance, Name == "ABIA")
# filter(station_performance, Name == "Republic Square")

alt_performance_wide <- 
  st_intersection(travis_demogs, alts) %>%
  mutate(new_area = units::drop_units(st_area(.)),
         estimate_share = new_area / orig_area * estimate) %>%
  st_drop_geometry() %>%
  group_by(alt, variable) %>%
  summarize(total = sum(estimate_share)) %>%
  pivot_wider(names_from = "alt", values_from = "total")

# write.csv(alt_performance_wide, "output/alt_performance_wide.csv")

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
  factor(station_performance$Name, 
         levels = 
           levels(forcats::fct_reorder(
             filter(station_performance, variable2 == "total jobs")$Name, 
             filter(station_performance, variable2 == "total jobs")$total, 
             .desc = TRUE)))

p_jobs <- 
  ggplot(
    filter(station_performance, 
           variable2 %in% c("total jobs"))) + 
    geom_segment(aes(y = forcats::fct_rev(Name), yend = forcats::fct_rev(Name), 
                   x = 0, xend = total)) +
    geom_point(aes(y = forcats::fct_rev(Name), x = total), color = "orange", size = 3) +
    # facet_wrap(~wrapping, scales = "free_x") + 
    ylab(NULL) + 
    xlab("count") + 
    labs(title = "Jobs within 1/2 mile walk of proposed Project Connect light-rail stations",
     subtitle = "3/27/2023",
     caption = "Source: Alex Karner, UT-Austin Community & Regional Planning") +
    theme_bw()

ggsave(filename = "stationPerformance_jobs.png", plot = p_jobs,
        width = 2700, height = 1800, units = "px")



station_performance$Name <-
  factor(station_performance$Name, 
         levels = 
           levels(forcats::fct_reorder(
             filter(station_performance, variable2 == "total population")$Name, 
             filter(station_performance, variable2 == "total population")$total, 
             .desc = TRUE)))


p_pop <- 
  ggplot(
    filter(station_performance, 
           variable2 %in% c("total population"))) + 
    geom_segment(aes(y = forcats::fct_rev(Name), yend = forcats::fct_rev(Name), 
                   x = 0, xend = total)) +
    geom_point(aes(y = forcats::fct_rev(Name), x = total), color = "blue", size = 3) +
    # facet_wrap(~wrapping, scales = "free_x") + 
    ylab(NULL) + 
    xlab("count") + 
    labs(title = "People within 1/2 mile walk of proposed light-rail stations",
       subtitle = "3/27/2023",
       caption = "Source: Alex Karner, UT-Austin Community & Regional Planning") +
    theme_bw()

ggsave(filename = "stationPerformance_pop.png", plot = p_pop,
        width = 2700, height = 1800, units = "px")
           


## Alternatives performance ----------------------------------------------------

# Add ABIA's jobs back manually
# filter(station_performance, Name == "ABIA") # 77.6 jobs at base
# 92697.13 - 77.6 + 2570
# filter(alt_performance_final, alt == "3. 29th/ABIA")

alt_performance_final$total[alt_performance_final$variable2 == "total jobs" & alt_performance_final$alt == "3. 29th/ABIA"] <- 95189.53


ggplot(filter(alt_performance_final,
              variable2 %in% c("BIPOC", "poverty", "total population", "total jobs", "low-wage jobs"))) + 
  geom_bar(aes(x = variable2, y = total, col = NULL, fill = alt), 
           position = "dodge", stat = "identity") + 
  facet_wrap(~ wrapping, scales = "free_x") + 
  scale_fill_manual(values = c("#238b45", "#41ae76", "#d7301f", "#ef6548", "#7570b3", "#e6ab02", "#666666")) + 
  xlab(NULL) + 
  ylab("count") + 
  guides(fill = guide_legend(title = NULL)) + 
  labs(title = "Population and jobs within 1/2 mile of proposed light-rail stations by scenario",
       subtitle = "REVISED 3/27/2023",
       caption = "Source: Alex Karner, UT-Austin Community & Regional Planning") +
  theme_bw() + 
  theme(plot.subtitle = element_text(color = "red"))

ggsave("scenarioAnalysis.png", width = 2700, height = 1800, units = "px")
