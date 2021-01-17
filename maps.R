library(tidyverse)
library(sf)
library(here)
library(tidycensus)
library(classInt)
library(Hmisc)

# cdc life expectancy 
cdcdf <- read_csv("https://ftp.cdc.gov//pub//Health_Statistics//NCHS//Datasets//NVSS//USALEEP//CSV//NYC_A.csv")

df <- janitor::clean_names(cdcdf)

df %>%
  group_by(cnty2kx) %>%
  summarise(mean_le = mean(e_0))


nyc_tracts <- read_sf("C:\\Users\\grish\\Desktop\\SpatialStats\\NYC_tractsNYU\\nyu_2451_34505.shp")
plot(nyc_tracts['bcode'])


names(df)
names(nyc_tracts)


# census data
census_data <- load_variables("2018", "acs5", cache = T)

vars <- c(total = "B01001_001",
         poverty = "B06012_002",
         income = "B06011_001",
         no_internet = "B28011_008",
         total_household_intrenet_con = "B28011_001")

# get census data with specified variables 
df_census <- get_acs(state = "ny", 
             geography = "tract",
             variables = vars,
             geometry = F,
             survey = "acs5",
             output = "wide")

df_census <- janitor::clean_names(df_census)


full_df <- df %>%
  merge(nyc_tracts,
        by.x = "tract_id",
        by.y = "tractid",
        all.y = T)

full_df <- full_df %>%
  merge(df_census,
        by.x = "tract_id",
        by.y = "geoid",
        all.x = T)


full_df <- st_as_sf(full_df)
plot(full_df['e_0'])


# create new vars.
full_df <- full_df %>%
    mutate(
      pct_no_internet = (no_internet_e / total_household_intrenet_con_e) * 100
      )



# impute NA with mean..
summary(full_df$e_0)

full_df$e_0 <- impute(full_df$e_0, mean)


quantile_brks <- classIntervals(full_df$e_0, n = 5, style = "quantile")
full_df$life_exp_cut <- cut(full_df$e_0, breaks = quantile_brks$brks)

full_df$pct_no_internet <- impute(full_df$pct_no_internet, mean)

quantile_brks2 <- classIntervals(full_df$pct_no_internet, n = 10, style = "quantile")
full_df$pct_no_internet_cut = cut(full_df$pct_no_internet, breaks = quantile_brks2$brks)

#
int_access <- ggplot(full_df, aes(fill = pct_no_internet_cut)) +
  geom_sf() +
  scale_fill_viridis_d(
    name = "% With No Internet Access"
  ) +
  labs(
    title = "Percent of People With No Internet",
    subtitle = "Data Source: Census 2015-2018 Estimates "
  ) +
  theme(
    plot.title = element_text(color = "royalblue1", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_blank()
  )

int_access

ggsave("no_internet_access.png", int_access)


# average life expectncy map 

avg_life_exp_plot <- ggplot(full_df, aes(fill = life_exp_cut), size = 0.01) +
  geom_sf() +
  scale_fill_viridis_d(
    name = "Mean Life Expectancy"
  ) +
  labs(
    title = "Average Life Expectancy",
    subtitle = "Data Source: CDC.gov "
  ) +
  theme(
    plot.title = element_text(color = "royalblue1", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_blank()
  ) 

avg_life_exp_plot

ggsave("average_life_expectancyCDC.png", avg_life_exp_plot)







