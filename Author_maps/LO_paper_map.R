
setwd("C:\\Users\\qzhan\\OneDrive - NIOZ\\Documents\\GitHub\\LO_cyano\\Author_maps")

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)

list.files()

df <- read_csv("Step2_Papers_by_decade.csv", col_names = TRUE)
df <- df[-1,]

df_clean <- df[,c("Article number", "Country/Counties", "Publication Year")]

df_clean <- df_clean %>%
  # If multiple codes appear in one row (e.g., "NL, DE"), we split them:
  filter(!str_detect(`Country/Counties`, "NA")) %>%
  filter(!str_detect(`Country/Counties`, "Na")) %>%
  separate_rows(`Country/Counties`, sep = ",") %>%
  mutate(`Country/Counties` = str_trim(`Country/Counties`)) %>%
  # convert year to numeric if needed
  mutate(year = as.numeric(`Publication Year`)) %>%
  # compute decade: 1960s, 1970s, ...
  mutate(decade = floor(`Publication Year`/ 10) * 10)

unique(df_clean$`Country/Counties`)


country_dict <- c(
  "Australia" = "AU",
  "Austria" = "AT",
  "Canada" = "CA",
  "China" = "CN",
  "Chech Republic" = "CZ",
  "Czech Republic" = "CZ",
  "Finland" = "FI",
  "France" = "FR",
  "Germany" = "DE",
  "Greece" = "GR",
  "Iceland" = "IS",
  "Ireland" = "IE",
  "IRELAND" = "IE",
  "Italy" = "IT",
  "Japan" = "JP",
  "South Korea" = "KR",
  "SOUTH KOREA" = "KR",
  "Macedonia" = "MK",
  "Netherland" = "NL",
  "The Netherlands" = "NL",
  "New Zealand" = "NZ",
  "Romania" = "RO",
  "Scotland" = "GB",
  "UK" = "GB",
  "USA" = "US",
  "US" = "US",
  "California" = "US",       # state → US
  "Kanya" = "KE",             # typo for Kenya
  "Chad" = "TD",
  "Sweden" = "SE",
  "Sweden (Baltic Sea)" = "SE",
  "Baltic Ocean/Sweden" = "SE",
  "Switzerland" = "CH",
  "Greenland" = "GL",
  "Norway" = "NO",
  "Estonia" = "EE",
  "Europe (Estonia" = "EE",
  "Denmark" = "DK",
  "Spain" = "ES",
  "SPAIN" = "ES",
  "Belgium" = "BE"
)

not_countries <- c(
  "Antarctica", "Baltic Sea", "Central Baltic Sea",
  "Europe", "Global", "Northern Baltic Sea", "Siberia"
)

df_clean2 <- df_clean %>%
  filter(!`Country/Counties` %in% not_countries) %>%
  mutate(
    Country = country_dict[`Country/Counties`],
    Country = ifelse(is.na(Country) & str_detect(`Country/Counties`, "^[A-Z]{2}$"),
                           `Country/Counties`, Country)
  ) %>%
  filter(!is.na(Country))

df_sum <- df_clean2 %>%
  group_by(Country) %>%
  summarize(article_count = n(), .groups = "drop") %>%
  arrange(desc(article_count)) %>%
  mutate(percentage = article_count / sum(article_count) * 100)


world <- ne_countries(scale = "medium", returnclass = "sf")

world_joined <- world %>%
  left_join(df_sum, by = c("iso_a2" = "Country"))

# 1. by article count
ggplot() +
  # base map (light gray, minimal borders)
  geom_sf(data = world, fill = "gray95", color = "gray80", size = 0.1) +
  
  # overlay countries with publications
  geom_sf(data = world_joined %>% filter(!is.na(article_count)),
          aes(fill = article_count), color = "gray50", size = 0.2) +
  
  # color scale
  scale_fill_viridis_c(option = "plasma", na.value = "gray95") +
  
  theme_minimal() +
  labs(
    title = "Global Distribution of Articles by Country",
    fill = "Article Count"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = NA)   # remove grid
  )

# 2. by percentage:
ggplot() +
  # base map (light gray, minimal borders)
  geom_sf(data = world, fill = "gray95", color = "gray80", size = 0.1) +
  
  # overlay countries with publications
  geom_sf(data = world_joined %>% filter(!is.na(article_count)),
          aes(fill = percentage), color = "gray50", size = 0.2) +
  
  # color scale
  scale_fill_viridis_c(option = "plasma", na.value = "gray95") +
  
  theme_minimal() +
  labs(
    title = "Global Distribution of Articles by Country",
    fill = "Percentage (%)"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = NA)   # remove grid
  )


library(tidyverse)

continent_dict <- c(
  "AU" = "Oceania",
  "NZ" = "Oceania",
  "US" = "North America",
  "CA" = "North America",
  "MX" = "North America",
  "FR" = "Europe",
  "DE" = "Europe",
  "NL" = "Europe",
  "CH" = "Europe",
  "DK" = "Europe",
  "SE" = "Europe",
  "NO" = "Europe",
  "IT" = "Europe",
  "IE" = "Europe",
  "ES" = "Europe",
  "AT" = "Europe",
  "BE" = "Europe",
  "IS" = "Europe",
  "GR" = "Europe",
  "CZ" = "Europe",
  "MK" = "Europe",
  "KE" = "Africa",
  "TD" = "Africa",
  "CN" = "Asia",
  "JP" = "Asia",
  "KR" = "Asia",
  "IL" = "Asia",
  "SA" = "Africa",
  "GL" = "North America",      # Greenland often treated as NA
  "UK" = "Europe"
)


df_continent <- df_clean %>%
  # Map country names/ISO2 to ISO2 code (using your previous dictionary)
  mutate(Country_ISO2 = country_dict[`Country/Counties`]) %>%
  
  # Assign continent
  mutate(Continent = ifelse(!is.na(Country_ISO2),
                            continent_dict[Country_ISO2],
                            # Handle special regions
                            case_when(
                              `Country/Counties` %in% c("Global") ~ "Global",
                              `Country/Counties` %in% c("Antarctica") ~ "Antarctica",
                              `Country/Counties` %in% c("Baltic Sea", "Central Baltic Sea",             "Europe", "Northern Baltic Sea", "Siberia") ~ "Europe",
                              TRUE ~ NA_character_
                            ))) %>%
  filter(!is.na(Continent))


df_cont_sum <- df_continent %>%
  group_by(Continent) %>%
  summarize(article_count = n(), .groups = "drop") %>%
  mutate(percentage = article_count / sum(article_count) * 100) %>%
  arrange(desc(article_count))

df_cont_sum

df_decade_sum <- df_continent %>%
  group_by(Continent, decade) %>%
  summarize(article_count = n(), .groups = "drop") %>%
  
  # compute percentage within each decade
  group_by(decade) %>%
  mutate(percentage = article_count / sum(article_count) * 100) %>%
  ungroup()


ggplot(df_cont_sum, aes(x = reorder(Continent, -percentage), y = percentage, fill = Continent)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percentage,1), "%")), vjust = -0.5) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Distribution of Articles by Continent", y = "Percentage (%)", x = "")

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Standardize continent names in world map
world <- world %>%
  mutate(continent_std = case_when(
    continent == "Europe" ~ "Europe",
    continent == "Asia" ~ "Asia",
    continent == "Africa" ~ "Africa",
    continent == "Oceania" ~ "Oceania",
    continent == "North America" ~ "North America",
    continent == "South America" ~ "South America",
    TRUE ~ continent
  ))

# Join percentages to world map
world_joined <- world %>%
  left_join(df_cont_sum, by = c("continent_std" = "Continent"))

ggplot() +
  # base map (light gray)
  geom_sf(data = world, fill = "gray95", color = "gray80", size = 0.1) +
  
  # overlay continents with articles
  geom_sf(data = world_joined %>% filter(!is.na(percentage)),
          aes(fill = percentage), color = "gray50", size = 0.2) +
  
  # color scale
  scale_fill_viridis_c(option = "plasma", na.value = "gray95") +
  
  theme_minimal() +
  labs(
    title = "Global Article Distribution by Continent",
    fill = "Percentage (%)"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = NA)
  )

#

# Aggregate continent polygons
continent_polygons <- world %>%
  group_by(continent) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

# Compute centroids
continent_centroids <- continent_polygons %>%
  st_centroid() %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()  # remove geometry for ggplot

# df_cont_sum: columns Continent, article_count
continent_centroids <- continent_centroids %>%
  rename(Continent = continent) %>%
  left_join(df_cont_sum, by = "Continent")   # missing continents get NA


ggplot() +
  # continents colored
  geom_sf(data = world, aes(fill = continent), color = "gray80", size = 0.1) +
  
  # overlay bubbles only for continents with articles
  geom_point(data = continent_centroids %>% filter(!is.na(article_count)),
             aes(x = lon, y = lat, size = article_count),
             color = "red", alpha = 0.6) +
  
  scale_size_continuous(range = c(3, 15)) +
  scale_fill_brewer(palette = "Set3") +
  
  theme_minimal() +
  labs(
    title = "Global Article Distribution by Continent",
    fill = "Continent",
    size = "Article Count"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = NA),
    legend.position = "right"
  )


# put trend and article account by continent:
install.packages("ggforce")
library(ggforce)

# df_bubbles: one row per continent
df_bubbles <- continent_totals %>%
  left_join(continent_centroids, by = "Continent") %>%
  left_join(df_decade_sum %>% group_by(Continent) %>% nest(), by = "Continent") %>%
  rename(trend_data = data)

# normalize sizes for plotting
max_articles <- max(df_bubbles$total_articles)
df_bubbles <- df_bubbles %>%
  mutate(circle_radius = 3 + 12 * total_articles / max_articles,   # for geom_point size
         vp_width = 0.05 + 0.1 * total_articles / max_articles,
         vp_height = 0.05 + 0.1 * total_articles / max_articles)

library(ggpp)

df_bubbles <- df_bubbles %>%
  rowwise() %>%
  mutate(plot = list(
    ggplot(trend_data, aes(x = decade, y = percentage)) +
      geom_line(color = "black", size = 0.8) +
      geom_point(color = "black", size = 1) +
      
      # x-axis: decades as labels
      scale_x_continuous(
        limits = c(1960, 2029),
        breaks = seq(1960, 2020, 10),
        labels = c("60s","70s","80s","90s","2000s","2010s","2020s")
      ) +
      
      # y-axis: only 0, 50, 100
      scale_y_continuous(
        limits = c(0, 100),
        breaks = c(0, 50, 100),
        labels = c("0","50","100")
      ) +
      
      # remove axis titles, tweak text & margins
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 8) +
      theme(
        plot.background = element_rect(fill = "darkgrey", color = NA),  # bubble background
        plot.margin = grid::unit(c(1,1,1,1), "pt"),  # small margins
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6),
        panel.grid = element_blank()
      )
  )) %>%
  ungroup()


world <- ne_countries(scale = "medium", returnclass = "sf")

p <- ggplot() +
  geom_sf(data = world, aes(fill = continent), color = "gray80", size = 0.1) +
  
  # circular bubbles
  geom_point(data = df_bubbles,
             aes(x = lon, y = lat, size = total_articles),
             color = "black", alpha = 0.6) +
  
  scale_size_continuous(range = c(3, 20)) +
  
  # overlay mini-line plots inside circles
  ggpp::geom_plot(data = df_bubbles,
                  aes(x = lon, y = lat, label = plot),
                  vp.width = 0.1,
                  vp.height = 0.1) +
  
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  labs(
    title = "Global Article Distribution by Continent",
    fill = "Continent",
    size = "Article Count"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = NA),
    legend.position = "right"
  )

# Save a high-resolution PNG of the map with inset bubble plots
ggsave(
  filename = "world_map_bubbles.png",
  plot = p,                 # <-- your final ggplot object
  width = 16,               # in inches (wide enough for clarity)
  height = 9,               # adjust to your preference
  dpi = 400,                # high resolution for manuscripts
  bg = "white"              # ensures no transparency issues
)
