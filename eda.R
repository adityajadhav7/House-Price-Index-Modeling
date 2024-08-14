# Install and load required packages
packages <- c("tidyverse", "vroom", "lubridate", "viridis", "quantreg", "scales", "tidyquant", "fda", "broom", "gridExtra", "forecast")
install.packages(packages[!packages %in% installed.packages()[,"Package"]])
lapply(packages, library, character.only = TRUE)

# Set figure output path
fig_path <- 'results/'
dir.create(fig_path, showWarnings = FALSE)

# Define recession periods
recession_periods <- tibble(
  peak = as.Date(c('2001-03-01', '2007-12-01', '2020-02-01')),
  trough = as.Date(c('2001-11-01', '2009-06-01', '2020-04-01'))
)

# Load and process Zillow Home Value Index (ZHVI) data
zhvi_data <- vroom("https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv", 
                   show_col_types = FALSE)

zhvi_processed <- zhvi_data %>%                                                                            
  pivot_longer(!c(RegionID, SizeRank, RegionName, RegionType, StateName),
               names_to = 'date',
               values_to = 'zhvi') %>% 
  mutate(date = as_date(str_c(str_sub(date, 1, 4), str_sub(date, 6, 7), '01', sep = "-"))) %>% 
  group_by(RegionName) %>% 
  mutate(yoy_growth = zhvi / dplyr::lag(zhvi, 12) - 1) %>% 
  ungroup()

# Fetch and process Personal Consumption Expenditures (PCE) data
pce_data <- tq_get("PCEPI", get = "economic.data", from = '2000-01-01') %>% 
  mutate(pce_yoy_growth = price / dplyr::lag(price, 12) - 1)

# Combine ZHVI and PCE data
zhvi_pce_combined <- zhvi_processed %>% 
  left_join(pce_data, by = 'date') %>% 
  mutate(real_zhvi_growth = yoy_growth - pce_yoy_growth,
         real_zhvi_growth_lag12 = dplyr::lag(real_zhvi_growth, 12)) %>% 
  drop_na()

# Heatmap of ZHVI growth across all regions
# Select top 20 regions by population
top_20_regions <- zhvi_processed %>%
  filter(date == max(date)) %>%
  arrange(SizeRank) %>%
  head(20) %>%
  pull(RegionName)

# Prepare data for heatmap
heatmap_data <- zhvi_processed %>%
  filter(RegionName %in% top_20_regions) %>%
  filter(!is.na(yoy_growth)) %>%
  mutate(year = year(date)) %>%
  group_by(RegionName, year) %>%
  summarise(avg_growth = mean(yoy_growth, na.rm = TRUE)) %>%
  ungroup()

# heatmap
heatmap_plot <- ggplot(heatmap_data, aes(x = year, y = reorder(RegionName, -avg_growth), fill = avg_growth)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", labels = scales::percent, name = "Avg YoY Growth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.y = element_blank(),
        legend.position = "right") +
  scale_x_continuous(breaks = unique(heatmap_data$year)) +
  labs(x = "Year", 
       title = "Heatmap of ZHVI Year-over-Year Growth",
       subtitle = "Average annual growth for top 20 regions by population",
       caption = "Source: Zillow | Created by: @adityajadhav")

print(heatmap_plot)
ggsave(file.path(fig_path, 'zhvi_heatmap_improved.png'), plot = heatmap_plot, bg = 'white', width = 15, height = 10)

# Box plot of ZHVI growth distribution over time
boxplot_data <- zhvi_processed %>%
  filter(!is.na(yoy_growth)) %>%
  mutate(year = year(date))

boxplot <- ggplot(boxplot_data, aes(x = factor(year), y = yoy_growth)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  coord_cartesian(ylim = quantile(boxplot_data$yoy_growth, c(0.01, 0.99))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Year", y = "YoY Growth", 
       title = "Distribution of ZHVI Year-over-Year Growth",
       subtitle = "Boxplot showing growth distribution across regions",
       caption = "Source: Zillow | Created by: @adityajadhav")

print(boxplot)
ggsave(file.path(fig_path, 'zhvi_boxplot.png'), plot = boxplot, bg = 'white', width = 12, height = 6)

# Select top 5 regions by population 
top_5_regions <- zhvi_processed %>%
  filter(date == max(date), RegionName != "United States") %>%
  arrange(SizeRank) %>%
  head(5) %>%
  pull(RegionName)

# Plot ZHVI Index for top 5 regions
zhvi_plot <- zhvi_processed %>%
  filter(RegionName %in% top_5_regions) %>%
  ggplot(aes(date, yoy_growth, color = RegionName)) +
  geom_line() +
  scale_color_viridis_d() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Year-over-Year Growth", 
       title = "Zillow Home Value Index Year-over-Year Growth", 
       subtitle = "Top 5 regions by population",
       caption = "Source: Zillow | Created by: @adityajadhav")

print(zhvi_plot)
ggsave(file.path(fig_path, 'zhvi_growth_top5.png'), plot = zhvi_plot, bg = 'white', width = 12, height = 6)

# Plot Real and Nominal ZHVI Growth for top 5 regions
growth_comparison_plot <- zhvi_pce_combined %>%
  filter(RegionName %in% top_5_regions) %>%
  ggplot(aes(date, color = RegionName)) +
  geom_line(aes(y = real_zhvi_growth)) +
  geom_line(aes(y = yoy_growth), linetype = 'dashed') +
  geom_line(aes(y = pce_yoy_growth), color = "black", size = 1) + 
  scale_color_viridis_d() +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.title = element_blank(), plot.caption = element_text(hjust = 0)) +
  labs(x = "", y = "Growth Rate", 
       title = "Zillow Home Value Index: Real vs Nominal Growth",
       subtitle = "Top 5 regions by population. Solid = Real, Dashed = Nominal, Black = PCEPI",
       caption = "Source: Zillow | Note: PCEPI = Personal Consumption Expenditures Chain Type Price Index | Created by: @adityajadhav") +
  geom_rect(data = recession_periods, 
            inherit.aes = FALSE, 
            aes(xmin = peak, xmax = trough, ymin = -Inf, ymax = +Inf), 
            fill = 'gray', alpha = 0.3)

print(growth_comparison_plot)
ggsave(file.path(fig_path, 'real_nominal_zhvi_growth_top5.png'), plot = growth_comparison_plot, bg = 'white', width = 12, height = 6)
