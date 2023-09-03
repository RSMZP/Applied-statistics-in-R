# Load necessary libraries
install.packages('reshape2')
library(dplyr)
library(ggplot2)
library(reshape2)

# Read the data
financial_df <- read.csv('C:\\Users\\SHUKLAR7\\OneDrive - London School of Economics\\Desktop\\financial_metrics.csv')
diversity_df <- read.csv('C:\\Users\\SHUKLAR7\\OneDrive - London School of Economics\\Desktop\\diversity_metrics.csv')

# Merge the dataframes
merged_df <- merge(financial_df, diversity_df, by = c('year', 'unit', 'cusip', 'company', 'sector'))

# Debugging: Check the structure of merged_df to see data types
str(merged_df)

# Replace "NULL" with NA for selected columns
merged_df$Market.Cap[merged_df$Market.Cap == "NULL"] <- NA
merged_df$Long.Term.Growth.Mean[merged_df$Long.Term.Growth.Mean == "NULL"] <- NA
merged_df$EPS...Mean[merged_df$EPS...Mean == "NULL"] <- NA

# Convert selected columns to numeric
merged_df$Market.Cap <- as.numeric(merged_df$Market.Cap)
merged_df$Long.Term.Growth.Mean <- as.numeric(merged_df$Long.Term.Growth.Mean)
merged_df$EPS...Mean <- as.numeric(merged_df$EPS...Mean)

# Define the column names for financial and diversity metrics
financial_metrics <- c('Market.Cap', 'Long.Term.Growth.Mean', 'EPS...Mean')
diversity_metrics <- c('Total.Employees', 'ASIANF10', 'ASIANM10', 'ASIANT10')

# Calculate correlation
correlation_matrix <- cor(merged_df[, c(financial_metrics, diversity_metrics)], use = "complete.obs")

# Print correlation matrix
print(correlation_matrix)


# Filter for 'Information Technology' sector
it_sector_df <- merged_df[merged_df$sector == 'Information Technology',]

# Group by year and calculate mean
it_sector_yearly <- it_sector_df %>% group_by(year) %>% summarise_all(mean, na.rm = TRUE)

# Melt the data for ggplot
it_sector_yearly_melt <- melt(it_sector_yearly, id.vars = 'year')

# Plot changes in workforce composition over time
ggplot(it_sector_yearly_melt[it_sector_yearly_melt$variable %in% diversity_metrics,], aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free_y') +
  theme_minimal() +
  labs(title = 'Changes in Workforce Composition Over Time in Information Technology Sector', x = 'Year', y = 'Value')

# Plot changes in financial performance over time
ggplot(it_sector_yearly_melt[it_sector_yearly_melt$variable %in% financial_metrics,], aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free_y') +
  theme_minimal() +
  labs(title = 'Changes in Financial Performance Over Time in Information Technology Sector', x = 'Year', y = 'Value')


install.packages("ggpubr")


# Plot changes in workforce composition over time
workforce_plot <- ggplot(it_sector_yearly_melt[it_sector_yearly_melt$variable %in% diversity_metrics,], aes(x = year, y = value, color = variable)) +
    geom_line() +
    labs(title = 'Changes in Workforce Composition Over Time in Information Technology Sector', x = 'Year', y = 'Value') +
    theme_minimal()

# Plot changes in financial performance over time
financial_plot <- ggplot(it_sector_yearly_melt[it_sector_yearly_melt$variable %in% financial_metrics,], aes(x = year, y = value, color = variable)) +
    geom_line() +
    labs(title = 'Changes in Financial Performance Over Time in Information Technology Sector', x = 'Year', y = 'Value') +
    theme_minimal()

# Combine the plots
final_plot <- ggarrange(workforce_plot, financial_plot, 
                        ncol = 1, nrow = 2,
                        align = "hv", 
                        common.legend = TRUE, 
                        legend = "bottom")

# Show the final plot
final_plot


library(ggplot2)
library(dplyr)
library(ggpubr)  # for ggplot2 extensions

# Assuming it_sector_yearly is your data frame
# Assuming 'Financial_Performance' and 'Diversity_Metric' are your metrics

# Filter for a specific sector, e.g., 'Information Technology'
it_sector_yearly <- it_sector_yearly[it_sector_yearly$sector == 'Information Technology',]

# Create the base plot
base_plot <- ggplot(it_sector_yearly, aes(x = year)) +
    geom_line(aes(y = Financial_Performance, color = "Financial Performance")) +
    labs(title = "Significance of Diversity in Financial Performance",
         x = "Year",
         y = "Financial Performance") +
    theme_minimal()

# Create the plot for the first diversity metric (e.g., BIPOC-White)
diversity_plot1 <- ggplot(it_sector_yearly, aes(x = year)) +
    geom_line(aes(y = Diversity_Metric1, color = "BIPOC-White")) +
    labs(y = "Diversity Metric (BIPOC-White)") +
    theme_minimal()

# Create the plot for the second diversity metric (e.g., Male-Female)
diversity_plot2 <- ggplot(it_sector_yearly, aes(x = year)) +
    geom_line(aes(y = Diversity_Metric2, color = "Male-Female")) +
    labs(y = "Diversity Metric (Male-Female)") +
    theme_minimal()

# Combine the plots
final_plot <- ggarrange(base_plot, diversity_plot1, diversity_plot2,
                        ncol = 2, nrow = 2,
                        align = "hv", 
                        widths = c(2, 1), 
                        heights = c(1, 1))

# Show the final plot
final_plot
