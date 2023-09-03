# Install and Load necessary libraries
install.packages(c('reshape2', 'ggpubr'))
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)

# Read the data
financial_df <- read.csv('C:\\Users\\SHUKLAR7\\OneDrive - London School of Economics\\Desktop\\financial_metrics.csv')
diversity_df <- read.csv('C:\\Users\\SHUKLAR7\\OneDrive - London School of Economics\\Desktop\\diversity_metrics.csv')


# Merge the dataframes
merged_df <- merge(financial_df, diversity_df, by = c('year', 'unit', 'cusip', 'company', 'sector'))

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


# Make sure to load dplyr
library(dplyr)

# Filter for 'Information Technology' sector
it_sector_df <- merged_df[merged_df$sector == 'Information Technology',]

# Group by year and calculate mean for relevant columns
it_sector_yearly <- it_sector_df %>% 
    group_by(year) %>% 
    summarise(across(c(financial_metrics, diversity_metrics), mean, na.rm = TRUE))

# Melt the data for ggplot
it_sector_yearly_melt <- reshape2::melt(it_sector_yearly, id.vars = 'year')

# Plot changes in workforce composition over time
workforce_plot <- ggplot2::ggplot(it_sector_yearly_melt[it_sector_yearly_melt$variable %in% diversity_metrics,], ggplot2::aes(x = year, y = value, color = variable)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = 'Changes in Workforce Composition Over Time in Information Technology Sector', x = 'Year', y = 'Value') +
    ggplot2::theme_minimal()

# Plot changes in financial performance over time
financial_plot <- ggplot2::ggplot(it_sector_yearly_melt[it_sector_yearly_melt$variable %in% financial_metrics,], ggplot2::aes(x = year, y = value, color = variable)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = 'Changes in Financial Performance Over Time in Information Technology Sector', x = 'Year', y = 'Value') +
    ggplot2::theme_minimal()

# Combine the plots
final_plot <- ggpubr::ggarrange(workforce_plot, financial_plot, 
                                ncol = 1, nrow = 2,
                                align = "hv", 
                                common.legend = TRUE, 
                                legend = "bottom")

# Show the final plot
final_plot
