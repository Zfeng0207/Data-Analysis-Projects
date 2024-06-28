# load necessary libraries
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggpubr)

# set working directory and read the datasets
setwd("C:/Users/zfeng/Downloads")

#---------------------------------------------------------------------------------------
#Data preprocessing 
#---------------------------------------------------------------------------------------

amazon_sale_report <- read_csv("amazon_sales_report.csv", col_types = cols(.default = "c"))

# check the unique values and their counts in the 'ship-country' column
country_counts <- amazon_sale_report %>%
  group_by(`ship-country`) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# print the counts of each country
print(country_counts)

# calculate the total number of orders
total_orders <- nrow(amazon_sale_report)
# get the count of orders shipped to India
india_orders <- country_counts %>% filter(`ship-country` == "IN") %>% pull(count)
# calculate the percentage of orders shipped to India
percentage_india <- (india_orders / total_orders) * 100

# print the percentage of orders shipped to India
print(paste("Percentage of orders shipped to India:", round(percentage_india, 2), "%"))

# Select relevant columns and clean the data (Remove NA values in Amount and Date)
amazon_sale_report <- amazon_sale_report %>%
  select(Style, SKU, Size, Qty,"Order ID", Date, Amount) 
amazon_sale_report <- na.omit(amazon_sale_report)

# Ensure Date column is in the correct date format (m-d-y)
amazon_sale_report$Date <- as.Date(amazon_sale_report$Date, format="%m-%d-%y")



sales_data <- read.csv("sale_report.csv",header = TRUE)
# Total 9271 observations, 7 variables
# 5 categorical variables, 2 integer variables
str(sales_data)
summary(sales_data)
# Replace empty strings with NA
sales_data[sales_data == ""] <- NA 
colSums(is.na(sales_data)) 


#After removing the missing values is 9188 
#Remove missing values using omit function
cleaned_sales_data <- na.omit(sales_data) 
nrow(cleaned_sales_data)
ncol(cleaned_sales_data)

colSums(is.na(cleaned_sales_data))
summary(cleaned_sales_data)

march2021 <- read.csv("march.csv", header = TRUE)
cleaned_march <- na.omit(march2021)
head(march2021)

#Size of dataset
glimpse(march2021)
#The dataset contains 1330 rows with 18 variables

#Perform data cleaning 
# Check for missing values
any(is.na(march2021))

#No varibles contains missing values.
#But, if you go through the dataset, you will see missing values are being replaced by String "#VALUE!" and "Nill"
#Define a custom function to replace multiple values with NA
replace_with_na <- function(x) {
  na_values <- c("", "#VALUE!", "Nill")
  x[x %in% na_values] <- NA
  return(x)
}

#Apply the custom function across all character columns
march2021_replaced <- march2021 %>%
  mutate(across(where(is.character), replace_with_na))
#Check the sum of missing values (Sum of NAs)
sum(is.na(march2021_replaced))

#Remove the missing values/incomplete data
march2021_clean_data <- na.omit(march2021_replaced)
nrow(march2021_clean_data)
ncol(march2021_clean_data)

international_sales <- read_csv("international_sale.csv", col_types = cols(.default = "c"))

international_sales <- international_sales[1:19675, ]

# display the dimensions of the international_sales dataset
dim(international_sales)

# remove rows with missing values in international_sales
international_sales_cleaned <- na.omit(international_sales)

# display the number of rows in cleaned international_sales
nrow(international_sales_cleaned)

#---------------------------------------------------------------------------------------
#Problem Statement 1
#---------------------------------------------------------------------------------------

# Aggregate the total amount by date
date_summary <- amazon_sale_report %>%
  group_by(Date) %>%
  summarise(total_amount = sum(as.numeric(Amount), na.rm = TRUE)) %>%
  arrange(Date)

# Plot the time series of total amounts with enhancements
plot_time_series <- ggplot(date_summary, aes(x = Date, y = total_amount)) +
  geom_line(color = "steelblue", size = 1) +  # Original line plot
  geom_point(color = "darkblue", size = 1) +  # Add points for each data point
  labs(
    title = "Total Sales Amount Over Time",
    subtitle = "Analyzing temporal trends in e-commerce sales",
    x = "Date",
    y = "Total Sales Amount",
    caption = "Data source: Amazon Sales Report"
  ) +
  theme_minimal(base_size = 15) +  # Use a minimal theme with a base size for better readability
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),  # Center-align and bold the title
    plot.subtitle = element_text(hjust = 0.5),  # Center-align the subtitle
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
  ) 

# Print the time series
print(plot_time_series)

#---------------------------------------------------------------------------------------
#Problem Statement 2
#---------------------------------------------------------------------------------------

summarized_colour_data <- cleaned_sales_data %>%
  group_by(Color) %>%
  summarize(Total_Sales = n(), .groups = 'drop')


top_10_combinations <- summarized_colour_data %>%
  arrange(desc(Total_Sales)) %>%
  head(10)
top_10_combinations

top_10_colour <- top_10_combinations %>% mutate(percentage = 
                                                  (top_10_combinations$Total_Sales
                                                   /sum(top_10_combinations$Total_Sales)) * 100 )

print(top_10_colour)

# Create a pie chart
pie_chart <- ggplot(top_10_colour, aes(x = "", y = Total_Sales, fill = Color)) +
  geom_bar(stat = "identity", width = 1, alpha = 3/5, color = "white") + 
  coord_polar("y", start = 0) +
  labs(title = "Sales Distribution of Top 10 Colours", fill = "Colour") +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3.5)

# Print the pie chart
print(pie_chart)
#---------------------------------------------------------------------------------------
#Find the sales distribution of categories  (Bar chart visualization)

sum_cat_data <- cleaned_sales_data %>%
  group_by(Category) %>%
  summarize(Total_Sales = n(), .groups = 'drop')
sum_cat_data

# Create a bar chart
bar_chart <- ggplot(sum_cat_data, aes(x = reorder(Category, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "royalblue") +
  labs(title = "Sales distribution of Categories", x = "Category", y = "Total Sales") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    plot.title = element_text(hjust = 0.5),  # Center the title
    panel.border = element_rect(color = "black", fill = NA)  # Add a square box around the plot area
  )

# Print the bar chart
print(bar_chart)


#---------------------------------------------------------------------------------------
#Problem Statement 3
#---------------------------------------------------------------------------------------


#Convert TP.1 and TP.2 into integers
march2021_clean_data$TP.1 <- as.numeric(as.character(march2021_clean_data$TP.1))
march2021_clean_data$TP.2 <- as.numeric(as.character(march2021_clean_data$TP.2))
str(march2021_clean_data)

#The total trading price 1 of each category
ttl_tprice_1 <- march2021_clean_data %>% 
  group_by(Category) %>%
  summarise(ttl_tp_1 = sum(TP.1)) %>% 
  arrange(desc(ttl_tp_1))
ttl_tprice_1

#The total trading price 2 of each category
ttl_tprice_2 <- march2021_clean_data %>% 
  group_by(Category) %>%
  summarise(ttl_tp_2 = sum(TP.2)) %>% 
  arrange(desc(ttl_tp_2))
ttl_tprice_2

#The avg trading price 1 of each category
avg_tprice_1 <- march2021_clean_data %>% 
  group_by(Category) %>%
  summarise(avg_tp_1 = mean(TP.1)) %>% 
  arrange(desc(avg_tp_1))
avg_tprice_1

#The avg trading price 2 of each category
avg_tprice_2 <- march2021_clean_data %>% 
  group_by(Category) %>%
  summarise(avg_tp_2 = mean(TP.2)) %>% 
  arrange(desc(avg_tp_2))
avg_tprice_2

#The percentage drop between both trading prices in each category
percentage_drop <- avg_tprice_1 %>% 
  inner_join(avg_tprice_2, by = "Category") %>% 
  mutate(percentage_drop = ((avg_tp_1 - avg_tp_2) / avg_tp_1) * 100) %>% 
  arrange(desc(percentage_drop))
percentage_drop

#Summary stat
summary(march2021_clean_data)

#Basic Boxplot for TP.1
bp_tp1 <- ggplot(march2021_clean_data, aes(x = Category, y = TP.1, fill = Category))+
  geom_boxplot(outlier.shape = NA, alpha=0.7) +
  geom_jitter(size = 0.75)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") + 
  labs(title = "Distribution of Initial Trading Prices (TP.1) across Categories", 
       x = "Category",
       y = "TP.1") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12)) +
  scale_y_continuous(breaks = seq(0, 1200, by = 200))
bp_tp1

#Basic Boxplot for TP.2
bp_tp2 <- ggplot(march2021_clean_data, aes(x = Category, y = TP.2, fill = Category))+
  geom_boxplot(outlier.shape = NA, alpha=0.7) +
  geom_jitter(size = 0.75)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="red", fill="red") + 
  labs(title = "Distribution of Final Trading Prices (TP.2) across Categories", 
       x = "Category",
       y = "TP.2") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12)) +
  scale_y_continuous(breaks = seq(0, 1200, by = 200))
bp_tp2

#Arrange on one page 
ggarrange(bp_tp1, bp_tp2,
          ncol = 2, nrow = 1)

#---------------------------------------------------------------------------------------
#Problem Statement 4
#---------------------------------------------------------------------------------------




# trim the international_sales dataset to the first 19675 rows
international_sales <- international_sales[1:19675, ]

# display the dimensions of the international_sales dataset
dim(international_sales)

# select specific columns from international_sales
international_sales_cleaned <- international_sales_cleaned %>%
  select(Style, SKU, Size, PCS, `GROSS AMT`)

# convert necessary columns to numeric and calculate gross revenue
international_sales_cleaned <- international_sales_cleaned %>%
  mutate(PCS = as.numeric(PCS),
         `GROSS AMT` = as.numeric(`GROSS AMT`)) %>%
  mutate(gross_revenue = PCS * `GROSS AMT`)

# display summary statistics of the cleaned international_sales dataset
summary(international_sales_cleaned)


# calculate revenue for amazon sales
amazon_sale_report <- amazon_sale_report %>%
  mutate(revenue_amazon = as.numeric(Amount) * as.numeric(Qty))

# display summary statistics of the cleaned amazon_sale_report dataset
summary(amazon_sale_report)

# find total revenue by style in amazon_sale_report
amazon_style_revenue <- amazon_sale_report %>%
  group_by(Style) %>%
  summarise(total_revenue_amazon = sum(revenue_amazon, na.rm = TRUE))

# find total gross revenue by style in international_sales
international_style_revenue <- international_sales_cleaned %>%
  group_by(Style) %>%
  summarise(total_gross_revenue = sum(gross_revenue, na.rm = TRUE))

# join the style revenue data from both datasets
join_style_revenue <- inner_join(amazon_style_revenue, international_style_revenue, by = "Style")

# rename columns for clarity
join_style_revenue <- join_style_revenue %>%
  rename(
    international_total_gross_revenue = total_gross_revenue,
    amazon_total_revenue = total_revenue_amazon
  )

# remove outliers (top 1% of revenue values) from the dataset
join_style_revenue <- join_style_revenue %>%
  filter(amazon_total_revenue < quantile(amazon_total_revenue, 0.99))

# print the 99th percentile value of amazon total revenue
print(quantile(join_style_revenue$amazon_total_revenue, 0.99))

# reshape the data for plotting
melted_new_data <- melt(join_style_revenue, id.vars = "Style", variable.name = "Sales_Channel", value.name = "Sales")

# create a scatter plot comparing sales by style between amazon and international sales
ggplot(melted_new_data, aes(x = Style, y = Sales, color = Sales_Channel)) +
  geom_point(size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of International Sales and Amazon Sales by STYLE",
       x = NULL,
       y = "Total Sales",
       color = "Sales Channel") +
  theme(axis.text.x = element_blank(),  # remove x-axis labels
        axis.ticks.x = element_blank(),  # remove x-axis ticks
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey"),
        legend.position = c(0.005, 1),  # move legend to top left corner
        legend.justification = c(0, 1),  # justify legend to top left
        legend.title = element_text(size = 8),  # smaller legend title
        legend.text = element_text(size = 6)  # smaller legend text
  ) +
  scale_color_manual(values = c("international_total_gross_revenue" = "orange", "amazon_total_revenue" = "blue")) +
  scale_y_continuous(breaks = seq(0, 3000000, 50000), labels = scales::unit_format(unit = "K", scale = 1e-3))

# find total revenue by size in amazon_sale_report
amazon_size_revenue <- amazon_sale_report %>%
  group_by(Size) %>%
  summarise(total_revenue_amazon = sum(revenue_amazon, na.rm = TRUE))

# find total gross revenue by size in international_sales
international_size_revenue <- international_sales_cleaned %>%
  mutate(gross_revenue = PCS * `GROSS AMT`) %>%
  group_by(Size) %>%
  summarise(total_gross_revenue = sum(gross_revenue, na.rm = TRUE))

# join the size revenue data from both datasets
join_size_revenue <- inner_join(amazon_size_revenue, international_size_revenue, by = "Size")

# define the correct order of sizes
size_order <- c("XS", "S", "M", "L", "XL", "XXL", "4XL", "5XL", "6XL", "Free")

# order the dataset by size
size_revenue_ordered <- join_size_revenue %>%
  mutate(Size = factor(Size, levels = size_order)) %>%
  arrange(Size)

# reshape the data for plotting
melted_size_revenue <- size_revenue_ordered %>%
  gather(key = "Sales_Channel", value = "Revenue", -Size)

# create the line graph comparing total revenue by size between amazon and international sales
size_plot <- ggplot(melted_size_revenue, aes(x = Size, y = Revenue, color = Sales_Channel, group = Sales_Channel)) +
  geom_line() +
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Total Revenue by Size: Amazon vs International",
       x = "Size",
       y = "Total Revenue",
       color = "Sales Channel") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# print the plot
print(size_plot)
