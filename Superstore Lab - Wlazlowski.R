library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)

#importing file
library(readxl)
df <- read_xlsx("Superstore.xlsx")
#view(df)

#importing janitor and cleaning column names
library(janitor)
superstore <- df %>%
  clean_names(case = "snake")
#view(superstore)

#selecting only the columns needed
superstore <- superstore |> 
  select(category, 
         department, 
         discount, 
         order_date, 
         order_priority, 
         order_quantity, 
         profit, 
         region, 
         sales, 
         ship_mode, 
         shipping_cost, 
         state, 
         customer_name)
#view(superstore)

#creating a column called expenses
superstore <- superstore |> 
  mutate(expenses = sales - profit)
#view(superstore)

#creating a column called large_discount
superstore <- superstore |> 
  mutate(large_discount = discount > 0.10)
#view(superstore)

#finding the number of customers with middle names
superstore <- superstore |> 
  mutate(split = str_count(superstore$customer_name, '\\s+')+1) #|>
  #mutate(middle_name = split > 2)
#superstore$split <- as.factor(superstore$split)
middle_names <- superstore |>
  distinct(customer_name, split) |>
  filter(split > 2) 
count(middle_names)

#removing customers with a middle name
superstore <- superstore |>
  filter(split == 2)
#view(superstore)
glimpse(superstore)

#relocating categorical columns
superstore <- superstore |>
  relocate(where(is.character))
#view(superstore)

#for "East" region, sum of the sales for each state
east <- superstore |>
  filter(region == "East") |>
  group_by(state) |>
  summarize(total_sales = sum(sales))
#view(east)

#for each state, average profit by department
state_profits <- superstore |>
  group_by(state, department) |>
  summarize(average_profit = mean(profit))
view(state_profits)

#for each month, total order quantity
 superstore <- superstore |>
   separate(order_date,
            into = c("year", "month", "day"), sep = "-")
 month_order_quantity <- superstore |>
   group_by(month) |>
   summarize(total_order_quantity = sum(order_quantity)) |>
   arrange(desc(total_order_quantity))
# view(month_order_quantity)

#average, lowest, and highest discount with count for each department
department_discounts <- superstore |>
  group_by(department) |>
  summarize(
    average_discount = mean(discount),
    lowest_discount = min(discount),
    highest_discount = max(discount),
    num_of_data_points = length(discount)
  )
#view(department_discounts)

#graph of monthly and yearly total profit
monthly_total_profits <- superstore |>
  group_by(department, month, year) |>
  summarize(total_profit = sum(profit)) |>
  mutate(year_month = lubridate::parse_date_time(paste(year, month), orders = "ym"))
view(monthly_total_profits)

#Attempt 1: it works but does not display months on graph
ggplot(monthly_total_profits) +
  geom_line(aes(
    x = year_month,
    y = total_profit,
    group = department,
    color = department
  )) +
  labs(
    x = "Date",
    y = "Total Profit",
    title = "Total Profit Change for Each Department Over Time"
  )

#Attempt 2: much better :)
ggplot(
  monthly_total_profits,
  aes(
    x = paste(month, year),
    y = total_profit,
    group = department,
    color = department
  )) +
  geom_line() +
  labs(
    x = "Date",
    y = "Total Profit",
    title = "Total Profit Change for Each Department Over Time"
  ) +
  theme(axis.text.x = element_text(angle = 90, size = 5))

#relative frequency dataframe for order priority by month
monthly_order_priority <- superstore |>
  group_by(month, order_priority) |> 
  summarize(total_orders = n()) |>
  mutate(proportion = total_orders / sum(total_orders))
view(monthly_order_priority)

#relative frequency graph for order priority by month
ggplot(
  monthly_order_priority,
  aes(
    x = month,
    y = proportion,
    fill = order_priority
  )) +
  geom_bar(stat = "identity") +
  labs(
    x = "Month",
    y = "Relative Frequency",
    title = "Relative Frequency of Order Priority by Month"
  )

#relative frequency dataframe for order priority by department
department_order_priority <- superstore |>
  group_by(department, order_priority) |>
  summarize(total_orders = n()) |>
  mutate(proportion = total_orders / sum(total_orders))
view(department_order_priority)

#relative frequency graph for order priority by department
ggplot(
  department_order_priority,
  aes(
    x = department,
    y = proportion,
    fill = order_priority
  )) +
  geom_bar(stat = "identity") +
  labs(
    x = "Department",
    y = "Relative Frequency",
    title = "Relative Frequency of Order Priority by Department"
  )

#ANOVA: is there a different between the profits of each region?
oneway.test(profit ~ region,
            data = superstore,
            var.equal = TRUE)