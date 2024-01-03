library(tidyverse)

students <- read_csv("https://pos.it/r4ds-students-csv", na = c("N/A", ""))
students <- students |>
  janitor::clean_names() |>
  mutate(
    meal_plan = factor(meal_plan),
    age = parse_number(if_else(age == "five", "5", age))
  )
students

read_csv(
  "blah
  blah
  #blah
  a,b,c
  1,2,3
  4,5,6",
  skip = 2,
  comment = "#",
  col_names = c("x", "y", "z")
)

read_csv("a;b\n1;3")
