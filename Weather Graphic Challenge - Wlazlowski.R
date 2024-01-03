library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

#importing data
library(readxl)
philly <- read_xlsx("data/weather.xlsx", sheet = "Philadelphia")

#cleaning file
library(janitor)
philly <- philly %>%
  clean_names(case = "snake")
view(philly)

#creating past dataframe
past <- philly |>
  mutate(newDay = seq(1, length(day))) |>
  ungroup()
past <- past |>
  mutate(temp = (high_f + low_f) / 2)

#creating present dataframe
present <- philly |>
  mutate(newDay = seq(1, length(day))) |>
  mutate(temp = (average_high_f + average_low_f) / 2)

pastLows <- past |>
  rename(Pastlow = record_low_f)

presentLows <- present |>
  left_join(pastLows) |>
  mutate(record = ifelse(low_f <= record_low_f, "Y", "N")) |>
  filter(record == "Y")

pastHighs <- past |>
  rename(Pasthigh = record_high_f)

presentHighs <- present |>
  left_join(pastHighs) |>
  mutate(record = ifelse(high_f >= record_high_f, "Y", "N")) |>
  filter(record == "Y")

dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

a <- dgr_fmt(seq(-20, 100, by = 10))

legend_data <- data.frame(x = seq(175, 182), y = rnorm(8, 15, 2))

p <- ggplot(past, aes(newDay, high_f)) + 
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  geom_linerange(past, mapping = aes(x = newDay, ymin = record_low_f, ymax = record_high_f), color = "darkslateblue", alpha = 0.1)

p <- p +
  geom_linerange(past, mapping = aes(x = newDay, ymin = average_low_f, ymax = average_high_f), color = "darkslateblue", alpha = 0.7)

p <- p +
  geom_line(present, mapping = aes(x = newDay, y = high_f, group = 1, color = "darkblue", show.legend = FALSE)) +
  geom_vline(xintercept = 0, color = "darkblue", linetype = 1, size = 1) +
  theme(legend.position = "none")

p <- p +
  geom_line(present, mapping = aes(x = newDay, y = low_f, group = 1, color = "firebrick3", show.legend = FALSE)) +
  geom_vline(xintercept = 0, color = "firebrick3", linetype = 1, size = 1) +
  theme(legend.position  ="none")

p <- p +
  geom_hline(yintercept = -20, color = "white", linetype = 1) +
  geom_hline(yintercept = -10, color = "white", linetype = 1) +
  geom_hline(yintercept = 0, color = "white", linetype = 1) +
  geom_hline(yintercept = 10, color = "white", linetype = 1) +
  geom_hline(yintercept = 20, color = "white", linetype = 1) +
  geom_hline(yintercept = 30, color = "white", linetype = 1) +
  geom_hline(yintercept = 40, color = "white", linetype = 1) +
  geom_hline(yintercept = 50, color = "white", linetype = 1) +
  geom_hline(yintercept = 60, color = "white", linetype = 1) +
  geom_hline(yintercept = 70, color = "white", linetype = 1) +
  geom_hline(yintercept = 80, color = "white", linetype = 1) +
  geom_hline(yintercept = 90, color = "white", linetype = 1) +
  geom_hline(yintercept = 100, color = "white", linetype = 1)

p <- p +
  geom_vline(xintercept = 31, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 59, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 90, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 120, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 151, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 181, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 212, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 243, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 273, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 304, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 334, color = "wheat4", linetype = 3, size = 0.5) +
  geom_vline(xintercept = 365, color = "wheat4", linetype = 3, size = 0.5)

p <- p +
  coord_cartesian(ylim = c(-20, 100)) +
  scale_y_continuous(breaks = seq(-20, 100, by = 10), labels = a) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(15, 45, 75, 105, 135, 165, 195, 228, 258, 288, 320, 350),
                     labels = c("January", "February", "March", "April",
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))
print(p)

p <- p +
  geom_point(data = presentLows, aes(x = newDay, y = low_f), color = "blue3") +
  geom_point(data = presentHighs, aes(x = newDay, y = high_f), color = "firebrick3")
print(p)