# Pra instalar as bibliotecas, utilize install.packages("nome da biblioteca")
# A base flights vem com a biblioteca nycflights13
library(tidyverse)
library(nycflights13)
base_agrupada <- flights %>%
  filter(dest == "IAH") %>%
  group_by(year, month, day) %>%
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

flights <- flights |>
  group_by(year, month, day) |>
  mutate(arr_delay_day = mean(arr_delay, na.rm = TRUE)) |>
  ungroup()

plot <- flights |>
  filter(dest == "IAH") |>
  ggplot(aes(x=arr_delay, y=dep_delay)) + geom_point(colour = "#45ff66")