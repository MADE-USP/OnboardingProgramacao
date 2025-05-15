library(tidyverse)
library(nycflights13)
dados_resumidos <- flights %>% 
  group_by(carrier) %>%
  summarize(
  arr_delay = mean(arr_delay, na.rm = TRUE),
  dep_delay = mean(dep_delay, na.rm = TRUE))

plot_arrival <- dados_resumidos %>% 
  arrange(desc(arr_delay)) %>% 
  mutate(carrier = factor(carrier, carrier)) %>%
  ggplot(aes(x=carrier, y=arr_delay))+ geom_bar(stat="identity", width=0.5)

plot_departure <- dados_resumidos %>% 
  arrange(desc(dep_delay)) %>% 
  mutate(carrier = factor(carrier, carrier)) %>%
  ggplot(aes(x=carrier, y=dep_delay)) + geom_bar(stat="identity", width=0.5)