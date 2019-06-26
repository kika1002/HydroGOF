install.packages ("hydroGOF")
require(pacman)
pacman::p_load(tidyverse, rgeos, stringr, raster, rgdal, gtools, readxl, lubridate, zoo, hydroGOF)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

lbl <- data.frame(MON = 1:12,
                  MONTH = month.abb)

tbl <- read_csv('G:/_infadd/Cabuyaritoflow_2/Cabuyaritoflow.csv')
acc <- read_excel('G:/_infadd/Cabuyaritoflow_2/SWATOutput.xlsx')
smm <- tbl %>% 
  mutate(month = factor(month, levels = c(month.abb))) %>% 
  arrange(month) %>% 
  group_by(year, month) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  drop_na()
acc <- acc %>% 
  dplyr::select(SUB, YEAR, MON, FLOW_OUTcms) %>% 
  inner_join(., lbl, by = 'MON') %>% 
  dplyr::select(SUB, YEAR, MONTH, FLOW_OUTcms)
fnl <- inner_join(smm, acc, by = c('year' = 'YEAR', 'month' = 'MONTH')) %>% 
  dplyr::select(SUB, year, month, value, FLOW_OUTcms) %>% 
  mutate(month = factor(month, levels = month.abb)) 

fn2 <- fnl %>% 
  mutate(date = as.yearmon(ymd(paste0(year, '-', month, '-01')))) %>% 
  dplyr::select(-SUB, -year, -month) %>% 
  rename(real = value,
         model = FLOW_OUTcms) %>% 
  gather(variable, value, -date) %>% 
  mutate(date2 = as.character(date))

tbl <- fn2 %>% 
  spread(variable, value)

# Coeficientes # Library hydroGOF
NSE(pull(tbl, 3), pull(tbl, 4))
pbias(pull(tbl, 3), pull(tbl, 4))

sim <- pull(tbl, 3)
obs <- pull(tbl, 4)
br2 <- br2(sim, obs)

PBIAS <- 100 * (sum(sim-obs)/sum(obs))

gg <- ggplot(data = fn2, aes(x = date, y = value, colour = variable)) +
  geom_line() +
  labs(x = '',
       y = 'm3/s') +
  scale_colour_manual(name = ' ', 
                      values = c('model' = '#0040FF', 'real' = '#FE2E2E'),
                      labels = c("model" = "Modeled", 'real' = 'Observed')) +
  theme(legend.position = 'top')
ggsave(plot = gg, filename = 'caudales.png', units = 'cm', width = 16, height = 12, dpi = 300)

write.csv(smm, 'Cabuyaritoflow_smm.csv', row.names = FALSE)

