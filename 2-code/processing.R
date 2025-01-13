library(tidyverse)
theme_set(theme_bw(base_size = 14))

sample_key = read.csv("1-datasample_key.csv")


#
# ssa ---------------------------------------------------------------------
ssa = read.csv("1-data/dissolved_oxygen.csv")

ssa_processed = 
  ssa %>% janitor::clean_names() %>% 
  filter(water != "OW")

ssa_processed %>% 
  ggplot(aes(y = ssa_m2_g, x = depth_increment, color = water))+
  geom_point(size = 3,
             position = position_dodge(width = 0.2))+
  coord_flip()



ssa_processed %>% 
  ggplot(aes(y = ssa_m2_g, x = depth_increment, color = water))+
  geom_point(size = 1,
             position = position_dodge(width = 0.5))+
  stat_summary(fun.y = mean, geom = "point", size = 4,
               position = position_dodge(width = 0.5)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3,
               position = position_dodge(width = 0.5))+
  coord_flip()+
  facet_wrap(~horizon)+
  expand_limits(y = 0)


#

# DO ----------------------------------------------------------------------

do = read.csv("1-data/dissolved_oxygen.csv", na = "")
do_processed = 
  do %>% 
  separate(sample, into = c("sample_name", "horizon", "treatment"), sep = "_", remove = F) %>% 
  janitor::remove_empty()

do_processed %>% 
  #filter(treatment == "FW") %>% 
  ggplot(aes(x = days_elapsed, y = DO_mgL, color = treatment))+
  geom_path()+
  geom_point()+
  facet_wrap(~treatment + horizon + sample, ncol = 4)

#

# surface chemistry -------------------------------------------------------

chem = read.csv("1-data/surface_chemistry.csv", na = "")
chem_processed = 
  chem %>% 
  janitor::clean_names()

chem_processed %>% 
  ggplot(aes(x = x_floods, y = exch_na, color = water))+
  geom_path()+
  geom_point(size = 3)+
  facet_wrap(~horizon)
