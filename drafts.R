# SETUP ----
library(tidyverse)
library(forcats)
dayCat <- unique(ctrlLog$`Day #`)
geneCat <- c("Head", "Body", "Paw")
# TIDY ----
workingCL <- ctrlLog %>% 
  pivot_longer(
    cols = contains("Allele"),
    names_to = "location",
    values_to = "allele"
  ) %>%
  
  # rename 'Day #' to "day"
  rename( day = 'Day #',
          name = "Name",
          sex = "Sex")

# get rid of allele 1 and allele 2
workingCL$location <- str_extract(workingCL$location,"(\\w+)")
# WRANGLE ----

# in want to get it to show selected for gene on the bottom of the stacked chart always
# https://stackoverflow.com/questions/42773764/set-a-level-of-a-factor-to-be-the-last

# put number of observations on top of barchart
# https://stackoverflow.com/questions/35097489/put-total-observation-number-n-on-top-of-stacked-percentage-barplot-in-ggplot

# make color of desired allele the same

# head stacked bar
dfHead <- workingCL %>%
  filter(location == "Head") %>%
  group_by(day) %>%
  summarise(plyr::count(allele)) %>%
  rename(
    allele = x,
    count = freq
  )

forcats::fct_relevel(dfHead$allele, "Poison Fangs", after = Inf)
dfHead %>%
  ggplot(aes(fill = allele, x = day, y = count)) +
  geom_bar(position  = "fill", stat = "identity")

# body stacked bar
dfBody <- workingCL %>%
  filter(location == "Body") %>%
  group_by(day) %>%
  summarise(plyr::count(allele)) %>%
  rename(
    allele = x,
    count = freq
  )

dfBody$allele <- factor(dfBody$allele, levels = c("Heat", "Water", "Big", "Medium", "Spikey",  "Lean"))

dfBody%>%
  ggplot(aes(fill = allele, x = day, y = count)) +
  geom_bar(position  = "fill", stat = "identity")

# paw stacked bar
dfPaw <- workingCL %>%
  filter(location == "Paw") %>%
  group_by(day) %>%
  summarise(plyr::count(allele)) %>%
  rename(
    allele = x,
    count = freq
  )

dfPaw$allele <- factor(dfPaw$allele, levels = c("Deformed Paw", "Runners Leg", "Claw"))

dfPaw %>%
  ggplot(aes(fill = allele, x = day, y = count)) +
  geom_bar(position  = "fill", stat = "identity")
