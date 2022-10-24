## Katie Kuhlman
## Princeton University POL Department
## Junior Paper Spring 2022
## Data Frame Work and Cleaning


library(readr)
library(ggplot2)
library(dplyr)
conflict <- readxl::read_xls("conflictdata.xls")
summary(conflict)
sum(conflict$Type == 3 | conflict$Type == 4)
 # 1717 rows with Internal armed conflict OR Internationalized internal 
    # armed conflict

summary(conflict$YEAR)
# goes from 1946-2008; has at least one conflict from every year

ggplot(conflict, aes(x = Type, y = YEAR)) + geom_point()


summary(conflict$ID)
# there are 260 conflicts recorded here, over 62 years

sort(unique(conflict$ID[conflict$Type == 3 | conflict$Type == 4]))
# there are 174 unique internal OR internationalized internal conflicts

ggplot(conflict, aes(x = Type, y = YEAR, color=as.factor(Region))) + 
    geom_point()

sum(conflict$Type == 1) # 119
sum(conflict$Type == 2) # 121
sum(conflict$Type == 3) # 1536
sum(conflict$Type == 4) # 181

library(haven)
olympic <- read_dta("olympic_data_final copy.dta")
regime <- read_dta("ddrevisited data v1.dta")
stats <- read_dta("country_stats.dta")

conflict$Conflict <- ifelse(conflict$Type %in% 1:4, 1, 0)

conflict$Extrasystemic <- ifelse(conflict$Type == 1, 1, 0)
conflict$Interstate <- ifelse(conflict$Type == 2, 1, 0)
conflict$AnyInternal <- ifelse(conflict$Type %in% 3:4, 1, 0)
conflict$JustInternal <- ifelse(conflict$Type == 3, 1, 0)
conflict$InternationalizedInternal <- ifelse(conflict$Type == 4, 1, 0)

ggplot(conflict, aes(x = YEAR, y = Region, color=as.factor(Type))) + 
    geom_point()

ggplot(conflict, aes(x = YEAR, color = as.factor(Type))) + geom_histogram()

ggplot(olympic, aes(x = totgold_w)) + geom_histogram()

write.csv(olympic, file = "olympic.csv")


summary(olympic$totavailable_m[olympic$year== 1948])

olympic$totalgold <- olympic$totgold_m + olympic$totgold_w
summary(olympic$totalgold)
ggplot(olympic, aes(x=totalgold)) + geom_histogram()

ggplot(olympic.US, aes(x = year, y = totalgold)) +
    geom_point()

olympic.US <- olympic[olympic$ctryname == "United States of America", na.rm=TRUE]
which(olympic$ctryname == "United States of America")

## possible some medal counts are slightly off...appear generally 
    # correct/close, tho...question for Prof Vreeland

ggplot(olympic, aes(x = totalgold, color = un_continent_name)) + 
    geom_histogram() + xlim(0,20) + ylim(0,500)

# Merging dataframes: attempt 1

    # merging Olympics data to regime data
olympic_regime <- merge(regime, olympic, by = c("ctryname", "year"), all = TRUE)
    # there are 705,755 NA's in total in this dataset...

    # editing conflict data so that column names match
conflict <- conflict %>% mutate(ctryname = Location) %>%
    mutate(year = YEAR)

    # adding in conflict data
o_r_c_fulldata <- merge(olympic_regime, conflict, by = c("ctryname", "year"), all = TRUE)

# saving data as CSV (commented out so don't continue to do it)
#write.csv(o_r_c_fulldata, "o_r_c_fulldata.csv")

# adding 0's to conflict columns to reflect no conflicts in
    # country-years which are coded NA right now

o_r_c_fulldata$Conflict[is.na(o_r_c_fulldata$Conflict)] <- 0
summary(o_r_c_fulldata$Conflict)

o_r_c_fulldata$Extrasystemic[is.na(o_r_c_fulldata$Extrasystemic)] <- 0
summary(o_r_c_fulldata$Extrasystemic)

o_r_c_fulldata$Interstate[is.na(o_r_c_fulldata$Interstate)] <- 0
summary(o_r_c_fulldata$Interstate)

o_r_c_fulldata$AnyInternal[is.na(o_r_c_fulldata$AnyInternal)] <- 0
summary(o_r_c_fulldata$AnyInternal)

o_r_c_fulldata$JustInternal[is.na(o_r_c_fulldata$JustInternal)] <- 0
summary(o_r_c_fulldata$JustInternal)

o_r_c_fulldata$InternationalizedInternal[is.na(o_r_c_fulldata$InternationalizedInternal)] <- 0
summary(o_r_c_fulldata$InternationalizedInternal)

library(readr)
library(dplyr)
library(knitr)
library(tinytex)
library(ggplot2)
library(kableExtra)
library(janitor)
library(stringr)
library(forcats)
library(lubridate)
library(gridExtra)
library(foreign)
library(haven)
library(stargazer)
library(arsenal)
library(performance) # for diagnistics test
library(see) # for diagnistics test
library(qqplotr) # for diagnistics test
library(sjPlot)
library(xtable)
library(interactions)
library(ggeffects)
library(scales)
library(MatchIt)
library(MASS)
library(janitor)
suppressMessages(library(stargazer))
suppressMessages(library(tidyverse))
library(naniar)
library(cowplot)
library(Amelia)
library(dataReporter)
library(effects)
library(CBPS)
library(cobalt)
library(car)

pwt <- read_dta("pwt90.dta") # Penn World Tables 9.0
pwt <- pwt %>%
    mutate(ctryname = country)

kuhlman_finaldata <- merge(o_r_c_fulldata, pwt, by = c("ctryname", "year"))
#write.csv(kuhlman_finaldata, "kuhlman_finaldata.csv")

# adding column for GDP per capita using PWT data
kuhlman_finaldata <- kuhlman_finaldata %>%
    mutate(gdp_percap = rgdpna/pop.y)

# test regression -- woooooo!!
summary(lm(AnyInternal~totalgold, data = kuhlman_finaldata))

# NOTE: pop.y is total population from PWT data
summary(lm(AnyInternal~totalgold + gdp_percap + pop.y, data = kuhlman_finaldata))

## intro should be 2-4 pages
## for summary statistics: x should be gold medals, y should be conflict (bars?)
## International victory in sport has galvanizing effect on increasing nationalism 
    # and decreasing conflict.
kuhlman_finaldata <- read_csv("kuhlman_finaldata.csv")
