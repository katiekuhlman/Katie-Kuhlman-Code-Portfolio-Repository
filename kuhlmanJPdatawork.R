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
library(ggExtra)

kuhlman_finaldata <- read_csv("kuhlman_finaldata.csv")
summary_stats <- arsenal::tableby(AnyInternal~totalgold + gdp_percap + pop.y,
                                  data = kuhlman_finaldata,
                                  numeric.stats = c("N", "mean", "sd", "median", "q1q3"),
                                  #weights = cbps_weights,
                                  test = FALSE) %>%
    summary(title = "Brief Summary Statistics")
summary_stats

summary_stats2 <- arsenal::tableby(un_continent_name.x~year + totalgold + gdp_percap +
                                       pop.y, numeric.stats = c("N", "mean", "sd", "median", "q1q3"), data = kuhlman_finaldata, test=FALSE) %>% summary()

summary_stats2

country_totals <- arsenal::tableby(~ctryname, data=kuhlman_finaldata,
                                   numeric.stars = c("N")) %>%
    summary()
country_totals

ggplot(kuhlman_finaldata) + 
    geom_histogram(aes(x=totalgold, y=..density.., fill=as.factor(AnyInternal)),
                   alpha = 0.5, position = "identity") + 
    labs(x="Total Gold Medals Won", y = "Density",
         title = "Histogram of Total Gold Medals Won by Presence of Internal Conflict",
         fill = "Internal Violence")

## summary graph as requested by prof Vreeland:

# looking at totalgold column
table(kuhlman_finaldata$totalgold)
kuhlman_finaldata <- kuhlman_finaldata %>% 
    mutate(totalgold_grouped = case_when(
        (is.na(totalgold))~"No Olympics",
        (totalgold==0)~"0",
        (totalgold<=5)~"1-5",
        (totalgold<=10)~"6-10",
        (totalgold<=15)~"11-15",
        (totalgold<=20)~"16-20",
        (totalgold<=30)~"21-30",
        (totalgold<=40)~"31-40",
        (totalgold<=51)~"41-51"
    ))

## replicating data to get the right order for the plot

## commenting out all of this because going with proportions graph at bottom!!
#data_reordered <- kuhlman_finaldata
#data_reordered$totalgold_grouped <- factor(data_reordered$totalgold_grouped,
#                      levels = c("No Olympics", "0", "1-5", "6-10", "11-15", "16-20", "21-30", "31-40", "41-51"))

#ggplot(data_reordered, aes(x = totalgold_grouped, y = AnyInternal)) +
#   geom_bar(stat="identity") +
#  labs(x = "Number Gold Medals Won", y = "Amount of Internal Conflict",
#      title="Breakdown of Internal Conflict Presence by Number of Gold Medals Won for Each Country-Year")

#ggplot(data_reordered, aes(x = totalgold_grouped, y = AnyInternal)) +
#   geom_bar(aes(y=..count../sum(..count..))) +
#  labs(x = "Number Gold Medals Won", y = "Amount of Internal Conflict",
#      title="Breakdown of Internal Conflict Presence by Number of Gold Medals Won for Each Country-Year")

summary.df <- kuhlman_finaldata %>%
    dplyr::select(totalgold_grouped, AnyInternal)

totalgold <- c("No Olympics", "0", "1-5", "6-10", "11-15", "16-20", "21-30", "31-40", "41-51")

# calculating proportions for new dataframe for chart
conflictproportion <- c(sum(kuhlman_finaldata$AnyInternal[kuhlman_finaldata$totalgold_grouped == "No Olympics"], na.rm = TRUE)/sum(kuhlman_finaldata$totalgold_grouped == "No Olympics", na.rm=TRUE), sum(kuhlman_finaldata$AnyInternal[kuhlman_finaldata$totalgold_grouped == "0"], na.rm = TRUE)/sum(kuhlman_finaldata$totalgold_grouped == "0", na.rm=TRUE), sum(kuhlman_finaldata$AnyInternal[kuhlman_finaldata$totalgold_grouped == "1-5"], na.rm = TRUE)/sum(kuhlman_finaldata$totalgold_grouped == "1-5", na.rm=TRUE), sum(kuhlman_finaldata$AnyInternal[kuhlman_finaldata$totalgold_grouped == "6-10"], na.rm = TRUE)/sum(kuhlman_finaldata$totalgold_grouped == "6-10", na.rm=TRUE), sum(kuhlman_finaldata$AnyInternal[kuhlman_finaldata$totalgold_grouped == "11-15"], na.rm = TRUE)/sum(kuhlman_finaldata$totalgold_grouped == "11-15", na.rm=TRUE), sum(kuhlman_finaldata$AnyInternal[kuhlman_finaldata$totalgold_grouped == "16-20"], na.rm = TRUE)/sum(kuhlman_finaldata$totalgold_grouped == "16-20", na.rm=TRUE), sum(kuhlman_finaldata$AnyInternal[kuhlman_finaldata$totalgold_grouped == "21-30"], na.rm = TRUE)/sum(kuhlman_finaldata$totalgold_grouped == "21-30", na.rm=TRUE), sum(kuhlman_finaldata$AnyInternal[kuhlman_finaldata$totalgold_grouped == "31-40"], na.rm = TRUE)/sum(kuhlman_finaldata$totalgold_grouped == "31-40", na.rm=TRUE), sum(kuhlman_finaldata$AnyInternal[kuhlman_finaldata$totalgold_grouped == "41-51"], na.rm = TRUE)/sum(kuhlman_finaldata$totalgold_grouped == "41-51", na.rm=TRUE))



summary.df_proportions <- data.frame(
    totalgold, conflictproportion
)

summary.df_proportions$totalgold <- factor(summary.df_proportions$totalgold,
                                           levels = c("No Olympics", "0", "1-5", "6-10", "11-15", "16-20", "21-30", "31-40", "41-51"))

ggplot(summary.df_proportions, aes(x = totalgold, y = conflictproportion)) +
    geom_bar(stat="identity") +
    labs(x = "Number Gold Medals Won", y = "Proportion of Internal Conflict",
         title="Breakdown of Internal Conflict Presence by Number of Gold Medals Won for Each Country-Year") +
    theme(plot.title = element_text(size=10))

## create column of lagged medal winning data by 1 year: 
    ## LAG MEDAL DATA because want to see if correlates with violence
    ## ONE YEAR LATER
kuhlman_finaldata <- kuhlman_finaldata %>% group_by(ctryname) %>%
    mutate(totalgold_lagged = dplyr::lag(totalgold, order_by=year))
## NEVERMIND!!!

#Same Regressions on Internal Conflict Data LAGGED by ONE YEAR in REVERSE (to keep
#as many data points as possible, it's as though the conflict that occurred in the
#year AFTER winning the medal ACTUALLY occurred in the year the medal was won so
#that a regression can be run). Because lagging the gold medal data adds in too
#many NAs)
kuhlman_finaldata <- kuhlman_finaldata %>% group_by(ctryname) %>%
    mutate(AnyInternal_revlagged = dplyr::lead(AnyInternal, order_by=year))

## models on un-lagged conflict data
reglm1 <- lm(AnyInternal~totalgold, data = kuhlman_finaldata)
reglm2 <- lm(AnyInternal~totalgold + gdp_percap + pop.y, data = kuhlman_finaldata)
reglm3 <- lm(AnyInternal~totalgold + gdp_percap + pop.y +
                 hostnation + democracy.x + un_continent_name.x, data = kuhlman_finaldata)


## models on LAGGED conflict data
laglm1 <- glm(AnyInternal_revlagged~totalgold, data = kuhlman_finaldata)
laglm2 <- glm(AnyInternal_revlagged~totalgold + gdp_percap + pop.y, data = kuhlman_finaldata)
laglm3 <- glm(AnyInternal_revlagged~totalgold + gdp_percap + pop.y +
                  hostnation + democracy.x + un_continent_name.x, data = kuhlman_finaldata)
