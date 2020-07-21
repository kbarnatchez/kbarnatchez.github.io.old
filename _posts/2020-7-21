---
title: "Excess Deaths Due to Covid"
author: "Keith Barnatchez"
date: "July 21, 2020"
output: html_document
---

``` r
# Load in main data
data<-read.csv('Excess_Deaths_Associated_with_COVID-19.csv',sep=',')
data<-data[as.Date(data[,1])>=as.Date('2020-01-01'),]

# Data for all deaths 
data_all<-data[data$Outcome == 'All causes',]
data_all<-data_all[data_all$Type != 'Unweighted',]

# Data for all deaths excluding covid
data_ex_covid<-data[data$Outcome == 'All causes, excluding COVID-19',]
```

``` r
covid_deaths<-data_all$Observed.Number - data_ex_covid$Observed.Number
data_all$cum_covid_deaths <- ave(covid_deaths,data_all$State,FUN=cumsum)
data_all$cum_excess <- ave(data_all$Excess.Higher.Estimate,data_all$State,FUN=cumsum)
data_all$excess_ratio <- data_all$cum_excess/data_all$cum_covid_deaths
```

``` r
colnames(data_all)[1] <- 'date'

deaths_june <- data_all[as.Date(data_all$date)==as.Date('2020-06-06'),]
deaths_june <- deaths_june[deaths_june$State != 'United States',]
deaths_june <- deaths_june[!is.na(deaths_june$excess_ratio),]
```

``` r
# Partition data to label only a subset of points
deaths_june_label <- deaths_june[deaths_june$cum_covid_deaths>10000 | deaths_june$excess_ratio>5,]
deaths_june_nolabel <- deaths_june[deaths_june$cum_covid_deaths<=10000 & deaths_june$excess_ratio<=5,]
```

``` r
ggplot(deaths_june_label, aes(x=cum_covid_deaths,y=excess_ratio)) + 
       geom_point(size=3,alpha=1,color='red') +
       geom_label_repel(aes(label=State)) +
       geom_point(data = deaths_june_nolabel, color='steelblue',size=3,alpha=1) +
       ggtitle('States with low COVID death tolls have suspiciously high excess death estimates') +
       xlab('Cumulative covid deaths through 06-06-2020') +
       ylab('Ratio of CDC excess deaths to cumulative COVID deaths') +
       labs(caption = 'Excess deaths are estimated by the CDC. New York state data excludes New York City, which is reported separately.') +
       theme(plot.caption = element_text(hjust=0))
```
