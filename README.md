---
title: "2020 Consumer finiacial Protection Bureau Complaints Data Analysis"
output: html_document
author: Daniel Okay
course: SMPA 3230 
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

About:

The following analysis is taken from the 2020 Consumer Financial Protection Bureau complaints from March-December. The following are the clearest most identifiable trends that the public ought to know about.

These analyses range from the most common type of complaint, top company offenders, and where these complaints most frequently happen.

Each takeaway from the data is neatly summarized in three histograms -- as well as a couple tables to better visualize these trends. I hope you find the information useful.


```{r}

#Run Libraries

library(tidyverse)
library(tidyverse)
library(tigris)
library(sf)
library(tmap)
library(tmaptools)
library(htmltools)
library(janitor)
library(rmapshaper)
library(here)
library(kableExtra)
library(ggthemes)

```


```{r}
#Run the Dataset

complaints <- readRDS("~/Tunespotter Dropbox/Daniel Okay/My Mac (Daniel’s MacBook Pro)/Desktop/Github_Okay/DOkay_Complaints/complaints.rds")

```

```{r}
#We're listing the companies that were most frequently had complaints filed against them.

complaints%>%
 count(company,sort = TRUE)

```
```{r} 

#Here we're making a datagframe so it can better work in a graph that we then have the code for in the following chunk. 
top_companies_df <- complaints %>% 
  count(company) %>% 
  filter(n>70000)
```


```{r}
ggplot(top_companies_df, aes (x = company, y = n)) +
  theme_economist_white() +
         geom_bar(stat = "identity", color = "black", fill = "#DD8888", width = .8) +
  theme(axis.text = element_text(size = 7.5)) +
         ggtitle(label = "Companies with Most Complaints") +
        xlab(label = "Companies") +
        ylab(label= "Number of Complaints") 
 
```

```{r}
#Here we're organizing the most common/frequently cited type of issuee within the complaints dataset.

complaints%>%
 count(issue,sort = TRUE)

```

```{r}
#Once again we're making a dataframe so that it's easier create a data visualization.

df_top_issues <- complaints %>% 
  count(issue, sort = TRUE) %>%
  filter(n>12000)


```

```{r}

ggplot(df_top_issues, aes (x =issue, y = n)) +
  theme_fivethirtyeight() + 
         geom_bar(stat = "identity", color = "black", fill = "#b9e38d", width = .8) +
        theme(axis.text = element_text(size = 6)) +
         ggtitle(label = "Most Common Complaints") +
        xlab(label = "Type of Issue") +
        ylab(label= "Number of Complaints") 
```

```{r}
#We're showing the states were complaints were most often filed.

complaints %>%
  count(state, sort = TRUE)

```
```{r}
table1 <- df_top_issues
```

```{r}
table1 %>%
  knitr::kable(caption = "Table 1: Top Issues within Complaints") %>% 
  kableExtra::kable_styling(bootstrap_options = "striped")
```


```{r}
#Creating another dataframe for easier data visualization.

state_complaints <- complaints %>% 
  count(state, sort = TRUE) %>%
  filter(state%in% c("FL", "CA" , "TX"))

```


```{r}
ggplot(state_complaints, aes (x =state, y = n)) +
         geom_bar(stat = "identity", color = "black", fill = "#d9b1f0", width = .8) +
        theme(axis.text = element_text(size = 12)) +
         ggtitle(label = "States with the Most Complaints") +
        xlab(label = "State") +
        ylab(label= "Number of Complaints") +
  coord_flip() +
  theme_economist() + scale_colour_economist()


```


```{r}
summary_names <- complaints %>% 
  count(company, issue) %>% 
  filter(n>45000)

```

```{r}
tabl <- table(summary_names)

tabl
```

```{r, results='asis'}
tabl %>%
  knitr::kable(caption = "Table 2: Most Frequently Complained about Companies and their top Complaints") %>% 
  kableExtra::kable_styling(bootstrap_options = "striped")
```

