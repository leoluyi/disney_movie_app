
# install required packages -----------------------------------------------

list.of.packages <-
  c("dplyr", "pipeR", "stringr", "lazyeval", "Hmisc", "DT",
    "scales", "IDPmisc", "randomForest", "shinythemes")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages, new.packages)


# library -----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(DT)
library(stringr) # for str_replace_all()
library(lazyeval)
library(directlabels)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) # for date_format(), dollar_format()

source("data/disney data manipulation.R", local=TRUE, encoding="UTF-8")
source('data/randomForest data manipulation.R', local=TRUE, encoding="UTF-8")

varlist <- function(x) {
  library(stringr)
  x = str_c('(',paste(x, collapse='|'),')')
  x = str_replace_all(x,'\\.','\\\\.')
  return(x)
}

per.var <- c(
  "Aware", "Interest", "DI", "Interest_TOP2", "FirstChoice")

# basic.tab[, per.var] <- round(basic.tab[, per.var]*100,digits=1) # percentage

# strata.tab[, 9:length(strata.tab)] <-
#   round(strata.tab[, 9:length(strata.tab)]*100,digits=1) # percentage
