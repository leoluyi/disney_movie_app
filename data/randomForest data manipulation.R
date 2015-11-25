library(dplyr)
library(pipeR)
library(randomForest)
library(IDPmisc)

# Model data preparation --------------------------------------------------

model.var <- c("MovieID","ChiTitle","EngTitle", "ReleaseDate","classBO","WeekendBO",
               "Budget_mil", "Country", "D_Gen8", "vacation",
               "Aware", "Interest", "FirstChoice")

train.data <- merge.data_1 %>%
  filter(!is.na(WeekendBO)) %>>% # remove WeekendBO NA
  (data.frame(select(., one_of(model.var)),select(.,contains("DI")))) %>%
  tbl_df()

## remove outliers from WeekendBO
train.data <- train.data %>%
  filter((Interest!=0) & (DI!=0) & (Aware!=0) & (FirstChoice!=0)) %>%
  mutate(logDI = log(DI),
         logFirstChoice = log(FirstChoice),
         logInterest = log(Interest),
         logAware = log(Aware)) %>%
  filter(!log(WeekendBO) %in%
           boxplot.stats(log(WeekendBO))$out)


## remove outliers
# library(mvoutlier)
# outliers <- train.data %>%
#   select(logAware, logInterest, logDI, logFirstChoice) %>%
#   chisq.plot(.) %>% `$`(outliers) # multivariate chi-square plot
# train.data %>% slice(c(291, 404, 141, 368)) %>% select(MovieID) %>% `[[`(1)

train.data <- train.data %>%
  filter(! MovieID %in% c("EMD0589004", "EMD0584403", "EMD0595502", "EMD0585502"))

# color.plot(as.data.frame(train.data[c("Aware", "DI")]))


# Random Forest prediction of Kyphosis data -------------------------------

test.data <- merge.data_1 %>>%
  (data.frame(select(., one_of(model.var)),select(.,contains("DI")))) %>%
  tbl_df()

train.data.NAomit <- train.data %>%
  select(-Budget_mil) %>% # remove Budget_mil
  IDPmisc::NaRV.omit(.)   # remove Inf/NA/NAs

test.data.NAomit <- test.data%>%
  filter(!is.na(Aware)) %>%
  filter(!is.na(DI)) %>%
  filter(!is.na(FirstChoice)) %>%
  filter(!is.na(Country)) %>%
  filter(!is.na(D_Gen8)) %>%
  select(-Budget_mil)


## fit randomForest
fit.rf <- randomForest(classBO ~
                         Aware+
                         DI_20_24age+DI_25_29age+DI_30_34age+DI_35_39age+DI_40_44age+
                         FirstChoice+Country+D_Gen8,
                       data = train.data.NAomit,
                       proximity=TRUE,
                       importance=TRUE)


test.result <- test.data.NAomit %>%
  select(ChiTitle, EngTitle, ReleaseDate, WeekendBO, classBO)

test.data.NAomit %>>%
  (~ test.result$classBO.predict <- ordered(predict(fit.rf,. ))) %>>%
  (cbind(test.result, predict(fit.rf, .,type='prob')) -> test.result)


# # randomforest confusion matrix ---------------------------------------------
#
# test.result %>%
#   select(classBO, classBO.predict) %>%
#   table
#
# fit.rf
