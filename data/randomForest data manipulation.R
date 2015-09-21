library(dplyr)
library(pipeR)


## ===== Model data preparation=====
## remove outliers
model.var <- c("MovieID","ChiTitle","EngTitle", "ReleaseDate","classBO","WeekendBO",
               "Budget_mil", "Country", "D_Gen8", "vacation", 
               "Aware", "Interest", "FirstChoice")
train.data <- merge.data_1 %>% 
  filter(!is.na(WeekendBO)) %>>% # remove WeekendBO NA
  (data.frame(select(., one_of(model.var)),select(.,contains("DI")))) %>% 
  tbl_df()

test.data <- merge.data_1 %>>% 
  (data.frame(select(., one_of(model.var)),select(.,contains("DI")))) %>% 
  tbl_df()

train.data <- train.data %>%
  mutate(logDI = log(DI)) %>%
  mutate(logFirstChoice = log(FirstChoice)) %>% 
  # remove outliers - 從WeekendBO
  filter(
    !log(train.data$WeekendBO) %in% 
      boxplot.stats(log(train.data$WeekendBO))$out
  )


# remove outliers - 從其他數值變數
library(mvoutlier)
# outliers <- chisq.plot(train.data %>%
#              select(Aware, Interest, DI,FirstChoice))$outliers# 多變量chi-square plot
train.data <- train.data[-c(306, 168, 53, 323, 233, 76, 177)]

# color.plot(as.data.frame(train.data[c("Aware", "DI")]))
## ===== End (Model data preparation) =====


## === Random Forest prediction of Kyphosis data ===
library(IDPmisc)

train.data.NAomit <- train.data %>% 
  select(-Budget_mil) %>% # remove Budget_mil
  NaRV.omit()   # remove Inf/NA/NAs

test.data.NAomit <- test.data%>% 
  filter(!is.na(Aware)) %>% 
  filter(!is.na(DI)) %>% 
  filter(!is.na(FirstChoice)) %>% 
  filter(!is.na(Country)) %>% 
  filter(!is.na(D_Gen8)) %>%
  select(-Budget_mil)

library(randomForest)
fit.rf <- randomForest(classBO ~ 
                         Aware+
                         DI_20_24age+DI_25_29age+DI_30_34age+DI_35_39age+DI_40_44age+
                         FirstChoice+Country+D_Gen8, 
                       data = train.data.NAomit,
                       proximity=TRUE,
                       importance=TRUE)

test.result <-test.data.NAomit %>%
  select(ChiTitle, EngTitle, ReleaseDate, WeekendBO, classBO)

test.data.NAomit %>>%
  (~ test.result$classBO.predict <- ordered(predict(fit.rf,. ))) %>>%
  (cbind(test.result, predict(fit.rf, .,type='prob')) -> test.result)

