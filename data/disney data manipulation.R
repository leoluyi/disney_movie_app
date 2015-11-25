library(dplyr)
library(readr)
library(readxl)
library(gridExtra)
library(pipeR)


# Read data ---------------------------------------------------------------

# movie.data.string <- readLines(con<-file("http://isas.panelpower.com.tw/isas45/surveyfile/2015/3522/data/movie_data.csv",
#                                          encoding="Big5"),
#                                warn=FALSE); close(con)
movie.data.string <- readLines(con<-file("data/movie_data.csv",
                                         encoding="Big5"),
                               warn=FALSE); close(con)

# Encoding(movie.data.string)
# movie.data.string <- iconv(movie.data.string, from = "Big5", to = "UTF-8")

movie.data <- read.csv(textConnection(movie.data.string),
                       header = TRUE,
                       na.strings = c("NA",""),
                       stringsAsFactors =F,
                       fileEncoding = "UTF-8") %>% as_data_frame

# survey.data.string <- readLines(con<-file("http://isas.panelpower.com.tw/isas45/surveyfile/2015/3522/data/survey_data.csv",
#                                           encoding="Big5"),
#                                 warn=FALSE); close(con)
survey.data.string <- readLines(con<-file("data/survey_data.csv",
                                          encoding="Big5"),
                                warn=FALSE); close(con)
# survey.data.string <- iconv(survey.data.string, from = "Big5", to = "UTF-8")
survey.data <- read.csv(textConnection(survey.data.string),
                        header = TRUE,
                        na.strings = c("NA",""),
                        stringsAsFactors =F,
                        fileEncoding = "UTF-8") %>% as_data_frame

#  ------------------------------------------------------------------------

# con1 <- file("http://isas.panelpower.com.tw/isas45/surveyfile/2015/3522/data/movie_data.csv",
#           encoding="Big5")
# movie.data <- readr::read_csv(con1, na = c("NA",""), locale=locale(encoding = "Big5"),
#                               col_types = paste0(rep("c", 14), collapse = ""))
#
# survey.data <- readr::read_csv("http://isas.panelpower.com.tw/isas45/surveyfile/2015/3522/data/survey_data_big5.csv",
#                         col_names = TRUE,
#                         na = c("NA",""),
#                         col_types = paste0(rep("c", 43), collapse = ""))


#  ------------------------------------------------------------------------

# wait for reading url link function
# movie.data <- read_excel("\\\\10.10.20.210\\SurveyFile\\2015\\3522\\data\\dinsey_data.xlsx",
#                          sheet = 1, na = "NA")
#
# survey.data <- read_excel("http://isas.panelpower.com.tw/isas45/surveyfile/2015/3522/data/disney_data.xlsx",
#                          sheet = 2, na = "NA")


# Data Manipulation -------------------------------------------------------

movie.data <- movie.data %>%
  # as.numeric
  mutate_each(funs(as.numeric),
              ends_with("No", ignore.case = FALSE)) %>%
  mutate(Budget_mil = as.numeric(Budget_mil)) %>%
  # date format
  mutate(ReleaseDate = as.Date(ReleaseDate, "%Y/%m/%d")) %>%
  # elminate thousands separator
  mutate(WeekendBO = as.numeric(gsub(",","", WeekendBO)),
         FinalBO = as.numeric(gsub(",","", movie.data$FinalBO))) %>%
  # BO ranking
  mutate(classBO = cut(WeekendBO,
                       breaks = c(0,1e6,5e6,8e6,12e6,18e6,25e6,Inf),
                       labels = c("C","B","A--","A-", "A", "AA", "AAA"),
                       right = FALSE,
                       ordered_result = TRUE)) %>%
  # add facter labels of var: `Country`
  mutate(Country = factor(Country,
                          levels=0:2,
                          labels=c("USA & Europe",
                                   "Taiwan",
                                   "China, Japan, others"))) %>%
  # Genre
  mutate(D_Gen8 = factor(D_Gen8,
                         levels = 1:8,
                         labels = c("Crime, Action",
                                    "Thriller, Horror",
                                    "Sci-Fi, Mystery, Adventure",
                                    "Drama, Romance",
                                    "Comedy",
                                    "Musical",
                                    "Documentary",
                                    "Animation"))) %>%
  # vacation
  mutate(vacation = factor(vacation,
                           levels = 0:3,
                           labels = c("non-vacation",
                                      "Chinese New Year",
                                      "3-day vacation",
                                      "summer vacation"))) %>%
  # arrange by release date
  arrange(desc(ReleaseDate), MovieID)



survey.data <- survey.data %>%
  # only extract "taipei" data
  filter(Area=="TP") %>%
  # eliminate duplicate column
  select(c(-EOLWeek, -ChiTitle)) %>%
  # as.numeric
  mutate_each(funs(as.numeric),
              matches("WeekNo|Aware|Interest|DI|FirstChoice", ignore.case = FALSE))


# Combine data-------------------------------------------------------------------

merge.data <- dplyr::left_join(survey.data, movie.data, by="MovieID") %>%
  # mutate wk_before_release
  mutate(wk_before_release = as.integer(WeekNo - ReleaseWkNo)) %>%
  # drop unused factor levels
  droplevels() %>%
  # arrange by release date
  arrange(desc(ReleaseDate), MovieID)


merge.data_1 <- merge.data%>%
  # extract week -1 data
  dplyr::filter(wk_before_release == -1)



# Produce Shiny Data --------------------------------------------------------

## ===== basic.tab =====
# variables for basic.tab
movie.tab <- movie.data %>%
  # select variables
  select(-StartWeekNo, -ReleaseWkNo,-Budget_mil)



## ===== strata.tab =====
# variables for strata.tab
vars.strata <- c(
  "MovieID","ChiTitle", "EngTitle","classBO", "note","ReleaseDate","Country","D_Gen8",
  "Aware", "Aware_Male", "Aware_Female",
  "Aware_15_19age", "Aware_20_24age", "Aware_25_29age", "Aware_30_34age",
  "Aware_35_39age", "Aware_40_44age", "DI", "DI_Male", "DI_Female", "DI_15_19age",
  "DI_20_24age", "DI_25_29age", "DI_30_34age", "DI_35_39age", "DI_40_44age",
  "Interest", "Interest_Male", "Interest_Female", "Interest_15_19age", "Interest_20_24age",
  "Interest_25_29age", "Interest_30_34age", "Interest_35_39age",
  "Interest_40_44age", "FirstChoice", "FirstChoice_Male", "FirstChoice_Female",
  "FirstChoice_15_19age", "FirstChoice_20_24age", "FirstChoice_25_29age",
  "FirstChoice_30_34age", "FirstChoice_35_39age", "FirstChoice_40_44age"
)
strata.tab <- merge.data_1 %>%
  dplyr::select_(.dots = unique(vars.strata))

# names(strata.tab) %>% anyDuplicated


