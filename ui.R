# ui.R

source("external/serverHead.R", local=TRUE, encoding="UTF-8")

# ----------------------------------------------------------------------------

library(shiny)
library(shinythemes)


shinyUI(navbarPage(
  title = "[EOLembrain] Movie Library Tools",
  windowTitle = "[EOLembrain] Movie Library Tools",
  theme = shinytheme("united"),
  
  # source tabPanel.DataTable
  source("external/tabPanel.DataTable.R", local=TRUE, encoding="UTF-8")$value,
  
  # source tabPanel.PredicitonModel
  source("external/tabPanel.PredicitonModel.R", local=TRUE, encoding="UTF-8")$value
))

