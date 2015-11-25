source("external/serverHead.R", local=TRUE, encoding="UTF-8")
password <- "bviPass01"
# ----------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  source("external/app.R", local=TRUE)
})



