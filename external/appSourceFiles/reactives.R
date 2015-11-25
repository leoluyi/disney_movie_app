
# movie.tbl ---------------------------------------------------------------

movie.tbl <- reactive({
  if (input$gen != "All") {
    movie.tab <- movie.tab %>% filter(D_Gen8==input$gen)
  }
  if (input$country != "All") {
    movie.tab <- movie.tab %>% filter(Country==input$country)
  }
  if (input$class != "All") {
    movie.tab <- movie.tab %>% filter(classBO %in% input$class)
  }
  #filter title
  if (length(input$ChiTitle) != 0 || length(input$EngTitle) != 0) {
    condition_title <-
      (movie.tab$EngTitle %in% input$EngTitle) |
      (movie.tab$ChiTitle %in% input$ChiTitle)
    movie.tab <- movie.tab %>% filter(condition_title)
  }

  # filter date
  if(!is.null(input$date_from)) {
    movie.tab <- movie.tab %>% filter(ReleaseDate >= as.Date(input$date_from))
  }
  if(!is.null(input$date_to)) {
    movie.tab <- movie.tab %>% filter(ReleaseDate <= as.Date(input$date_to))
  }
  movie.tab <- movie.tab %>% arrange(desc(ReleaseDate))

  if (input$note_text_input != "") {
    movie.tab <- movie.tab %>%
      filter(grepl(input$note_text_input, note, ignore.case=T))
  }


  movie.tab <- movie.tab %>% as.data.frame()
  return(movie.tab)
})


movie.tbl.format <- reactive({
  movie.tbl.temp <- movie.tbl()

  # if (!is.null(movie.tbl()$WeekendBO)) {
  #   movie.tbl.temp <- movie.tbl.temp %>%
  #     mutate(WeekendBO = format(WeekendBO, big.mark = ",")) # thousand mark
  # }
  #
  # if (!is.null(movie.tbl()$FinalBO)) {
  #   movie.tbl.temp <- movie.tbl.temp %>%
  #     mutate(FinalBO = format(FinalBO, big.mark = ",")) # thousand mark
  # }

  movie.tbl.temp
})


# strata.tbl ----------------------------------------------------------------------

strata.tbl <- reactive({
  # Strata: input$show_vars
  strata.tab.temp <- strata.tab

  if (length(input$show_vars) != 0) {
    strata.tab.temp <- strata.tab.temp %>%
      select(matches(varlist(input$show_vars))) %>%
      cbind(strata.tab.temp[1:8],.)
  } else {
    strata.tab.temp <- strata.tab.temp[1:8]
  }

  if (input$gen != "All") {
    strata.tab.temp <- strata.tab.temp %>% filter(D_Gen8==input$gen)
  }
  if (input$country != "All") {
    strata.tab.temp <- strata.tab.temp %>% filter(Country==input$country)
  }
  if (input$class != "All") {
    strata.tab.temp <- strata.tab.temp %>% filter(classBO %in% input$class)
  }
  if (length(input$ChiTitle) != 0) {
    strata.tab.temp <- strata.tab.temp %>% filter(ChiTitle %in% input$ChiTitle)
  }
  if (input$note_text_input != "") {
    note_text_input <- input$note_text_input
    strata.tab.temp <- strata.tab.temp %>%
      filter(grepl(note_text_input, note, ignore.case=TRUE))
  }

  # filter date
  if(!is.null(input$date_from)) {
    strata.tab.temp <- strata.tab.temp %>% filter(ReleaseDate >= as.Date(input$date_from))
  }
  if(!is.null(input$date_to)) {
    strata.tab.temp <- strata.tab.temp %>% filter(ReleaseDate <= as.Date(input$date_to))
  }

  # arrange the order by desc(ReleaseDate)
  strata.tab.temp <- strata.tab.temp %>% arrange(desc(ReleaseDate))

  ## show percentage
  per.var.text <- c(
    "Aware", "Interest", "DI", "Interest_TOP2", "FirstChoice")

  per.var <- strata.tab.temp %>%
    select(matches(varlist(per.var.text))) %>%
    names

  # strata.tab.temp[, per.var] <-
  #   sapply(strata.tab.temp[, per.var], FUN = scales::percent) # percentage

  ## round
  strata.tab.temp[, per.var] <-
    sapply(strata.tab.temp[, per.var], FUN = round, 3)

  strata.tab.temp <- strata.tab.temp %>% as.data.frame()
  strata.tab.temp
})

# randomForest Model -----------------------------------------------------------------------

outVar <- reactive({
  chiTitle <- test.data.NAomit %>%
    filter(ReleaseDate>=input$dates[1] & ReleaseDate<=input$dates[2]) %>%
    select(ChiTitle)
  chiTitle <- chiTitle[[1]]
  chiTitle
})

observe({updateSelectInput(session,
                           inputId="ChiTitle_var",
                           choices = outVar(),
                           selected = outVar())
})

pred_data <- reactive({
  test.result %>% filter(ChiTitle %in% input$ChiTitle_var)
})


# benchmark plot ----------------------------------------------------------

movie_benchmark <-  reactive({
  #filter title
  if (length(input$ChiTitle_ben) != 0 || length(input$EngTitle_ben) != 0) {
    condition_title <-
      (merge.data$EngTitle %in% input$EngTitle_ben) |
      (merge.data$ChiTitle %in% input$ChiTitle_ben)
    merge.data <- merge.data %>% filter(condition_title)
  } else NULL

})


