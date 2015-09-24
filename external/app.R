# source reactive expressions
source("external/appSourceFiles/reactives.R",local=T)

# Primary outputs ---------------------------------------------------------

output$ex1 <- DT::renderDataTable({
  if (input$password_input == password) {
    action = dataTableAjax(session, movie.tbl.format())
    DT::datatable(movie.tbl.format(),
                  options = list(pageLength = 10, searching = TRUE,
                                 ajax = list(url = action)))
  } else NULL
})


output$ex2 <- DT::renderDataTable({
  if (input$password_input == password) {
    action = dataTableAjax(session, strata.tbl())
    DT::datatable(strata.tbl(),
                  options = list(pageLength = 10, searching = FALSE,
                                 ajax = list(url = action)))
  } else NULL
})


output$summary1<- renderPrint({
  if (input$password_input == password) {
    dataset <- movie.tbl() %>%
      select(classBO, Country, D_Gen8)
    Hmisc::describe(dataset)
  } else NULL
})

output$summary2<- renderPrint({
  if (input$password_input == password) {
    dataset <- strata.tbl() %>%
      select(classBO, Country, D_Gen8,
             matches("Aware|DI|FirstChoice"))
    Hmisc::describe(dataset)
  } else NULL
})



output$pred_output <- renderTable({
  if (input$password_input == password) {
    pred_out <- pred_data()
    pred_out <- pred_out %>% arrange(desc(ReleaseDate))
    pred_out$ReleaseDate <- date_format()(pred_out$ReleaseDate) # date format
    pred_out$WeekendBO <- dollar_format()(pred_out$WeekendBO)  # dollar format
    # percentage format
    for (j in which(names(pred_out) %in% c("C","B","A--","A-","A","AA","AAA"))) {
      pred_out[j] <- scales::percent(pred_out[,j])
    }
    pred_out
  } else NULL
})


output$output_benchmark_plot <- renderPlot({

  if (!is.null(movie_benchmark())) {
    movie_benchmark <- movie_benchmark()
    p <- movie_benchmark %>%
      dplyr::filter(wk_before_release %in% 0:-8) %>%
      dplyr::select(EngTitle, ChiTitle, wk_before_release, DI, Aware) %>%
      tidyr::gather(key = variable, value = value, DI, Aware, na.rm = T) %>%
      # filter(grepl("少女時代|蟻人|變形金剛|環太平洋", ChiTitle)) %>%
      ggplot() +
      aes(x = wk_before_release, y = value, colour=EngTitle) +
      facet_grid( ~ variable, scales = "free_y") +
      geom_line(aes(group=EngTitle), size=1.5, alpha=0.7) +
      scale_x_continuous("Week-before-release", limits=c(-8, 0), breaks=(-8:0)) +
      scale_y_continuous(labels = scales::percent) +
      expand_limits(y=0) +
      ggtitle("Benchmark") +
      guides(colour = guide_legend(nrow = 2)) +
      theme(legend.position="top",
            text = element_text(size = 16)
      )

    print(p)
    # p2 <- direct.label(p,list("last.polygons", cex=1))
  } else NULL
},
width=800,
height=600
)


output$output_benchmark_table <- renderTable({

  if (!is.null(movie_benchmark())) {
    movie_benchmark <- movie_benchmark()
    movie_benchmark %>%
      dplyr::select(EngTitle, ChiTitle, wk_before_release, DI, Aware, FirstChoice) %>%
      mutate(wk_before_release=as.integer(wk_before_release)) %>%
      mutate(DI = scales::percent(DI),
             Aware = scales::percent(Aware),
             FirstChoice = scales::percent(FirstChoice)
      )
  }
})
