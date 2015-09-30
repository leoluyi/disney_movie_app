movie_benchmark_plot <- reactive({

  if (!is.null(movie_benchmark())) {
    movie_benchmark <- movie_benchmark()
    p <- movie_benchmark %>%
      dplyr::filter(wk_before_release %in% 0:-8) %>%
      dplyr::select(EngTitle, ChiTitle, wk_before_release, DI, Aware) %>%
      tidyr::gather(key = variable, value = value, DI, Aware, na.rm = T) %>%
      ggplot() +
      aes(x = wk_before_release, y = value, colour=EngTitle) +
      facet_grid( ~ variable, scales = "free_y") +
      geom_line(aes(group=EngTitle), size=1.5, alpha=0.7) +
      geom_point(size = 4) +
      geom_point(colour="white", size = 1.5, show_guide = TRUE) +
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
})
