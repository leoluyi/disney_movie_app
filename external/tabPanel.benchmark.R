tabPanel("Benchmark",
         pageWithSidebar(
           headerPanel('Movie Indexes Benchmark'),

           sidebarPanel(
             selectizeInput('EngTitle_ben',
                            label='English Title: ',
                            choices=unique(movie.data$EngTitle),
                            multiple=TRUE,
                            options=list(
                              placeholder = '',
                              plugins = I("['remove_button']"))),
             selectizeInput('ChiTitle_ben',
                            label='Chinese Title: ',
                            choices=unique(movie.data$ChiTitle),
                            multiple=TRUE,
                            options=list(
                              placeholder = '',
                              plugins = I("['remove_button']"))),

             p(actionButton("goButton", "Go!"))
           ),

           mainPanel(
             wellPanel(
               plotOutput("output_benchmark_plot", height = "500px")
               , style = "padding: 5px;"
             ),
             h4("Table"),
             tableOutput("output_benchmark_table")
           )

         )
)
