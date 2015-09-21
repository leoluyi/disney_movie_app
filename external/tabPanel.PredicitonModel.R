tabPanel('Prediciton Model',
         pageWithSidebar(
           headerPanel('Movie Box Office Prediction'),
           sidebarPanel(width = 3,
                        
                        dateRangeInput("dates",
                                       label = h3("Release Date"),
                                       start=Sys.Date(), 
                                       end=Sys.Date()+7
                        ),
                        
                        selectInput("ChiTitle_var",
                                    label = "Chinese Title",
                                    choices=NULL,
                                    selected = NULL,
                                    multiple=TRUE, 
                                    selectize=TRUE)
           ),
           mainPanel(
             tableOutput("pred_output")    
           )
         )
)
