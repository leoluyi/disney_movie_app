tabPanel("DataTable",
         pageWithSidebar(
           headerPanel('Movie Library'),
           sidebarPanel(
             width=3,

             passwordInput(inputId = "password_input",
                           label = "Please enter password",
                           value = ""),

             selectizeInput('EngTitle',
                            label='English Title: ',
                            choices=unique(movie.data$EngTitle),
                            multiple=TRUE,
                            options=list(
                              placeholder = '',
                              plugins = I("['remove_button']"))),
             selectizeInput('ChiTitle',
                            label='Chinese Title: ',
                            choices=unique(movie.data$ChiTitle),
                            multiple=TRUE,
                            options=list(
                              placeholder = '',
                              plugins = I("['remove_button']"))),

             conditionalPanel(
               condition = "input.tabs1 == 'Strata Table'",
               checkboxGroupInput('show_vars',
                                  'Columns in Strata table to show:',
                                  c("Awareness"="Aware",
                                    "Interest"="Interest",
                                    "DI"="DI",
                                    "First Choice"="FirstChoice"))
             ),

             helpText('For the strata data, we can select
                      variables to show in the table'),

             dateInput("date_from",
                       label="Release Date from: ",
                       value="2011-11-01",
                       language="en"),

             dateInput("date_to",
                       label="Release Date to: ",
                       value=Sys.Date()+60,
                       language="en"),

             selectInput("class",
                         "classBO:",
                         c("All",
                           unique(as.character(levels(movie.data$classBO)))),
                         selected ="All",
                         multiple=TRUE,
                         selectize=FALSE),
             selectInput("gen",
                         "Genre:",
                         c("All",
                           unique(as.character(levels(movie.data$D_Gen8))))),
             selectInput("country",
                         "Country:",
                         c("All",
                           unique(as.character(levels(movie.data$Country))))),
             textInput("note_text_input",
                       label = "Notes to filter",
                       value = "")
           ),

           mainPanel(
             tabsetPanel(id ="tabs1",
                         tabPanel('Movies',
                                  fluidRow(
                                    DT::dataTableOutput('ex1')
                                  ),
                                  fluidRow(
                                    h4("Summary"),
                                    verbatimTextOutput("summary1")
                                  )
                         ),
                         tabPanel('Strata Table',
                                  fluidRow(
                                    DT::dataTableOutput('ex2')
                                  ),
                                  fluidRow(
                                    h4("Summary"),
                                    verbatimTextOutput("summary2")
                                  )
                         )
             )
           )
         )
)
