library(shiny)

fluidPage(
  titlePanel("Naive Bayes"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
								 'text/comma-separated-values,text/plain', 
								 '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(6,
               textInput("myQuery", NULL, width = "450px" ,placeholder = "Seperate attributes with comma")
        ),
        column(2,
               actionButton("confirm","Sumit Query")
        )
      ),
      tableOutput('contents')
    )
  ),
  verbatimTextOutput("NaiveBayesAns")
)
