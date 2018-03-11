library(shiny)

shinyUI(navbarPage("Individual Project",
  tabPanel("Model Prediction Plot",
  h3("how fertility rate associate with violity crime rate controlled by average income per person?"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("economic", label = "average income / person: (to cancontrol average income/person in three different level)", 
                         choices = list("low" = 1, "Medium" = 2, "high" = 3),
                         selected = 1),
      sliderInput("burnrate", "fertility rate: (to control the fertility rate to which range you want)",
                  min = 0, max = 10,
                  value = c(2,5)),
      sliderInput("observation", "samplesize:",
                  min = 0, max = 10000,
                  value = 500, step = 10),
      radioButtons("rawdata", "Raw data output: (to control if you want to plot raw data out to caompare)",
                   choices = list("yes" = 1, "no" = 2))
    ), 
    mainPanel(plotOutput("distPlot"))
  )),
  tabPanel("Summary",
           verbatimTextOutput("summary")
  ),
  navbarMenu("More",
             tabPanel("Table",
                      DT::dataTableOutput("table") ),
            tabPanel("About",
                    h4("Crime rate association with fertility rate in Taiwan"),
                    h5("The data included the crime, fertility, economic nd the education. Note that the data were recorded in the 2000 ~ 2008(since when the ferility in Taiwan drop significantly)."),
                    br(),
                    h5("- Crime : number of violent crimes per 10,000 people."),
                    h5("- fertility : average number of children born per year for every houndred fertile woman (15-49 years)."),
                    h5("- economic : average disposable income per person per year."),
                    h5("- education : percentage of population over people who are aged 15 years-old and above by the education level over and above college."),
                    br(),
                    br(),
                    tags$small(
                      "all datas are collected from ",
                      a(href="https://www.stat.gov.tw/mp.asp?mp=4",
                        "National Statistic, R.O.C(Taiwn)")
                    )
                    )
            )
))