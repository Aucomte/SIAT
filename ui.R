
header <- dashboardHeader(title = "Symptoms length Analysis", titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 150,
  sidebarMenu(
    menuItem("Table", tabName = "tabHome", icon = icon("book-open")),
    menuItem("Mean/Sd", tabName = "Mean", icon = icon("calculator")),
    menuItem("Anova", tabName = "Anova", icon = icon("calculator")),
    menuItem("ACP", tabName = "ACP", icon = icon("calculator")),
    menuItem("Heatmap", tabName = "Heatmap", icon = icon("eye")),
    menuItem("Visu", tabName = "Visu", icon = icon("eye")),
    menuItem("Evolution", tabName = "Evol", icon = icon("eye")),
    menuItem("Debug", tabName = "tabDebug", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  #includeCSS('www/styles.css'),
  useShinyjs(),
  useShinyFeedback(),
  tabItems(
    tabItem(
      tabName ="tabHome",
      fluidRow(
        box(
          fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")) %>% 
                 helper(icon = "question",
                        type = "markdown",
                        content = "file1"),
          checkboxInput("header", "Header", TRUE),
          HTML("Is there a time factor (day/month/year) in your dataset?"),
          pickerInput(inputId='TimeFactor', label ='Time factor', "")
        ),
        box(
          radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            ';'),
          radioButtons('dec', 'decimal',
                           c(Comma=',',
                             Dot='.'),
                            ',')
        )
      ),
      fluidRow(
        box(width = 12,
          DTOutput(outputId = "DataSet")
        )
      )
    ),
    
    tabItem(
      tabName ="Mean",
      fluidRow(
        box(width = 12,
          pickerInput(inputId='responseVar1', label ='Choose the response variable', ""),
          pickerInput(inputId='factors1', label ='Choose the factors', "", multiple = TRUE)
        )
      ),
      fluidRow(
        box(width = 12,
          DTOutput(outputId = "moyenne")
        )
      )
    ),
    
    tabItem(
      tabName ="Anova",
      fluidRow(
        box(width = 12,
            pickerInput(inputId='responseVar', label ='Choose the response variable', ""),
            pickerInput(inputId='factors', label ='Choose the factors', "", multiple = TRUE)
            
        )
      ),
      fluidRow(
        box(width = 12,
            verbatimTextOutput(outputId = "anov"),
            plotOutput(outputId = "anovplot")
        )
      )
    ),
    tabItem(
      tabName = "ACP"
    ),
    tabItem(
      tabName = "Heatmap",
        fluidRow(
          box(width = 12,
             pickerInput(inputId='responseVarHeat', label ='Choose the response variable', ""),
             pickerInput(inputId='factorH1', label ='Choose the first factor', ""),
             pickerInput(inputId='factorH2', label ='Choose the second factor', ""),
             pickerInput(inputId='factorH3', label ='Choose the third factor', "")
          )
        ),
      fluidRow(
        box(width = 12,
            plotOutput(outputId = "heatplot")
        )
      ),
      fluidRow(
        box(width = 12,
             sliderInput(inputId="thresSR", label = "Treshold of sensibility/resistance", value = 12, min=0, max=20, step=1)
          )
        ),
      fluidRow(
        box(width = 12,
           plotOutput(outputId = "heatplotSR")
        )
      )
    ),
    tabItem(
      tabName = "Visu",
      fluidRow(
        box(width=12,
           pickerInput(inputId='responseVarPG', label ='Choose the response variable', ""),
           pickerInput(inputId='factorPG1', label ='Choose the first factor', ""),
           pickerInput(inputId='factorPG2', label ='Choose the second factor', ""),
           pickerInput(inputId='factorPG3', label ='Choose the third factor', "")
        )
      ),
      fluidRow(
        box(width = 12,
            plotOutput(outputId = "PrettyG")
        )
      )
    ),
    tabItem(
      tabName = "Evolution",
        fluidRow(
          box(width = 12,
             pickerInput(inputId='responseVarT', label ='Choose the response variable', ""),
             pickerInput(inputId='factorT1', label ='Choose the first factor', ""),
             pickerInput(inputId='factorT2', label ='Choose the second factor', ""),
             pickerInput(inputId='factorT3', label ='Choose the third factor', "")
        )
      ),
      fluidRow(
        box(width = 12,
          plotOutput(outputId = "TimePlot")  
        )
      )
    ),
    tabItem(
      tabName = "tabDebug",
      h1("DEBUG"),
      verbatimTextOutput("debug")
    )
  )
)

shinyUI(
  dashboardPage(title="symptom", skin = "yellow", header, sidebar, body)
)

