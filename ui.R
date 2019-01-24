
header <- dashboardHeader(title = "Symptoms length Analysis", titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 150,
  sidebarMenu(
    menuItem("Table", tabName = "tabHome", icon = icon("home")),
    # menuItem("Calibration", tabName = "tabCalibration", icon = icon("balance-scale")),
    # menuItem("Analysis", tabName = "tabAnalysis", icon = icon("pagelines")),
    # menuItem("Edit", tabName = "tabEdit", icon = icon("edit")),
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
                            ",")
        )
      ),
      fluidRow(
        box(width = 12,
          DTOutput(outputId = "DataSet")
        )
      )
    ),
    tabItem(
      tabName = "tabDebug",
      h1("DEBUG"),
      verbatimTextOutput("debug"),
      shinythemes::themeSelector()  # <--- Add this somewhere in the UI
    )
  )
)

shinyUI(
  dashboardPage(title="symptom", skin = "yellow", header, sidebar, body)
)

  # HTML("<p align='left'>
  #        <img src='logo_ird.png' width='10%' height='10%'>
  #      </p>"),
  # navbarPage("Symptoms length Analysis", id="nav", selected = "Table",
  #      tabPanel("Table",
  #          sidebarLayout(
  #             sidebarPanel(
  #                  fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")) %>% 
  #                    helper(icon = "question",
  #                           type = "markdown",
  #                           content = "file1"),
  #                  checkboxInput("header", "Header", TRUE),
  #                  radioButtons('sep', 'Separator',
  #                               c(Comma=',',
  #                                 Semicolon=';',
  #                                 Tab='\t'),
  #                               ';'),
  #                 radioButtons('dec', 'decimal',
  #                              c(Comma=',',
  #                                Dot='.'),
  #                 ","),
  #                 HTML("Is there a time factor (day/month/year) in your dataset?"),
  #                 pickerInput(inputId='TimeFactor', label ='Time factor', "")
  #             ),
  #             mainPanel(
  #               DTOutput(outputId = "DataSet")
  #             )
  #          )
  #      ),
  #      tabPanel("Mean/SD",
  #           sidebarLayout(
  #             sidebarPanel(
  #                 pickerInput(inputId='responseVar1', label ='Choose the response variable', ""),
  #                 pickerInput(inputId='factors1', label ='Choose the factors', "", multiple = TRUE),
  #                 actionButton(inputId="CalculationMean",label="Calculate")
  #                ),
  #              mainPanel(
  #                DTOutput(outputId = "moyenne")
  #              )
  #           )
  #      ),
  #       tabPanel("Anova",
  #         sidebarLayout(
  #           sidebarPanel(
  #             pickerInput(inputId='responseVar', label ='Choose the response variable', ""),
  #             pickerInput(inputId='factors', label ='Choose the factors', "", multiple = TRUE),
  #             actionButton(inputId="CalculationAnov",label="Calculate")
  #           ),
  #           mainPanel(
  #             h3('ANOVA Table'),
  #             verbatimTextOutput(outputId = "anov"),
  #             h3('boxplot'),
  #             plotOutput(outputId = "anovplot")
  #           )
  #         )
  #       ),
  #     tabPanel("Statistics"),
  #     tabPanel("Heatmaps",
  #        sidebarLayout(
  #          sidebarPanel(
  #            pickerInput(inputId='responseVarHeat', label ='Choose the response variable', ""),
  #            pickerInput(inputId='factorH1', label ='Choose the first factor', ""),
  #            pickerInput(inputId='factorH2', label ='Choose the second factor', ""),
  #            pickerInput(inputId='factorH3', label ='Choose the third factor', ""),
  #            HTML("<br><br><br>"),
  #            sliderInput(inputId="thresSR", label = "Treshold of sensibility/resistance", value = 12, min=0, max=20, step=1),
  #            actionButton(inputId="CalculationHeat",label="Calculate")
  #          ),
  #          mainPanel(
  #            plotOutput(outputId = "heatplot"),
  #            HTML("<br>"),
  #            h3("resistance/sensibility"),
  #            plotOutput(outputId = "heatplotSR")
  #          )
  #        )
  #     ),
  #     tabPanel("Pretty Graph",
  #        sidebarLayout(
  #          sidebarPanel(
  #            pickerInput(inputId='responseVarPG', label ='Choose the response variable', ""),
  #            pickerInput(inputId='factorPG1', label ='Choose the first factor', ""),
  #            pickerInput(inputId='factorPG2', label ='Choose the second factor', ""),
  #            pickerInput(inputId='factorPG3', label ='Choose the third factor', ""),
  #            actionButton(inputId="CalculationPG", label="Calculate")
  #          ),
  #        mainPanel(
  #          plotOutput(outputId = "PrettyG")
  #        )
  #       )
  #     ),
  #     tabPanel("EvolutionDuringTime",
  #      sidebarLayout(
  #        sidebarPanel(
  #          pickerInput(inputId='responseVarT', label ='Choose the response variable', ""),
  #          pickerInput(inputId='factorT1', label ='Choose the first factor', ""),
  #          pickerInput(inputId='factorT2', label ='Choose the second factor', ""),
  #          pickerInput(inputId='factorT3', label ='Choose the third factor', ""),
  #          actionButton(inputId="CalculationT", label="Calculate")
  #        ),
  #        mainPanel(
  #          verbatimTextOutput("debug"),
  #          plotOutput(outputId = "TimePlot")
  #        )
  #      )
  #     )
  #   )
  # )
