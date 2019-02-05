
header <- dashboardHeader(title = "Symptoms Length Analysis", titleWidth = 320)

sidebar <- dashboardSidebar(
  width = 150,
  sidebarMenu(
    menuItem("Home", tabName = "menu", icon = icon("home")),
    menuItem("Input Table", tabName = "Table", icon = icon("book-open")),
    menuItem("Mean/Sd", tabName = "Mean", icon = icon("calculator")),
    menuItem("Anova", tabName = "Anova", icon = icon("calculator")),
    menuItem("ACP", tabName = "ACP", icon = icon("calculator")),
    menuItem("Heatmap/Cluster", tabName = "Heatmap", icon = icon("eye")),
    menuItem("Heatmap/Visu", tabName = "Heatmap2", icon = icon("eye")),
    menuItem("Visualization", tabName = "Visu", icon = icon("eye")),
    menuItem("Evolution", tabName = "Evolution", icon = icon("eye"))
  )
)

body <- dashboardBody(
  #includeCSS('www/styles.css'),
  useShinyjs(),
  useShinyFeedback(),
  tabItems(
    tabItem(
      tabName ="menu",
      fluidRow(
        box(width = 12,
          HTML("
            <h2 align='center'>Symptoms Length Analysis</h2>
            <hr style='height: 3px; color: #FF4000; background-color: #FF4000; width: 50%; border: none;'>
            ")
        ),
        box(width = 12,
          HTML("
              <p>
                <h3>HOW DO YOU USE THIS APP ?</h3><br>
                There is a lot of tab you can use:<br>
                <ul>
                  <li><b>Home:</b> That's this page !</li>
                  <li><b>Input table:</b> </li>
                  <li><b>Mean/Sd:</b></li>
                  <li><b>Anova:</b></li>
                  <li><b>ACP:</b></li>
                  <li><b>Heatmap/Cluster:</b></li>
                  <li><b>Heatmap/Visu:</b></li>
                  <li><b>Visualization:</b></li>
                  <li><b>Evolution:</b></li>
                </ul>
              </p>
          ")
        ),
        # box(width = 6,
        #     HTML("
        #       <p>
        #         
        #       </p>
        #   ")
        # ),
        box(width = 12,
            HTML("
              <p align='center'>
                <u>contacts:</u><br><br>
                Aurore Comte - <a href='mailto:aurore.comte@ird.fr'>aurore.comte@ird.fr</a>
              </p>

              <footer align='right'>
                <a href='http://www.umr-ipme.ird.fr'><img  style = 'width: 5%;' src='IPME.jpg'></a>
                &nbsp; &nbsp; &nbsp; 
                <a href='https://www.ird.fr'><img style = 'width:5%;' src='logo_ird.png'></a><br>
              </footer>
          ")
        )
      )
    ),
    tabItem(
      tabName ="Table",
      fluidRow(
        box(
          fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")) %>% 
                 helper(icon = "question",
                        type = "markdown",
                        content = "file1"),
          pickerInput(inputId='responseVar0', label ='Choose the response variable', ""),
          checkboxInput("header", "Header", TRUE)
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
      ),
      fluidRow(
        box(width = 12,
            verbatimTextOutput(outputId = "ShapiroWilk")
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
            verbatimTextOutput(outputId = "anov")
        ),
        fluidRow(
          box(width = 12, 
            plotOutput(outputId = "anovplot", height = "1000px")
          )
        )
      )
    ),
    tabItem(
      tabName = "ACP",
      fluidRow(
        box(width = 12,
            pickerInput(inputId='respacp', label ='Choose the response variable', ""),
            column(width = 6,
              pickerInput(inputId='individual', label ='individuals', "")
            ),
            column(width = 6,
              pickerInput(inputId='variable', label ='variables', "")
            ),
            column(width = 3,
              checkboxInput("reduct", "reduct variable", FALSE)
            ),
            column(width = 3,
              checkboxInput("center", "center variable", FALSE)
            ),
            column(width = 6,
              pickerInput(inputId='axis', label ='Number of axis', selected = 2, choices = c(1,2,3,4,5))
            )
        )
      ),
      fluidRow(
        box(width = 6,
            plotOutput(outputId = "indPlot", height = "600px")
        ),
        box(width = 6,
            plotOutput(outputId = "varPlot", height = "600px")
        )
      ),
      fluidRow(
        box(width = 6,
            plotOutput(outputId = "vpPlot", height = "600px")
        ),
        box(width = 6,
            plotOutput(outputId = "bothPlot", height = "600px")
        )
      )
      # ,
      # fluidRow(
      #   box(width = 12,
      #     downloadButton("downloadRMD", "Download Analysis")
      #   )
      # )
    ),
    tabItem(
      tabName = "Heatmap",
        fluidRow(
          box(width = 12,
             pickerInput(inputId='responseVarHeat', label ='Choose the response variable', ""),
             pickerInput(inputId='factorH1', label ='Choose the first factor', ""),
             pickerInput(inputId='factorH2', label ='Choose the second factor', ""),
             
             column(width=3,
                HTML("Clusterisation : ")
             ),
             column(width=3,
                checkboxInput("column", "col", TRUE)
             ),
             column(width=3,
                checkboxInput("row", "row", TRUE)
             )  
          )
        ),
      fluidRow(
        box(width = 12,
          plotOutput(outputId = "heatplot", height = "700px")
        )
      ),
      fluidRow(
        box(width = 12,
           sliderInput(inputId="thresSR", label = "Threshold of sensibility/resistance", value = 12, min=0, max=20, step=1)
          )
        ),
      fluidRow(
        box(width = 12,
            column(width = 6,
                HTML("
                 <br>
                 <p align='left'>
                 <img src='legendSR.png' width='20%' height='20%'>
                 </p>
                ")
            ),
            column(width = 12,
              plotOutput(outputId = "heatplotSR", height = "700px")
            )
        )
      )
    ),
    tabItem(
      tabName = "Heatmap2",
      fluidRow(
        box(width = 12,
            pickerInput(inputId='responseVarHeat2', label ='Choose the response variable', ""),
            pickerInput(inputId='factorH12', label ='Choose the first factor', ""),
            pickerInput(inputId='factorH22', label ='Choose the second factor', ""),
            pickerInput(inputId='factorH32', label ='Choose the third factor', "") 
        )
      ),
      fluidRow(
        box(width = 12,
            plotOutput(outputId = "heatplot2", height = "700px")
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
          plotOutput(outputId = "PrettyG", height = "1000px")
        )
      )
    ),
    tabItem(
      tabName = "Evolution",
        fluidRow(
          box(width = 12,
            column(width = 6,
              pickerInput(inputId='responseVarT', label ='Choose the response variable (y)', "")
            ),
            column(width = 3,
              pickerInput(inputId='TimeFactor', label ='Choose the factor 1 (x)', "")
            ),
            column(width = 3,
              radioButtons("Time", "Is factor1 a Time factor (ex : 10/02/2018)?", c("no", "dmy", "ymd"), selected="no")
            ),
            column(width = 4,
              pickerInput(inputId='factorT2', label ='Choose the factor2 (grid x)', "")
            ),
            column(width = 4,
              pickerInput(inputId='factorT3', label ='Choose the factor3 (grid y)', "")
            ),
            column(width = 4,
              pickerInput(inputId='factorT4', label ='Choose the factor4 (z)', "")
            )
        )
      ),
      fluidRow(
        box(width = 12,
          plotOutput(outputId = "TimePlot", height = "1000px")  
        )
      )
    )
  )
)

shinyUI(
  dashboardPage(title = "symptom", skin = "yellow", header, sidebar, body)
)

