
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
            <h1 align='center'>Symptoms Length Analysis</h1>
            <hr style='height: 3px; color: #FF4000; background-color: #FF4000; width: 50%; border: none;'>
            ")
        ),
        box(width = 12,
            HTML("
              <h4>
              This is an application web developped in order to visualize and analyze the mesure of symptom intensity as quantitative variable in function of several experimental factors.
              </h4>
            ")
        ),
        box(width = 12,
          HTML("
              <p>
                <h3>Comment utiliser cette application ?</h3><br>
                
                <p>
                  &#9888;<br>
                  Avant toute chose vous devez aller dans l'onglet Input Table et upload votre jeu de données en format CSV (format long). Reglez les parametres Separator, response variable et decimal. Une fois que la datatable s'affiche correctement et qu'il n'y a plus de message d'erreurs vous pouvez commencer à aller sur les autres onglets!
                  <br>&#9888;
                </p>
              
                Description des differents onglets:<br><br>
                <ul>
                  <li><b>Home :</b> Il s'agit de la page sur laquelle vous êtes déjà. </li><br>
                  <li><b>Input table :</b> Onglet dans lequel on doit upload le jeu de données avec lequel effectuer l'analyse. Les données peuvent ensuite être filtrées en fonction des valeurs dans les colonnes. Les analyses dans les autres onglets seront effectuées sur le jeu de données filtré. Le jeu de données filtré peut également être re-téléchargé. </li><br>
                  <li><b>Mean/Sd :</b> Cet onglet permet d'explorer le jeu de données en calculant le nombre de points, la moyenne et l'écartype de la variable réponse quantitative choisie en fonction d'une variable ou d'un groupe de variables.</li><br>
                  <li><b>Anova :</b> Cet onglet permet de faire des statistiques permettant de comparer les moyennes des longueurs de lésion entre différents facteurs de variabilité. L'objectif est de savoir si la variable étudiée a une influence significative sur la variabilité de la distribution. L'utilisateur a la possibilité dd'effectuer une ANOVA sur un facteur ou sur deux facteurs de variabilité maximum. </li><br>
                  <li><b>ACP :</b> Analyse en Composantes Principales. Méthode  factorielle  de  réduction  de  dimension. L'utilisateur peut choisir les individus et les variables et l'ACP va permettre de visualiser les représentations graphiques optimales dans l'espace des individus et des variables. </li><br>
                  <li><b>Heatmap/Cluster :</b> Le but de cet onglet est de visualiser sous forme d'une heatmap les valeurs moyennes des longueurs de liaison en fonction de deux variables choisies. Cela permet ensuite de clusteriser ces variables (par exemple clusteriser les souches). Une seconde représentation est présente dans cet onglet et permet</li><br>
                  <li><b>Heatmap/Visu :</b></li><br>
                  <li><b>Visualization :</b> Le but de cette page est de faire une sortie graphique permettant de visualiser la distribution des longueurs de lésions en fonction de plusieurs facteurs choisis.</li><br>
                  <li><b>Evolution :</b> Le but de cette page est de faire une sortie graphique montrant l'évolution des valeurs de longueur de lésion en fonction du temps (lorsque les analyse s'étalent sur plusieurs expériences). La visualisation peut néanmoins se faire également en fonction d'un parametre non temporel. </li><br>
                </ul>
              </p>
          ")
        ),
        box(width = 12,
            HTML("
              <footer align='right'>
              <p align='left'>
                <u>contacts</u> : &nbsp;&nbsp;
                Aurore Comte - <a href='mailto:aurore.comte@ird.fr'>aurore.comte@ird.fr</a>
              </p>
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
        box(width=12,
          column(width = 6,
           downloadButton("downloadData", label = "Download a test file"),
           bsPopover("downloadData", "Example Data Set", content = "A CSV file. Separator is Semicolon. Decimal is Comma. The response variable is resultats. There is a Time factor dmy (date) and 3 other qualitative variables (cellules, varietes, souches)", placement = "bottom", trigger = "hover", options = NULL),
           HTML("<br><br>"),
            fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")) %>%
              helper(icon = "question",
                     type = "markdown",
                     content = "file1"),
            pickerInput(inputId='responseVar0', label ='Choose the response variable', "")
           ),
          column(width = 6,
            column(width = 3,
              radioButtons('sep', 'Separator',
                                c(Comma=',',
                                  Semicolon=';',
                                  Tab='\t'),
                                ';')
              ),
            column(width = 3,
              radioButtons('dec', 'decimal',
                               c(Comma=',',
                                 Dot='.'),
                                ',')
            )
          )
        )
      ),
      fluidRow(
        box(width=12,
          column(width = 6,
              HTML("
                  <b><u>Data Validation</u>:</b>
              "),
              verbatimTextOutput(outputId = "CheckPoint")
          ),
          column(width = 6,
              HTML("
                 <b><u>Normality test : Shapiro-Wilk</u>:</b>
                "),
              verbatimTextOutput(outputId = "ShapiroWilk")
          )
        )
      ),
      fluidRow(
        box(width = 12,
          DTOutput(outputId = "DataSet")
        )
      ),
      fluidRow(
        box(width = 12,
            DTOutput(outputId = "filtered_DataSet")
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
        )
      ),
      fluidRow(
        box(width = 12, 
            plotOutput(outputId = "anovplot", height = "800px") %>% withSpinner(color="#0dc5c1")
        )
      )
      # ,
      # fluidRow(
      #   box(width = 12,
      #     downloadButton("downloadAnov", "Download Anova Plot")
      #   )
      # )
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
            plotOutput(outputId = "indPlot", height = "600px") %>% withSpinner(color="#0dc5c1")
        ),
        box(width = 6,
            plotOutput(outputId = "varPlot", height = "600px") %>% withSpinner(color="#0dc5c1")
        )
      ),
      fluidRow(
        box(width = 6,
            plotOutput(outputId = "vpPlot", height = "600px") %>% withSpinner(color="#0dc5c1")
        ),
        box(width = 6,
            plotOutput(outputId = "bothPlot", height = "600px") %>% withSpinner(color="#0dc5c1")
        )
      )
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
          plotOutput(outputId = "heatplot", height = "700px") %>% withSpinner(color="#0dc5c1")
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
              plotOutput(outputId = "heatplotSR", height = "700px") %>% withSpinner(color="#0dc5c1")
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
            plotOutput(outputId = "heatplot2", height = "700px") %>% withSpinner(color="#0dc5c1")
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
          plotOutput(outputId = "PrettyG", height = "1000px") %>% withSpinner(color="#0dc5c1")
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
          plotOutput(outputId = "TimePlot", height = "1000px") %>% withSpinner(color="#0dc5c1")
        )  
      )
    )
  )
)

shinyUI(
  dashboardPage(title = "symptom", skin = "yellow", header, sidebar, body)
)

