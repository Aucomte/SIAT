
#header <- dashboardHeader(title = "Symptoms Length Analysis Tool", titleWidth = 380)
header <- dashboardHeader(title = img(style = 'width: 80%;', src = "SLATlogo2.png", class = 'img-responsive'), titleWidth = 500)

sidebar <- dashboardSidebar(
  width = 180,
  sidebarMenu(
    menuItem("Home", tabName = "menu", icon = icon("home")),
    menuItem("Input Table", tabName = "Table", icon = icon("book-open")),
    menuItem("Mean/Sd", tabName = "Mean", icon = icon("calculator")),
    menuItem("Boxplot", tabName = "Visu", icon = icon("eye")),
    menuItem("Barplot", tabName = "barplot", icon = icon("eye")),
    menuItem("Heatmap", tabName = "Heatmap", icon = icon("eye")),
    menuItem("Plot Time Series", tabName = "Evolution", icon = icon("eye")),
    menuItem("Anova", tabName = "Anova", icon = icon("calculator")),
    menuItem("ACP", tabName = "ACP", icon = icon("calculator")),
    menuItem("Categorical Analysis", tabName = "Heatmap2", icon = icon("eye"))
    #,menuItem("Generate Report", tabName = "RMD", icon = icon("book-open"))
  )
)

body <- dashboardBody(
  includeCSS('www/styles.css'),
  useShinyjs(),
  useShinyFeedback(),
  tabItems(
    tabItem( 
      tabName ="menu",
      fluidRow(
        box(class = "titlebox", width = 12,
            withTags(
              div(class = "title", 
               img(style = 'width: 75%;', src = "SLATtransparent.png")
              )
            )
        ),
        box(width = 12, class = "resumebox",
            withTags(
              div(class = "resume",
              "Effortlessly visualize and analyze the mesure of symptom intensity as a quantitative response variable in connection to several experimental factors."
              )
            )
        ),
        box(width = 12,
          withTags(
            div(class = "home",
                h2("Quickly get a sense of what is in your disease assay data."),
                br(),
                p("First of all, go to the 'Input Table' thumbnail to upload a file with your data set. It must be formated in a 'long format' with one row per symptom measurement and columns describing the levels of the experimental factors associated with this numeric value (e.g plant genotype, strain, replicate ID, experiment ID, etc). From there you can use the tools accessible on the left handside menu to filter, aggregate, visualize and transform you data in an intutitive and user-friendly fashion."),
                h3("A short description of the data analysis tools:"),
#### NEED TO REORDER THE ITEMS TO MATCH THE ORDER OF THE THUMBS
                ul(
                  li(b("Home"),": This is page you are currently reading with an overview of SLAT."),
                  br(),
                  li(b("Input table"), ": This is where to upload your data set with values organized in a long or 'tidy' format (https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html). You can provide files containing one of several field and decimal separators. It is then possible to filter your data and export it. Downstream analysis steps with the various tools will be performed on this filtered data."),
                  br(),
                  li(b("Mean/Sd"), ": This enables the computation of standard aggregate values (mean, standard deviation, count of observations) of the quantitative response variable as a function of the levels of one or several experimental factors."),
                  br(),
                  li(b("Anova"), ": Cet onglet permet de faire des statistiques permettant de comparer les moyennes des longueurs de lésion entre différents facteurs de variabilité. L'objectif est de savoir si la variable étudiée a une influence significative sur la variabilité de la distribution. L'utilisateur a la possibilité dd'effectuer une ANOVA sur un facteur ou sur deux facteurs de variabilité maximum."),
                  br(),
                  li(b("ACP"), ": Analyse en Composantes Principales. Méthode  factorielle  de  réduction  de  dimension. L'utilisateur peut choisir les individus et les variables et l'ACP va permettre de visualiser les représentations graphiques optimales dans l'espace des individus et des variables."),
                  br(),
                  li(b("Heatmap/Cluster"), ": Le but de cet onglet est de visualiser sous forme d'une heatmap les valeurs moyennes des longueurs de liaison en fonction de deux variables choisies. Cela permet ensuite de clusteriser ces variables (par exemple clusteriser les souches). Une seconde représentation présente dans cet onglet permet de fixer un seuil de sensibilité / résistance en fonction des longueurs de lésion. Cette représentation offre alors en sortie une heatmap binaire de résistance / sensibilité."),
                  br(),
                  li(b("Categorical Analysis"),": Le but de cet onglet est de visualiser sous forme d'une heatmap les valeurs moyennes des longueurs de liaison en fonction de deux ou trois variables choisies."),
                  br(),
                  li(b("boxplot"), ": Le but de cette page est de faire une sortie graphique permettant de visualiser la distribution des longueurs de lésions en fonction de plusieurs facteurs choisis."),
                  br(),
                  li(b("barplot"), ": Le but de cette page est de faire une sortie graphique permettant de visualiser la distribution des longueurs de lésions en fonction de plusieurs facteurs choisis."),
                  br(),
                  li(b("Plot Time Series"), ": Le but de cette page est de faire une sortie graphique montrant l'évolution des valeurs de longueur de lésion en fonction du temps (lorsque les analyse s'étalent sur plusieurs expériences). La visualisation peut néanmoins se faire également en fonction d'un parametre non temporel.")
                  )
              )
          )
        ),
        box(width = 12,
          withTags(
            footer(align='right',
              p(align='left',
                u("contacts :"), 
                " Aurore Comte - ", a(href='mailto:aurore.comte@ird.fr',"aurore.comte@ird.fr")
              ),
                a(href='http://www.bioinfo.ird.fr', img(style = 'width: 6%;', src='i-trop-longtransparent.png')),
                a(href='http://www.umr-ipme.ird.fr', img(style = 'width: 6%;', src='IPME.jpg')),
                a(href='https://www.ird.fr', img(style = 'width:6%;', src='logo_ird.png'))
            )
          )
        )
      )
    ),
    tabItem(
      tabName ="Table",
      fluidRow(
        box(width=12, class = "box2",
            "Onglet dans lequel on doit upload le jeu de données avec lequel effectuer l'analyse. Reglez les parametres Separator, response variable et decimal.\n Une fois que la datatable s'affiche correctement et qu'il n'y a plus de message d'erreurs vous pouvez commencer à aller sur les autres onglets! Les données peuvent ensuite être filtrées en fonction des valeurs dans les colonnes. Les analyses dans les autres onglets seront effectuées sur le jeu de données filtré. Le jeu de données filtré peut également être re-téléchargé."
        ),
        box(width=12, class = "box1",
          column(width = 6,
           downloadButton("downloadData", label = "Download a test file"),
           bsPopover("downloadData", "Example Data Set", content = "A CSV file: the separator is tab and the decimal is Comma. The response variable is resultats. There is a Time factor dmy (date) and 3 other qualitative variables (cellules, varietes, souches)", placement = "bottom", trigger = "hover", options = NULL),
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
                                c(Semicolon=';',
                                  Tab='\t'),
                                selected = ';')
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
        box(width=12,class = "box1",
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
        box(width=12,class = "box1",
           checkboxInput("logTrans", "logarithmic transformation", FALSE)
          )
      ),
      fluidRow(
        box(width = 12,
          DT::dataTableOutput(outputId = "DataSet")
        )
      ),
      fluidRow(
        box(width = 12,
          DT::dataTableOutput(outputId = "filtered_DataSet")
        )
      )
    ),
    
    tabItem(
      tabName ="Mean",
      fluidRow(
        box(width=12, class = "box2",
            "Cet onglet permet d'explorer le jeu de données en calculant le nombre de points, la moyenne et l'écartype de la variable réponse quantitative choisie en fonction d'une variable ou d'un groupe de variables."
        ),
        box(width = 12,class = "box1",
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
        box(width=12, class = "box2",
            "Cet onglet permet de faire des statistiques permettant de comparer les moyennes des longueurs de lésion entre différents facteurs de variabilité. L'objectif est de savoir si la variable étudiée a une influence significative sur la variabilité de la distribution. L'utilisateur a la possibilité dd'effectuer une ANOVA sur un facteur ou sur deux facteurs de variabilité maximum."     
        ),
        box(width = 12,class = "box1",
            pickerInput(inputId='responseVar', label ='Choose the response variable', ""),
            pickerInput(inputId='factors', label ='Choose the factors', "", multiple = TRUE)
        )
      ),
      fluidRow(
        box(width = 12,class = "box1",
          withTags(
              div(
                h4("Anova :")
              )
            ),
            verbatimTextOutput(outputId = "anov")
        )
      ),
      fluidRow(
        box(width = 12, 
            plotOutput(outputId = "anovplot", height = "800px") %>% withSpinner(color="#0dc5c1")
        )
      ),
      fluidRow(
        box(width = 12,class = "box1",
          downloadButton("downloadAnov", "Download Anova Plot")
        )
      ),
      fluidRow(
        box(width = 12 ,class = "box1",
          withTags(
            div(
              h4("Tukey's test :")
            )
          ),
          column(width = 12,
             verbatimTextOutput(outputId = "Tukey")
          )
          # ,
          # column(width = 6,
          #     DTOutput(outputId = "TukLetter")
          # )
        )
      )
    ),
    
    tabItem(
      tabName = "ACP",
      fluidRow(
        box(width=12, class = "box2",
          "Analyse en Composantes Principales. Méthode  factorielle  de  réduction  de  dimension. L'utilisateur peut choisir les individus et les variables et l'ACP va permettre de visualiser les représentations graphiques optimales dans l'espace des individus et des variables."
        ),
        box(width = 12, class = "box1",
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
              pickerInput(inputId='axis', label ='Number of axis', selected = 2, choices = c(2))
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
            downloadButton("downloadACPind", "Download Plot ind")
        ),
        box(width = 6,
            downloadButton("downloadACPVar", "Download Plot Var")
        )
      ),
      fluidRow(
        box(width = 6,
            plotOutput(outputId = "vpPlot", height = "600px") %>% withSpinner(color="#0dc5c1")
        ),
        box(width = 6,
            plotOutput(outputId = "bothPlot", height = "600px") %>% withSpinner(color="#0dc5c1")
        )
      ),
      fluidRow(
        box(width = 6,
            downloadButton("downloadACPVP", "Download Plot VP")
        ),
        box(width = 6,
            downloadButton("downloadACPBoth", "Download Plot Both")
        )
      )
    ),
    tabItem(
      tabName = "Heatmap",
        fluidRow(
          box(width=12, class = "box2",
             "Le but de cet onglet est de visualiser sous forme d'une heatmap les valeurs moyennes des longueurs de liaison en fonction de deux variables choisies. Cela permet ensuite de clusteriser ces variables (par exemple clusteriser les souches). Une seconde représentation présente dans cet onglet permet de fixer un seuil de sensibilité / résistance en fonction des longueurs de lésion. Cette représentation offre alors en sortie une heatmap binaire de résistance / sensibilité."
             ),
          box(width = 12, class = "box1",
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
            plotlyOutput(outputId = "heatplot", height = "700px") %>% withSpinner(color="#0dc5c1")
          )
        )
      ),
      tabItem(
        tabName = "Heatmap2",
        fluidRow(
          box(width=12, class = "box2",
             "Le but de cet onglet est de visualiser sous forme d'une heatmap les valeurs moyennes des longueurs de liaison en fonction de deux variables choisies."
             ),
          box(width = 12, class = "box1",
              pickerInput(inputId='responseVarHeat2', label ='Choose the response variable', ""),
              pickerInput(inputId='factorH21', label ='Choose the first factor', ""),
              pickerInput(inputId='factorH22', label ='Choose the second factor', ""),
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
          box(width = 12, class = "box1",
              HTML("<br>"),
              div("Subdivise your dataset in several categories of resistance:"),
              HTML("<br>"),
              column(width=6,
                     sliderInput(inputId="categories", label = 'Number of categories', value = 2, min=2, max=6, step=1)
              ),
              column(width = 6, class = "box1",
                     conditionalPanel(
                       condition = "input.categories == 2",
                       sliderInput(inputId="thresSR21", label = "Threshold between the categories 1 & 2", value = 12, min=0, max=20, step=1),
                       actionButton(inputId="submitCAT2","Submit")
                       
                     ),
                     conditionalPanel(
                       condition = "input.categories == 3",
                       sliderInput(inputId="thresSR31", label = "Threshold between the categories 1 & 2", value = 12, min=0, max=20, step=1),
                       sliderInput(inputId="thresSR32", label = "Threshold between the categories 2 & 3", value = 12, min=0, max=20, step=1),
                       actionButton(inputId="submitCAT3","Submit")
                     ),
                     conditionalPanel(
                       condition = "input.categories == 4",
                       sliderInput(inputId="thresSR41", label = "Threshold between the categories 1 & 2", value = 12, min=0, max=20, step=1),
                       sliderInput(inputId="thresSR42", label = "Threshold between the categories 2 & 3", value = 12, min=0, max=20, step=1),
                       sliderInput(inputId="thresSR43", label = "Threshold between the categories 3 & 4", value = 12, min=0, max=20, step=1),
                       actionButton(inputId="submitCAT4","Submit")
                     ),
                     conditionalPanel(
                       condition = "input.categories == 5",
                       sliderInput(inputId="thresSR51", label = "Threshold between the categories 1 & 2", value = 12, min=0, max=20, step=1),
                       sliderInput(inputId="thresSR52", label = "Threshold between the categories 2 & 3", value = 12, min=0, max=20, step=1),
                       sliderInput(inputId="thresSR53", label = "Threshold between the categories 3 & 4", value = 12, min=0, max=20, step=1),
                       sliderInput(inputId="thresSR54", label = "Threshold between the categories 4 & 5", value = 12, min=0, max=20, step=1),
                       actionButton(inputId="submitCAT5","Submit")
                     ),
                     conditionalPanel(
                       condition = "input.categories == 6",
                       sliderInput(inputId="thresSR61", label = "Threshold between the categories 1 & 2", value = 12, min=0, max=20, step=1),
                       sliderInput(inputId="thresSR62", label = "Threshold between the categories 2 & 3", value = 12, min=0, max=20, step=1),
                       sliderInput(inputId="thresSR63", label = "Threshold between the categories 3 & 4", value = 12, min=0, max=20, step=1),
                       sliderInput(inputId="thresSR64", label = "Threshold between the categories 4 & 5", value = 12, min=0, max=20, step=1),
                       sliderInput(inputId="thresSR65", label = "Threshold between the categories 5 & 6", value = 12, min=0, max=20, step=1),
                       actionButton(inputId="submitCAT6","Submit")
                     )
              )
          )
        ),
      fluidRow(
        box(width = 12,
            column(width = 12,
                   plotlyOutput(outputId = "heatplotSR", height = "700px") %>% withSpinner(color="#0dc5c1")
            )
        )
      ),
      fluidRow(
        box(width = 12,
            column(width = 12,
                   DTOutput(outputId = "tabsouches") 
                   # %>% formatStyle(
                   #   'groups',
                   #   target = 'row',
                   #   backgroundColor = styleEqual(
                   #     c("group1","group1","group2","group3","group4","group5","group6","group7","group8","group9","group10","group11","group12","group13","group14"), 
                   #     c("#ff9999","#ffb399","#ffcc99","#ffe699",	"#ffff99","#e6ff99","#ccff99","#b3ff99","#99ff99","#99ffb3","#99ffcc","#99ffe6","#99ffff","#99e6ff","#99ccff")
                   #     )
                   # )
            )
        )
      )
    ),
    tabItem(
      tabName = "Visu",
      fluidRow(
        box(width=12, class = "box2",
           "Le but de cette page est de faire une sortie graphique permettant de visualiser la distribution des longueurs de lésions en fonction de plusieurs facteurs choisis."
           ),
        box(width=12, class = "box1",
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
      ),
      fluidRow(
        box(width = 12,
            downloadButton("downloadVisu", "Download Plot Visu")
        )
      )
    ),
    tabItem(
      tabName = "barplot",
      fluidRow(
        box(width=12, class = "box2",
           "Le but de cette page est de faire une sortie graphique permettant de visualiser la distribution des longueurs de lésions en fonction de plusieurs facteurs choisis." 
           ),
        box(width=12, class = "box1",
          column(width = 12,
            pickerInput(inputId='responseVarBar', label ='Choose the response variable', "")
           ),
          column(width=6,
            pickerInput(inputId='factorBar1', label ='Choose the first factor', "")
          ),
          column(width=6,
            pickerInput(inputId='factorBar2', label ='Choose the second factor', "")
          ),
          column(width=12,
            pickerInput(inputId='factorBar3', label ='Choose the third factor', "")
          )
        )
      ),
      fluidRow(
        box(width = 12,
            plotOutput(outputId = "BarPlot", height = "1000px") %>% withSpinner(color="#0dc5c1")
        )  
      ),
      fluidRow(
        box(width = 12,
            downloadButton("downloadBarplot", "Download Barplot")
        )
      )
    ),
    tabItem(
      tabName = "Evolution",
        fluidRow(
          box(width=12, class = "box2",
              "Le but de cette page est de faire une sortie graphique montrant l'évolution des valeurs de longueur de lésion en fonction du temps (lorsque les analyse s'étalent sur plusieurs expériences). La visualisation peut néanmoins se faire également en fonction d'un parametre non temporel."
          ),
          box(width = 12, class = "box1",
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
            plotlyOutput(outputId = "TimePlot", height = "1000px") %>% withSpinner(color="#0dc5c1")
        )  
      ),
      fluidRow(
        box(width = 12,
            downloadButton("downloadEvol", "Download Plot Time")
        )
      )
    ),
    tabItem(
      tabName = "RMD",
      fluidRow(
        box(width = 12, class = "box1",
            downloadButton('downloadRMD', 'Download Report')
            #, downloadButton('save', 'Save Workspace')
        )
      )
    )
  )
)

shinyUI(
  dashboardPage(title = "symptom", skin = "yellow", header, sidebar, body)
)

