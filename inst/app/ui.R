
#header <- dashboardHeader(title = "Symptoms Length Analysis Tool", titleWidth = 380)
header <- dashboardHeader(title = img(style = 'width: 80%;', src = "SLATlogo2.png", class = 'img-responsive'), titleWidth = 500)

sidebar <- dashboardSidebar(
  width = 180,
  sidebarMenu(
    menuItem("Home", tabName = "menu", icon = icon("home")),
    menuItem("Input Table", tabName = "Table", icon = icon("book-open")),
    menuItem("Mean/Sd", tabName = "Mean", icon = icon("calculator")),
    menuItem("Boxplot", tabName = "boxplot", icon = icon("eye")),
    menuItem("Barplot", tabName = "barplot", icon = icon("eye")),
    menuItem("Heatmap", tabName = "Heatmap", icon = icon("eye")),
    menuItem("Plot Time Series", tabName = "Evolution", icon = icon("eye")),
    menuItem("Anova", tabName = "Anova", icon = icon("calculator")),
    menuItem("Mean comparison", tabName = "MeanPlot", icon = icon("calculator")),
    menuItem("PCA", tabName = "ACP", icon = icon("calculator")),
    menuItem("Categorical Analysis", tabName = "Heatmap2", icon = icon("eye")),
    menuItem("Session Info", tabName = "SI", icon = icon("book-open"))
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
              "Effortlessly visualize and analyze a measure of symptom intensity as a quantitative response variable in connection to several experimental factors."
              )
            )
        ),
        box(width = 12,
          withTags(
            div(class = "home",
##TODO: if possible homogenize font type across the box
                h2("Quickly get a sense of what is in your disease assay data."),
                br(),
                p("First of all, go to the 'Input Table' thumbnail to upload a file with your data set. It must be formated in a 'long format' with one row per symptom measurement and columns describing the levels of the experimental factors associated with this numeric value (e.g plant genotype, strain, replicate ID, experiment ID, etc). From there you can use the tools accessible on the left handside menu to filter, aggregate, visualize, transform and export your data in an intutitive and user-friendly fashion."),
                h3("A short description of the data analysis tools:"),
                ul(
                  li(b("Home"),": This is page you are currently reading with an overview of SLAT."),
                  br(),
                  li(b("Input table"), ": This is where to upload your data set with values organized in a long or 'tidy' format (https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html). You can provide files containing one of several field and decimal separators. It is then possible to filter your data and export it. Downstream analysis steps with the various tools will be performed on this filtered data."),
                  br(),
                  li(b("Mean/Sd"), ": This enables the computation of standard aggregate values (mean, standard deviation, count of observations) of the quantitative response variable conditioned on the levels of one or several experimental factors."),
                  br(),
                  li(b("boxplot"), ": Plot individual datapoints on a box and whisker plot, conditioned on up to three experimental variables (e.g. distribution of symptom by strain and genotype across several experiments.)"),
                  br(),
                  li(b("barplot"), ": Plot symptom mean and standard deviation as a barplot, conditioned on up to three experimental variables (e.g. distribution of symptom by strain and genotype across several experiments.)."),
                  br(),
                  li(b("Heatmap"), ": For very large data sets, it may be usefull to display the means of symptom measurements in the compact form of a heatmap with levels of experimental variables as rows and columns. It also enables clustering of factor levels in rows and columns."),
                  br(),
                  li(b("Plot Time Series"), ": Plot symptom mean and standard deviation as a function of a time variable (e.g. date of the experiment). The plot can be further conditioned on other experimental variables."),
                  br(),
                  li(b("Anova"), ": Allows to conduct Analysis of variance to identify experimental factors significantly  affecting the magnitude of symptoms. Also performs Tukey multiple comparisons of means and display results in a searchable table."),
                  br(),
                  li(b("Means Comparison"), ": When a factor is found to significantly impact symptoms readout, this tool performs multiple means comparison with a variety of methods to find means that are significantly different from each other and add the significant comparisons on top of a data plot."),
                  br(),
                  li(b("PCA"), ": Principal component analysis, a dimension reduction technique for multivariate datasets. This is usefull for example to identify clusters of isolates with similar virulence patterns or conversely identify groups of host genotypes with related susceptibility profiles."),
                  br(),
                  li(b("Categorical Analysis"),": In phytopathology, people often convert quantitative symptom measures into an ordinal scale representing a disease index. This index is then used to group pathogen isolates in 'races' based on identical disease index profiles on a set of plant host genotypes. This tool enable to perform this convertion, to visualize the results and export a table of races.")
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
            "Upload your data file using the 'Browse...' button. 
            It must be formated in a 'long format' with one row per symptom measurement and columns describing the levels of the experimental factors associated with this numeric value (e.g plant genotype, strain, replicate ID, experiment ID, etc). 
            Specify the type of field and decimal separators used to represent data in your file. 
            Once, there is no error message (Data Validation) and your data displays correctly in the table below, you can start your analysis with the tools."
        ),
        box(width=12, class = "box1",
          column(width = 6,
           downloadButton("downloadData", label = "Download a test file"),
           bsPopover("downloadData", "Example Data Set", content = "A CSV file: the separator is TAB and the decimal is DOT. 
                     The response variable is Symptom_lenght. 
                     There is a Time factor in format DMY (Date) and 3 other qualitative variables (Strain_name, Plant_genotype, Experiment_number)", 
                     placement = "bottom", trigger = "hover", options = NULL),
           HTML("<br><br>"),
            fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")) %>%
              helper(icon = "question",
                     type = "markdown",
                     content = "file1"),
            pickerInput(inputId='responseVar0', label ='Select the response variable', "")
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
        box(width=12, class = "box1",
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
        box(width=12, class = "box1",
           checkboxInput("logTrans", "logarithmic transformation", FALSE)
          )
      ),
      fluidRow(
        box(width = 12,
          DT::dataTableOutput(outputId = "DataSet")
        )
      ),
# STILL TRUE : THERE IS SOMETHING WEIRD WHEN TRYING TO SPECIFY FILTERING VALUES FOR 'Experiment_number' IN THE EXAMPLE DATASET
# Having two tables that display the same thing simultaneously is a bit confusing
      
      fluidRow(
        box(width=12, class = "box2",
            "You can a filter your dataset by specifying the criteria in the boxes on top of the columns above. The filtered table appears below and can be downloaded. All analysis will be done with the filtered dataset."
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
            "Group data points by levels of one or several experimental factor(s) and calculate aggregated values : the number of observations (Count), the median, the mean and the standard deviation (Sd) of the selected response variable (e.g. symptom length)."
        ),
          box(width = 12,class = "box1",
          pickerInput(inputId='responseVar1', label ='Select the response variable', ""),
          pickerInput(inputId='factors1', 
                      label ='Select experimental factors', 
                      "", 
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Deselect all",
                        `select-all-text` = "Select all"
                      )
                    )
        )
      ),
      fluidRow(
        box(width = 12,
          DTOutput(outputId = "moyenne")
        )
      )
    ),
    tabItem(
      tabName ="MeanPlot",
##TODO: Update as necessary according to the final interface
      fluidRow(
         box(width=12, class = "box2",
        "Violin plots for group or condition comparisons in between-subjects."
         ),
        box(width = 12,class = "box1",
          column(width =6,
            pickerInput(inputId='responseVarMP', label ='Choose the response variable (y)', ""),
            pickerInput(inputId='factorMP1', label ='Choose the factor for the x-axis (x)', ""),
            pickerInput(inputId='factorMP2', label ='Grouping variable', "")
          ),
          column(width =3,
            radioButtons( "testType", "Test Type", choiceValues = c("np", "p", "r", "bf"), choiceNames = c("non parametric", "parametric", "robust", "bayes factor"), selected = "np"),
            radioButtons( "PlotType", "Type of plot",choices =  c("box", "violin", "boxviolin"), selected = "box")
          ),
          column(width = 3,
            radioButtons("MeanPlotting", "Mean Plotting", choiceNames = c("yes", "no"), choiceValues =  c(TRUE, FALSE), selected = FALSE),
            radioButtons("Comparaison", "Pairwise Comparison", choiceNames = c("yes", "no"), choiceValues=  c(TRUE, FALSE), selected = TRUE)
          )
        )
      ),
      fluidRow(
        box(width = 12,
            plotOutput(outputId="meanplot", height = "1000px")  %>% withSpinner(color="#0dc5c1")
        )
      )
    ),
    tabItem(
      tabName ="Anova",
      fluidRow(
        box(width=12, class = "box2",
            "Examine the influence of experimental factors on the continuous response variable (e.g. symptom intensity) using analysis of variance (ANOVA). If two experimental factors are selected (maximum) the model will automatically include an interaction term."     
        ),
        box(width = 12,class = "box1",
            pickerInput(inputId='responseVar', label ='Select the response variable', ""),
            pickerInput(inputId='factors', label ='Select the factor(s)', "", multiple = TRUE,
                        options =  list(
                          "max-options" = 2
                        )
                      )
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
              h4("Tukey HSD tests results: post hoc comparisons on each combination of factor levels in the model.")
            )
          ),
          column(width = 12,
                 DTOutput(outputId = "Tukey")
             # TEXT OUTPUT TRUNCATED IF VERY LONG LIST OF COMPARISONS
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
          "Principal component analysis :  
           Dimensionality reduction method which transforms a large dataset into a simplified representation capturing most of the information of the original dataset.
          This is usefull in exploratory data analysis, for example to identify strains with a similar virulence profile on a set of plant genotypes."
        ),
        box(width = 12, class = "box1",
            pickerInput(inputId='respacp', label ='Select the response variable (e.g. symtpols intensity)', ""),
            column(width = 6,
              pickerInput(inputId='individual', label ='Select factor that will define individuals (e.g. strains)', "")
            ),
            column(width = 6,
              pickerInput(inputId='variable', label ='Select factor that will define variables (e.g. plant genotype)', "")
            ),
            column(width = 3,
              checkboxInput("reduct", "reduct variables", FALSE)
            ),
            column(width = 3,
              checkboxInput("center", "center variables", FALSE)
            ),
            column(width = 6,
              pickerInput(inputId='axis', label ='Number of axis', selected = 2, choices = c(2, 3))
            )
        )
      ),
      fluidRow(
        box(width = 6, 
            conditionalPanel(
              condition = "input.axis == 3",
                pickerInput(inputId='axisViz', label ='Axis to plot', selected = "axis1 vs axis2", choices = c("axis1 vs axis2", "axis1 vs axis3", "axis2 vs axis3"))
            ),
            conditionalPanel(
              condition = "input.axis == 2",
              pickerInput(inputId='axisViz', label ='Axis to plot', selected = "axis1 vs axis2", choices = c("axis1 vs axis2"))
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
             "Heatmap visualization of averrage values of the response variable as a function of the levels of two experimental variables (rows and column). Can optionally order rows and column based on a hierarchical clustering approach (dendrogram added on top and/or on the side of the color matrix)."
             ),
          box(width = 12, class = "box1",
             pickerInput(inputId='responseVarHeat', label ='Select the response variable', ""),
             pickerInput(inputId='factorH1', label ='Select the factor displayed in rows', ""),
             pickerInput(inputId='factorH2', label ='Select the factor displayed in columns', ""),
             column(width=3,
                HTML("Add clusterization for: ")
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
##TODO: keep working from here
      tabItem(
        tabName = "Heatmap2",
        fluidRow(
          box(width=12, class = "box2",
             p("Convert your symptom intensity data into a qualitative index for plant pathogen race analysis: 
             Pathogen races (also referred to as physiological races or pathotypes) are defined by their profile of pathogenicity on a defined set of differential host cultivars (i.e. a set of host genotypes that each carry a distinct profile of resistance genes). Oftentimes, pathogenicity is defined as an ordered categorical variable (ordinal) with levels depicting disease outcome (e.g. Resistant < Moderately Susceptible < Susceptible)"),
             p("This tool takes mean symptom measures and convert them into categories defined by the user. This categorical data is then plotted as a Heatmap where it is straightforward to observe the clustering of virulence profiles into races. Furthermore, unique pathogenicity profiles (i.e. races) in the data set are computed and assigned to each strain in the table summarizing the output data. Finally, the categories distribution for each individual (levels of the variable displayed in row) is displayed in the stacked barplot.")
             ),
          box(width = 12, class = "box1",
              pickerInput(inputId='responseVarHeat2', label ='Select the response variable', ""),
              pickerInput(inputId='factorH21', label ='Select the factor displayed in rows (e.g. Strain)', ""),
              pickerInput(inputId='factorH22', label ='Select the factor displayed in columns (e.g. Plant line)', ""),
              column(width=3,
                     HTML("Clusterisation : ")
              ),
              column(width=3,
                     checkboxInput("column2", "col", TRUE)
              ),
              column(width=3,
                     checkboxInput("row2", "row", TRUE)
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
            column(width = 6,
                   DTOutput(outputId = "tabsouches") 
                   # %>% formatStyle(
                   #   'groups',
                   #   target = 'row',
                   #   backgroundColor = styleEqual(
                   #     c("group1","group1","group2","group3","group4","group5","group6","group7","group8","group9","group10","group11","group12","group13","group14"), 
                   #     c("#ff9999","#ffb399","#ffcc99","#ffe699",	"#ffff99","#e6ff99","#ccff99","#b3ff99","#99ff99","#99ffb3","#99ffcc","#99ffe6","#99ffff","#99e6ff","#99ccff")
                   #     )
                   # )
            ),
            column(width = 6,
                plotOutput(outputId = "FreqOfResistance", height = "700px") %>% withSpinner(color="#0dc5c1")
            )
        )
      )
    ),
    tabItem(
      tabName = "boxplot",
      fluidRow(
        box(width=12, class = "box2",
            "Plot individual data points together with 'standard' box and whisker representations, conditionned on experimental factors."
           ),
        box(width=12, class = "box1",
          pickerInput(inputId='responseVarPG', label ='Select the response variable (y)', "") %>%
            helper(icon = "question",
                   type = "markdown",
                   content = "Boxplot",
                   colour = "red",
                   size = "l"),
          pickerInput(inputId='factorPG1', label ='Select the factor for the x-axis (x)', ""),
##TODO: May be? Currently this will color the points based on the factor. But the expected behavior should be to color the boxplots, shouldn't it?
          pickerInput(inputId='factorPG2', label ='Select a factor for coloring based on its levels (fill)', ""),
          pickerInput(inputId='factorPG3', label ='Select a third factor to generate one plot per level of this factor in a grid', "")
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
           "This tool plots aggregates of data values (Mean and standard variation) conditioned on one or several experimental variables." 
           ),
        box(width=12, class = "box1",
            pickerInput(inputId='responseVarBar', label ='Select the response variable (y)', "")%>%
              helper(icon = "question",
                     type = "markdown",
                     content = "Barplot",
                     colour = "red",
                     size = "l"),
            pickerInput(inputId='factorBar1', label ='Select the factor for the x-axis (x)', ""),
            pickerInput(inputId='factorBar3', label ='Select a factor for coloring based on its levels (fill)', ""),
            pickerInput(inputId='factorBar2', label ='Select a third factor to generate one plot per level of this factor (grid)', "")
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
              "This tool may be particularly usefull if there is a time variable in you data set (e.g. date of the experiment) and you want to plot values along time on the x-axis. It plots aggregates of data values (Mean and standard variation) conditioned on one or several experimental variables."
          ),
          box(width = 12, class = "box1",
            column(width = 6,
              pickerInput(inputId='responseVarT', label ='Select the response variable (y)', "")
            ),
            column(width = 3,
              pickerInput(inputId='TimeFactor', label ='Select the variable for the x-axis', "")
            ),
            column(width = 3,
              radioButtons("Time", "Specify its time format (e.g. 27/02/2018 -> dmy)", c("not a time format", "dmy", "ymd"), selected = "not a time format")
            ),
            column(width = 3,
              pickerInput(inputId='factorT2', label ='Select a factor for plots facetting (grid y)', "")
            ),
            column(width = 3,
              pickerInput(inputId='factorT3', label ='Select a factor for plots facetting (grid x)', "")
            ),
            column(width = 3,
              pickerInput(inputId='factorT4', label ='Select a factor for grouping/coloring on each sub-plot (z)', "")
            ),
            column(width = 3,
              radioButtons("smoothing", "Smoothing", c("no", "smooth"), selected = "no")
          )
        )
      ),
      fluidRow(
        box(width = 12,
            plotOutput(outputId = "TimePlot", height = "1000px") %>% withSpinner(color="#0dc5c1")
        )  
      ),
      fluidRow(
        box(width = 12,
            downloadButton("downloadEvol", "Download Plot Time")
        )
      )
    ),
    tabItem(
      tabName = "SI",
       fluidRow(
         verbatimTextOutput("urlText1"),
         verbatimTextOutput("urlText2")
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

