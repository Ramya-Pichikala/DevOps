


########CREATE JAVA SCRIPT HERE
jsc <- '
$(document).ready(function () {
$(".sidebar-menu").children("li").on("click", function() {
$("#mult, #single").toggle();
});
});
'
###### 
ui2 <- function(){
  
  library(shinyLP)
  library(shiny)
  library(shinythemes)
  
  fluidPage(useShinyjs(),theme = shinytheme("flatly"),singleton(tags$head(tags$script(src = "pop_patch.js"))),
            #shinythemes::themeSelector(),
            tags$style(HTML(".navbar-header { width:100% }
                   .navbar-brand { width: 100%; text-align: center }
                  .navbar{height: 122px}
                  .nav-tabs{font-weight: bold; font-size: 15px; display: flex !important;
  justify-content: left !important;
  width: 100%;}
                  .navbar-nav{font-weight: bold; font-size: 16px;}
                   .navbar-nav {
                           float: none !important;
                      
                           }
                           .navbar-nav > li:nth-child(1) {
                           float: left;
                          
                           left: 600px;
                           }
                           .navbar-nav > li:nth-child(2) {
                           float: left;
                           left: 640px;
                           }
                            .navbar-nav > li:nth-child(3) {
                           float: left;
                           left: 680px;
                            }
                             .navbar-nav > li:nth-child(4) {
                           float: left;
                           left: 730px;
                             }
                           .navbar-header .navbar-brand:hover, 
                          .navbar-header .navbar-brand:focus {
                            background-color: transparent;
                            color: #FFFFFF;
                          }
                   
       
       #abc {
       position:fixed;
       left: 45px;
       top: 17px;
        background-color: #f1f1f1;
    
       margin:6px 0 0 6px;
       box-shadow:0 0 0 6px;
       
       }
       
       #abcd {
       position:fixed;
       right: 45px;
       top: 16px;
       
       }
       b{
  font-style: bold;
  font-size: 20px;
       }
 small{
  font-style: Italic;
  font-size: 8px;
}
             
       
        #abcde {
       position:fixed;
       left: 216px;
       top: 61px;
       text-align: left;
       font-size: 16px;
    
       
        }
          h2 {
       position:fixed;
       left: 590px;
       top: 0px;
       #font-size: 15px;
       #font-style: bold;
      
       
        }
         h3 {
       position:fixed;
       left: 1050px;
       top: 5px;
       #font-size: 15px;
       #font-style: bold;
      
       
        }
    .selectize-input {
                      height: 43px;
                     font-size: 11.5pt;
                     
        
     }.action-button {
                      
                    
                      width: 140px;
                        }
      
       
        ")
                       
            ),
            
            navbarPage(title =div(div(id="abc",img(src="valeo2.png",height = 72,width = 150)),
                                  div(id="abcde",HTML("<b>THS BG Metier <br/>Reliability & Testing</b>") ),
                                  div(id="abcd" ,img(src="image.png",height = 90,width = 135)), div(id ="car",HTML("<h2><strong>SN Fatigue Curves Visualization</strong></h2>&nbsp;<h3><small><strong> (Tools for Fatigue Curves Analysis & Visu)
</strong></small></h3>"))),
                       position = "fixed-top",
                       tabPanel( id= "tab1","Access to Database",  icon = icon("database"),
                                 
                                 
                                 
                                 # Application title
                                 titlePanel(""),            
                                 
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 # Sidebar with controls to select city, month and type of plot
                                 sidebarLayout(
                                   div( id ="Sidebar", sidebarPanel(width = 4,
                                                                    fluidRow(column(12,
                                                                                    fileInput('file', 'Load Template DB', accept=c(".xlsx"), multiple = TRUE)
                                                                    )),
                                                                    fluidRow(column(12,
                                                                                    uiOutput("samplexvars"),
                                                                                    uiOutput("variable"),
                                                                                    h3(strong("Manual Entry of Coefficients:")),
                                                                                    numericInput("value_a","Enter 'a' value:",value="NULL"),
                                                                                    numericInput("value_b","Enter 'b' value:",value = "NULL"),
                                                                                    sliderInput("obs", "Stress Range:",
                                                                                                min = 90, max = 200, value = c(90, 120)),
                                                                                    checkboxGroupInput("checkbox", "Log X_Axis:",c("X_axis" = "log")),
                                                                                    checkboxGroupInput("checkbox1", "Log Y_Axis:",c("Y_axis" = "log")),
                                                                                    
                                                                                    actionButton("reset", "Clear All", icon("eraser"),
                                                                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                                                    
                                                                    )
                                                                    ))),
                                   
                                   
                                   #Main Panel contains the plot/s
                                   mainPanel(width = 8,
                                             
                                             
                                             tabsetPanel(type = "tabs",
                                                         tabPanel(p(strong("S-N Curve")),
                                                                  plotlyOutput(outputId = 'contents', width = "100%"),
                                                                  # )
                                                                  br(),br(),br(),br(),br(),br(),br(),
                                                                  br(),br(),br(),br(),br(),br(),br(),br(),
                                                                  
                                                                  dataTableOutput(outputId = 'text',width = "100%"),
                                                                  
                                                                  h4(strong("Material Test details"),align="center"),
                                                                  dataTableOutput(outputId = 'sample_table', width = "100%")),
                                                         tabPanel(title = "Material Coefficients",
                                                                  DT::dataTableOutput('sample'), width = "50%"))
                                             
                                   ))),
                       #####ABOUT THIS TOOL
                       tabPanel(id = "chart_tabs","About this tool",
                                icon = icon("question-circle"),
                                titlePanel(""),            
                                
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                
                               p(strong("SN Fatigue Curve"),style ="font-size: 30px;"),
                                
                                p("Tool developed by ",
                                  a(href="www.valeo.com", "Valeo"), "using",
                                  a(href = "https://www.r-project.org", "R.")),
                                br(),
                                p(strong("Version 0.3"),  "September 17, 2019"),
                                p("Support: Ramyasri Pichikala", a(href="mailto:ramyasri.pichikala@valeo.com", "ramyasri.pichikala@valeo.com")),
                                p(strong("How to use this tool"),style ="font-size: 20px;"),
                                p(""),
                                p("If no data are loaded, the tool's console will remain empty."),
                                p("The data should be in the prescribed RDBMS format, as generated by the appropriate script"),
                                
                                p(strong("Selection and Transformation Features"),style ="font-size: 20px;"),
                                
                                tags$ul(
                                  tags$li("Use the 'Choose File' button in order to load the Valeo Excel database"),
                                  tags$li("Select the field of the experiment of interest: Customer, Product, Project, Speficication"
                                  )
                                ),
                               p(strong("Plot Features"),style ="font-size: 20px;"),
                                
                                
                                tags$ul(
                                  tags$li("The resulting plot is highly responsive"),
                                  tags$li("Placing the cursor over the lines returns the values of both axes"),
                                  tags$li("Clicking the plot legend, lines can be removed from the plot"),
                                  tags$li("Zoom in and out, restrict the axis domain etc. are possible"),
                                  tags$li("Clicking on the first icon from left position over the plot one can save the
                             graphic in png format"),
                                  tags$li("Clicking on the toggle symbol will collapse and restore the sidebar"),
                                  tags$li("Clicking on the clear button will remove all inputs")
                                )  ) ,
                       
                       tabPanel(id = "chart_tabs","Suggest",
                                icon = icon("envelope-square"),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                tags$head(
                                  tags$style(type = 'text/css', 
                                             HTML('.tab-panel{ background-color: red;}
                               .navbar-default .navbar-nav > .active > a, 
                               .navbar-default .navbar-nav > .active > a:focus, 
                               ')
                                  )
                                ),
                                sidebarPanel(
                                  br(),
                                  textInput("subject", "Subject", value=""),
                                  selectInput('tdata', 'Tool Kind', choices = c("Bug","Suggestion","Support Required","others"), multiple = TRUE, selectize = TRUE, selected = NULL),
                                  p(strong("Message")),
                                  aceEditor("message", value=""),
                                  uiOutput("tab")             
                                )),
                       tabPanel(id = "chart_tabs","Version x.x",
                                icon = icon("info-circle"))
                       
            ))}

