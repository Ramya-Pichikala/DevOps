
###########################################################################################################
########################################## Create a list of packages to install
###########################################################################################################
packagelist_x<-c("shiny",'shinyWidgets',"ggplot2","shinyAce","sendmailR","plotly","ggforce", "dplyr","purrr","shinyLP" ,"tidyr", "mgcv", "DT", "stringr","scales", "shinyjs","readxl","readr", "openxlsx","plotly", "shinyalert", "reshape2", "pracma", 'janitor', "profvis", "shinyBS",  "DT")




############################################################################################### 
############################Install using apply after checking if available in working directory
###############################################################################################

options(DT.options = list(searching=FALSE, lengthChange = FALSE,info = FALSE))

options(digits = 3  )
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


#########SCIENTIFIC NOTATION FUNCTION DEFINED HERE
changeSciNot<-function(n) {
  if(n>=10|n<1){
    output <- format(n, scientific = TRUE)
    output <- sub("e", "*10^", output)
    output <- sub("\\+0?", "", output)
    output <- sub("-0?", "-", output)
    return(output)
  }}

###########################################################
##################INSTALL MULTIPLE PACKAGES
##########################################################
ipak(packagelist_x)
###########################################Load all the needed packages in R

###########################################################
###### WE DEFINE A FUNCTION HERE TO GET RELIABILITY RANGES
###########################################################


##################################################################
###################BLOCK ALL WARNINGS GLOBALLY
###################################################################

options(warn=-1)

##############################################################
######################### We now create the login portions here
##############################################################
options(shiny.sanitize.errors = FALSE)
Logged =TRUE;############ if u create false, u will need to use login ID and password
###########USER ID  AND PASSWORD ARE SET ON SERVER
my_username <- c("Marco", "Philippe")
my_password <- c("376d5693345ceabf6cc8a5893ad0564f", "60dcc3f65f768d989752d078bf0c1673")


####################################################################
######################### Source code from the folder 
####################################################################
source("ui1.r")
source("ui2.r")
source("ui1_wrong.R")


###########################################
ui = (htmlOutput("page"))
###########################################


####################################################################
###SERVER DEFINED HERE
######################################################################

server = (function(input, output, session) {
  options(shiny.maxRequestSize=3000*1024^2)######### WE INCREASE DATA SET SIZE
  withMathJax()
  #######################################################LOG IN CHECK
  USER <- reactiveValues(Logged = Logged)
  
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username %in% Username)
          Id.password <- which(my_password %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username %in% Id.password) {
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  # 
  observe({
    if (USER$Logged == FALSE) {
      
      ### LOG in not null but FALSE
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          output$page <- renderUI({
            div(class="outer",do.call(bootstrapPage,c("",ui1_wrong())))
          })
          
        }
      } else {
        
        output$page <- renderUI({
          div(class="outer",do.call(bootstrapPage,c("",ui1())))
        })
        
      }
    }
    # 
    ### OK WENT WELL!
    if (USER$Logged == TRUE)
    {
      output$page <- renderUI({
        # div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "Valeo Plotted",ui2())))
        div(class="outer",do.call(fluidPage,c("",ui2())))
      })
      print(ui)
    }
    
  })
  datasetInput <- reactive({
    
    
    
    ##### LOAD FILE HERE
    infile<- input$file
    if (is.null(infile))
      return(NULL)
    #### CHOOSE FILE TYPES HERE  CSV OR EXCEL
    if(grepl(infile, pattern = ".xlsx" )==T){
      data=read_excel(infile$datapath,sheet=1)
    } else if(grepl(infile , pattern = ".csv" )==T){
      data=read.csv(infile$datapath )
    }
    #data <- read.csv(file=infile$datapath)
    # data<-read_excel(infile$datapath)
    return(data)
  })
  
  datasetInput_sheet2 <- reactive({
    
    
    
    ##### LOAD FILE HERE
    infile<- input$file
    if (is.null(infile))
      return(NULL)
    #### CHOOSE FILE TYPES HERE  CSV OR EXCEL
    if(grepl(infile, pattern = ".xlsx" )==T){
      data=read_excel(infile$datapath,sheet=2)
    } else if(grepl(infile , pattern = ".csv" )==T){
      data=read.csv(infile$datapath )
    }
    #data <- read.csv(file=infile$datapath)
    # data<-read_excel(infile$datapath)
    return(data)
  })
  
  datasetInput_sheet3 <- reactive({
    
    
    
    ##### LOAD FILE HERE
    infile<- input$file
    if (is.null(infile))
      return(NULL)
    #### CHOOSE FILE TYPES HERE  CSV OR EXCEL
    if(grepl(infile, pattern = ".xlsx" )==T){
      data=read_excel(infile$datapath,sheet=3)
    } else if(grepl(infile , pattern = ".csv" )==T){
      data=read.csv(infile$datapath )
    }
    #data <- read.csv(file=infile$datapath)
    # data<-read_excel(infile$datapath)
    return(data)
  })
  output$samplexvars<-renderUI({ 
    
    DF<-datasetInput()
    prod_2<-unique(na.omit(DF$Material))
    selectInput('tdata', 'Plot from Test Data', prod_2, multiple = TRUE, selectize = TRUE, selected = NULL)
    
  })
  output$tab <- renderUI({
    mal <- paste("window.open('mailto:murali.krishnamoorthy@valeo.com?subject= SN Fatigue Curves Visualization :",input$subject,"-",input$tdata,"&body=",input$message,"')")
    actionButton("send", "Compose Mail",icon = icon("envelope"),onclick = mal)
  })
  output$variable<-renderUI({
    #req(input$material)
    DF<-datasetInput_sheet2()
    prod_2<-unique(na.omit(DF$Material))
    # selectInput('material', 'Plot from Material library (Theorectical coefficients)', prod_2, multiple = FALSE, selected = "NULL")
    selectizeInput("material", "Plot from Material library (Theorectical coefficients)", choices = NULL,multiple = TRUE,selected = NULL,options = list(maxItems = 1))
    
  })
  
  
  
  #Extract sheet 2 data from uploaded file
  details_data <- reactive({
    file1 <- input$file1
    req(file1)
    dataSet1 <- read_excel(file1$datapath, sheet = 3)
    dataSet1
  })
  
  # anbvalues <- reactive({
  #   file1 <- input$file1
  #   req(file1)
  #   dataSet1 <- read_excel(file1$datapath, sheet = 2)
  #   ds <- dataSet1$Material
  #   updateSelectizeInput(session, "material", choices = ds, selected = ds[3])
  #   dataSet1
  # })
  #To extraxt the materials from uploaded file
  observe({
    dt <- datasetInput()
    dtab <- datasetInput_sheet2() 
    varX <- unique(dt$Material)
    vars <- unique(dtab$Material)
    input$reset
    updateSelectizeInput(session, "tdata", choices = varX)
    updateSelectizeInput(session, "material", choices = vars,selected = NULL)
    updateCheckboxInput(session,"checkbox",value="NULL")
    updateCheckboxInput(session,"checkbox1",value="NULL")
    updateNumericInput(session,"value_a",value="NULL")
    updateNumericInput(session,"value_b",value="NULL")
  })
  
  #DataTable for Materials coefficient
  output$sample <- DT::renderDataTable({
    dt <-  datasetInput()
    if (is.null(dt))
      return(NULL)
    input <- unique(dt$Material)
    d = NULL
    for(i in 1:length(input)) {
      newdata <- subset(dt,Material %in% input[i],select=c(Stress,Cycles,Material))
      #newdata<-newdata[!(newdata$Cycles>7000000 | newdata$Cycles<7000),]
      model <- lm(log10(Cycles)~log10(Stress),newdata)
      if(coef(model)[2]>0){
        g<-as.character("Log10(N) = a+b*Log10(S), R\u00B2= r2");
      } else {
        g<-as.character("Log10(N) = a-(b*Log10(S)), R\u00B2= r2");
      }
      eq <- g %>%
        gsub("a", format(coef(model)[1], digits = 5), .) %>%
        gsub("b", format(abs(coef(model)[2]), digits = 5), .) %>%
        gsub("r2", format(summary(model)$r.squared, digits = 3), .);
      d = rbind(d, data.frame(eq))
    }
    d$Material <- input
    colnames(d) <- c("Regression Equation","Material")
    result <- d[,c(2,1)]
    
    DT::datatable(result,options = list(pageLength=10,searching = FALSE,lengthChange = FALSE))
  })
  output$basqi <-renderText({
    if(is.null(input$material))
      return(NULL)
    Basquin_equation <- basquin()
    Material <- input$material
    result <- data.frame(Material,Basquin_equation)
    coef <- NULL
    if(!is.null(input$value_a)) {
      req(input$value_a,input$value_b)
      coef <- coefficient()
      
    }
    result$Coefficient_equation <- coef
    DT::datatable(result,options = list(pageLength=10,searching = FALSE,lengthChange = FALSE))
  })
  
  
  basquin <- reactive({
    if(input$material=="")
      return(NULL)
    slop <- datasetInput_sheet2()
    
    sub1 <- subset(slop,Material %in% input$material,select=c(Slope,Intercept))
    
    lm_eqn <- function(sub1){
      if(sub1$Slope > 0) {
        g<-as.character("Log10(N) = b+a*Log10(S)");
      } else {
        g<-as.character("Log10(N) = b-a*Log10(S)");
      }
      Equation <- g %>%
        gsub("a", format(abs(sub1$Slope), digits = 5), .) %>%
        gsub("b", format(sub1$Intercept, digits = 5), .);
      
      as.character(as.expression(Equation));                 
    }
    
    return(lm_eqn(sub1))
  })
  
  coefficient <- reactive({
    if(input$value_a=="")
      #print(input$value_a)
      return(NULL)
    req(input$value_a|input$value_b)
    va <-input$value_a
    vb <- input$value_b
    
    if(va > 0) {
      g<-as.character("Log10(N) = b+a*Log10(S)");
    }
    else {
      g<-as.character("Log10(N) = b-a*Log10(S)");
    }
    Equation <- g %>%
      gsub("a", format(abs(va), digits = 5), .) %>%
      gsub("b", format(vb, digits = 5), .);
    
    eq<- as.character(as.expression(Equation));
    
  })
  
  table <- reactive({
    dt <- datasetInput()
    if (input$tdata=="")
      return(NULL) 
    # else if(is.null(input$tdata))
    #   return()
    d = NULL
    for(i in 1:length(input$tdata)) {
      sub <- subset(dt,Material %in% input$tdata[i],select=c(Stress,Cycles,Material))
      # sub<-sub[!(sub$Cycles>7000000 | sub$Cycles<7000),]
      mod <- lm(log10(Cycles)~log10(Stress),sub)
      if(coef(mod)[2]>0){
        g<-as.character("Log10(N) = a+b*Log10(S), R\u00B2= r2");
      } else {
        g<-as.character("Log10(N) = a-(b*Log10(S)), R\u00B2= r2");
      }
      Equation <- g %>%
        gsub("a", format(coef(mod)[1], digits = 5), .) %>%
        gsub("b", format(abs(coef(mod)[2]), digits = 5), .) %>%
        gsub("r2", format(summary(mod)$r.squared, digits = 3), .);
      d <- rbind(d, data.frame(Equation))
    }
    d$Material <- input$tdata
    colnames(d) <- c("Regression Equation","Material")
    result <- d[,c(2,1)]
    return(result)
  })
  
  
  output$text <- DT::renderDataTable({
    
    if((is.null(input$tdata)==F) & (is.null(input$material)==T || is.na(input$material)==T) & (is.na(input$value_a)==T || is.null(input$value_a)==T)) {
      req(input$tdata)
      sn_result <- table()
      sn_result$Data_source <- "Test Data"
      DT::datatable(sn_result,options = list(pageLength=10,searching = FALSE,lengthChange = FALSE))
    }
    else if((is.null(input$material)==F) & (is.null(input$tdata)==T || is.na(input$tdata)==T) & (is.na(input$value_a)==T || is.null(input$value_a)==T)) {
      req(input$material)
      testdata_result <- data.frame( "Material from test data" = input$material, "Regression Equation" = basquin())
      colnames(testdata_result) <- c("Material","Regression Equation")
      testdata_result$Data_source <- "Material Library"
      DT::datatable(testdata_result,options = list(pageLength=10,searching = FALSE,lengthChange = FALSE))
    }
    else if((is.null(input$value_a)==F) & (is.null(input$tdata)==T || is.na(input$tdata)==T) & (is.na(input$material)==T || is.null(input$material)==T)) {
      req(input$value_a)
      manual_values <- data.frame("Material" = NA, "Regression Equation" = coefficient())
      colnames(manual_values) <- c("Material","Regression Equation")
      manual_values$Data_source <- "User Input"
      DT::datatable(manual_values,options = list(pageLength=10,searching = FALSE,lengthChange = FALSE))
    }
    else if((is.null(input$tdata)==F) & (is.null(input$material)==F) & (is.na(input$value_a)==T | is.null(input$value_a)==T)) {
      req(input$tdata,input$material)
      sn_result <- table()
      sn_result$Data_source <- "Test Data"
      testdata_result <- data.frame("Material from test data" = input$material, "Regression Equation" = basquin())
      colnames(testdata_result) <- c("Material","Regression Equation")
      testdata_result$Data_source <- "Material Library"
      complete_table <- rbind(sn_result,testdata_result)
      DT::datatable(complete_table,options = list(pageLength=10,searching = FALSE,lengthChange = FALSE))
    }
    
    else if((is.null(input$tdata)==F) &  (is.null(input$value_a)==F) & ((is.na(input$material)==T) || (is.null(input$material)==T))) {
      req(input$tdata,input$value_a)
      sn_result <- table()
      sn_result$Data_source <- "Test Data"
      manual_values <- data.frame("Material" = NA, "Regression Equation" = coefficient())
      colnames(manual_values) <- c("Material","Regression Equation")
      manual_values$Data_source <- "User Input"
      complete_table <- rbind(sn_result,manual_values)
      DT::datatable(complete_table,options = list(pageLength=10,searching = FALSE,lengthChange = FALSE))
    }
    else if((is.null(input$material)==F) & (is.null(input$value_a)==F) & ((is.na(input$tdata)==T) || (is.null(input$tdata)==T))) {
      req(input$material,input$value_a)
      testdata_result <- data.frame("Material from test data" = input$material, "Regression Equation" = basquin())
      colnames(testdata_result) <- c("Material","Regression Equation")
      testdata_result$Data_source <- "Material Library"
      manual_values <- data.frame("Material" = NA, "Regression Equation" = coefficient())
      colnames(manual_values) <- c("Material","Regression Equation")
      manual_values$Data_source <- "User Input"
      complete_table <- rbind(testdata_result,manual_values)
      DT::datatable(complete_table,options = list(pageLength=10,searching = FALSE,lengthChange = FALSE))
      
    }
    else if((is.null(input$tdata)==F) & (is.null(input$value_a)==F) & (is.null(input$material)==F)) {
      req(input$tdata,input$value_a,input$material)
      sn_result <- table()
      sn_result$Data_source <- "Test Data"
      testdata_result <- data.frame("Material from test data" = input$material, "Regression Equation" = basquin())
      colnames(testdata_result) <- c("Material","Regression Equation")
      testdata_result$Data_source <- "Material Library"
      manual_values <- data.frame("Material" = NA, "Regression Equation" = coefficient())
      colnames(manual_values) <- c("Material","Regression Equation")
      manual_values$Data_source <- "User Input"
      # print(manual_values)
      complete_table <- rbind(sn_result,testdata_result,manual_values)
      DT::datatable(complete_table,options = list(pageLength=10,searching = FALSE,lengthChange = FALSE))
    }
    
  })
  
  #S-N Curve
  output$contents <- renderPlotly({
    dt <- datasetInput()
    mat <- datasetInput_sheet2()
    
    b<-input$obs
    stres_assign <- seq(from=b[1], to=b[2], by=5)
    stres_assign <- round(stres_assign[-(1)])
    
    updat_data <- "NULL"
    if((is.null(input$tdata)==F) & (is.null(input$material)==T || is.na(input$material)==T) & (is.na(input$value_a)==T || is.null(input$value_a)==T)) {
      req(input$tdata)
      newdata <- subset(dt,Material %in% input$tdata,select=c(Stress,Cycles,Material))
      if(length(input$tdata)>1) {
        newdata$fv <- newdata %>%
          filter(!is.na(Stress)) %>%
          lm(log10(Cycles)~log10(Stress)*Material,.) %>%
          fitted.values()
      }
      else {
        newdata$fv <- newdata %>%
          filter(!is.na(Stress)) %>%
          lm(log10(Cycles)~ log10(Stress) ,.) %>%
          fitted.values()
      }
      newdata$exponential <- 10^(newdata$fv)
      
      plot_ly(height=700) %>%
        add_lines(data=newdata,x = ~exponential, y = ~Stress,color = ~Material) %>%
        layout(legend=list(orientation="l"),
               xaxis = list(type = input$checkbox,title="Number of Cycles(n)"),
               yaxis = list(type = input$checkbox1,title="Stress(MPa)"))
      
    }
    else if((is.null(input$material)==F) & (is.null(input$tdata)==T || is.na(input$tdata)==T) & (is.na(input$value_a)==T || is.null(input$value_a)==T)) {
      req(input$material)
      sub <- subset(mat,Material %in% input$material,select=c("Slope","Intercept"))
      base <- ((sub$Slope)*log10(stres_assign))+sub$Intercept
      N <- 10^base
      updat_data1 <- as.data.frame(cbind(stres_assign,N))
      
      plot_ly(height=700) %>%
        add_trace(data = updat_data1, x = ~N, y = ~stres_assign,name = "Basquin Curve",text = basquin(),mode="lines+markers", marker = list(color = 'rgb(0,0,0)',size = 10),line = list(color = 'rgb(0,128,0)', width = 4,dash = 'dash')) %>%
        layout(legend=list(orientation="l"),
               xaxis = list(type = input$checkbox,title="Number of Cycles(n)"),
               yaxis = list(type = input$checkbox1,title="Stress(MPa)"))
    }
    else if((is.null(input$value_a)==F) & (is.null(input$tdata)==T || is.na(input$tdata)==T) & (is.na(input$material)==T || is.null(input$material)==T)) {
      
      
      #### ADDED THESE LINES AND ITS SOLVED
      req(input$value_a, input$value_b)
      base <- (input$value_a)*log10(stres_assign)+input$value_b
      N1 <- 10^base
      updat_data <- as.data.frame(cbind(stres_assign,N1))
      
      plot_ly(height=700) %>%
        add_trace(data = updat_data, x = ~N1, y = ~stres_assign,name = "Coefficient Curve",mode="lines+markers",text = coefficient(), marker = list(color = 'rgb(0,0,0)',size = 10),line = list(color = 'rgb(22, 96, 167)', width = 4,dash = 'dash')) %>%
        layout(legend=list(orientation="l"),
               xaxis = list(type = input$checkbox,title="Number of Cycles(n)"),
               yaxis = list(type = input$checkbox1,title="Stress(MPa)"))
    }
    else if((is.null(input$tdata)==F) & (is.null(input$material)==F) & (is.na(input$value_a)==T | is.null(input$value_a)==T)) {
      req(input$tdata,input$material)
      newdata <- subset(dt,Material %in% input$tdata,select=c(Stress,Cycles,Material))
      if(length(input$tdata)>1) {
        newdata$fv <- newdata %>%
          filter(!is.na(Stress)) %>%
          lm(log10(Cycles)~log10(Stress)*Material,.) %>%
          fitted.values()
      }
      else {
        newdata$fv <- newdata %>%
          filter(!is.na(Stress)) %>%
          lm(log10(Cycles)~ log10(Stress) ,.) %>%
          fitted.values()
      }
      newdata$exponential <- 10^(newdata$fv)
      sub <- subset(mat,Material %in% input$material,select=c("Slope","Intercept"))
      base <- ((sub$Slope)*log10(stres_assign))+sub$Intercept
      N <- 10^base
      updat_data1 <- as.data.frame(cbind(stres_assign,N))
      
      #
      plot_ly(height=700) %>%
        add_lines(data=newdata,x = ~exponential, y = ~Stress, color = ~Material) %>%
        add_trace(data = updat_data1, x = ~N, y = ~stres_assign,name = "Basquin Curve",mode="lines+markers",text = basquin(), marker = list(color = 'rgb(0,0,0)',size = 10),line = list(color = 'rgb(0,128,0)', width = 4,dash = 'dash')) %>%
        layout(legend=list(orientation="l"),
               xaxis = list(type = input$checkbox,title="Number of Cycles(n)"),
               yaxis = list(type = input$checkbox1,title="Stress(MPa)"))
      
    }
    
    else if((is.null(input$tdata)==F) &  (is.null(input$value_a)==F) & ((is.na(input$material)==T) || (is.null(input$material)==T))) {
      req(input$tdata,input$value_a)
      newdata <- subset(dt,Material %in% input$tdata,select=c(Stress,Cycles,Material))
      if(length(input$tdata)>1) {
        newdata$fv <- newdata %>%
          filter(!is.na(Stress)) %>%
          lm(log10(Cycles)~log10(Stress)*Material,.) %>%
          fitted.values()
      }
      else {
        newdata$fv <- newdata %>%
          filter(!is.na(Stress)) %>%
          lm(log10(Cycles)~ log10(Stress) ,.) %>%
          fitted.values()
      }
      newdata$exponential <- 10^(newdata$fv)
      base <- (input$value_a)*log10(stres_assign)+input$value_b
      N1 <- 10^base
      updat_data <- as.data.frame(cbind(stres_assign,N1))
      
      
      
      plot_ly(height=700) %>%
        add_lines(data=newdata,x = ~exponential, y = ~Stress,  color = ~Material) %>%
        add_trace(data = updat_data, x = ~N1, y = ~stres_assign,name = "Coefficient Curve", mode="lines+markers",text = coefficient(),marker = list(color = 'rgb(0,0,0)',size = 10),line = list(color = 'rgb(22, 96, 167)', width = 4,dash = 'dash')) %>%
        layout(legend=list(orientation="l"),
               xaxis = list(type = input$checkbox,title="Number of Cycles(n)"),
               yaxis = list(type = input$checkbox1,title="Stress(MPa)"))
      
    }
    else if((is.null(input$material)==F) & (is.null(input$value_a)==F) & ((is.na(input$tdata)==T) || (is.null(input$tdata)==T))) {
      req(input$material,input$value_a)
      sub <- subset(mat,Material %in% input$material,select=c("Slope","Intercept"))
      base <- ((sub$Slope)*log10(stres_assign))+sub$Intercept
      N <- 10^base
      updat_data1 <- as.data.frame(cbind(stres_assign,N))
      
      base <- (input$value_a)*log10(stres_assign)+input$value_b
      N1 <- 10^base
      updat_data <- as.data.frame(cbind(stres_assign,N1))
      
      plot_ly(height=700) %>%
        add_trace(data = updat_data1, x = ~N, y = ~stres_assign,name = "Basquin Curve",text = basquin(), mode="lines+markers",marker = list(color = 'rgb(0,0,0)',size = 10),line = list(color = 'rgb(0,128,0)', width = 4,dash = 'dash')) %>%
        add_trace(data = updat_data, x = ~N1, y = ~stres_assign,name = "Coefficient Curve",mode="lines+markers",text = coefficient(),marker = list(color = 'rgb(0,0,0)',size = 10),line = list(color = 'rgb(22, 96, 167)', width = 4,dash = 'dash')) %>%
        layout(legend=list(orientation="l"),
               xaxis = list(type = input$checkbox,title="Number of Cycles(n)"),
               yaxis = list(type = input$checkbox1,title="Stress(MPa)"))
    }
    else if((is.null(input$tdata)==F) & (is.null(input$value_a)==F) & (is.null(input$material)==F)) {
      req(input$tdata,input$value_a,input$material)
      newdata <- subset(dt,Material %in% input$tdata,select=c(Stress,Cycles,Material))
      if(length(input$tdata)>1) {
        newdata$fv <- newdata %>%
          filter(!is.na(Stress)) %>%
          lm(log10(Cycles)~log10(Stress)*Material,.) %>%
          fitted.values()
      }
      else {
        newdata$fv <- newdata %>%
          filter(!is.na(Stress)) %>%
          lm(log10(Cycles)~ log10(Stress) ,.) %>%
          fitted.values()
      }
      newdata$exponential <- 10^(newdata$fv)
      base <- (input$value_a)*log10(stres_assign)+input$value_b
      N1 <- 10^base
      updat_data <- as.data.frame(cbind(stres_assign,N1))
      
      # -------------------------------------------------------------------------
      
      sub <- subset(mat,Material %in% input$material,select=c("Slope","Intercept"))
      base <- ((sub$Slope)*log10(stres_assign))+sub$Intercept
      N <- 10^base
      updat_data1 <- as.data.frame(cbind(stres_assign,N))
      
      
      
      plot_ly(height=700) %>%
        add_lines(data=newdata,x = ~exponential, y = ~Stress, color = ~Material) %>%
        add_trace(data = updat_data, x = ~N1, y = ~stres_assign,name = "Coefficient Curve",mode="lines+markers",text = coefficient(),marker = list(color = 'rgb(0,0,0)',size = 10), line = list(color = 'rgb(205, 12, 24)', width = 4,dash = 'dash')) %>%
        add_trace(data = updat_data1, x = ~N, y = ~stres_assign,name = "Basquin Curve",mode="lines+markers",text = basquin(), marker = list(color = 'rgb(0,0,0)',size = 10),line = list(color = 'rgb(0,128,0)', width = 4,dash = 'dash')) %>%
        layout(legend=list(orientation="l"),
               xaxis = list(type = input$checkbox,title="Number of Cycles(n)"),
               yaxis = list(type = input$checkbox1,title="Stress(MPa)"))
    }
    
    
  })
  #Data Table for Material Coefficient
  output$sample_table<- DT::renderDataTable({
    dt <- datasetInput_sheet3()
    if (is.null(dt))
      return(NULL) else if(is.null(input$tdata))
        return(NULL)
    colnames(dt) <- c("Material","Project","Core","E1Alloy","EClad1","E2Alloy","EClad2","Int1Alloy","Int1","IntAlloy","Int","State","Thickness_in_mm","Supplier","RP0_2","Rm","A_in_Percentage","Date_of_beginning_of_test","Type_of_test","Stress_ratio",
                      "TestT","Rm_fatigueMachine_fatigueSpecimen","Basquin_coefficient","Material_Brazed")
    dt$`Date of beginning of test`<- c("1/13/2017", "11/27/2017","1/15/2018","12/05/2017", "02/06/2018","06/04/2018", "03/05/2018","7/24/2018", "04/11/2018", "12/04/2018")
    a<-subset(dt,Material %in% input$tdata,select=c(Material,Basquin_coefficient,RP0_2,Rm,A_in_Percentage,Core,State,Thickness_in_mm,Supplier,Date_of_beginning_of_test,Type_of_test,Stress_ratio,TestT,Rm_fatigueMachine_fatigueSpecimen,Material_Brazed))
    transposed <- t(a[,-1]);
    colnames(transposed) <- a[[1]];
    transposed <- data.table(transposed, keep.rownames=T);
    transposed<-setnames(transposed, 1, names(a)[1]);
    # DT::datatable(transposed,options = list(pageLength = 14,scrollX = TRUE))
    DT::datatable(transposed,options = list(pageLength=10,searching = FALSE,lengthChange = FALSE,scrollX = TRUE))
  })
  
  
})


shinyApp(ui = ui, server = server)


