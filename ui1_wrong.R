ui1_wrong <- function(){

  fluidPage(
  tagList(
    div(id = "login",
        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))
        
        ),
    
#     wellPanel(
#       span("Wrong Username and/or Password!", style = "color:red")
#     ),
#     
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  ),
  
  ###
  fixedRow(
    column(8, offset = 4,
           h1("Wrong Username and/or Password!", style = "color:red", size = "20px")
    )
  )
  ##
  
  )
}
