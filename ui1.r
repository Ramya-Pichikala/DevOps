
ui1 <- function(){
  tagList(

    div(id = "login",

        wellPanel(textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),


    tags$style(type="text/css", "#login {font-size:10px;
               text-align: left;position:absolute;
               top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}"),

    tags$script(type="text/javascript", src = "md5.js"),
    tags$script(type="text/javascript", src = "passwdInputBinding.js"),
    useShinyjs()

  )}
