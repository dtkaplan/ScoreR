library(shiny)
library(shinyIncubator)
courseName="Math 155"
# a flag to indicate when the person has logged in.
# This is a string containing JavaScript code
loginFlag = "output.loginStatus=='Login Successful!'"



#possibleProblems <- dir("Contents")
#htmlFiles <- possibleProblems[grep(".+\\.html",possibleProblems)]
#htmlNames <- sub(".html","",htmlFiles)


shinyUI(
  pageWithSidebar(
    headerPanel(paste(courseName,"Assignments")),
    sidebarPanel(
      p(textOutput("loginStatus")),
      conditionalPanel(
        condition="output.loginStatus == 'Please login ...'",
        textInput("loginID","Login ID:",value=""),
        textInput("password","Password:",value="")
      ),
      conditionalPanel(
        condition = "output.loginStatus == 'Login Successful!'",
        p(textOutput("asLoggedInStatus")),
        uiOutput("assignmentSelector"),
        uiOutput("problemSelector"),
        actionButton("save","Submit")
      ),
      verbatimTextOutput("mainStatus")
    ),
    mainPanel(
      conditionalPanel(
        condition = loginFlag, 
        tabsetPanel(
        tabPanel("Problem",
                 htmlOutput("probContents")
                ),
        tabPanel("Scores",
                 p("Make sure to select an assignment and problem if you want specifics."),
                 selectInput("scoreChoice","What do you want to see?",
                             c("All assignments","Current assignment","Current problem")),
                 h2(tableOutput("submissions")) # to track submissions for now
        ),
        tabPanel("Account Maintenance",
                 h2(textOutput("AccountHeader"))
                )
        )
      )
    )
  )
)