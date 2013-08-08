library(shiny)
library(shinyIncubator)
courseName="Math 155"
# a flag to indicate when the person has logged in.
# This is a string containing JavaScript code
loginFlag = "output.loginStatus=='Login Successful!'"



#possibleProblems <- dir("Contents")
#htmlFiles <- possibleProblems[grep(".+\\.html",possibleProblems)]
#htmlNames <- sub(".html","",htmlFiles)
 mathjax <- function(){
tagList(singleton(tags$head(
  tags$script(src="https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"))))
 }

shinyUI(
  pageWithSidebar(
    headerPanel(paste(courseName,"Assignments")), # just to see if I can set up mathjax
    sidebarPanel(
      mathjax(),
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
        actionButton("save","Submit"),
        actionButton("reload","Reload (move this)")
      )
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
                             c("Current problem","Current assignment","All assignments")),
                 h2(tableOutput("submissions")) # to track submissions for now
        ),
        tabPanel("Account Maintenance",
                 h2(textOutput("AccountHeader")),
                 p("This facility not available in this version of ScoreR.")
                ),
        tabPanel("Class Grading",
                 p("Available only to instructors."),
                 uiOutput("studentSelector"),
                 uiOutput("gradeLevelSelector"), # choose assignments, 
                 uiOutput("gradeFileDownload"),
                 h2(tableOutput("classScores")))
        )
      )
    )
  )
)