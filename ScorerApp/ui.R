library(shiny)
courseName="Math 155"
# a flag to indicate when the person has logged in.
# This is a string containing JavaScript code
loginFlag = "output.loginStatus=='Login Successful!'"

# Get the list of problems
possibleProblems <- dir("Contents")
htmlFiles <- possibleProblems[grep(".+\\.html",possibleProblems)]
htmlNames <- sub(".html","",htmlFiles)

# get the list of current assignments:
# Make this into something that reads from the assignment structure:
assignmentList <- function(){c("One","Two","Three")}
# Read from the problems under that assignment
problemList <- function(){c("First","Second")}

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
        selectInput("ThisProb", "Problem: ",c("Select Problem",htmlNames) )
      ),
      verbatimTextOutput("mainStatus"),
      verbatimTextOutput("testingTextOutput")
    ),
    mainPanel(
      verbatimTextOutput("inProblemOutput"),
      conditionalPanel(
        condition = loginFlag, 
        tabsetPanel(
        tabPanel("Problem",
                 htmlOutput("probContents")
                ),
        tabPanel("Scores",
                 selectInput("scoreChoice","What do you want to see?",
                             c("Current problem","Current assignment", "All assignments")),
                 h2(tableOutput("submissions")) # to track submissions for now
        ),
        tabPanel("Assignments",
                  selectInput("assignmentList", "Assignments:",
                              assignmentList()),
                  selectInput("problemList","Problems in that assignment:",
                              problemList())
        ),
        tabPanel("Account Maintenance",
                 h2(textOutput("AccountHeader"))
                )
        )
      )
    )
  )
)