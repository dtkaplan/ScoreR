library(shiny)
library(shinyIncubator)
courseName="Math 155"
# a flag to indicate when the person has logged in.
# This is a string containing JavaScript code
loginFlag = "output.loginStatus=='Login Successful!'"
instructorFlag = "output.userStatus=='instructor'"


mathjax <- function(){
  tagList(singleton(tags$head(
  tags$script(src="https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"))))
 }

shinyUI(
  pageWithSidebar(
    headerPanel(paste(courseName,"Assignments")), # just to see if I can set up mathjax
    #sidebarPanel( # sidebarPanel is class span4, make it a bit narrower
      div(class="span3",tags$form(#class="well",
      mathjax(),
      
      conditionalPanel(
        condition="output.loginStatus == 'Please login ...'",
        p(textOutput("loginStatus")),
        textInput("loginID","Login ID:",value=""),
        textInput("password","Password:",value="")
      ),
      conditionalPanel(
        condition = "output.loginStatus == 'Login Successful!'",
        span(textOutput("asLoggedInStatus"),textOutput("userStatus")),
        uiOutput("assignmentSelector"),
        uiOutput("problemSelector"),
        actionButton("save","Submit")
      )
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "output.userStatus=='instructor'",
        wellPanel(
          tabsetPanel(  
            tabPanel("Score Report",
                     # p("Available only to instructors."),
                     uiOutput("studentSelector"),
                     uiOutput("gradeLevelSelector"), # choose assignments, 
                     uiOutput("gradeFileDownload"),
                     h2(tableOutput("classScores"))
                     ),
            tabPanel("Manual Grading",
                     uiOutput("studentSelectorMan"),
                     uiOutput("itemSelectorMan"), # choose an item from the present problem, 
                     h2(tableOutput("gradedTable")),
                     h2(tableOutput("itemTable")), # temporary
                     p("Put the text,next, and score selectors here."),
                     numericInput("whichSubmission","Submission being graded:",value=1,min=1,max=1,step=1),
                     selectInput("scoreAssigned","Score:",
                                 choices=as.list(0:9),selected=3),
                     pre(textOutput("currentItemText"))
                     ),
            tabPanel("Account Maintenance",
                     p("After editing the problem list, press this button. Then login again."),
                     actionButton("reload","Reset Problem List")
            )    
          )
        )
      ),
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
          )
        )
      )
      
    )
  )
)