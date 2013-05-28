library(shiny)
library(RJSONIO)
library(RSQLite)

## Logging functions
library(logging)
basicConfig()
addHandler(writeToFile, logger="shiny", file="/tmp/shiny.log")

logit <- function(...) {
  logwarn(paste(gsub("\\n", "", list(...)), collapse=" "), logger="shiny.module")
}

## some helpers
isFalsy <- function(x) is.null(x) || is.na(x) || (is.character(x) && x == "") || (is.numeric(x) && (is.nan(x) || x==0))

## handles hidden values
from_JSON <- function(x) {
  x <- RJSONIO::fromJSON(x)
  val <- x$val
  if(x$hidden) {
    con <- textConnection(val)
    val <- readRDS(con)
    close(con)
  }
  val
}


##################################################
## creating the database
db = dbConnect(dbDriver("SQLite"),dbname="submissions.db",loadable.extensions=TRUE)
dbGetQuery(db,paste("create table if not exists submit (",
                    "id INTEGER PRIMARY KEY,",
                    "setID TEXT NOT NULL,",
                    "assignment TEXT NOT NULL,",
                    "probID TEXT NOT NULL,",
                    "itemName TEXT NOT NULL,",
                    "firsttime TEXT NOT NULL,",
                    "lasttime TEXT NOT NULL,",
                    "score INTEGER,",
                    "answer TEXT NOT NULL,",
                    "freetext TEXT NOT NULL,",
                    "user TEXT NOT NULL)",sep=""))

# words = c("apple","berry","cherry","danish","egret")
# for (k in 1:length(words)) {
#   assignment=paste("assigment",sqrt(k),sep="")
#   problem = paste("prob",k^2,sep="")
#   score = k 
#   user = paste("Fred",k,sep="-")
#   dbGetQuery(db,paste("insert into submit values (null, '",
#                       assignment,"','",problem,"','",
#                       date(),"','",date(),"',",score,",'",words[k],"','",user,"')",sep=""))
# }

# permanent data (until database system is set up)
passwords <- read.csv("passwords.csv",stringsAsFactors=FALSE)
# Get the list of problems
problemSets <- read.csv("problemSets.csv",stringsAsFactors=FALSE)
assignmentList <- with(problemSets,Assignment[!duplicated(Assignment)])

# just a variable until I set up the database
responses <- NULL





## =================
shinyServer(function(input, output) {
  logit("Call shiny sever")

  # Function to handle a submission
  
  doSubmit <- function(val,text="NA",who="invalid") {
    if(is.null(val) || is.na(val) || !isValidJSON(I(val))) {
      return(invisible(1))
    }

    
    info <- from_JSON(I(val))

    ## some things still need grading!
    switch(info$type,
           "NumericInput" = {
             ## adjust points for numeric input if outside tolerance
             ans <- as.numeric(info$ans); tolerance <- as.numeric(info$tolerance)
             info$content <- student_ans <- as.numeric(text)
             if(is.na(student_ans) || abs(student_ans - ans) > tolerance) info$pts <- 0
           },
           "Fixed Choice" = { text = info$content },
           "MC"           = { text = info$content }
           )
                     
    ## Don't submit blank responses, otherwise they will be sent on initialization of the page
    if(!isFalsy(text)) {
      dbGetQuery(db,paste("insert into submit values (null, '",
                          info$itemInfo$setID,"','",
                          input$thisAssignment,"','",
                          input$thisProblem,"','",
                          info$itemInfo$name,"','",
                          date(),"','",date(),"',",
                          info$pts,",'",
                          info$content,"','",
                          toJSON(I(text)),"','",
                          who,"')",sep=""))
    }
    invisible(3)
  }
  
  output$assignmentSelector <- renderUI({
    selectInput("thisAssignment","Select Assignment:",assignmentList)
  })
  
  output$problemSelector <- renderUI({
    pList <- c("No problems available.")
    probs <- subset(problemSets,Assignment==input$thisAssignment)
    if( nrow(probs)>0 ) pList=as.character(probs$Problem)
    # Character string "Select Problem" is a flag not to load in any problem
    selectInput("thisProblem","Select a Problem:",c("Select Problem",pList))
  })

  # is the user logged in?
  loggedIn <- reactive({
 #   return("for debugging") # SKIP login during development
    m <- subset(passwords, name==input$loginID) ## use tolower()?
    if( nrow(m)>0  & m[1,]$pass==input$password) 
      return(m[1,]$name)
    else return(NULL)
  })
  
  # just for debugging.
  statusMessage <- reactive({
    return(paste( "Status Message to go here." )) 
  })
  
# Store the problem name, mode, and other information
  probData <- reactive({
#    cat(paste("A:",input$thisAssignment,
#              "P:", input$thisProblem,"\n"),file=stderr())
    probs <- subset(problemSets, Assignment==input$thisAssignment & Problem == input$thisProblem)
    if( nrow(probs)==0) stop("BUG: No such problem in problem list.")
    if( nrow(probs)>1 ) warning(paste(
      "More than one problem matches:",input$thisProblem, 
      "in", input$thisAssignment,". Check problemSets.csv"))
    return(as.list(probs[1,]))
  })
   
  probHTML <- reactive({        
    # Character string "Select Problem" is a flag not to load in any problem

    if(is.null(input$thisProblem)) {
      logit("null problem")
      logit(names(input))
      return()
    }
    
    if( input$thisProblem == "Select Problem")
      return("<center>No problem selected.</center>")
    
    prob <- probData() # Get the data on the selected problem, File, Answers, etc.

    
    cat(paste("File name: ", prob$File,"\n"),file=stderr())
    contents <- readChar(prob$File, file.info(prob$File)$size)

    if( !prob$Answers ) # Strip answers from the HTML file
      contents <- gsub("<aside.*?</aside>","",contents)
    # The regex will match the first closing aside, so can handle multiple asides
    # but it won't handle nested asides.
    
    # MathJax update at the end of the file.
    # This kills the activity of the Shiny controls
#    contents <- c(contents,"<script type='text/javascript'>MathJax.Hub.Typeset()</script>")
    return(contents)
  }) 

  output$asLoggedInStatus = renderText(
    paste("Logged in as",loggedIn())
  )
  
  output$loginStatus = renderText(
    ifelse (length(loggedIn())>0 ,"Login Successful!","Please login ..." )
  )
  
  output$probContents <- renderText({
    HTML(probHTML())
  })
  
#  output$mainStatus <- renderPrint({statusMessage()})
  

  
    output$mainStatus <- renderPrint({
      val <- input$nin1
    })


  ## connect the various controls to call doSubmit. Names match those from writeR
  sapply(1:10, function(n) {
    output[[sprintf("out%s", n)]]  <- renderPrint({doSubmit(input[[sprintf("in%s", n)]],
                                                            
                                                            who=loggedIn())})
    output[[sprintf("nout%s", n)]]  <- renderPrint({doSubmit(input[[sprintf("ninfo%s", n)]],
                                                             text=input[[sprintf("nin%s", n)]],  who=loggedIn())})

    
    output[[sprintf("tout%s", n)]]  <- renderPrint({
      input[[sprintf("trigger%s", n)]]  # the radio box trigger
      doSubmit(val=isolate(input[[sprintf("info%s", n)]]),
               text=isolate(input[[sprintf("text%s",n)]]),
               who=loggedIn())
    })
    output[[sprintf("MCout%s", n)]] <- renderPrint({
      try(doSubmit(input[[sprintf("MC%s", n)]],
                   who=loggedIn())
          )
    }) 
  })
  ## # For the Scores Tab ==================
  ## # Need to figure out how to trigger this when "Scores" tab is revealed.
  
  ## TO DO:
  # Add an "out of" score.
  # In the relevant display, list all of the problems and assignments, even those that the person hasn't answered.
  output$submissions <- 
    renderTable(
      { 
        user <- loggedIn()
        query <- paste("select assignment,probID,answer,score,lasttime ",
                      "from submit where user=='",
                      user,"'",sep="")
       # tab = dbGetQuery(db, query)
       # tab = aggregate( score ~ assignment, data=tab, FUN=sum)
       # return(tab)
        cat(paste("Choice: ", input$scoreChoice), file=stderr())
        
        if (input$scoreChoice=="Current problem") {
          query <- paste( query, " and probID=='",
                          input$thisProblem,"'",sep="") 
          tab <- dbGetQuery(db, query)
        }
        if (input$scoreChoice=="Current assignment") {
          query <- paste( query, " and assignment=='",
                          input$thisAssignment,"'",
                          sep="")
          tab <- dbGetQuery(db, query)
          tab = aggregate(score ~ probID, data=tab, FUN=sum)
        }
        if (input$scoreChoice=="All assignments") {
          tab <- dbGetQuery(db, query)
          tab <- aggregate(score ~ assignment, data=tab, FUN=sum)
        }
        return(tab)})
  
})

