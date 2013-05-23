library(shiny)
library(RJSONIO)
# creating the database
library(RSQLite)
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





# =================
# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Function to handle a submission
  doSubmit <- function(val,text="NA",who="invalid") {
    #  cat(paste(text,"\n"),file=stderr())
    #  cat(paste(val,"\n"),file=stderr())
    if (isValidJSON(I(val))) {
      info <- fromJSON(I(val))

      # Don't submit blank responses, otherwise they will be sent on initialization of the page
      if( text != "" ) {
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
    else {
      #cat("initializing ...")
      invisible(1)
    }
    
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
    probs <- subset(problemSets,Assignment==input$thisAssignment & Problem == input$thisProblem)
    if( nrow(probs)==0) stop("BUG: No such problem in problem list.")
    if( nrow(probs)>1 ) warning(paste(
      "More than one problem matches:",input$thisProblem, 
      "in", input$thisAssignment,". Check problemSets.csv"))
    return(as.list(probs[1,]))
  })
   
  probHTML <- reactive({        
    # Character string "Select Problem" is a flag not to load in any problem
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
  
  output$mainStatus <- renderPrint({statusMessage()})
  

  output$tout1 <- renderPrint({input$trigger1 #just to trigger the text
                               doSubmit(val=isolate(input$info1),text=isolate(input$text1),who=loggedIn())})
  output$tout2 <- renderPrint({input$trigger2 #just to trigger the text
                               doSubmit(val=isolate(input$info2),text=isolate(input$text2),who=loggedIn())})
  output$tout3 <- renderPrint({input$trigger3 #just to trigger the text
                               doSubmit(val=isolate(input$info3),text=isolate(input$text3),who=loggedIn())})
  output$tout4 <- renderPrint({input$trigger4 #just to trigger the text
                               doSubmit(val=isolate(input$info4),text=isolate(input$text4),who=loggedIn())})
  output$tout5 <- renderPrint({input$trigger5 #just to trigger the text
                               doSubmit(val=isolate(input$info5),text=isolate(input$text5),who=loggedIn())})
  # Choice from List
  output$out1 <- renderPrint({doSubmit(input$in1,who=loggedIn())}) 
  output$out2 <- renderPrint({doSubmit(input$in2,who=loggedIn())}) 
  output$out3 <- renderPrint({doSubmit(input$in3,who=loggedIn())}) 
  output$out4 <- renderPrint({doSubmit(input$in4,who=loggedIn())}) 
  output$out5 <- renderPrint({doSubmit(input$in5,who=loggedIn())}) 
  
  # Multiple Choice
  #
  # BUG: I don't know why I have to wrap this in try().  If not, on initialization I get
  # an error message which shows up in the document.
  output$MCout1 <- renderPrint({try(doSubmit(input$MC1,who=loggedIn()))}) 
  output$MCout2 <- renderPrint({try(doSubmit(input$MC2,who=loggedIn()))})
  output$MCout3 <- renderPrint({try(doSubmit(input$MC3,who=loggedIn()))})
  output$MCout4 <- renderPrint({try(doSubmit(input$MC4,who=loggedIn()))})
  output$MCout5 <- renderPrint({try(doSubmit(input$MC5,who=loggedIn()))})
  # For the Scores Tab ==================
  # Need to figure out how to trigger this when "Scores" tab is revealed.
  
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

