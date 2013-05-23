library(shiny)
library(RJSONIO)

# permanent data (until database system is set up)
passwords <- read.csv("passwords.csv",stringsAsFactors=FALSE)
# Get the list of problems
problemSets <- read.csv("problemSets.csv",stringsAsFactors=FALSE)
assignmentList <- with(problemSets,Assignment[!duplicated(Assignment)])

# just a variable until I set up the database
responses <- NULL


# Function to handle a submission
doSubmit <- function(val,text="NA",who="invalid") {
#  cat(paste(text,"\n"),file=stderr())
#  cat(paste(val,"\n"),file=stderr())
  if (isValidJSON(I(val))) {
    info <- fromJSON(I(val))
    # package up submission for saving
    submit <- list(user=who,when=date(),pts=info$pts,
                   set=info$itemInfo$setID,
                   itemN=info$itemInfo$itemN,
                   name=info$itemInfo$name,
                   text=toJSON(I(text)),
                   content=info$content)
    #cat("OK")
    # cat(as.character(submit))
    # Don't submit blank responses, otherwise they will be sent on initialization of the page
    if( text != "") responses <<- rbind(responses,submit)
    # res <- as.character(info$itemInfo$setID)
    # cat(paste(" with set ID",res))
    invisible(3)
  }
  else {
    #cat("initializing ...")
    invisible(1)
  }
  
}


# =================
# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
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
    return("for debugging") # SKIP login during development
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
  output$submissions <- renderTable({input$scoreChoice
                                     responses})
  
})

