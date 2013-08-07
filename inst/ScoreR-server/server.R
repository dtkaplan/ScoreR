library(shiny)
library(RJSONIO)
library(mosaic)  # for fetchGoogle
# creating the database
library(RSQLite)
library(RCurl)  # This might be causing problems on glimmer.
source("SharedAmongSessions.R", local=TRUE)
source("ItemSubmit.R", local=TRUE)


# =================
shinyServer(function(input, output, session) {
  source("UserSession.R",local=TRUE) # read in code from external file
  source("Grading.R",local=TRUE) # instructor grading
  source("ProbHTML.R",local=TRUE) # loading the HTML of a problem
  source("ShowScores.R",local=TRUE) # showing students their scores
  
  output$problemSelector <- renderUI({
    pList <- c("No problems available.")
    probs <- subset(problemSets,Assignment==input$thisAssignment)
    if( nrow(probs)>0 ) pList=as.character(probs$Problem)
    # Character string "Select Problem" is a flag not to load in any problem
    selectInput("thisProblem","Select a Problem:",c("Select Problem",pList))
  })


  
  # Information about the user
  # this will replace loggedIn()
  userInfo <- reactive({
    # ONLY for debugging
    return(list(name="Danny",grader=TRUE))
    # Normal code
    m <- subset(passwords, name==input$loginID)
    if( nrow(m)>0  && m[1,]$pass==input$password ){
      return(list(name=m[1,]$name,grader=m[1,]$role=="grader"))
    }
    else return(list(name=NULL,grader=FALSE))
  })
  
  # just for debugging.
  statusMessage <- reactive({
    return(cat("Starting up." )) 
  })
  
# Store the problem name, mode, and other information
  probData <- function(whence="nowhere"){ # not reactive
#    cat(paste("A:",input$thisAssignment,
#              "P:", input$thisProblem,"\n"),file=stderr())
    if( input$thisProblem=="Select Problem") {
      return(list(Assignment=input$thisAssignment,Problem="No prob. selected",Accept=FALSE,Answers=FALSE,Available=TRUE))
    }
    else{
    probs <- subset(problemSets,Assignment==input$thisAssignment & Problem == input$thisProblem)
#  cat(paste("whence=",whence,"nrow",nrow(probs),"\n"),file=stderr())
    if( nrow(probs)==0) stop("BUG: No such problem in problem list.")
    if( nrow(probs)>1 ) warning(paste(
      "More than one problem matches:",input$thisProblem, 
      "in", input$thisAssignment,". Check problemSets.csv"))
    return(as.list(probs[1,]))
    }
  }
   

  output$asLoggedInStatus = renderText(
    paste("Logged in as",userInfo()$name)
  )
  
  output$loginStatus = renderText(
    ifelse (!is.null(userInfo()$name) ,"Login Successful!","Please login ..." )
  )
  
  # handle pressing the submit button
  userSubmit <- observe({
    if( input$save==0 ) return() # Nothing's happened yet.
    if( isolate(input$thisProblem)=="Select Problem") return() # No problem's displayed

    # The user is pressing the submit button.
    # Get the roster
    roster <- getRoster() #isolate(fromJSON(I(input$roster)))
    # don't activate just because one of the other inputs changes, unless there is a submit press
    isolate({
        prob <- input$thisProblem
        assign <- input$thisAssignment
        who <- userInfo()$name
        for (k in seq_along(roster)) {
          thisProb <- paste("ScoreR",k,sep="")
          val <- input[[thisProb]]
          info <- input[[paste(thisProb,"info",sep="")]]
          newItemSubmit(val,info,P=prob,A=assign,who=who)
        }
    })
    return()
    })
  # =============
  getRoster <- reactive({ 
    if (is.null(input$roster)) return(NULL)
    else fromJSON(I(gsub("\\\\","",input$roster))) 
  })
  # ==============
  output$probContents <- renderText({
    input$thisProblem # just for the dependency
    # Construct the HTML to load into the problem tab
    HTML(probHTML())
  })
  # ==============
  # Once the HTML has been loaded, get the roster and submitted answers and
  # update the displays for the user selections to reflect any values
  # already in the database.
  
  # perhaps use invalidateLater()???
  # Or maybe user the state of roster to handle this?
  updateSelectionDisplay <- observe({
    roster <- getRoster() # for the dependency
    if (is.null(roster)) return() # no itemps in the update
    prob <- isolate(input$thisProblem)
    assign <- isolate(input$thisAssignment)
    who <- userInfo()$name
    # Get the answers already submitted
    fromDB <- getSubmittedAnswers(who,assign,prob)
    # Walk through the problems on the roster and pull out any that have free-text
    # associated with them
    submittedNames <- fromDB$itemName
    for (k in seq_along(roster)){
      inds <- which( roster[k] == submittedNames )
      if (length(inds) != 0 ) {
        itemID <- paste("ScoreR",k,sep="")
        outputID <- paste(itemID,"out",sep="")
        # If it's free text, update that from the input from the database
        freetext <- fromJSON(fromDB[inds[1],"freetext"])
        answer <- fromDB[inds[1],"answer"]
        if (nchar(freetext) > 0) updateTextInput(session,itemID,value=freetext)
        else updateTextInput(session,outputID,value=paste("last =",answer))
        
      }
    }
  })
})

