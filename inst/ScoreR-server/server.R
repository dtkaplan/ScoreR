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
 #   ifelse (length(loggedIn())>0 ,"Login Successful!","Please login ..." )
    ifelse (!is.null(userInfo()$name) ,"Login Successful!","Please login ..." )
  )
  
  
  checkOne <- observe({
    # Just checking how to update a value
    # updateTextInput(session,"text1",value="When in the course...")
    if( input$save==0 ) {
      # cat("Initial save value\n",file=stderr())
      return(3)
    }
    else {
      # cat("One\n",file=stderr())
      if( isolate(input$thisProblem)=="Select Problem") return()
      # Get the roster
      # cat("Two\n",file=stderr())
      roster <- getRoster() #isolate(fromJSON(I(input$roster)))
      #  cat( paste("check One Roster: ",paste(roster,collapse=" "),"\n"),file=stderr())
      isolate({
        prob <- input$thisProblem
        assign <- input$thisAssignment
        who <- userInfo()$name
        for (thisProb in roster) {
          cat(paste(thisProb," hi \n"),file=stderr())
          val <- input[[thisProb]]
          info <- input[[paste(thisProb,"info",sep="")]]
          newItemSubmit(val,info,P=prob,A=assign,who=who)
        }
      })
      return()
    }
  }
  )
  
  getRoster <- reactive({
                         cat(paste((fromJSON(I(input$roster))),"\n"),file=stderr())
    input$roster # just to trigger the event
                         return(fromJSON(I(gsub("\\\\","",input$roster))))
                         ### UNDO the gsub when you solve the roster problem.
  })
  
  output$mainStatus <- renderPrint({statusMessage()})
  
 
  checkRoster <- observe({
    input$roster
  })
  
  output$probContents <- renderText({
    input$thisProblem # Just to create the reaction
    HTML(probHTML())
  })
  

  
})

