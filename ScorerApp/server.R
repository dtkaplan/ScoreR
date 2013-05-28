library(shiny)
library(RJSONIO)
# creating the database
library(RSQLite)
justLoaded <- FALSE # flag about whether the problem HTML has just been loaded
selectSetStatus <- rep(TRUE,10)
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
                    "possible INTEGER,",
                    "autoScore INTEGER,", # has it been automatically scored
                    "answer TEXT NOT NULL,",
                    "freetext TEXT NOT NULL,",
                    "user TEXT NOT NULL)",sep=""))

# permanent data (until database system is set up)
passwords <- read.csv("passwords.csv",stringsAsFactors=FALSE)
# Get the list of problems
problemSets <- read.csv("problemSets.csv",stringsAsFactors=FALSE)
assignmentList <- with(problemSets,Assignment[!duplicated(Assignment)])
# ====================
printDebug <- function(x){
  cat(paste("DEBUG:",paste(names(x),as.character(x),sep="=",collapse="; "),file=stderr()))
}
# ===================
itemSubmitB <- function(val="bogus",assignment="abogus",problem="bogus",flag="blank"){
  if( is.null(val) ) cat("Item was empty\n",file=stderr())
  cat(paste("problem ",problem,"assignment", assignment,"itemSubmitB ",val, " with flag",flag,"\n"),file=stderr())
}
itemSubmit <- function(Assignment="bogus",Problem="bogus",ItemName="bogus",
                       setID="bogus",pts=999,totalpts=999,type="bogus",
                       content="bogus",text="",who="bogus",flag="blank"){
  cat(paste("itemSubmit with flag",flag,"\n"),file=stderr())
  prob <- probData(assignment=Assignment,problem=Problem) # Get the data on the selected problem, File, Answers, etc.
  if( !prob$Accept | !prob$Available) invisible(0) # don't accept submission
  # TO DO : if (prob$Accept=="Immediate") flash up the hint or reward.  
  
  # This if() is to avoid overly fast updates from textEntry objects
  # which appear to send an update every few characters or seconds
  # Don't submit blank responses, otherwise they will be sent on initialization of the page
  if( text != "" ) {
    # check if the problem has an earlier entry
    searchQuery = paste("select * from submit where ",
                        "user='",who,"' and ",
                        "assignment='",Assignment,"' and ",
                        "probID='",Problem,"' and ",
                        "itemName='",ItemName,"'",
                        sep="")
    res = dbGetQuery(db,searchQuery)
    cat(paste(searchQuery,"\n"),file=stderr())
    if( nrow(res) == 0 ) {
      query = paste("insert into submit values (null, '",
                    setID,"','",
                    Assignment,"','",
                    Problem,"','",
                    ItemName,"','",
                    date(),"','",date(),"',",
                    pts,",", # points scored
                    totalpts,",", # points possible
                    ifelse(type %in% c("Free text"),0,1),",'", # not scored if Free text, etc
                    content,"','",
                    toJSON(I(text)),"','",
                    who,"')",sep="")
      cat(paste(query,"\n"),file=stderr())
      dbGetQuery(db,query)
    }
    else { # replace the item
      # cat(paste("item id",res$id,"\n"),file=stderr())
      query = paste("update submit set ",
                    "answer='",content,"',",
                    "freetext='",toJSON(I(text)),"',",
                    "score=",pts,",",
                    "lasttime='",date(),"' ",
                    "where id='",res$id[1],"'",sep="")
      cat(query,file=stderr())
      dbGetQuery(db,query)
    }
  }
  invisible(3)
}
# ===============
probData <- function(assignment,problem){
  cat( paste("probData: Assignment ' ", assignment, "' Problem '",problem,"'\n",sep=""),file=stderr())
  if(problem=="Select Problem") {
    return(list(Assignment=assignment,Problem="No prob. selected",
                Accept=FALSE,Answers=FALSE,Available=TRUE))
  }
  else{
    probs <- subset(problemSets,Assignment==assignment & Problem == problem)
    if( nrow(probs)==0) stop("BUG: No such problem in problem list.")
    if( nrow(probs)>1 ) warning(paste(
      "More than one problem matches problem '",problem, 
      "' in assignment '", assignment,"'. Check problemSets.csv"))
    return(as.list(probs[1,]))
  }
}


# =================
# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Function to handle a submission
#   doSubmit <- function(val="invalid",text="",who="invalid",flag="none") {
#     if( justLoaded ) {
#       cat("in doSubmit: page just loaded\n",file=stderr())
#       justLoaded <<- FALSE
#       invisible(0)
#     }
#     cat("in doSubmit\n",file=stderr())
#     printDebug(list(a=3,b=6:7))
#     itemSubmit(flag=flag,Problem=input$thisProblem,Assignment=input$thisAssignment,
#                who=loggedIn())
#   }

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
  probData <- function(whence="nowhere"){ # not reactive
#    cat(paste("A:",input$thisAssignment,
 #             "P:", input$thisProblem,"\n"),file=stderr())
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
    paste("Logged in as",loggedIn())
  )
  
  output$loginStatus = renderText(
    ifelse (length(loggedIn())>0 ,"Login Successful!","Please login ..." )
  )
  
  checkOne <- observe({
    if( input$save==0 ) {
      cat("Initial save value\n",file=stderr())
      return()
    }
    else {
      isolate({
        prob <- input$thisProblem
        assign <- input$thisAssignment
        cat(paste("Successive save value",isolate(input$save),"\n"),file=stderr())
        itemSubmitB(val=isolate(input$in1),flag="from save in1",problem=prob,assignment=assign)
        itemSubmitB(val=isolate(input$in2),flag="from save in2",problem=prob,assignment=assign)
        itemSubmitB(val=isolate(input$in5),flag="from save in5",problem=prob,assignment=assign)
        itemSubmitB(val=isolate(input$in32),flag="from save in32",problem=prob,assignment=assign)
        itemSubmitB(val=isolate(input$MC1),flag="from save MC1",problem=prob,assignment=assign)
        itemSubmitB(val=isolate(input$text1),flag="from save text1",problem=prob,assignment=assign)
      })
      return()
    }
  }
  )
  
  output$mainStatus <- renderPrint({statusMessage()})
  
#   # free text
#   output$tout1 <- renderPrint({input$trigger1 #just to trigger the text
#                                doSubmit(val=isolate(input$info1),text=isolate(input$text1),who=loggedIn(),flag="text1")})
#   output$tout2 <- renderPrint({input$trigger2 #just to trigger the text
#                                doSubmit(val=isolate(input$info2),text=isolate(input$text2),who=loggedIn(),flag="text2")})
#   output$tout3 <- renderPrint({input$trigger3 #just to trigger the text
#                                doSubmit(val=isolate(input$info3),text=isolate(input$text3),who=loggedIn(),flag="text3")})
#   output$tout4 <- renderPrint({input$trigger4 #just to trigger the text
#                                doSubmit(val=isolate(input$info4),text=isolate(input$text4),who=loggedIn(),flag="text4")})
#   output$tout5 <- renderPrint({input$trigger5 #just to trigger the text
#                                doSubmit(val=isolate(input$info5),text=isolate(input$text5),who=loggedIn())})
#   output$tout6 <- renderPrint({input$trigger6 #just to trigger the text
#                                doSubmit(val=isolate(input$info6),text=isolate(input$text6),who=loggedIn())})
#   output$tout7 <- renderPrint({input$trigger7 #just to trigger the text
#                                doSubmit(val=isolate(input$info7),text=isolate(input$text7),who=loggedIn())})
#   output$tout8 <- renderPrint({input$trigger8 #just to trigger the text
#                                doSubmit(val=isolate(input$info8),text=isolate(input$text8),who=loggedIn())})
#   output$tout9 <- renderPrint({input$trigger9 #just to trigger the text
#                                doSubmit(val=isolate(input$info9),text=isolate(input$text9),who=loggedIn())})
#   output$tout10 <- renderPrint({input$trigger10 #just to trigger the text
#                                doSubmit(val=isolate(input$info10),text=isolate(input$text10),who=loggedIn(),flag="text10")})
#   
#   
#   # Choice from List
#   output$out1 <- renderPrint({doSubmit(val=input$in1,who=loggedIn(),flag="SS1")}) 
#   output$out2 <- renderPrint({doSubmit(input$in2,who=loggedIn(),flag="SS2")}) 
#   output$out3 <- renderPrint({doSubmit(input$in3,who=loggedIn())}) 
#   output$out4 <- renderPrint({doSubmit(input$in4,who=loggedIn())}) 
#   output$out5 <- renderPrint({doSubmit(input$in5,who=loggedIn())}) 
#   output$out6 <- renderPrint({doSubmit(input$in6,who=loggedIn())}) 
#   output$out7 <- renderPrint({doSubmit(input$in7,who=loggedIn())}) 
#   output$out8 <- renderPrint({doSubmit(input$in8,who=loggedIn())}) 
#   output$out9 <- renderPrint({doSubmit(input$in9,who=loggedIn())}) 
#   output$out10 <- renderPrint({doSubmit(input$in10,who=loggedIn(),flag="SS10")}) 
#   
#   # Multiple Choice
#   #
#   # BUG: I don't know why I have to wrap this in try().  If not, on initialization I get
#   # an error message which shows up in the document.
#   output$MCout1 <- renderPrint({try(doSubmit(input$MC1,who=loggedIn(),flag="MC1"))}) 
#   output$MCout2 <- renderPrint({try(doSubmit(input$MC2,who=loggedIn(),flag="MC2"))})
#   output$MCout3 <- renderPrint({try(doSubmit(input$MC3,who=loggedIn()))})
#   output$MCout4 <- renderPrint({try(doSubmit(input$MC4,who=loggedIn()))})
#   output$MCout5 <- renderPrint({try(doSubmit(input$MC5,who=loggedIn()))})
#   output$MCout6 <- renderPrint({try(doSubmit(input$MC6,who=loggedIn()))}) 
#   output$MCout7 <- renderPrint({try(doSubmit(input$MC7,who=loggedIn()))})
#   output$MCout8 <- renderPrint({try(doSubmit(input$MC8,who=loggedIn()))})
#   output$MCout9 <- renderPrint({try(doSubmit(input$MC9,who=loggedIn()))})
#   output$MCout10 <- renderPrint({try(doSubmit(input$MC10,who=loggedIn(),flab="MC10"))})
  # For the Scores Tab ==================
  # Need to figure out how to trigger this when "Scores" tab is revealed.
  
  ## TO DO:
  # In the relevant display, list all of the problems and assignments, even those that the person hasn't answered.
  output$submissions <- 
    renderTable(
      { 
        user <- loggedIn()
        query <- paste("select assignment,probID,itemName,answer,score,autoScore,possible,lasttime,firsttime ",
                      "from submit where user=='",
                      user,"'",sep="")
        # Which scoring mode        
        if (input$scoreChoice=="Current problem") {
          if( input$thisProblem == "Select Problem") tab=data.frame("No problem selected"=0)
          else {
            prob <- probData() # Get the data on the selected problem, File, Answers, etc.
            query <- paste( query, " and probID=='",
                            input$thisProblem,"'",sep="") 
            tab <- dbGetQuery(db, query)
            tab$score <- as.character(tab$score)
            if (!prob$Answers) tab$score <- "not released"
            else tab$score[tab$autoScore==0] <- "not yet scored"
            tab$autoScore <- NULL
            tab$assignment <- NULL
            tab$probID <- NULL

          }
        }
        else { # Current assignment or current problem
          if (input$scoreChoice=="Current assignment") {
            query <- paste( query, " and assignment=='",
                          input$thisAssignment,"'",
                          sep="")
          }
          tab <- dbGetQuery(db, query)
          tab <- merge(tab,problemSets, # See if the answers are released
                       by.x=c("assignment","probID"),
                       by.y=c("Assignment","Problem"))
          # Keep all the problems in this assignment so that the report shows zero for 
          # those not yet done.
          
          tab$notYetScored <- tab$possible
          tab$notYetReleased <- tab$possible
          tab$notYetScored[tab$autoScore>0] <- 0 # leave only those that are not scored automatically
          tab$notYetReleased[tab$Answers | (tab$notYetScored>0)] <- 0 #  released or not yet scored
          tab$score[(tab$notYetScored>0) | (tab$notYetReleased>0)] <- 0# Don't show scores unless answers are available
          tab$outOf <- with(tab,possible-(notYetReleased+notYetScored))
          if (nrow(tab) != 0 | any(!is.na(tab$score))) {
            if(input$scoreChoice=="Current assignment"){
              tab = aggregate(cbind(score,outOf,notYetReleased,notYetScored,possible) ~ probID, data=tab, FUN=sum)
            }
            else { # All assignments
              tab <- aggregate(cbind(score,outOf,notYetReleased,notYetScored,possible) ~ assignment, data=tab, FUN=sum)
            }    
 #           tab$possible <- NULL
          }
          else tab=data.frame("No Submissions Made"=0) # just to show the message
        }
        return(tab)
        }
      )
  
  output$probContents <- renderText({
    input$thisProblem # Just to create the reaction
    HTML(probHTML())
  })
  
  probHTML <- reactive({  
    # Character string "Select Problem" is a flag not to load in any problem
    if( input$thisProblem == "Select Problem"){
      return(readChar("IntroBanner.html", file.info("IntroBanner.html")$size))
      # return("<center>No problem selected.</center>")
    }
    prob <- probData("from probHTML") # Get the data on the selected problem, File, Answers, etc.
    #    cat(paste("File name: ", prob$File,"\n"),file=stderr())
    
    # Check to see if it's available.  If not, give a message to that effect.
    if (prob$Available) contents <- readChar(prob$File, file.info(prob$File)$size)
    else contents <- paste("Problem '",prob$Problem,
                           "' in assignment '",prob$Assignment,
                           "' not yet available.",sep="")
    
    if( !prob$Answers ) # Strip answers from the HTML file
      contents <- gsub("<aside.*?</aside>","",contents)
    # The regex will match the first closing aside, so can handle multiple asides
    # but it won't handle nested asides.
    
    # MathJax update at the end of the file.
    # This kills the activity of the Shiny controls
    #    contents <- c(contents,"<script type='text/javascript'>MathJax.Hub.Typeset()</script>")
    return(contents)
  }) 
  
  
})

