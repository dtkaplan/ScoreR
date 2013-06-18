library(shiny)
library(RJSONIO)
library(mosaic)  # for fetchGoogle
# creating the database
library(RSQLite)
library(RCurl)  # This might be causing problems on glimmer.

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
tmp <- fetchGoogle("https://docs.google.com/spreadsheet/pub?key=0Am13enSalO74dF9ZZnRmekpxcVp6MllOZDlPZU5GcXc&single=true&gid=0&output=csv")
# Reading from the local files.
problemSets2 <- read.csv("problemSets.csv",stringsAsFactors=FALSE)
tmp <- rbind(problemSets2,tmp)
tmp$File <- as.character(tmp$File)
tmp$Assignment <- as.character(tmp$Assignment)
tmp$Problem <- as.character(tmp$Problem)
problemSets <- tmp

assignmentList <- with(problemSets,Assignment[!duplicated(Assignment)])
# ===================
itemSubmit <- function(val,A="bogus",P="bogus",who="bogus",text="",flag="blank",roster=NULL){
#  cat(paste("Flag", flag, "Roster", paste(roster,collapse=" "),"\n"),file=stderr())
  if( !(flag %in% roster)) return()
  if (is.null(val) || is.na(val) || val=="NA") { 
    # cat("empty val\n",file=stderr()); 
    return(0)
  }
  if (who == "bogus") {cat("bogus who\n",file=stderr()); return()}
  prob <- probData(assignment=A,problem=P) # Get the data on the selected problem, File, Answers, etc.
  if( !prob$Accept || !prob$Available) {cat("prob not avail\n",file=stderr());return() }# don't accept submission
  if( substr(flag,1,4)=="text" && nchar(text)==0) return(); # empty text submission
  #cat(paste(nchar(val),"\n"),file=stderr())
  
  v <- fromJSON(I(val)) # read in the structure from the stored data
  content <- v$content
  type <- v$type
  pts <- v$pts
  totalpts <- v$itemInfo$totalpts
  setID <- v$itemInfo$setID
  ItemName <- v$itemInfo$name
  
  # TO DO : if (prob$Accept=="Immediate") flash up the hint or reward.  
  
  
  # check if the problem has an earlier entry
  searchQuery = paste("select * from submit where ",
                      "user='",who,"' and ",
                      "assignment='",A,"' and ",
                      "probID='",P,"' and ",
                      "itemName='",ItemName,"'",
                      sep="")
  res = dbGetQuery(db,searchQuery)
#  cat(paste(searchQuery,"\n"),file=stderr())
  if( nrow(res) == 0 ) {
    query = paste("insert into submit values (null, '",
                  setID,"','",
                  A,"','",
                  P,"','",
                  ItemName,"','",
                  date(),"','",date(),"',",
                  pts,",", # points scored
                  totalpts,",", # points possible
                  ifelse(type %in% c("Free text"),0,1),",'", # not scored if Free text, etc
                  content,"','",
                  toJSON(I(text)),"','",
                  who,"')",sep="")
 #   cat(paste(query,"\n"),file=stderr())
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
  #  cat(query,file=stderr())
    dbGetQuery(db,query)
  }
}
# ===============
probData <- function(assignment,problem){
#  cat( paste("probData: Assignment ' ", assignment, "' Problem '",problem,"'\n",sep=""),file=stderr())
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

  output$assignmentSelector <- renderUI({
    selectInput("thisAssignment","Select Assignment:",assignmentList)
#    output$mainStatus <- renderPrint({cat("\n")})
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
    paste("Logged in as",loggedIn())
  )
  
  output$loginStatus = renderText(
    ifelse (length(loggedIn())>0 ,"Login Successful!","Please login ..." )
  )
  
  
  checkOne <- observe({
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
        who <- loggedIn()
        # cat(paste("Successive save value",isolate(input$save),"\n"),file=stderr())
        itemSubmit(val=input$in1,flag="in1",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$in2,flag="in2",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$in3,flag="in3",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$in4,flag="in4",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$in5,flag="in5",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$in6,flag="in6",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$in7,flag="in7",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$in8,flag="in8",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$in9,flag="in9",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$in10,flag="in10",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$in32,flag="in32",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$MC1,flag="MC1",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$MC2,flag="MC2",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$MC3,flag="MC3",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$MC4,flag="MC4",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$MC5,flag="MC5",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$MC6,flag="MC6",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$MC7,flag="MC7",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$MC8,flag="MC8",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$MC9,flag="MC9",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$MC10,flag="MC10",P=prob,A=assign,who=who,roster=roster)
#        cat(paste("Text1 is '",input$text1,"'\n",sep=""),file=stderr())
        itemSubmit(val=input$info1,text=input$text1,
                   flag="text1",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$info2,text=input$text2,
                   flag="text2",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$info3,text=input$text3,
                   flag="text3",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$info4,text=input$text4,
                   flag="text4",P=prob,A=assign,who=who,roster=roster)
        itemSubmit(val=input$info5,text=input$text5,
                   flag="text5",P=prob,A=assign,who=who,roster=roster)
        
      })
      return()
    }
  }
  )
  
  getRoster <- reactive({#cat("three\n",file=stderr())
                         #cat(paste(class(input$roster),"\n"),file=stderr())
                         return(fromJSON(I(input$roster)))})
  
  output$mainStatus <- renderPrint({statusMessage()})
  
  # For the Scores Tab ==================
  # Need to figure out how to trigger this when "Scores" tab is revealed.
  
  ## TO DO:
  # In the relevant display, list all of the problems and assignments, even those that the person hasn't answered.
  output$submissions <- 
    renderTable(
      { 
        input$save # for the dependency
        input$save2 # for the dependency
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
  
  checkRoster <- observe({
    input$roster
  })
  
  output$probContents <- renderText({
    input$thisProblem # Just to create the reaction
    HTML(probHTML())
  })
  
  probHTML <- reactive({  
    # Character string "Select Problem" is a flag not to load in any problem
    if( length(input$thisProblem)==0 || input$thisProblem == "Select Problem"){
      return(readChar("IntroBanner.html", file.info("IntroBanner.html")$size))
      # return("<center>No problem selected.</center>")
    }
    prob <- probData("from probHTML") # Get the data on the selected problem, File, Answers, etc.
     #   cat(paste("File name: ", prob$File,"\n"),file=stderr())
    
     if (grepl("http",prob$File,fixed=TRUE)) { # a web address
         contents <- getURL(prob$File)
     } # a file on this system
     else 
      contents <- readChar(prob$File, file.info(prob$File)$size)
    
    # Check to see if it's available.  If not, give a message to that effect.
    if (!prob$Available) {
      contents <- paste("Problem '",prob$Problem,
                           "' in assignment '",prob$Assignment,
                           "' not yet available.",sep="")
    }
    # Set up a retrigger of Mathjax
#      contents <- gsub("</head>",
#                        "<script type='text/javascript'>MathJax.Hub.Queue(['Typeset',MathJax.Hub]);</script></head>",
#                        contents,fixed=TRUE)
     
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

