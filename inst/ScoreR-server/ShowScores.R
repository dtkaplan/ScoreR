# For the Scores Tab ==================
# Need to figure out how to trigger this when "Scores" tab is revealed.

## TO DO:
# In the relevant display, list all of the problems and assignments, even those that the person hasn't answered.
output$submissions <- 
  renderTable(
{ 
  input$save # for the dependency
  input$save2 # for the dependency
  #user <- loggedIn()
  user <- userInfo()$name
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
