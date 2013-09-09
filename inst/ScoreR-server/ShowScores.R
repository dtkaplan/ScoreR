# For the Scores Tab ==================
output$submissions <- 
  renderTable(
{ 
  input$save # for the dependency
  user <- userInfo()$name
  query <- paste("select assignment,probID,itemName,answer,score,autoScore,possible,lasttime,firsttime ",
                 "from submit where user=='",
                 user,"'",sep="")
  # Which scoring mode        
  if (input$scoreChoice=="Current problem") {
    if( input$thisProblem == "Select Problem") 
      tab=data.frame("No problem has been selected."=0)
    else {
      prob <- probData() # Get the data on the selected problem, File, Answers, etc.
      query <- paste( query, " and probID=='",
                      input$thisProblem,"'",sep="") 
      tab <- dbGetQuery(db, query)
      if (nrow(tab)==0) 
        return(data.frame("Nothing submitted for this problem."="Remember to press the submit button. You can do so right now!"))
      tab$score <- as.character(tab$score)
      if (!prob$Answers) tab$score <- "not released"
      else tab$score[tab$autoScore==0 & tab$score==0] <- "not yet scored"
      tab$autoScore <- NULL
      tab$assignment <- NULL
      tab$probID <- NULL
      
    }
  }
  else { # Current assignment or all assignments
    if (input$scoreChoice=="Current assignment") {
      query <- paste( query, " and assignment=='",
                      input$thisAssignment,"'",
                      sep="")
    }
    tab <- dbGetQuery(db, query)
    if (nrow(tab)==0) 
      return(data.frame("No answers yet submitted"=0))
    checkit(names(tab), "tab names")
    checkit(names(problemList), "assignmentList names")
    tab <- merge(tab,problemList, # See if the answers are released
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
