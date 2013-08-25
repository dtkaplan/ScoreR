# Grading Code


## MANUAL GRADING
assignmentItems <- reactive({
  query <- paste("select id,assignment,user,probID,itemName,answer,",
                 "freetext,score,autoScore,possible,lasttime,firsttime ",
                 "from submit where assignment=='",
                  input$thisAssignment,"'",
                  sep="")
  tab <- dbGetQuery(db, query)
  return(tab)
})
# Get a table showing which items have been scored manually and which not
manuallyScoredTable <- reactive({
  tab <- assignmentItems()
  notAuto <- subset(tab,autoScore==0)
  scoreCounts <- aggregate(cbind('Already Scored'=score>0,'Not Scored'=score==0)~probID+itemName,
                           data=notAuto,FUN=sum)
  return(scoreCounts)
})
# Get a table giving the contents of the answers to the selected item in the selected problem
itemContentsTable <- reactive({
  tab <- subset(assignmentItems(),
         probID==input$thisProblem & itemName==input$itemForGrading)
  # Display only the ungraded submissions.
  if (input$ungradedOnly) tab <- subset(tab, score==0)
  # Display only those students selected.
  studentsChosen <- input$studentsForGrading # which students to list
  if( studentsChosen[1] != "Everyone in class") {
      tab <- subset(tab, user %in% unlist(studentsChosen))
  }
  updateNumericInput(session,"whichSubmission",value=1,max=nrow(tab))
  return(tab)
})

# update the score for a submission
updateScoreSubmission <- observe({
  # cat("In updateScoreSubmission\n",file=stderr())
  if (input$scoreAssigned!="NA") {
    query = paste("update submit set score=",input$scoreAssigned," ",
                  " where id='",scoredItemID,"'",sep="")
    # cat(paste(query,"\n"),file=stderr())
    dbGetQuery(db,query)
  }
})

# SCORE REPORTS

# Create a table to display the grades of selected students and assignments
graderScores <- reactive({
  assignmentsChosen <- input$levelForGrading # which assignments to list
  query <- paste("select assignment,user,probID,itemName,answer,",
                 "score,autoScore,possible,lasttime,firsttime ",
                 "from submit",sep="")
  if(assignmentsChosen[1] == "Selected Assignment"){
    # take all problems in the assignment 
    query <- paste( query, " where assignment=='",
                    input$thisAssignment,"'",
                    sep="")
    tab <- dbGetQuery(db, query)
  }
  else {
    if (assignmentsChosen[1] == "Selected Problem"){
      # take only the problem
      query <- paste( query, " where probID=='",
                      input$thisProblem,"'",sep="") 
      tab <- dbGetQuery(db, query)
    }
    else { # a set of assignments
      tab <- dbGetQuery(db, query)
      if( assignmentsChosen[1] != "All Assignments") {
        tab <- subset(tab, assignment %in% unlist(assignmentsChosen)  )
      }
    }
  }
  studentsChosen <- input$studentsForGrading # which students to list
  if( studentsChosen[1] == "Everyone in class")
    studentsChosen <- subset(passwords,role=="reader")$name

  tab$notYetScored <- tab$possible
  tab$notYetScored[tab$autoScore>0] <- 0 # leave only those that are not scored automatically
  tab <- aggregate(cbind(score,notYetScored,possible) ~ user, data=tab, FUN=sum)
  # Make sure there's a column for every student listed.
  thoseChosen <- data.frame(user=unlist(studentsChosen))
  tab <- merge(tab,thoseChosen,all.y=TRUE)
  tab <- tab[order(tab$user),]
  
  return(tab)
})
                        