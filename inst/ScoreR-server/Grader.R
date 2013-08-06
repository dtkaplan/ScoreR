# Grading Code

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
                        