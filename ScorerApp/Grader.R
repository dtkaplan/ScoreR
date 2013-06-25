# Grading Code

# Create a table to display the grades of selected students and assignments
graderScores <- reactive({
  query <- paste("select assignment,user,probID,itemName,answer,",
                 "score,autoScore,possible,lasttime,firsttime ",
                 "from submit",sep="")
  tab <- dbGetQuery(db, query)
  return(tab)
})
                        