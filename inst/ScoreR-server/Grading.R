output$assignmentSelector <- renderUI({
  selectInput("thisAssignment","Select Assignment:",assignmentList)
  #    output$mainStatus <- renderPrint({cat("\n")})
})
# =============================
# Choose which students to grade
# For the grade report
output$studentSelector <- renderUI({
  if( userInfo()$grader) {
    readerNames <- c(list("Everyone in class"),as.character(subset(passwords,role=="reader")$name))
    selectInput("studentsForGrading","Students to Grade:",
                readerNames,
                selected="Everyone in class",
                multiple=TRUE)
  } else p("You are not an instructor.")
})
# a copy of the above for manual grading tab.  
output$studentSelectorMan <- renderUI({
  if( userInfo()$grader) {
    readerNames <- c(list("Everyone in class"),as.character(subset(passwords,role=="reader")$name))
    selectInput("studentsForGrading","Students to Grade:",
                readerNames,
                selected="Everyone in class",
                multiple=TRUE)
  } else p("You are not an instructor.")
})
# ==============================
# Which assignments to score
# For the grade report
output$gradeLevelSelector <- renderUI({
  if( userInfo()$grader) {
    # See corresponding levels in "Grader.R"
    assignmentNames <- c(list("Selected Problem","Selected Assignment","All Assignments"),
                         assignmentList)
    selectInput("levelForGrading","Assignment(s) to Grade:",
                assignmentNames,
                selected="Selected Assignment",
                multiple=TRUE)
  } else p("")
})
# Which item from the present problem
output$itemSelectorMan <- renderUI({
  if( userInfo()$grader) {
    tab <- manuallyScoredTable()
    thisProblem <- subset(tab,probID==input$thisProblem)
    itemNames <- c(list(), thisProblem$itemName)
    if( length(itemNames) > 0 ) 
      selectInput("itemForGrading","Item to Grade Manually:",
                itemNames)
    else h3("No manually scored items in this problem.")
  } else p("")
})

# How many have been graded.
output$gradedTable <- renderTable({ manuallyScoredTable() })
# The contents of the selected item.
output$itemTable <- renderTable({itemContentsTable()})
# The currently selected text for manual grading
output$currentItemText <- renderText({
  tab <- itemContentsTable()
  if( nrow(tab) > 0 ) {
    #currentScore <- tab$score[input$whichSubmission]
    query <- paste("select score from submit where id=='",
                   tab$id[input$whichSubmission],"'",
                   sep="")
    response <- dbGetQuery(db, query)
    currentScore <- response$score[1] # get it from the database 
    # in case it was changed in the present session.
    browser()
    # Make the default NA unless a non-zero score has been assigned.
    updateSelectInput(session,"scoreAssigned",
                      choices=c("NA",as.list(0:(tab$possible[1]))),
                      selected=ifelse(currentScore>0,currentScore,"NA"))
    scoredItemID <<- tab$id[input$whichSubmission]
    # PUT THE USER, Date, and Score information here.
    # is there an update text?
    # Now enter the submission text in the box, so it can be seen.
    fromJSON(I(tab$freetext)[input$whichSubmission])
  }
  else
    "--- No submission ----"
  })
# ==============================
# The download button
output$gradeFileDownload <- renderUI({
  if( userInfo()$grader)
    downloadButton('downloadGrades', 'Download Grade Table')
  else p("")
})
output$downloadGrades <- downloadHandler(
  filename = function() { paste('grade-table', '.csv', sep='') },
  content = function(file) {
    write.csv(graderScores(), file)
  }
)
output$classScores <- renderTable({graderScores()})
source("Grader.R",local=TRUE)
#   graderScores <- reactive({
#     if( userInfo()$grader ) {
#       data.frame(hello=1:5,bye=11:15)
#     }
#     else data.frame(hello=1,bye=1)
#   })
