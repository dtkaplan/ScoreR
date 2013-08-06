output$assignmentSelector <- renderUI({
  selectInput("thisAssignment","Select Assignment:",assignmentList)
  #    output$mainStatus <- renderPrint({cat("\n")})
})
# Choose which students to grade
output$studentSelector <- renderUI({
  if( userInfo()$grader) {
    readerNames <- c(list("Everyone in class"),subset(passwords,role=="reader")$name)
    selectInput("studentsForGrading","Students to Grade:",
                readerNames,
                selected="Everyone in class",
                multiple=TRUE)
  } else p("You are not an instructor.")
})
# Which assignments to score
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
