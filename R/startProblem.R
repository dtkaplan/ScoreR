#' Construct a new ScoreR problem 
#' 
#' A ScoreR problem file contains one or more "questions".  Each problem must be assigned 
#' a unique ID.  This is assigned with the \code{startProblem} function.  Problems must be 
#' terminated with a call to the \code{endProblem} function.
#' 
#' @param ID a character string specifying the unique ID of the problem
#' @export
#' @aliases startProblem, endProblem
#' @examples
#' startProblem("Calc1-Ch3-P4-2013-07-30")
#' endProblem()
#' 
#' @details 
#' The ID needs to be unique among the set of problems used together on the server.
#' There are several ways to construct problems that are effectively unique, for instance 
#' including the composition date in the problem ID or the author's initials.
#' On the server, the problem will be referred to in another way; the unique ID is just for 
#' storing problem submissions in the server database in a way that's linked to the problem itself.
startProblem <-function(ID) {
  assign("questionCount",0,envir=.scorerEnv) # number of questions
  assign("fixedChoiceCount", 0, envir=.scorerEnv) # count of select/dropdown
  assign("textCount", 0, envir=.scorerEnv) # count of text entry
  assign("multiChoiceCount", 0, envir=.scorerEnv) # Count of multiple choice
  assign("uniqueID", ID, envir=.scorerEnv) # which item set is being displayed
  assign("timeStamp", date(),envir=.scorerEnv)
  assign("roster", c(), envir=.scorerEnv)
  # Put boilerplate, e.g. CSS definitions, here.
}

# Generates a roster
#' @rdname startProblem
#' @export
endProblem <- function() {
  nQ <- get("questionCount", envir=.scorerEnv)
  roster <- get('roster', envir=.scorerEnv)
  holdvals <- paste("<select style='visibility: hidden' width='5' name='roster'>",
                    "<option value='",toJSON(I(roster)),
                    "'>problem roster</option></select>",sep="")
  return(holdvals)
}



