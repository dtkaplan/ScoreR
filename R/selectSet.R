#' Add a select-from a set question into a ScoreR problem
#'
#' Inserts a dropdown box style selector for short answers.
#' 
#' @details
#' \code{selectNumber} and \code{selectSet} create a question item
#' in the form of choose-from-a-set.  Each of the items available for choice should be
#' short, a couple dozen characters or so.
#' 
#' \code{selectSet} takes a ... list of items for selection. These are
#' assigned values \code{TRUE} or \code{FALSE} depending on whether 
#' the selected item is deemed a correct answer.
#' \code{selectNumber} is a convenience wrapper function that 
#' takes a numerical vector and a correct set of values and then 
#' invokes the call to \code{selectSet}
#' 
#' @aliases selectNumber, selectSet
#' @param name Character string naming the question
#' @param style Character string for type of format.  Currently only \code{'dropdown'} (the default)
#' is supported.
#' @param totalPts integer giving the number of points the problem is worth.
#' @param hint character string giving a hint for the question overall
#' @param reward character string to be printed out when a correct choice
#' is made in interactive mode.
#' @param ... the list of choices ("Not yet selected" is put in by default.)  See the examples.
#' @param choices a numerical vector for \code{selectNumber}
#' @param correct a numerical vector of correct values from \code{choices} for
#' \code{selectNumber}
#' 
#' @examples
#' startProblem(ID="Prob1-2013-07-30")
#' selectSet(name="Weekend",Sun=TRUE,Mon=FALSE,Tues=FALSE,Sat=FALSE,"None of these"=FALSE)
#' selectSet(name="favorite",
#'           Monday=list(hint="Really?",credit=0),Wed=list(hint="Hump day.",credit=.5),Friday=TRUE)
#' selectNumber(name="lucky",choices=1:7, correct=7)
#' endProblem()
#' @export
selectSet <- function(name=NULL,style=c("dropdown"),totalPts=1,hint="",reward="Right!",...) {
  dots <- list(...)
  nms <- names(dots)
  itemInfo <- nextScorerItem(name=name,possiblePoints=totalPts)
  itemInfo$type <- "Fixed Choice"
  
  # Name of the user interactive part
  questionID <- paste("ScoreR",itemInfo$itemN,sep="")
  # Name of the static data holding part
  infoID <- paste(questionID,"info",sep="")
  # Name of a place to put output information (Is this needed?)
  outputID <- paste(questionID,"out",sep="")
  
  # Start the selection markup
  res <- paste("<select name='",questionID,
               "'><option value='NA' selected>UNANSWERED</option>",
               sep="")
  vv <- list()

  for (k in 1:length(dots)) {
    # See if the item itself is a list giving credit, hint, reward
    if( is.list(dots[[k]])) {
      vals <- dots[[k]]
      vv$pts <- round(as.numeric(totalPts*vals$credit))
      vv$hint <- vals$hint
      vv$reward <- vals$reward
    }
    else {
      # handle TRUE or FALSE values for list items
      vv$pts <- ifelse(dots[[k]],as.numeric(totalPts),0) # Accept TRUE and FALSE to indicate credit
      # Use the overall questions hint and reward
      vv$hint <- hint
      vv$reward <- reward
    }
    vv$content <- nms[k]
    
    opt <- paste("<option value='",
                 toJSON(vv),
                 "'>",nms[k],"</option>",sep="")
    res <- paste(res, opt)
  }
  res <- paste(res, "</select>",sep="") # finished with selector markup
  # markup for questions specific output.
  # outputvals <- paste("<span id='",outputID,
  #                   "' class='shiny-html-output'> </span>",sep="")
  outputvals <- paste("<small><input type='text' readonly='readonly' style='color:green' size='3' id='",outputID,"'></input></small>",
                      sep="")
  # Add the information-holding field
  holdvals <- paste("<select style='visibility: hidden' width='5' name='",infoID,
                    "'><option value='",toJSON(itemInfo),
                    "'>Info for Select Set</option></select>",sep="")
  res <- paste(res,outputvals, holdvals,sep="")
  
  return(res)
}
# Numerical version of selectSet()
#' @rdname selectSet
#' @export
selectNumber <- function(choices,correct, name=NULL,totalPts=1) {
  vals <- choices %in% correct
  names(vals) <- paste(choices)
  # put things in the format for selectSet()
  do.call(selectSet,c(list(name=name,totalPts=1),vals))
}
