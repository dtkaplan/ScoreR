#' Add a multiple-choice question to a ScoreR document
#' 
#' These functions can be used in series to generate multiple-choice questions.  
#' Multiple-choice, as opposed to select-from-a-set (see \code{\link{selectSet}}), 
#' is an appropriate format when the items from which the selection is to be made
#' are lengthy or themselves require non-text markup as with equations, images, etc.
#' 
#' @details
#' The first step in generating a multiple-choice question is to call \code{newMC}
#' which returns a generating function that is used to generate the markup for the multiple-choice 
#' selectors.  (The content which the selector refers to is constructed with ordinary
#' markup, as in a series of paragraphs.) Calling the generating function with the 
#' argument \code{finish=TRUE}
#' terminates the question.
#' 
#' @return a function with argument \code{correct} (TRUE or FALSE) and 
#' optional arguments \code{credit} (a numerical fraction between zero and one for partial credit)
#' \code{hint} (a character string), \code{reward} (a character string).  The argument 
#' \code{finish=TRUE} signals the end of the multiple-choice set.
#' 
#' @param name Character string naming the problem
#' @param totalPts The point value (for complete credit) of the problem
#' @param hint A character string
#' @param reward A character string
#' @param markers A vector of character strings giving the sequential identifiers for the multiple
#' choice items.  Default: \code{LETTERS}, that is, \code{c("A", "B", "C", ...)}
#'  
#'
#' @examples
#' # Markup, e.g., What day is bad news for Caesar?
#' Q <- newMC(name="example", totalPts=4)
#' Q(FALSE) # Markup for the item, e.g., "The Fourth of July"
#' Q(TRUE) # Markup for the item, e.g., "The Ides of March"
#' Q(credit=.5) # Markup for the item, e.g., "The days around the Ides of March"
#' Q(finish=TRUE)
#'
#' @export
newMC <- function(name=NULL,totalPts=1,hint="",reward="",markers=LETTERS){
  itemInfo <- nextScorerItem(name=name,possiblePoints=totalPts)
  itemInfo$type="MC"
  itemInfo$reward=reward
  
  # Name of the user interactive part
  questionID <- paste("ScoreR",itemInfo$itemN,sep="")
  # Name of the static data holding part
  infoID <- paste(questionID,"info",sep="")
  # Name of a place to put output information (Is this needed?)
  outputID <- paste(questionID,"out",sep="")
  
  thisCount <- 0

  # The return value is   
  # a function to create multiple-choice items.
  # Invoke that function to create the individual MC items, then "finish".
  res <- function(correct=TRUE,credit=as.numeric(correct),finish=FALSE,hint="",reward="Right!"){
    if(thisCount < 0) stop("You've already closed this multiple choice with 'finish=TRUE'")
    if(finish) {
      thisCount <<- -1 # Signal that it's finished to trigger error if re-used.
      # place the markup 
      #outputvals <- paste("<span id='",outputID,
      #             "' class='shiny-html-output'> </span>",
      #             sep="")
      outputvals <- paste("<input type='text' readonly='readonly' style='color:green' size='3' id='",outputID,"'></input>",
                          sep="")
      holdvals <- paste("<select style='visibility: hidden' width='5' name='",
                        infoID,
                        "'><option value='",toJSON(itemInfo),
                        "'>Question info</option></select>",sep="")
      return(paste(outputvals,holdvals,sep=""))
    }
    thisCount <<- thisCount + 1
  
    thisID <- paste("MCitem",thisCount,sep="")
    identifier <- markers[thisCount] # marker to use for the multiple-choice item
    # update the information for this item
    valsForThisChoice <- list()
    # scale points depending on whether answer is correct
    valsForThisChoice$pts <- round(totalPts*credit) # scale total points according to credit.
    valsForThisChoice$hint <- hint
    valsForThisChoice$reward <- reward
    valsForThisChoice$content=identifier # just a placeholder
    itemStr = paste("<label for='",thisID,
                    "'><input type='radio' name='",questionID,
                    "' id='",thisID,"' value='",toJSON(valsForThisChoice),
                    "'> <b>",identifier,"</b></label>", sep="")
    return(itemStr)
  }
  return(res) # the function to create items   
}