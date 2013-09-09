#' Add a free-text question to a ScoreR problem
#' 
#' Insert a text-entry box into a ScoreR problem.  
#' 
#' 
#' @param name text string giving name of the question.  This should be 
#' unique within the problem.
#' @param totalPts integer saying how many points the question is worth
#' @param hint text string giving a hint
#' @param rows (default 2) the number of rows in the text box.
#' @param cols (default 30) the number of columns in the text box.
#' 
#' @details
#' In many browsers, the text box can be resized by the user, so specifying the 
#' number of rows and columns is not needed.
#' @export
textItem <- function(name=NULL,totalPts=1,hint="",rows=2,cols=30){
  itemInfo <- nextScorerItem(name=name,possiblePoints=totalPts)
  itemInfo$type="Free text"
  itemInfo$hint=hint
  itemInfo$reward=""
  itemInfo$uniqueID <- get("uniqueID", envir=.scorerEnv)
  # Name of the user interactive part
  questionID <- paste("ScoreR",itemInfo$itemN,sep="")
  # Name of the static data holding part
  infoID <- paste(questionID,"info",sep="")
  # Name of a place to put output information (Is this needed?)
  outputID <- paste(questionID,"out",sep="")
  
  # Markup for the interactive part
  res <- paste("<textarea cols='",cols,"' rows='",rows,"' id='",questionID,
               "' placeholder='Your answer here ...'></textarea>",
               sep="")
  # Something to hold the values, e.g. the name of the item, etc.
  holdvals <- paste("<select style='visibility: hidden' ",
                    "width='5' name='",infoID,
                    "'><option value='",toJSON(itemInfo),
                    "'>Question info for free text</option></select>",sep="")
  outputvals <- paste("<input type='text' readonly='readonly' style='color:green' size='3' id='",outputID,"'></input>",
                      sep="")

  res <- paste(res,holdvals,sep="")
  # Free text doesn't need an output box.
  # res <- paste(res,outputvals, holdvals,sep="")
}
