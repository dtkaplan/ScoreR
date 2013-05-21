#' @title Constructs IDs for questions for the ScoreR system
#' 
#' @description Create the IDs for a new ScoreR set and generate the numerical count 
#' for successive items within the set.
#' 
#' @rdname newScorerSet
#' @aliases newScorerSet,nextScorerItem
#' @param ID a character string giving a (hopefully) unique ID for the set of questions
#' @param increment (default TRUE) for each item, add to the count of items within
#' the set.
#' 
#' @details
#' \code{newScorerSet} is for use in an Rmd or similar environment.  It should be called
#' once at the start of the file, so that later item-creation commands can generate
#' appropriate IDs for themselves.
#' 
#' 
#' @examples
#' newScorerSet("Math155-Sept-3-2014") 
#' nextScorerItem(name="algebra")
#' @export
newScorerSet <-function(ID) {
  assign("itemCount", 0, envir=.scorerEnv)
  assign("uniqueID", ID, envir=.scorerEnv)
}
#' @rdname newScorerSet
#' @export
nextScorerItem <- function(increment=TRUE,name="default"){
  nextScorerCount <- function(increment=TRUE) {
    .nextScorerCount <- function() {
      n <- get("itemCount", envir=.scorerEnv)
      assign("itemCount",n+1, envir=.scorerEnv)
      return(n+1)
    }
    if (increment) n <- .nextScorerCount()
    else n <- get("itemCount", envir=.scorerEnv)
    return(n)
  }
  # return a list with pertinent information
  setID <- get("uniqueID", envir=.scorerEnv)
  if( is.null(setID)) stop("Must set an ID with newScorerSet() at top of file.")
  itemN=nextScorerCount(increment)
  return(list(setID=get("uniqueID",envir=.scorerEnv),
              itemN=itemN,
              name=ifelse(is.null(name),as.character(itemN),name)
              )
         )
}

#' @export
choiceItem <- function(name=NULL,style="dropdown",pts=0,hint="",reward="",...) {
  dots <- list(...)
  nms <- names(dots)
  itemInfo <- nextScorerItem(name=name)
  res <- paste("<select name='in",itemInfo$itemN,
               "'><option value='NA' selected>UNANSWERED</option>",
               sep="")
  for (k in 1:length(dots)) {
    # make sure that it's a list.
    if( is.list(dots[[k]]))
      vals <- dots[[k]]
    else
      vals <- dots[k]
    vals$itemInfo <- itemInfo
    vals$type <- "Fixed Choice"
    vals$pts <- pts
    vals$hint <- hint
    vals$reward <- reward
    vals$content <- nms[k]
    opt <- paste("<option value='",
                 toJSON(vals),
                 "'>",nms[k],"</option>",sep="")
    res <- paste(res, opt)
  }
  res <- paste(res, 
               paste("</select><span id='out",itemInfo$itemN,
                     "' class='shiny-html-output'> </span>",sep=""))
  return(res)
}


# Storage for internal data

.scorerEnv <- new.env()
assign("itemCount",0,envir=.scorerEnv)
assign("uniqueID",NULL,envir=.scorerEnv)

