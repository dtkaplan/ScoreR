##
##' Utilities
`%||%` <- function(x, default)
  if(is.null(x) || is.na(x) || (is.character(x) && x== "") || (is.numeric(x) && is.nan(x))) default else x

to_JSON <- function(x, hide=getOption("hidden_json") %||% FALSE) {
  if(hide) {
    saveRDS(x, con <- textConnection("io", open="w"), ascii=TRUE)
    close(con)
    out <- list(hidden=hide, val=io)
  }  else {
    out <- list(hidden=hide, val=x)
  }
  toJSON(out)
}

from_JSON <- function(x) {
  x <- fromJSON(x)
  val <- x$val
  if(x$hidden) {
    con <- textConnection(val)
    val <- readRDS(con)
    close(con)
  }
  val
}


#' @title Constructs IDs for questions for the ScoreR system
#' 
#' @description Create the IDs for a new ScoreR set and generate the numerical count 
#' for successive items within the set.
#' 
#' @rdname newScorerSet
#' @aliases newScorerSet,nextScorerItem,textItem,choiceItem
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
  assign("fixedChoiceCount", 0, envir=.scorerEnv) # count of select/dropdown
  assign("textCount", 0, envir=.scorerEnv) # count of text entry
  assign("numberCount", 0, envir=.scorerEnv) # count of text entry
  assign("multChoiceCount", 0, envir=.scorerEnv) # Count of multiple choice
  assign("uniqueID", ID, envir=.scorerEnv) # which item set is being displayed
  assign("timeStamp", date(),envir=.scorerEnv)
  # Put boilerplate, e.g. CSS definitions, here.
}
#' @rdname newScorerSet
#' @export
nextScorerItem <- function(counter="fixedChoiceCount",increment=TRUE,name="default"){
  nextScorerCount <- function(increment=TRUE) {
    .nextScorerCount <- function() {
      n <- get(counter, envir=.scorerEnv)
      assign(counter,n+1, envir=.scorerEnv)
      return(n+1)
    }
    if (increment) n <- .nextScorerCount()
    else n <- get(counter, envir=.scorerEnv)
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

##' MC is a radio button type for multiple choice
#' @export
## symbols are spanid=MCout, ids=thisIDs, MCname, MCid
newMC <- function(name=NULL,pts=1,hint="",reward="",markers=LETTERS){
  itemInfo <- nextScorerItem(counter="multiChoiceCount",name=name)
  N <- itemInfo$itemN
  
  thisCount <- 0
  # These must in be in the same order in all the versions, e.g., choiceItem(),textItem()
  vals <- list(pts=pts,hint=hint,itemInfo=itemInfo)
  vals$type="MC"
  vals$reward=reward
  
  # a function to create
  res <- function(correct=TRUE,credit=as.numeric(correct),finish=FALSE,hint="",reward=""){
      if(thisCount < 0) stop("You've already closed this multiple choice with 'finish=TRUE'")
      if(finish) {
        thisCount <<- -1 # Signal that it's finished to trigger error if re-used.
        N <- itemInfo$itemN
        
        tpl <- '
<span id="MCout{{N}}" class="shiny-html-output"></span>
'
        return(whisker::whisker.render(tpl))
        
        ## return(paste("<span id='MCout",itemInfo$itemN,
        ##              "' class='shiny-html-output'> </span>",
        ##              sep=""))
      } else {
        thisCount <<- thisCount + 1
        thisName <- paste("MC",itemInfo$itemN,sep="")            # name=MC1
        thisID <- paste("MCitem", N, ".", thisCount,sep="") # MCitem1.4
        identifier <- markers[thisCount]
                                        # update the information for this item
        valsForThisChoice <- vals
                                        # scale points depending on whether answer is correct
        valsForThisChoice$pts <- valsForThisChoice$pts*credit 
        valsForThisChoice$hint <- hint
        valsForThisChoice$reward <- reward
        valsForThisChoice$content=identifier # just a placeholder

        valsForThisChoice <- to_JSON(valsForThisChoice)
        tpl <- '

<label for="{{thisID}}">
  <input type="radio" name="{{thisName}}" id="{{thisID}}" value=\'{{{valsForThisChoice}}}\'>
  <b>{{{identifier}}}</b>
</label>
'
        return(whisker::whisker.render(tpl))
        ## itemStr = paste("<label for='",thisID,
        ##   "'><input type='radio' name='",thisName,
        ##           "' id='",thisID,"' value='",to_JSON(valsForThisChoice),
        ##   "'> <b>",identifier,"</b></label>", sep="")
        ## return(itemStr)
        }
  }
  return(res) # the function to create items   
}

#' @export
textItem <- function(name=NULL,pts=1,hint="",rows=2,cols=30){
  itemInfo <- nextScorerItem(counter="textCount",name=name)
  N <- itemInfo$itemN
  
  # These must in be in the same order in all the versions, e.g., choiceItem(),textItem()
  vals <- list(pts=pts, hint=hint, itemInfo=itemInfo)
  vals$type="Free text"
  vals$reward=""
  vals$content="Free text" # just a placeholder
  vals <- to_JSON(vals)
  
  placeholder <- "Your answer here ..."
  
  tpl <- '

<textarea cols="{{cols}}" rows="{{rows}}" id="text{{N}}" placeholder="{{placeholder}}"></textarea>
<input type="radio" name="trigger{{N}}" id="g2" value="F">
<input type="radio" name="trigger{{N}}" id="g1" value="M">
<select style="visibility: hidden" width=5 name="info{{N}}">
  <option value=\'{{{vals}}}\'>Ignore me</option>
</select>
<span id="tout{{N}}" value="spanvalue" class="shiny-html-output"></span>
'

  return(whisker::whisker.render(tpl))
  ####
  ## res <- paste("<textarea cols='",cols,"' rows='",rows,"' id='text",
  ##              itemInfo$itemN,
  ##              "' placeholder='Your answer here ...'></textarea>",
  ##              sep="")
  ## # Trigger for the text so that the input isn't updated every few character strokes
  ## trigger = paste("<input type='radio' name='trigger",
  ##                 itemInfo$itemN,"' id='g2' value='F'>",
  ##                 "<input type='radio' name='trigger",
  ##                 itemInfo$itemN,"' id='g1' value='M'>",sep="")
  ## res <- paste(res,trigger)
  ## # Something to hold the values, e.g. the name of the item, etc.
  ## holdvals <- paste("<select style='visibility: hidden' width='5' name='info",itemInfo$itemN,
  ##                   "'><option value='",to_JSON(vals),
  ##                   "'>Ignore me</option></select>",sep="")
  ## res <- paste(res, holdvals,sep="")
  ## res <- paste(res,
  ##              "<span id='tout", itemInfo$itemN,
  ##              "' value='spanvalue' class='shiny-html-output'></span>",sep="")
}




#' @export
#' @param from optional from to use spinbox
#' @param to optional to to use spinbox, likely should have both from and to.
#' @param optional step to use spinbox
#' comment: wrapped in DIV tag to get bootstrap validation states.
#' @param label optional label for form entry
#' @param help optional help text
#' @TODO This should -- and the free text one -- should have a custom widget to override the subscribe methods
#' they currently subscribe to keyup and change (line 1100 in shiny.js), but should likely just be "focusout".
#' The following blocks keyup
numberItem <- function(name=NULL, pts=1, hint="", ans, tolerance=0, from=NULL, to=NULL, by=NULL, help=NULL, label=NULL){
  itemInfo <- nextScorerItem(counter="numberCount",name=name)
  N  <- itemInfo$itemN
  
  ## These must in be in the same order in all the versions, e.g., choiceItem(),textItem()
  vals <- list(pts=pts,
               hint=hint,
               itemInfo=itemInfo,
               type="NumericInput",
               reward="",
               content="Numeric input", # just a placeholder
               ans=ans,
               tolerance=tolerance
               )
  vals <- to_JSON(vals)


  ## could put this javascript to add/drop warning at *end* of HTML page, but here we add to each item
  tpl <- '

<div class="control-group">
  {{#label}}<label class="control-label" for="nin{{N}}">{{{label}}}</label>{{/label}}
  <div class="controls">
    <input id="nin{{N}}" type="number" {{#from}}min="{{from}}"{{/from}} {{#to}}max="{{to}}"{{/to}} {{#by}}step="{{by}}"{{/by}}>
    {{#help}}<span class="help-inline">{{{help}}}</span>{{/help}}
  </div>
</div>
<script>
$("#nin{{N}}").keyup(function(event) {
  event.preventDefault();
  $(this).val().length == 0 ? $(this).parent().parent().addClass("warning") : $(this).parent().parent().removeClass("warning")
})
</script>
<select style="visibility: hidden" width="5" name="ninfo{{N}}">
  <option value=\'{{{vals}}}\'>ignore the man behind this curtain</option>
</select>
<span id="nout{{N}}" value="spanValue" class="shiny-html-output"></span>
'

  res <- whisker::whisker.render(tpl)
  res
}


## Choice item uses combo box to select one of many
#' @export
choiceItem <- function(name=NULL,style="dropdown",pts=0,hint="",reward="",...) {
  dots <- list(...)
  nms <- names(dots)
  
  itemInfo <- nextScorerItem(name=name)
  N <- itemInfo$itemN
  
  v <- list()
  v[[1]] <- list(label="Unanswered",
                 value="NA")

  for (k in 1:length(dots)) {
    ## make sure that dots is a list.
    vals <- if( is.list(dots[[k]])) dots[[k]] else dots[k]
    vals$label=nms[k]
    
    vv <- list()
    vv$itemInfo <- itemInfo
    vv$type <- "Fixed Choice"
    vv$pts <- ifelse( "pts" %in% names(vals), vals$pts, pts) 
    vv$hint <- ifelse( "hint" %in% names(vals), vals$hint, hint)
    vv$reward <- ifelse( "reward" %in% names(vals), vals$reward, reward)
    vv$content <- nms[k]
    vals$value = to_JSON(vv)
    v[[k + 1]] <- vals
  }

  tpl <- '
<select id="in{{N}}">
{{#v}}
<option value=\'{{{value}}}\'>{{{label}}}</option>
{{/v}}
</select>
<span id="out{{N}}" class="shiny-html-output"></span>
'

  return(whisker::whisker.render(tpl))
                 

  
  ## res <- paste("<select name='in",itemInfo$itemN,
  ##              "'><option value='NA' selected>UNANSWERED</option>",
  ##              sep="")
  ## for (k in 1:length(dots)) {
  ##   # make sure that it's a list.
  ##   if( is.list(dots[[k]]))
  ##     vals <- dots[[k]]
  ##   else
  ##     vals <- dots[k]
  ##   vv = list()
  ##   vv$itemInfo <- itemInfo
  ##   vv$type <- "Fixed Choice"
  ##   vv$pts <- ifelse( "pts" %in% names(vals), vals$pts, pts) 
  ##   vv$hint <- ifelse( "hint" %in% names(vals), vals$hint, hint)
  ##   vv$reward <- ifelse( "reward" %in% names(vals), vals$reward, reward)
  ##   vv$content <- nms[k]
  ##   opt <- paste("<option value='",
  ##                to_JSON(vv),
  ##                "'>",nms[k],"</option>",sep="")
  ##   res <- paste(res, opt)
  ## }
  ## res <- paste(res, 
  ##              paste("</select><span id='out",itemInfo$itemN,
  ##                    "' class='shiny-html-output'> </span>",sep=""))
  ## return(res)
}


# Storage for internal data

.scorerEnv <- new.env()
assign("fixedChoiceCount",0,envir=.scorerEnv)
assign("textCount",0,envir=.scorerEnv)
assign("numberCount",0,envir=.scorerEnv)
assign("multiChoiceCount",0,envir=.scorerEnv)
assign("uniqueID",NULL,envir=.scorerEnv)
assign("timeStamp",NULL,envir=.scorerEnv)

