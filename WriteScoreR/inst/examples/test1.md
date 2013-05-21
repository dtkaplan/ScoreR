Testing the WriteScoreR Package for Rmd files
========================================================

### Danny Kaplan
### May 21, 2013

This problem tests the `WriteScoreR` package.

First, you must load the package:

```r
require(WriteScoreR)
```

Generally you will want to do this with an `include=FALSE` directive.

Next, give a unique ID to the set of items.

```r
newScorerSet("test1-v0.1")
```


Now you can ask your questions:

You may find it easier to define the items in one place and use them in another:

```r
spainrain = choiceItem(Algeria = list(pts = 0, hint = "Wrong continent"), Spain = list(pts = 2, 
    reward = "My Fair Lady!"))
hurricanes = choiceItem(often = list(pts = 0), usually = "no", `hardly ever` = "yes")
```


* The rain in BLANK falls mainly on the plain. <select name='indefault'><option value='NA' selected>UNANSWERED</option> <option value='{
 "pts":      0,
"hint": "Wrong continent",
"itemInfo": {
 "setID": "test1-v0.1",
"itemN":      1,
"name": "default" 
} 
}'>Algeria</option> <option value='{
 "pts":      2,
"reward": "My Fair Lady!",
"itemInfo": {
 "setID": "test1-v0.1",
"itemN":      1,
"name": "default" 
} 
}'>Spain</option> </select><span id='outdefault' class='shiny-html-output'> </span>
* In Hartford, Hereford and Hampshire, hurricanes BLANK happen: <select name='indefault'><option value='NA' selected>UNANSWERED</option> <option value='{
 "pts":      0,
"itemInfo": {
 "setID": "test1-v0.1",
"itemN":      2,
"name": "default" 
} 
}'>often</option> <option value='{
 "usually": "no",
"itemInfo": {
 "setID": "test1-v0.1",
"itemN":      2,
"name": "default" 
} 
}'>usually</option> <option value='{
 "hardly ever": "yes",
"itemInfo": {
 "setID": "test1-v0.1",
"itemN":      2,
"name": "default" 
} 
}'>hardly ever</option> </select><span id='outdefault' class='shiny-html-output'> </span>
