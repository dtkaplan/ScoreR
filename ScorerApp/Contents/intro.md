Writing ScoreR Problems
========================================================




<aside>

### With answers displayed!
This example shows the conditional display of answers using the "aside" HTML flag.</aside>

A **ScoreR** problem is a Rmd/HTML file containing zero or more items.  There are three types of items:


### Selection from a set.

This is intended for a set of short answers, e.g. one-word or number.




* The rain in BLANK falls mainly on the plain. 
<select id="in1">
<option value='NA'>Unanswered</option>
<option value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "6", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "1", "16", "1", "262153", "4", "Rain", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "12", "Fixed\\040Choice", "14", "1", "0", "16", "1", "262153", "15", "Wrong\\040continent", "16", "1", "262153", "0", "", "16", "1", "262153", "7", "Algeria", "1026", "511", "16", "6", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "3", "pts", "262153", "4", "hint", "262153", "6", "reward", "262153", "7", "content", "254" ] 
}'>Algeria</option>
<option value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "6", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "1", "16", "1", "262153", "4", "Rain", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "12", "Fixed\\040Choice", "14", "1", "2", "16", "1", "262153", "0", "", "16", "1", "262153", "13", "My\\040Fair\\040Lady!", "16", "1", "262153", "5", "Spain", "1026", "511", "16", "6", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "3", "pts", "262153", "4", "hint", "262153", "6", "reward", "262153", "7", "content", "254" ] 
}'>Spain</option>
</select>
<span id="out1" class="shiny-html-output"></span>

* In Hartford, Hereford and Hampshire, hurricanes BLANK happen: 
<select id="in2">
<option value='NA'>Unanswered</option>
<option value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "6", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "2", "16", "1", "262153", "10", "Hurricanes", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "12", "Fixed\\040Choice", "14", "1", "0", "16", "1", "262153", "0", "", "16", "1", "262153", "0", "", "16", "1", "262153", "5", "often", "1026", "511", "16", "6", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "3", "pts", "262153", "4", "hint", "262153", "6", "reward", "262153", "7", "content", "254" ] 
}'>often</option>
<option value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "6", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "2", "16", "1", "262153", "10", "Hurricanes", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "12", "Fixed\\040Choice", "14", "1", "1", "16", "1", "262153", "0", "", "16", "1", "262153", "0", "", "16", "1", "262153", "7", "usually", "1026", "511", "16", "6", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "3", "pts", "262153", "4", "hint", "262153", "6", "reward", "262153", "7", "content", "254" ] 
}'>usually</option>
<option value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "6", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "2", "16", "1", "262153", "10", "Hurricanes", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "12", "Fixed\\040Choice", "14", "1", "2", "16", "1", "262153", "0", "", "16", "1", "262153", "0", "", "16", "1", "262153", "11", "hardly\\040ever", "1026", "511", "16", "6", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "3", "pts", "262153", "4", "hint", "262153", "6", "reward", "262153", "7", "content", "254" ] 
}'>hardly ever</option>
</select>
<span id="out2" class="shiny-html-output"></span>


<aside>You can also put answers in the markup, which will be displayed only if the Answer field is set to `TRUE`</aside>

### Multiple choice, which can have short or long answers.

Which of these is the start of the "Declaration of Independence"?





<label for="MCitem1.1">
  <input type="radio" name="MC1" id="MCitem1.1" value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "6", "14", "1", "0", "16", "1", "262153", "0", "", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "1", "16", "1", "262153", "11", "Declaration", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "2", "MC", "16", "1", "262153", "0", "", "16", "1", "262153", "1", "A", "1026", "511", "16", "6", "262153", "3", "pts", "262153", "4", "hint", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "6", "reward", "262153", "7", "content", "254" ] 
}'>
  <b>A</b>
</label>
 "We the people of the United States of America, in order to form a more perfect union ..."



<label for="MCitem1.2">
  <input type="radio" name="MC1" id="MCitem1.2" value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "6", "14", "1", "5", "16", "1", "262153", "0", "", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "1", "16", "1", "262153", "11", "Declaration", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "2", "MC", "16", "1", "262153", "0", "", "16", "1", "262153", "1", "B", "1026", "511", "16", "6", "262153", "3", "pts", "262153", "4", "hint", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "6", "reward", "262153", "7", "content", "254" ] 
}'>
  <b>B</b>
</label>
 "When in the course of human events, it becomes necessary ..."



<label for="MCitem1.3">
  <input type="radio" name="MC1" id="MCitem1.3" value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "6", "14", "1", "0", "16", "1", "262153", "0", "", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "1", "16", "1", "262153", "11", "Declaration", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "2", "MC", "16", "1", "262153", "0", "", "16", "1", "262153", "1", "C", "1026", "511", "16", "6", "262153", "3", "pts", "262153", "4", "hint", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "6", "reward", "262153", "7", "content", "254" ] 
}'>
  <b>C</b>
</label>
 "Four score and seven years ago, our forefathers brought forth upon this continent, a new nation, conceived in liberty and dedicated to the proposition that all men are created equal." 

<span id="MCout1" class="shiny-html-output"></span>


## Number choice
* Enter in a value for pi 


<div class="control-group">
  
  <div class="controls">
    <input id="nin1" type="number"   >
    <span class="help-inline">three digits accuracy</span>
  </div>
</div>
<script>
$("#nin1").keyup(function(event) {
  event.preventDefault();
console.log("block keyup");
  $(this).val().length == 0 ? $(this).parent().parent().addClass("warning") : $(this).parent().parent().removeClass("warning")
})
</script>
<select style="visibility: hidden" width="5" name="ninfo1">
  <option value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "8", "14", "1", "1", "16", "1", "262153", "0", "", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "1", "16", "1", "262153", "13", "number-choice", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "12", "NumericInput", "16", "1", "262153", "0", "", "16", "1", "262153", "13", "Numeric\\040input", "14", "1", "3.141592653589793", "14", "1", "0.001", "1026", "511", "16", "8", "262153", "3", "pts", "262153", "4", "hint", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "6", "reward", "262153", "7", "content", "262153", "3", "ans", "262153", "9", "tolerance", "254" ] 
}'>ignore the man behind this curtain</option>
</select>
<span id="nout1" value="spanValue" class="shiny-html-output"></span>


* Enter in an integer between 1 and 10 divisible by 7 

<div class="control-group">
  
  <div class="controls">
    <input id="nin2" type="number" min="0" max="10" step="1">
    
  </div>
</div>
<script>
$("#nin2").keyup(function(event) {
  event.preventDefault();
console.log("block keyup");
  $(this).val().length == 0 ? $(this).parent().parent().addClass("warning") : $(this).parent().parent().removeClass("warning")
})
</script>
<select style="visibility: hidden" width="5" name="ninfo2">
  <option value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "8", "14", "1", "1", "16", "1", "262153", "0", "", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "2", "16", "1", "262153", "13", "number-choice", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "12", "NumericInput", "16", "1", "262153", "0", "", "16", "1", "262153", "13", "Numeric\\040input", "14", "1", "7", "14", "1", "0", "1026", "511", "16", "8", "262153", "3", "pts", "262153", "4", "hint", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "6", "reward", "262153", "7", "content", "262153", "3", "ans", "262153", "9", "tolerance", "254" ] 
}'>ignore the man behind this curtain</option>
</select>
<span id="nout2" value="spanValue" class="shiny-html-output"></span>


### Free text.

What would you like to write a question about? 

<textarea cols="30" rows="1" id="text1" placeholder="Your answer here ..."></textarea>
<input type="radio" name="trigger1" id="g2" value="F">
<input type="radio" name="trigger1" id="g1" value="M">
<select style="visibility: hidden" width=5 name="info1">
  <option value='{
 "hidden": true,
"val": [ "A", "2", "196608", "131840", "531", "6", "14", "1", "1", "16", "1", "262153", "0", "", "531", "3", "16", "1", "262153", "20", "intro-DTK-2013-05-23", "14", "1", "1", "16", "1", "262153", "14", "question-topic", "1026", "1", "262153", "5", "names", "16", "3", "262153", "5", "setID", "262153", "5", "itemN", "262153", "4", "name", "254", "16", "1", "262153", "9", "Free\\040text", "16", "1", "262153", "0", "", "16", "1", "262153", "9", "Free\\040text", "1026", "511", "16", "6", "262153", "3", "pts", "262153", "4", "hint", "262153", "8", "itemInfo", "262153", "4", "type", "262153", "6", "reward", "262153", "7", "content", "254" ] 
}'>Ignore me</option>
</select>
<span id="tout1" value="spanvalue" class="shiny-html-output"></span>


You can re-size the text box on most browsers.  

**Remember to press** the radio button to submit your answer.

## For the future

1. Numerical entry
2. The markup contains provisions for hints and rewards.  These are not yet displayed.
3. In an interactive mode, the result will be shown immediately.



#### Danny Kaplan
#### May 22, 2013
