barChartOutput <- function(id, color_left="#fc4c02", color_right = "#777777", height="2em") {
  HTML(
    glue::glue(
      '<div id="{id}" class="stravachaser barchart">
<div class="leftwrapper">
       <div id="{id}-left" data-width="0.01" class="leftelem" data-text=" " data-height="{height}" data-direction="right" data-background="{color_left}"></div>
</div><div class="rightwrapper">
        <div id="{id}-right" data-width="0.01" class="rightelem" data-height="{height}" data-direction="left" data-text=" " data-background="{color_right}"></div>
      </div></div>
      <script>
      $("#{id}-left").simpleSkillbar({{width:0}});
$("#{id}-right").simpleSkillbar({{width:0}});
</script>
      '
    )
    
  )#HTML
  
  
}

renderBarChart <- function(expr, env=parent.frame(), quoted=FALSE) {
  # This piece of boilerplate converts the expression `expr` into a
  # function called `func`. It's needed for the RStudio IDE's built-in
  # debugger to work properly on the expression.
  
  func <- exprToFunction(expr, env, quoted)
  
  function() {
    data = func();
  }
}