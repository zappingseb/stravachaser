var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
  return $(scope).find(".stravachaser.barchart");
};

binding.renderValue = function(el, data) {
  
  var $el = $(el);  

  var limit = Math.max(Math.max.apply(null,data.left),Math.max.apply(null,data.right));
  if((typeof data.label)=="undefined"){
    data.label = "a";
  }
  if((typeof data.round)=="undefined"){
    data.round = 100;
  }
  // For each label, rebuild the whole barchart if there
  // is more than one label
  if(data.label.length > 1){
    
      // get the former described height and colors
      var height_elem = $el.find(".leftelem").css('height');
      var color_left = $el.find(".leftelem").attr('data-background');
      var color_right = $el.find(".rightelem").attr('data-background');
      
      // Remove the old bar chart
      $el.find('.leftwrapper').each(function(e){$(this).remove()});
      $el.find('.rightwrapper').each(function(e){$(this).remove()});
      $el.find('label').each(function(e){$(this).remove()});
      $el.find('br').each(function(e){$(this).remove()});
      
      // Append the new barchart for each element
      for (i = 0; i < data.label.length; i++) { 
        var id = $el.attr('id');
        
        console.log(data);
        
        $el.append('<label>'+data.label[i]+'</label><br/>');
      $el.append('<div class="leftwrapper">' +
       '<div id="'+id+'-'+i+'-left" data-width="0.01" class="leftelem" data-text=" " data-height="'+height_elem+'" data-direction="right" data-background="'+color_left+'"></div>' + 
 ' </div><div class="rightwrapper">' +
        '<div id="'+id+'-'+i+'-right" data-width="0.01" class="rightelem" data-height="'+height_elem+'" data-direction="left" data-text=" " data-background="'+color_right+'"></div>' +
     ' </div></div><br style="line-height:'+height_elem+'"/><br style="line-height:'+height_elem+'"/>'
      );
      
      console.log(data.left[i])
      console.log(limit)
      
      // Make the left side element animate
      $el.find("#"+id+"-"+i+"-left").simpleSkillbar({
        width: data.left[i]/limit*100, 
        text: data.left[i],
        direction: 'right',
        unit: data.unit,
        max: limit,
        round: data.round
      });
      
      // Make the left side element animate
      $el.find("#"+id+"-"+i+"-right").simpleSkillbar({
        width: data.right[i]/limit*100,
        text: data.right[i],
        direction: 'left',
        unit: data.unit,
        max: limit,
        round: data.round
      });
    }//for
  }else{
    
    // In case just one label is used just animate two bars
    $el.find(".leftelem").simpleSkillbar({
      width: data.left/limit*100, 
      text: data.left,
      direction: 'right',
      unit: data.unit,
      max: limit,
      round: data.round
    });
    
    $el.find(".rightelem").simpleSkillbar({
      width: data.right/limit*100,
      text: data.right,
      direction: 'left',
      unit: data.unit,
      max: limit,
      round: data.round
    });

  }
  

};

Shiny.outputBindings.register(binding, "shiny.barchart");