$(".scoremessage").change(function() {
    $(this).children().each(function(item){$(this).fadeIn();});
});

/* simple fucntion to enlarge and smaller images */
function enlarge(a) {
    item_width = $('#' + a).width();
    body_width = $("body").width();

    if(item_width < body_width*0.79){
        $('#' + a).width(body_width*0.8);
    }else{
        $('#' + a).width(body_width*0.4);
    }
}

$("#stravachaserdropdownitem").appendTo("#placeholder");

 
function activate_panel(id){

    $(".tab-pane").each(function(elem){
        if($(this).data("value") == id){
            $(this).addClass('active');
        }else{
            $(this).removeClass('active');
        }
    })
    $(".navbar-nav a").each(function(elem){
        if($(this).data("value") == id){
            $(this).parent().addClass('active');
        }else{
            $(this).parent().removeClass('active');
        }
    })

}