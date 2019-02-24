/*!jQuery Simple SkillBar*/

/**
 *  Version: 1.0.0 (06/11/2016)
 *  Copyright (c) 2016 Leonhard Sinaga
 *  Under MIT and GPL licenses:
 *  http://www.opensource.org/licenses/mit-license.php
 *  http://www.gnu.org/licenses/gpl.html
 *  Source: https://github.com/leonhards/jquery-simple-skillbar
 */

(function ( $ ) {

    "use strict";

    var sb = {};

    sb.o = function() {
        this.o = null;
        this.$ = null;

        this.run = function() {
            
            this.o = $.extend(
                {
                    width: this.$.data('width') || 80,
                    height: this.$.data('height') || '2em',
                    textColor: this.$.data('text-color') || '#ffffff',
                    background: this.$.data('background') || '#337ab7',
                    text: this.$.data('text') || this.$.data('width'),
                    direction: this.$.data('direction') || 'left',
                    max: this.$.data('max') || '100',
                    round: this.$.data('round') || 2
                }, this.o
            );

            this.class(this.$);
            this.intv(this.$);
          
            return this;
        };
    };

    sb.cb = function() {
        sb.o.call(this);

        this.class = function(i) {
            i.addClass("sb_progress");
            i.html("<div class='sb_bar'><div class='sb_label' data-round='" + 
              this.o.round + "' data-unit='" + 
              this.o.unit + "' data-max='"+this.o.max+"'>"+this.o.text+' '+this.o.unit+"</div></div>");
            i.css({
                position: 'relative',
                width: '100%',
                backgroundColor: 'white',
                height: this.o.height
            });
            var left ='';
            var right = '';
            if(this.o.direction=='left'){left = '0';}else{left = '';}
            if(this.o.direction=='right'){right = '0';}else{right = '';}
            i.find('.sb_bar').css({
                position: 'absolute',
                left: left,
                right: right,
                width: '1%',
                height: '100%',
                backgroundColor: this.o.background
            });
            i.find('.sb_label').css({
                paddingLeft: '5px',
                lineHeight: this.o.height,
                color: this.o.textColor
            });
        };

        this.intv = function(i) {
            var s = this;
            var e = i.find('.sb_bar');
            var w = 4;
            var t = setInterval( function() { itv(); }, 10 );            
            var rounder = e.find('.sb_label').attr('data-round')
            
            var itv = function() {
                if ( w >= s.o.width ) {
                    clearInterval(t);
                } else {
                    w++;
                    var value = Math.round(parseFloat(e.find('.sb_label').attr('data-max')) * w / 100*rounder)/rounder + ' ' + e.find('.sb_label').attr('data-unit');
                    e.css('width', w+'%');
                    e.find('.sb_label').text(value);
                }
            };
        };
    };

    $.fn.simpleSkillbar = function(o) {

        return this.each(function() {
            var d = new sb.cb();
            d.o = o;
            d.$ = $(this);
            d.run();
        });
    };
 
}( jQuery ));