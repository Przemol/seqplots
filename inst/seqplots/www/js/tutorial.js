SpotlightRect = (function (window, document) {
  'use strict';

  function SpotlightRect(rectToCircumscribe, opts) {
    var ri = this.ri = rectToCircumscribe;
    var ro = this.ro = opts.outer || {x: 0, y: 0, w: window.innerWidth, h: window.innerHeight};
    var el = this.el = [0,0,0,0].map(div);
    if ('opacity' in opts) {
      setOpacity(el, this.opacity = sanitizeOpacity(opts.opacity));
    }
    position(el, ro, ri);
    this.attach();
  }

  SpotlightRect.prototype = {
    attached: false,
    attach: function () {
      if (!this.attached) {
        var f = document.createDocumentFragment();
        this.el.forEach(f.appendChild.bind(f));
        document.body.appendChild(f);
        this.attached = true;
      }
      return this;
    },
    detach: function () {
      if (this.attached) {
        this.el.forEach(function (el) {el.remove()});
        this.attached = false;
      }
      return this;
    },
    animateTo: function (rectToCircumscribe, opts) {
      opts = opts || {};
      var o0 = this.opacity;
      var oN = 'opacity' in opts ? sanitizeOpacity(opts.opacity) : null;
      var ease = opts.ease || swing;
      var r0 = this.ri;
      var rN = rectToCircumscribe;
      var ms = opts.ms || calcDurationMs(r0, rN);
      var ms_1 = 1 / ms;
      var t0 = window.performance && performance.now();  // shame on Safari!
      var tN = t0 + ms;
      var tick = this.tick = (function (t) {
        if (this.tick === tick) {
          if (!t0) {
            t0 = t;
            tN = t0 + ms;
          }
          if (t < tN) {
            window.requestAnimationFrame(tick);
            var alpha = t > t0 ? (t - t0) * ms_1 : 0;
            position(this.el, this.ro, this.ri = interpolateRect(ease(alpha), r0, rN));
            if (oN != null) {
              setOpacity(this.el, this.opacity = o0 + (oN - o0) * alpha);
            }
          } else {
            this.tick = null;
            if (opts.detach) {
              this.detach();
            } else {
              position(this.el, this.ro, this.ri = rN);
            }
            if (oN != null) {
              setOpacity(this.el, this.opacity = oN);
            }
          }
        }
      }).bind(this);
      window.requestAnimationFrame(tick);
      return ms;
    }
  };

  function div() {
    var el = document.createElement('div');
    el.style.cssText = 'position:fixed;z-index:9999;background-color:#000';
    return el;
  }

  function position(el, ro, ri) {
    var r = Math.round(Math.sqrt(ri.w * ri.w + ri.h * ri.h) / 2);
    if (r > 0) {
      var cx = Math.round(ri.x + ri.w / 2);
      var cy = Math.round(ri.y + ri.h / 2);
      positionOne(el[0], r, cx, cy, ro.x, ro.y, ro.w, ri.y - ro.y);
      positionOne(el[1], r, cx, cy, ro.x, ri.y, ri.x - ro.x, ri.h);
      positionOne(el[2], r, cx, cy, ri.x + ri.w, ri.y, ro.x + ro.w - ri.x - ri.w, ri.h);
      positionOne(el[3], r, cx, cy, ro.x, ri.y + ri.h, ro.w, ro.y + ro.h - ri.y - ri.h);
    } else {
      var s = el[0].style;
      s.display = '';
      s.left = s.top = '0';
      s.width = s.height = '100%';
      el[1].style.display = el[2].style.display = el[3].style.display = 'none';
    }
  }

  function positionOne(el, r, cx, cy, x, y, w, h) {
    var s = el.style;
    if (w > 0 && h > 0) {
      s.display = '';
      s.left = Math.round(x) + 'px';
      s.top = Math.round(y) + 'px';
      s.width = Math.round(w) + 'px';
      s.height = Math.round(h) + 'px';
    } else {
      s.display = 'none';
    }
  }

  function sanitizeOpacity(val) {
    return typeof val === 'number' ? (val < 0 ? 0 : (val > 1 ? 1 : val)) : 1;
  }

  function setOpacity(el, val) {
    for (var i = 0; i < el.length; i++) {
      el[i].style.opacity = val;
    }
  }

  function calcDurationMs(r1, r2) {
    var px = Math.max.apply(null, [r1.x - r2.x, r1.y - r2.y, r1.x + r1.w - r2.x - r2.w, r1.y + r1.h - r2.y - r2.h].map(Math.abs));
    return 200 * Math.log((px + 80) / 60) | 0;
  }

  function interpolateRect(alpha, r1, r2) {
    return rectOnPxGrid({
      x: r1.x + (r2.x - r1.x) * alpha,
      y: r1.y + (r2.y - r1.y) * alpha,
      w: r1.w + (r2.w - r1.w) * alpha,
      h: r1.h + (r2.h - r1.h) * alpha
    });
  }

  function rectOnPxGrid(r) {
    return {
      x: Math.round(r.x),
      y: Math.round(r.y),
      w: Math.round(r.w),
      h: Math.round(r.h)
    };
  }

  function swing(p) {
    return .5 - Math.cos(p * Math.PI) / 2;
  }

  return SpotlightRect;
}(window, document));



$(function() {
    
paddedRect =  function (el, pad) {
    var rect = el.getBoundingClientRect();
    var padT = pad[0];
    var padR = pad.length > 1 ? pad[1] : padT;
    var padB = pad.length > 2 ? pad[2] : padT;
    var padL = pad.length > 3 ? pad[3] : padR;
    return {
      x: rect.left - padL,
      y: rect.top - padT,
      w: rect.width + padL + padR,
      h: rect.height + padT + padB
    };
  }
  
    tutorial = {
        step: 0,
        light: light = new SpotlightRect({x: 0, y: 0, w: window.innerWidth, h: window.innerHeight}, {opacity: 0}),
        hints: [
           
            {el: '[data-target="#calcModal"]', head: 'Bring up plot setup', body: "In order to set up the plot press this button. It brings up the list of files avilable for plotting."},
            {el: 'tr[class="odd"]', head: 'Select track', body: "Click the first row"},
            {el: '[data-value="Features"]', head: 'Select feature', body: "Please select feature"},
            {el: "#featureDT tbody tr:first-child", head: 'Plotting', body: "let's plot"}
        ],
        next: function() {
            $('#tutorial').remove();
            var hint = tutorial.hints[tutorial.step];
            
            var check = function(){
                if($(hint.el).length){
                    var item = $(hint.el);
                    tutorial.light.animateTo(paddedRect(item[0], [5]), {opacity: .65});
                    item.first().popover({title: hint.head, content: hint.body, animation: true}).popover('show');
                    tutorial.step = tutorial.step + 1;
                    item.one('click', function() {
                        $(this).popover("destroy");
                        tutorial.next();
                    })
                } else {
                    setTimeout(check, 1000); // check again in a second
                }
            };
            check();
        
        }
    }
    

    $('body').append('<div id="tutorial"></div>');
    $('#tutorial').load('tutorial.html');

});





    
