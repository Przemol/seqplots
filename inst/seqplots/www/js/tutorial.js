var startTutorial = function() { $(function() {
    
    if(!window.location.search.substring(1).match('tutorial')) 
        if(!!$.cookie('skip_tutoial')) 
        return;
    

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

hints = [{
    p: 'right', 
    el: '[data-target="#calcModal"]', 
    head: 'Bring up plot setup window', 
    body: "In order to set up the plot press this button. It brings up the list of files available for plotting and various plotting options."
},{
    p: 'bottom', 
    el: 'tr:contains(H3K4me3_celegans_N2_L3_chrI.bw)', 
    head: 'Select track', 
    body: 'Select H3K4me3 coverage track by clicking "H3K4me3_celegans_N2_L3_chrI.bw"'
},{
    p: 'bottom', 
    el: '[data-value="Features"]', 
    head: 'Select genomic features', 
    body: "Go to Features selection panel"
},{
    p: 'bottom', 
    el: 'tr:contains(Genes_celegans_bottom_20pct_expression_chr1.bed)', 
    head: 'Select 1st set of genomic intervals for plotting', 
    body: 'Select lowly (bootom 20%) expressed genes in C. elegans BED file by clicking "Genes_celegans_bottom_20pct_expression_chr1.bed" file'
},{
    p: 'bottom', 
    el: 'tr:contains(Genes_celegans_top_20pct_expression_chr1.bed)', 
    head: 'Select 2nd set of genomic intervals for plotting', 
    body: 'Select highly (top 20%) expressed genes in C. elegans BED file by clicking "Genes_celegans_top_20pct_expression_chr1.bed" file'
},{
    p: 'bottom', 
    el: '[data-value="Sequence features"]', 
    head: 'Select motifs', 
    body: 'Go to "Sequence features" selection panel'
},{
    p: 'bottom', 
    el: '#SFpattern', 
    head: 'Select motifs - pattern', 
    body: "Enter DNA motif, e.g. CG"
},{
    p: 'bottom', 
    el: '#SFname', 
    head: 'Select motifs - name', 
    body: "Enter pattern name, e.g. CpG",
    delay: 2000
},{
    p: 'top', 
    el: '#SFadd', 
    head: 'Add motif', 
    body: "Click 'Add' this button to confirm selections",
    delay: 2000
},{
    p: 'top', 
    el: '#plot_type', 
    head: 'Select plot type', 
    body: 'This panel allows to select plot type. "Point Features" anchor plots on the start of a genomic intervals. "Midpoint" and "Endpoint" features are similar to point features, but plots are centered on the midpoint and end of the feature respectively. Anchored plot allows to investigate signal along genomic intervals, e.g. TSS to TTS on genes. Intervals with different lengths are scaled to width selected in "Anchored distance" input. Click on "Point Features" to confirm selection.'
},{
    p: 'top', 
    el: '#runcalc', 
    head: 'Start calculation', 
    body: 'Confirm the settings and start a calculation by clicking "Run calculation" button'
},{
    el: '#progressModal .modal-content',
    p: 'bottom',
    head: 'Wait for calculation to finish', 
    body: "This panel allows you to track the progress. The calculation might take a while, depending on settings selected and computer speed.",
    delay: 500,
    wait: 'plot_this'
},
// /?load=tutorial_plots.Rdata#
// tutorial.set(12)
{
    el: 'input[value="[1,1]"]',
    p: 'bottom',
    head: 'Select what to plot', 
    body: "Click on this check-box to select H3K4me3 on lowly (bottom 20%) expressed genes"
},{
    el: 'input[value="[1,2]"]',
    p: 'bottom',
    head: 'Select what to plot', 
    body: "Click on this check-box to select H3K4me3 on highly (top 20%) expressed genes"
},{
    el: '#replotL',
    p: 'bottom',
    head: 'Plot average signal profile', 
    body: "This button profile generates the plot in preview - you can download PDF version as well"
 
},{
    el: 'thead th:nth-child(2) button',
    p: 'bottom',
    head: 'Clear selection of H3K4me3 tracks', 
    body: "Buttons in table header allows toggling/clearing selections or select multiple features at once.",
    delay: 1000
},{
    el: 'thead th:nth-child(3) button',
    p: 'bottom',
    head: 'Select CpG profiles', 
    body: "Buttons in table header allows toggling/clearing selections or select multiple features at once.",
    delay: 1000
},{
    el: '#replotL',
    p: 'bottom',
    head: 'Re-plot average signal profile', 
    body: "This will apply new selections",
},{
    el: 'thead th:nth-child(1) button:nth-child(3)',
    p: 'bottom',
    head: 'Clear selections', 
    body: "Buttons in table header allows toggling/clearing selections or select multiple features at once.",
    delay: 1000
},
// tutorial.set(18)
{
    el: 'tbody tr:nth-child(2) a',
    p: 'bottom',
    head: 'Select H3K4me3 and CpG profiles on highly expressed genes', 
    body: "Buttons in table header allows toggling/clearing selections or select multiple features at once."
},{
    el: '#replotH',
    p: 'bottom',
    head: 'Plot heatmap', 
    body: "This button profile generates the heatmaps in preview - you can download PDF version as well",
},{
    el: '#preview-pdf-div',
    p: 'bottom',
    head: 'The plot preview', 
    body: "Click the plot preview to zoom it",
    delay: 3000 //plotting heatmap
},{
    el: '#zoomcanvas',
    p: 'left',
    head: 'The plot preview', 
    body: "Click the zoomed plot again to close it",
    delay: 500 //canvas popup
},{
    el: '#replotL',
    p: 'bottom',
    head: 'Come back to profile plot', 
    body: "",
    delay: 0 
},
// tutorial.set(23)
{
    el: '.well li:nth-child(2)',
    p: 'bottom',
    head: 'Change plot options', 
    body: "Plot appearance and annotations can be set up in these tabs",
    delay: 1000 //plotting line plot
},{
    el: '#title',
    p: 'bottom',
    head: 'Select plot title', 
    body: "This would be main title for the plot"
    
},{
    el: '#xlabel',
    p: 'bottom',
    head: 'Select X-axis label', 
    body: 'This annotation will show below X-axis, for example "Gene body"',
    delay: 3000 //text input
},{
    el: '.well li:nth-child(4)',
    p: 'bottom',
    head: 'Go to color options tab', 
    body: "You can control multiple other features of the plot, for example colors of average profiles and heatmaps",
    delay: 3000 //text input
},{
    el: '#subplot_options div:nth-child(1)',
    p: 'bottom',
    head: 'Bring up color selections', 
    body: "The default colors are pre-selected, you can change them using color picker",
    delay: 0
},{
    el: '#replotH',
    p: 'bottom',
    head: 'Re-plot heatmaps with new color scheme', 
    body: 'More advanced color spaces can be set up in "Heatmap setup" tabs',
    delay: 500
},
// tutorial.set(29)
{
    el: '.well li:nth-child(1)',
    p: 'bottom',
    head: 'Come back to main panel', 
    body: '',
    delay: 3000
},{
    el: '[data-target="#fileUploadModal"]',
    p: 'bottom',
    head: 'Bring up file selection modal', 
    body: '',
    delay: 0
},{
    el: '#fileUploadModal > div',
    p: 'bottom',
    head: '"Add files" window', 
    body: 'This window allows to upload new tracks and features to SeqPlots. Use it to upload your data. Before uploading a data coming from new organism make sure the corresponding reference genome is available in "Manage reference genomes" tab. To finish this tutorial click anywhere in highlighted area.',
    delay: 500
}];

  function preventDefault(e) {
    e.preventDefault();
  }
  
    tutorial = {
        step: 0,
        niter: 0,
        light: light = new SpotlightRect({x: 0, y: 0, w: window.innerWidth, h: window.innerHeight}, {opacity: 0}),
        hints: hints,
        next: function() {
            $('#tutorial').remove();
            var hint = tutorial.hints[tutorial.step];
            if(!hint) {
                tutorial.stop();
                $('body').append('<div id="tutorial"></div>');
                $('#tutorial').load('outro.html');
                return;
            }
            tutorial.niter=0;
            var check = function(){
                if($(hint.el).length){
                    
                    
                    $('.popover').popover("destroy");
                    var item = $(hint.el).first();
                    tutorial.light.animateTo(paddedRect(item[0], [5]), {opacity: .65});
                    window.addEventListener('wheel', preventDefault);
                    window.addEventListener('mousewheel', preventDefault);
                    
                    hint=hint; item=item;
                    
                    item.popover({title: hint.head, content: hint.body, animation: true, placement: hint.p, container: 'body'}).popover('show');
                    tutorial.step = tutorial.step + 1;
                    
                    
                    
                    if(hint.wait) {
                        if(isFinite(hint.wait)) {
                            item.one('click', function() {
                                
                                $(this).popover("destroy");
                                setTimeout(function(){ tutorial.next() }, 3000)
                                
                            })
                            
                            console.log('wait is num')
                        } else {
                        var observer = new MutationObserver(function(mutations) {
                          mutations.forEach(function(mutation) {
                            if (!mutation.addedNodes) return
                        
                            for (var i = 0; i < mutation.addedNodes.length; i++) {
                              // do things to your newly added nodes here
                              var node = mutation.addedNodes[i];
                              if(hint.wait==node.id) {
                                  console.log(node.id);
                                    item.popover("destroy");
                                    tutorial.next();
                                    observer.disconnect()
                              }
                            }
                         })
                        })
                            observer.observe(document.body, {
                                childList: true
                              , subtree: true
                              , attributes: false
                              , characterData: false
                            })
                        }
                    } else {
                        item.one('click', function() {
                            $(this).popover("destroy");
                            tutorial.next();
                        })
                    }
                    if(false) setTimeout(function() {
                        item.click();
                        if(item.is('input')) item.val('GC').change();
                    }, 1000)
                    
                } else {
                    demo='demo';
                    $('[placeholder="user"]').val('demo');
                    var timer = setTimeout(check, 100); // check again in a second
                    tutorial.niter=tutorial.niter+1;
                    console.log(tutorial.niter);
                    if(tutorial.niter > 20) {
                        alert('Make sure you have tutorial data uploaded to SeqPlots!');
                        tutorial.stop();
                        clearTimeout(timer);
                    }
                }
            };
            setTimeout(function(){ check(); }, hint.delay | 0);
        
        },
        stop: function() {
            var hint = tutorial.hints[tutorial.step-1];
            var item = $(hint.el).first();
            $('.popover').popover("destroy");
            item.unbind('click');
            demo=null;
            $('input[type="search"]').filter(function(){return this.value=='demo'}).each(function(){$(this).addClass('demo').siblings().click();})
            var ms = tutorial.light.animateTo({x: 0, y: 0, w: window.innerWidth, h: window.innerHeight}, {opacity: 0});
            window.removeEventListener('wheel', preventDefault);
            window.removeEventListener('mousewheel', preventDefault);
            //setTimeout(function (light) {
              //tutorial.light.detach();
            //}.bind(null, tutorial.light), ms);
            //alert('The tutorial is done!')
        },
        prev: function() {
            $('.popover').popover("destroy");
            tutorial.step=tutorial.step-2;
            tutorial.next();
        },
        set: function(n) {
            $('.popover').popover("destroy");
            tutorial.step=n;
            tutorial.next();
        },
        set: function(n) {
            $('.popover').popover("destroy");
            tutorial.step=n;
            tutorial.next();
        }
    }
    

    

    
    // stop watching using:
    //observer.disconnect()
    

    $('body').append('<div id="tutorial"></div>');
    $('#tutorial').load('tutorial.html');
    
    tutorial.cancel = function() {
        $('#tutorial').remove();
        if (confirm('Always skip tutorial?')) {
            $.cookie('skip_tutoial', true);
        } else {
            $.removeCookie('skip_tutoial');
        }
    }
    
    $(document).keyup(function(e) {

        if(e.keyCode === 27) {
            if(tutorial.step == 0) {
                $('#tutorial').remove();
            } else {
                tutorial.stop();
                
            }
        }
    });

})};





    
