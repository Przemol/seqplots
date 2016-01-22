

var colorBinding = new Shiny.InputBinding();
$.extend(colorBinding, {
  find: function(scope) {
    return $(scope).find("input[type=color]");
  },
  getValue: function(el) {
    return $(el).val();
  },
  setValue: function(el, value) {
    $(el).val(value);
  },
  subscribe: function(el, callback) {
    $(el).on("change.colorBinding", function(e) {
      callback(true);
    });
  },
  unsubscribe: function(el) {
    $(el).off(".colorBinding");
  },
  getRatePolicy: function() {
    return {policy: 'debounce', delay: 250};
  }
});

Shiny.inputBindings.register(colorBinding);


var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
  return $(scope).find(".pdf-output");
};

var renderPDF = function(el, fw, url) {
    
    if(!url) url=Shiny.shinyapp.$values.thecanvas.im;
	PDFJS.workerSrc = "http://mozilla.github.io/pdf.js/build/pdf.worker.js";
	
	PDFJS.getDocument(url).then(function getPdf(pdf) {
    	pdf.getPage(1).then(function getPage(page) {
    	    
        	var desiredWidth = $(el).parent().width();
        	var desiredHeight = $(el).parent().height();
        	
        	if(!fw) {
        	    desiredWidth = desiredWidth - 10;
        	    desiredHeight = desiredHeight - 10;
        	}
        	                 
            var viewport = page.getViewport(1);
                             
            var scale = desiredWidth / viewport.width;
            var scale2 = desiredHeight / viewport.height;
                             
            if(viewport.height > 320 & fw) {
                scale2 = 320 / viewport.height;
                fw = false;
            } 
                                 
            var scaledViewport = page.getViewport((scale>scale2) & !fw ? scale2 : scale);
        	                 
        	var canvas = el;
        	var context = canvas.getContext("2d");
        	canvas.height = scaledViewport.height;
        	canvas.width = scaledViewport.width;
        	                 
        
        	var renderContext = {
        	    canvasContext: context,
        	    viewport: scaledViewport
        	};
        	page.render(renderContext);
        	
    	});
	});
};

$( window ).resize(function() { 
    if(Shiny.shinyapp.$values.thecanvas) renderPDF(document.getElementById('thecanvas'), true);
});

binding.renderValue = function(el, data) {
                // If absolute URL from the remote server is provided, configure the CORS
	                 // header on that server.
	                 //
	                 console.log(data);
                    renderPDF(el, true);
};

binding.renderError = function(el, err) {
    $('#preview-pdf-div').append('<div id="err"><b>'+ err.message +'</b><hr\><\div>');
    el.height = 1;
    $('#preview-pdf-div').unbind( "click" );
};

binding.clearError = function(el) {
    $('#err').remove();
    $('#preview-pdf-div').click(function(e){
        if($('#zoom').length) return;
        
        $('body').append('<div id="zoom" class="zoom_image"><canvas id="zoomcanvas"></canvas></div>')
        $('#zoom').click(function(e){
            $(this).remove();
        });
        renderPDF(document.getElementById('zoomcanvas'))
        //$(this).toggleClass('zoom_image');
        //renderPDF(document.getElementById('thecanvas'), $(this).attr('class')!='zoom_image')
    });
};

Shiny.outputBindings.register(binding);
