/*
 * jQuery File Upload Plugin JS Example 7.0
 * https://github.com/blueimp/jQuery-File-Upload
 *
 * Copyright 2010, Sebastian Tschan
 * https://blueimp.net
 *
 * Licensed under the MIT license:
 * http://www.opensource.org/licenses/MIT
 */

/*jslint nomen: true, unparam: true, regexp: true */
/*global $, window, document */

$(function () {
	//var url = 'http://ws190.gurdon.private.cam.ac.uk:8888';
//	if (window.location.host == 'ws190.gurdon.private.cam.ac.uk:3838') {
//		url = 'http://ws190.gurdon.private.cam.ac.uk:8888';
//	} else {
//		url = 'http://ja-mac1.gurdon.cam.ac.uk/gfuploads';
//	}
	 
    // Initialize the jQuery File Upload widget:
    $('#fileupload').fileupload({
    	//url: url,
      type: 'POST',
      dataType: 'text',
      multipart: false,
      sequentialUploads: true,
    	acceptFileTypes: '/(\.|\/)(gff|bw|wig.gz|wig|bed)$/i',
    	submit: function (e, data) {
    		//var input = $('#input');
    		//data.formData = {example: input.val()};
        
       
        
        //$('#fileupload').fileupload('option', 'url')
        console.log('Upload started: ' + $('#fileupload').fileupload('option', 'url'));
        
    		var inputs = data.context.find(':input');
    		//if (!confirm('sure?')) {
      			//input.focus();
     			 //return false;
    		//}
    		if (inputs.filter('[required]').filter(function() { return $(this).val() == ""; }).first().focus().length) {
      		  return false;
    		}
    		//lastFile = inputs.serializeArray();
    		data.formData = inputs.serializeArray();
		},
    getFilesFromResponse: function (data) {
      return data.files;
    },
    done: function (e, data) {
                var that = $(this).data('blueimp-fileupload') ||
                        $(this).data('fileupload'),
                    getFilesFromResponse = data.getFilesFromResponse ||
                        that.options.getFilesFromResponse,
                    files = getFilesFromResponse(data),
                    template,
                    deferred;
                if (data.context) {
                    data.context.each(function (index) {
                        var file = files[index] ||
                                {error: 'Empty file upload result :)'};
                        deferred = that._addFinishedDeferreds();
                        that._transition($(this)).done(
                            function () {
                                var node = $(this);
                                template = $(that.options.templatesContainer).html(
                                  that.options.downloadTemplate({
                                        files: [file],
                                        data: data,
                                        formatFileSize: that._formatFileSize,
                                        options: that.options
                                  })
                                ).children().find('a[download]').each(that._enableDragToDesktop).end().replaceAll(node);
      
                                that._forceReflow(template);
                                that._transition(template).done(
                                    function () {
                                        data.context = $(this);
                                        that._trigger('completed', e, data);
                                        that._trigger('finished', e, data);
                                        deferred.resolve();
                                    }
                                );
                            }
                        );
                    });
                } else {
                    template = that._renderDownload(files)[
                        that.options.prependFiles ? 'prependTo' : 'appendTo'
                    ](that.options.filesContainer);
                    that._forceReflow(template);
                    deferred = that._addFinishedDeferreds();
                    that._transition(template).done(
                        function () {
                            data.context = $(this);
                            that._trigger('completed', e, data);
                            that._trigger('finished', e, data);
                            deferred.resolve();
                        }
                    );
                }
            }
        // Uncomment the following to send cross-domain cookies:
        //xhrFields: {withCredentials: true},
     //    ,done: function (e, data) {
       //     $.each(data.result.files, function (index, file) {
         //       alert(file.name);
          //  });
        //  }

    });

    


  $('#fileupload').bind('fileuploadcompleted', function (e, data) {
    dd=data;
    if( data.files.length != 1 ) {  alert ('Upload error'); return(0); }
    
    var file = data.files[0]
    Shiny.shinyapp.makeRequest(
      'uploadEnd', [data.jobId, data.jobId], 
      function(response) { console.log('Upload end: ' + response) }
    )
        
    var result = { name:file.name, jobID:data.jobId};
    if(typeof data.formData === 'undefined') { return(0); };
    	   
    $.each(data.formData, function() {result[this.name] = this.value;})
    Shiny.shinyapp.sendInput({"TR_addFile":result});
         console.log(result);
    });
  
  $('#fileupload').bind('fileuploadadded', function (e, data) {
	  $('.f1_genome').empty().append( $('#file_genome option').clone() );
  })
  
    $('#fileupload').bind('fileuploadadd', function (e, data) {
        var fileInfo = $.map(data.files, function(file, i) {
          return {
            name: file.name,
            size: file.size,
            type: file.type
          };
        });
        console.log(fileInfo);
        Shiny.shinyapp.makeRequest('uploadInit', [fileInfo], 
          function(response) { 
            data.url = response.uploadUrl; 
            data.jobId = response.jobId;
            console.log(response);
          }, function(error) { console.log(error) })
    })

});

function copyAnnotation(e) {
	var name = $(e).parent().children('input').prop('name');
	var value = $(e).parent().children('input').val();
	$("input[name='"+name+"']").val(value);
}
function copyGenome(e) {
	var name = $(e).parent().children('select').prop('name');
	var value = $(e).parent().children('select').val();
	$("select[name='"+name+"']").val(value);
}
