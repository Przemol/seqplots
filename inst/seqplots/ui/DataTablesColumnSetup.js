[
	{ "title": '<i class="icon-file-alt"></i> File name',
      "width": "480px",
      "render": function ( data, type, row ) {
          var short = data.length>60 ? data.substr(0,37)+'[...]'+data.substr(data.length-20,data.length) : data;
          if( data.length>60 ) { return '<abbr title="'+data+'">'+short+'</abbr>' } else { return short }
    }
	},
	{ "title": '<i class="icon-calendar"></i> Date created' },
	{ "title": '<i class="icon-beaker"></i> Format' },
	{ "title": '<i class="icon-info-sign"></i> Genome' },
	{ "title": '<i class="icon-user"></i> User'},
  
  { "title": '<i class="icon-comments-alt icon-large"></i>',
    "className": "table-center",
    "width": "1%",
    "orderable": false,
    "searchable": false,
    "render": function ( data, type, row ) {
      if (data.length) {
        return '<a class="btn btn-xs no_select" rel="popover" data-content="'+ data +'" data-original-title="Comment"'+ 
          ' onmouseover="$(this).popover({trigger: \'hover\', placement: \'left\'}).popover(\'show\')">'+'<i class="icon-comment icon-large no_select"></i></a>'
      } else {
        return '<div style="width: 29px"></div>'
      }
    }
  },
  { "title": '<i class="icon-hdd icon-large"></i>',
    "className": "table-center",
    "orderable": false,
    "searchable": false,
    "width": "1%",
    "render": function ( data, type, row ) {
      return '<a class="btn btn-xs no_select" href="files/' + row[0] + '" target="_blank">'+'<i class="icon-download-alt icon-large no_select"></i></a>'
    }
  },
  { "title": '<i class="icon-trash icon-large""></i>',
    "className": "table-center",
    "orderable": false,
    "searchable": false,
    "width": "1%",
    "render": function ( data, type, row ) {
      return '<button class="btn btn-xs btn-danger no_select" onClick="jsRmFile(&#39;'+row[0]+'&#39;)"><i class="icon-remove icon-large no_select"></i></button>'
    }
  }
]

