if (Sys.getenv("SHINY_SERVER_VERSION") != '') {div(
  HTML('<div class="row" style="margin-left:0px;"><div style="width:318px" class="alert alert-info alert-block span4">
    									<button type="button" class="close" data-dismiss="alert">&times;</button>
       <h4>Update!</h4>
       <b>NEW</b> SeqPlot for Mac: <strong><a href="http://ws190.gurdon.private.cam.ac.uk/SeqPlots_0.8.2b.dmg" target="_blank">SeqPlots_0.8.2b.dmg</a></strong><br />
       SeqPlots wiki <strong><a href="http://bitbucket.org/przemol/seqplots" target="_blank">HERE</a></strong><br />
       Report an error/issue <strong><a href="http://bitbucket.org/przemol/seqplots/issues/new"target="_blank">HERE</a></strong>
       </div></div>')
  )} else {
    tags$span()
}