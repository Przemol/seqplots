bw1 <- system.file("extdata", "GSM1208360_chrI_100Kb_q5_sample.bw", package="seqplots")
bed1 <- system.file("extdata", "Transcripts_ce10_chrI_100Kb.bed", package="seqplots")
bed2 <- system.file("extdata", "GSM1208361_chrI_100Kb_PeakCalls.bed", package="seqplots")

test_that("Test BSgenome package installation", {
    context("Testing BSgenome package installation")    
    if(!"BSgenome.Celegans.UCSC.ce10" %in% BSgenome::installed.genomes()) {
        source("http://bioconductor.org/biocLite.R")
        biocLite("BSgenome.Celegans.UCSC.ce10")
    }
    expect_true("BSgenome.Celegans.UCSC.ce10" %in% BSgenome::installed.genomes())
})

test_that("Test getPlotSetArray function and plotting interfaces", {
    
    tmp <- file.path(tempdir(), 'SeqPlots')
    context("Testing getPlotSetArray")
    #cat(tmp,'\n')
    expect_that({  
        #dir.create(tmp, showWarnings = FALSE); setwd(tmp)
      
        suppressWarnings(psa <- getPlotSetArray(bw1, c(bed1, bed2), 'ce10'))
    }, is_a("PlotSetArray"))
    
    context("Testing utils")
    expect_is(capture.output(show(psa)),    'character', info = NULL, label = NULL)
    expect_is(unlist(psa), 'PlotSetList', info = NULL, label = NULL)
    expect_is(psa[1],      'PlotSetList', info = NULL, label = NULL)
    expect_is(psa[2,1],    'PlotSetArray', info = NULL, label = NULL)
    expect_is(psa[[2]],    'PlotSetPair', info = NULL, label = NULL)
    
    context("Testing graphics")
    expect_error(plot(psa, what='h'))
    expect_null(plot(psa, what='a'))
    expect_null(plot(psa[2], what='h'))
    expect_null(plot(psa[2,1], what='h'))
    
})

test_that("Test motifs", {
    
    
    ms <- MotifSetup()
    ms$addMotif('GAGA')
    ms$addMotif('TATA')
    ms$addBigWig(bw1)
    expect_is(ms, 'MotifSetup', info = NULL, label = NULL)
    expect_equal(ms$nmotifs(), 3, info = NULL, label = NULL)

    context("Testing getPlotSetArray with motifs")
    expect_that({  
        suppressWarnings(psa <- getPlotSetArray(ms, c(bed1, bed2), 'ce10'))
    }, is_a("PlotSetArray"))
    
    context("Testing utils with motifs")
    expect_is(capture.output(show(psa)),    'character', info = NULL, label = NULL)
    expect_is(unlist(psa), 'PlotSetList', info = NULL, label = NULL)
    expect_is(psa[1],      'PlotSetList', info = NULL, label = NULL)
    expect_is(psa[2,1],    'PlotSetArray', info = NULL, label = NULL)
    expect_is(psa[[2]],    'PlotSetPair', info = NULL, label = NULL)
    
    context("Testing graphics with motifs")
    expect_error(plot(psa, what='h'))
    expect_null(plot(psa, what='a'))
    expect_null(plot(psa[2,1], what='h'))
    expect_null(plot(psa[c(1,3,5)], what='h'))
    
})




