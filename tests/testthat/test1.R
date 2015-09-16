bw1 <- system.file("extdata", "GSM1208360_chrI_100Kb_q5_sample.bw", package="seqplots")
bed1 <- system.file("extdata", "Transcripts_ce10_chrI_100Kb.bed", package="seqplots")
bed2 <- system.file("extdata", "GSM1208361_chrI_100Kb_PeakCalls.bed", package="seqplots")

test_that("Test BSgenome package installation", {
    context("Testing BSgenome package installation")    
    if(!"BSgenome.Celegans.UCSC.ce10" %in% BSgenome::installed.genomes()) {
        if(.Platform$OS.type != "windows" || .Machine$sizeof.pointer != 4) {
            source("http://bioconductor.org/biocLite.R")
            biocLite("BSgenome.Celegans.UCSC.ce10")
        }
    }
    expect_true("BSgenome.Celegans.UCSC.ce10" %in% BSgenome::installed.genomes())
})

test_that("Test getPlotSetArray function and plotting interfaces", {
    
    tmp <- file.path(tempdir(), 'SeqPlots')
    context("Testing getPlotSetArray")
    if(.Platform$OS.type != "windows" || .Machine$sizeof.pointer != 4) {
         psa <- getPlotSetArray(bw1, c(bed1, bed2), 'ce10')
    } else {
        psa <- get(load(system.file(
            "extdata", "precalc_plotset.Rdata", package="seqplots"))[[1]])
    }
    expect_that(psa, is_a("PlotSetArray"))
    
    context("Testing utils")
    expect_is(capture.output(show(psa)),    'character', info = NULL, label = NULL)
    expect_is(unlist(psa), 'PlotSetList', info = NULL, label = NULL)
    expect_is(psa[1],      'PlotSetList', info = NULL, label = NULL)
    expect_is(psa[2,1],    'PlotSetArray', info = NULL, label = NULL)
    expect_is(psa[[2]],    'PlotSetPair', info = NULL, label = NULL)
    
    context("Testing graphics")
    expect_error(plot(psa, what='h'))
    expect_null(plot(psa, what='a'))
    expect_is(plot(psa[2], what='h'), 'data.frame')
    expect_is(plot(psa[2,1], what='h'), 'data.frame')
    
    context("Extended functions tests for better test coverage")
    expect_null(plotAverage(psa, plotScale = 'log2'))
    expect_null(plotAverage(unlist(psa), plotScale = 'zscore'))
    expect_null(plotAverage(unlist(psa)[[1]]))
    expect_null(plotAverage(unlist(psa)[[1]], labels = 'A') )
    
    expect_is(plot(psa[2,1], what='h', ggplot=TRUE), 'data.frame')
    
    expect_is(
        plotHeatmap(psa[[2,1]], plotScale= "log2", sortrows = 'decreasing', clstmethod = 'hclust' )
        , 'data.frame'
    )
    expect_is(
        plotHeatmap(unlist(psa)[1], plotScale= "zscore", sortrows = 'increasing', clstmethod = 'ssom')
        , 'data.frame'
    )
    
    expect_is(
        plotHeatmap(unlist(psa)[[1]], ggplot = TRUE, indi = FALSE, autoscale=TRUE, raster=FALSE)
        , 'data.frame'
    )
    
    expect_is(
        plotHeatmap(unlist(psa)[[1]], clspace=c('red', 'blue'), raster=FALSE)
        , 'data.frame'
    )
    
    expect_is(
        plotHeatmap(unlist(psa)[[1]], clspace=c('red', 'blue'), raster=FALSE, ggplot = TRUE)
        , 'data.frame'
    )
    
    expect_is(
        plotHeatmap(unlist(psa)[[1]], indi=FALSE, raster=FALSE, ggplot=TRUE)
        , 'data.frame'
    )
    
    expect_is(
        plotHeatmap(psa[[1]], indi=FALSE, raster=FALSE, colved='red')
        , 'data.frame'
    )
    
    context("Anchored features test")
    getPlotSetArray(bw1, c(bed1, bed2), 'ce10', type = 'af')

    
    
})

test_that("Test motifs", {
    
    
    ms <- MotifSetup()
    ms$addMotif('GAGA')
    ms$addMotif('TATA', revcomp=TRUE)
    ms$addBigWig(bw1)
    expect_is(ms, 'MotifSetup', info = NULL, label = NULL)
    expect_equal(ms$nmotifs(), 3, info = NULL, label = NULL)

    context("Testing getPlotSetArray with motifs")
    if(.Platform$OS.type != "windows" || .Machine$sizeof.pointer != 4) {
        psa <- getPlotSetArray(ms, c(bed1, bed2), 'ce10')
    } else {
        psa <- get(load(system.file(
            "extdata", "precalc_plotset.Rdata", package="seqplots"))[[2]])
    }
    expect_that(psa, is_a("PlotSetArray"))
    
    context("Testing utils with motifs")
    expect_is(capture.output(show(psa)),    'character', info = NULL, label = NULL)
    expect_is(unlist(psa), 'PlotSetList', info = NULL, label = NULL)
    expect_is(psa[1],      'PlotSetList', info = NULL, label = NULL)
    expect_is(psa[2,1],    'PlotSetArray', info = NULL, label = NULL)
    expect_is(psa[[2]],    'PlotSetPair', info = NULL, label = NULL)
    
    context("Testing graphics with motifs")
    expect_error(plot(psa, what='h'))
    expect_null(plot(psa, what='a'))
    expect_is(plot(psa[2,1], what='h'), 'data.frame')
    expect_is(plot(psa[c(1,3,5)], what='h'), 'data.frame')
    
    context("Testing heatmap options")
    expect_is(plotHeatmap(psa[1,], indi = FALSE), 'data.frame')
    
    context("Testing heatmap cluster report")
    expect_true( all(is.na( psa[1,]$plot('h', clusters=0)$ClusterID )) )
    expect_true( all(is.na( plotHeatmap(psa[1,], clstmethod='none' )$ClusterID )) )
    
    context("Anchored features test")
    af <- getPlotSetArray(ms, c(bed1, bed2), 'ce10', type = 'af')
    expect_null(plot(af, what='a'))
    expect_is(plotHeatmap(af[[1]]), 'data.frame')
    expect_null(plotAverage(af, type = 'legend'))
    expect_null(plotAverage(af, type = 'legend', error.estimates=TRUE, legend_ext=TRUE))
    
    expect_is(plotHeatmap(af[[1]], indi=FALSE), 'data.frame')
    
    context("Server deployment test")
    deployServerInstance(server=tempdir())
    expect_error( run(root=tempdir(), shinyErrParam=TRUE) )
    
    context("Hepler functions")
    expect_equal(num2sci(100000), '100k')
    imPlot2(matrix(rnorm(100), 10, 10), xinds=1, horizontal = TRUE)
    imPlot2(matrix(rnorm(100), 10, 10), xinds=1, add = TRUE)
    imPlot2(matrix(rnorm(100), 10, 10), xinds=1, legend.only = TRUE)
    imPlot2(matrix(rnorm(100), 10, 10), xinds=1, graphics.reset=TRUE)
    
    
})


