require(RSelenium)
require(png)
library(jpeg)

checkForServer()
startServer()

#get the instance running
system(paste(Sys.which('Rscript'), ' -e "library(seqplots); seqplots::run(root=tempdir(),port=3456,launch.browser=FALSE);"'), wait=FALSE)
message('Starting Seqplots - 20s reserved')
Sys.sleep(20);


#remDr <-remoteDriver()
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "chrome")

# Startup ------------------------------------------------------

test_that("Browser works", {
    
    context("Loaeding and starting Selenium server")
   
    
    context("Chacking browser")
    remDr$open(); Sys.sleep(5);
    remDr$setWindowSize(1280, 1024, winHand = "current")
    expect_equal( remDr$getWindowSize(windowId = "current")$width, 1280)
    
    remDr$navigate("http://127.0.0.1:3456"); Sys.sleep(2);
    expect_equal( remDr$getTitle()[[1]], "SeqPlots")

})

# functions --------------------------------------------------------------------
reset <- function() {
    url <- remDr$getCurrentUrl()
    remDr$close()
    remDr$open()
    Sys.sleep(5);
    message('Setting up')
    remDr$setWindowSize(1280, 1024, winHand = "current")
    remDr$navigate("http://127.0.0.1:3456")
}

# 01_QuickStart p1 upload ------------------------------------------------------
test_that("Adding files works", {
    
    context("Uploading files")
    bw1 <-  normalizePath(system.file("extdata", "GSM1208360_chrI_100Kb_q5_sample.bw", package="seqplots"))
    bed1 <- normalizePath(system.file("extdata", "Transcripts_ce10_chrI_100Kb.bed", package="seqplots"))
    bed2 <- normalizePath(system.file("extdata", "GSM1208361_chrI_100Kb_PeakCalls.bed", package="seqplots"))
    
    
    # Add files
    remDr$findElement("css selector", 'a[href="#fileUploadModal"]')$clickElement(); Sys.sleep(1.5);
    
    remDr$findElement("css selector", "#ttfi")$sendKeysToElement(list(bw1))
    remDr$findElement("css selector", "input.f1_user")$sendKeysToElement(list('me'))
    remDr$findElement("css selector", "input[name=comments]")$sendKeysToElement(list('My comment'))
    
    remDr$findElement("css selector", "#ttfi")$sendKeysToElement(list(bed1))
    remDr$findElements("css selector", "input.f1_user")[[2]]$sendKeysToElement(list('me'))
    remDr$findElements("css selector", "input[name=comments]")[[2]]$sendKeysToElement(list('2nd comment'))
    
    remDr$findElement("css selector", "#ttfi")$sendKeysToElement(list(bed2))
    remDr$findElements("css selector", "input.f1_user")[[3]]$sendKeysToElement(list('me'))
    remDr$findElements("css selector", "input[name=comments]")[[3]]$sendKeysToElement(list('3rd comment'))
    
  
    uplMod <- remDr$findElements("css selector", "#fileUploadModal .label label-success")
    
    
    # Upload and screenshot and close
    remDr$findElement("css selector", 'button.start')$clickElement(); Sys.sleep(2);
    expect_equal(length( remDr$findElements("css selector", "#fileUploadModal .label-success") ), 3)
    remDr$findElement("css selector", 'button.close')$clickElement(); Sys.sleep(1.5);

})

# 01_QuickStart p2 calcuations -------------------------------------------------

test_that("Calculation works", {
    context("Calculating")
    remDr$findElement("css selector", 'a[href="#myModal"]')$clickElement(); Sys.sleep(1.5);
    
    # Select files and motifs, start to calculation
    remDr$findElements("css selector", '#myModal a[data-toggle="tab"]')[[1]]$clickElement()
    rows1 <- remDr$findElements("css selector", "#tracktable  tr[role='row']")
    rows1[[3]]$clickElement(); #rows1[[4]]$clickElement(); rows1[[5]]$clickElement()
    
    remDr$findElements("css selector", '#myModal a[data-toggle="tab"]')[[2]]$clickElement()
    rows2 <- remDr$findElements("css selector", "#featuretable  tr[role='row']")
    rows2[[3]]$clickElement(); rows2[[4]]$clickElement(); #rows2[[5]]$clickElement()
    
    remDr$findElements("css selector", '#myModal a[data-toggle="tab"]')[[3]]$clickElement()
    remDr$findElement("id", 'SFpattern')$sendKeysToElement(list('GAGA'))
    remDr$findElement("id", 'SFadd')$clickElement()
    remDr$findElement("id", 'SFpattern')$clearElement()
    remDr$findElement("id", 'SFpattern')$sendKeysToElement(list('GAGA'))
    remDr$findElement("id", 'SFadd')$clickElement()
    
    remDr$findElement("css selector", '#myModal button[onClick="sendToCalc()"]')$highlightElement(0.25)
    remDr$findElement("css selector", '#myModal button[onClick="sendToCalc()"]')$clickElement()
    
    Sys.sleep(5);
    #This is not present on windows/in SC mode
    #expect_equal( remDr$getAlertText()[[1]],  "Job done!")
    remDr$acceptAlert()
})

# 01_QuickStart p3 plotting ----------------------------------------------------
test_that("Plotting works", {
    context("Plotting")
    boxes <- remDr$findElements("css selector", 'input[name="plot_this"]')
    expect_equal( length(boxes),  6)
    boxes[[1]]$clickElement(); boxes[[2]]$clickElement(); boxes[[3]]$clickElement(); 
    
    context("Testing heatmap")
    remDr$findElement("id", 'replotL')$clickElement(); Sys.sleep(1);
    img <- remDr$findElement("css selector", '#image > img')
    img$highlightElement(0.25)
    data <- img$getElementAttribute('src')
    expect_true( grepl('data:image/png;base64', data) )
    
    pic <- readPNG(base64Decode(gsub('data:image/png;base64,', '', data), "raw"))
    expect_is(pic, "array")
    
    #plot(0, type='n', xlim=c(1,dim(pic)[2]), ylim=c(1,dim(pic)[1]), asp=1)
    #rasterImage(pic, 1, 1, dim(pic)[2], dim(pic)[1])
    
    remDr$findElement("id", 'replotH')$clickElement(); Sys.sleep(1);
    img <- remDr$findElement("css selector", '#image > img')
    img$highlightElement(0.25)
    data <- img$getElementAttribute('src')
    expect_true( grepl('data:image/png;base64', data) )
    
    pic <- readPNG(base64Decode(gsub('data:image/png;base64,', '', data), "raw"))
    expect_is(pic, "array")
    
})


# Further tests ----------------------------------------------------
test_that("Exiting works", {
    remDr$findElement("id", 'stopapp')$highlightElement()
    remDr$findElement("id", 'stopapp')$clickElement(); Sys.sleep(1);
    expect_equal(remDr$getAlertText()[[1]], "Are you sure you want to exit!?")
    remDr$acceptAlert(); Sys.sleep(1);
    
    remDr$close()
    remDr$quit()
    remDr$closeServer()
})

