
#devtools::install_github('przemol/seqplots', auth_token='5dc47e7bc3b49c07961d7528a796b8d5818451d4')


require(RSelenium)
require(png)
library(jpeg)

checkForServer()
startServer()

#get the instance running
system('Rscript -e \'devtools::load_all("/Users/przemol/code/seqplotsR"); run(root=tempdir(),port=3456, launch.browser=FALSE);\'', wait=FALSE)
message('Starting Seqplots - 30s reserved')
Sys.sleep(30);

# setup ------------------------------------------------------------------------

#remDr <- remoteDriver(remoteServerAddr = "localhost" , port = 4444, browserName = "firefox")
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName = "chrome")
#remDr <- remoteDriver(browserName = "phantomjs")

remDr$open(); Sys.sleep(5);
remDr$setWindowSize(1280, 1024, winHand = "current")
remDr$navigate("http://127.0.0.1:3456")

setwd('/Volumes/raid0/temp/pic')

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

elemScr <- function(e, out='test.png', marg=0, mtop=0) {
    point <- e$getElementLocationInView()
    size <- e$getElementSize()
    img <- remDr$screenshot(display = FALSE)
    pic <- readPNG(base64Decode(img, "raw"))
    pic_out <- pic[(point$y-marg-mtop):(point$y+size$height+1+marg), (point$x-marg):(point$x+size$width+1+marg), ]
    writePNG(pic_out, out, dpi=150)
    writeJPEG(pic_out, gsub('png', 'jpeg', out), quality = 0.5)
}

#sapply(webElems, function(x){x$getElementAttribute('id')})
#scr <- remDr$screenshot(display = TRUE)

# 01_QuickStart p1 upload ------------------------------------------------------

remDr$screenshot(display = FALSE, file='01_QuickStart_01.png')

bw1 <- system.file("extdata", "GSM1208360_chrI_100Kb_q5_sample.bw", package="seqplots")
bed1 <- system.file("extdata", "Transcripts_ce10_chrI_100Kb.bed", package="seqplots")
bed2 <- system.file("extdata", "GSM1208361_chrI_100Kb_PeakCalls.bed", package="seqplots")
Sys.sleep(1.5);

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

# Take screenshot
js=remDr$executeScript("$('.modal-backdrop').hide()")
uplMod <- remDr$findElements("css selector", "#fileUploadModal")[[1]]
elemScr(uplMod, "01_QuickStart_02.png", marg=-2)

# Upload and screenshot and close
remDr$findElement("css selector", 'button.start')$clickElement(); Sys.sleep(2);
elemScr(uplMod, "01_QuickStart_03.png", marg=-2)
remDr$findElement("css selector", 'button.close')$clickElement(); Sys.sleep(1.5);

# 01_QuickStart p2 calcuations -------------------------------------------------

# Show file modal and take screenshot of body and footer separatly
remDr$findElement("css selector", 'a[href="#myModal"]')$clickElement(); Sys.sleep(1.5);
js=remDr$executeScript("$('.modal-backdrop').hide()")
elemScr(remDr$findElement("css selector", "#myModal > div.modal-body"), "01_QuickStart_04.png", marg=-2)
elemScr(remDr$findElement("css selector", "#myModal > div.modal-footer"), "01_QuickStart_05.png", marg=-2)

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

#reset all
#remDr$findElement("id", 'SFreset')$clickElement()


remDr$findElement("css selector", '#myModal button[onClick="sendToCalc()"]')$highlightElement(0.25)
remDr$findElement("css selector", '#myModal button[onClick="sendToCalc()"]')$clickElement()

elemScr(remDr$findElement("id", 'progressModal'), "01_QuickStart_06.png", marg=0)
Sys.sleep(10);
remDr$getAlertText()[[1]] == "Job done!"
remDr$acceptAlert()

# 01_QuickStart p3 plotting ----------------------------------------------------

boxes <- remDr$findElements("css selector", 'input[name="plot_this"]')
boxes[[1]]$clickElement(); boxes[[2]]$clickElement(); boxes[[3]]$clickElement(); 

remDr$findElement("id", 'replotL')$clickElement(); Sys.sleep(1);
img <- remDr$findElement("css selector", '#image > img')
img$highlightElement(0.25)
data <- img$getElementAttribute('src')

elemScr(remDr$findElement("id", 'plotTable'), "01_QuickStart_07.png", marg=40, mtop=150)
elemScr(remDr$findElements("css selector", "body > div[class=span4] > form > div")[[1]],"01_QuickStart_08.png", mar=40)

js=remDr$executeScript("$('a[data-value=panel3]').click()")
elemScr(remDr$findElements("css selector", "body > div[class=span4] > form > div")[[2]],"01_QuickStart_09.png", mar=10)

pic <- readPNG(base64Decode(gsub('data:image/png;base64,', '', data), "raw"))
plot(0, type='n', xlim=c(1,dim(pic)[2]), ylim=c(1,dim(pic)[1]), asp=1)
rasterImage(pic, 1, 1, dim(pic)[2], dim(pic)[1])
writePNG(pic, '01_QuickStart_10.png')
writeJPEG(pic, '01_QuickStart_10.jpg')

js=remDr$executeScript("$('a[data-value=panel6]').click()")
elemScr(remDr$findElements("css selector", "body > div[class=span4] > form > div")[[2]],"01_QuickStart_11.png", mar=10)

remDr$findElement("id", 'replotH')$clickElement(); Sys.sleep(1);
img <- remDr$findElement("css selector", '#image > img')
img$highlightElement(0.25)
data <- img$getElementAttribute('src')

pic <- readPNG(base64Decode(gsub('data:image/png;base64,', '', data), "raw"))
plot(0, type='n', xlim=c(1,dim(pic)[2]), ylim=c(1,dim(pic)[1]), asp=1)
rasterImage(pic, 1, 1, dim(pic)[2], dim(pic)[1])
writePNG(pic, '01_QuickStart_12.png')
writeJPEG(pic, '01_QuickStart_12.jpg')


# OTHER STUFF ------------------------------------------------------------------

boxes[[1]]$clickElement(); boxes[[2]]$clickElement(); boxes[[3]]$clickElement(); 

cl <- function() remDr$findElement("css selector", "body")$clickElement() 

tabs <- remDr$findElements("css selector", "#ctltabs > li")
well <- remDr$findElement("css selector", "body > div[class=span4] > form ")

tabs[[1]]$clickElement(); cl(); Sys.sleep(1); elemScr(well, "well1.png")
tabs[[2]]$clickElement(); cl(); Sys.sleep(1); elemScr(well, "well2.png")
tabs[[3]]$clickElement(); cl(); Sys.sleep(1); elemScr(well, "well3.png")
tabs[[4]]$clickElement(); cl(); Sys.sleep(1); elemScr(well, "well4.png")
tabs[[5]]$clickElement(); cl(); Sys.sleep(1); elemScr(well, "well5.png")
tabs[[6]]$clickElement(); cl(); Sys.sleep(1); elemScr(well, "well6.png")
tabs[[7]]$clickElement(); cl(); Sys.sleep(1); elemScr(well, "well7.png")


# clese ----------------------------------------------------

remDr$findElement("id", 'stopapp')$highlightElement()
remDr$findElement("id", 'stopapp')$clickElement(); Sys.sleep(1);
expect_equal(remDr$getAlertText()[[1]], "Are you sure you want to exit!?")
remDr$acceptAlert(); Sys.sleep(1);
remDr$acceptAlert()

remDr$close()
remDr$quit()
remDr$closeServer()

# stuff ----------------------------------------------------

# z <- remDr$findElement("css selector", 'a[data-value=panel1]')
# ev=remDr$mouseMoveToLocation(webElement = z)
# remDr$click()
# remDr$click(buttonId = 0)
# elemScr(e, "1_NewPlotPanel.png")
# remDr$findElements("css selector", 'a[data-value=panel3]')[[1]]$clickElement()
# elemScr(e, "2_TitleAndAxispanel.png")
# #remDr$findElements("css selector", 'a[data-value=panel3]')[[1]]$highlightElement(wait = .1)
# e$highlightElement(wait = .1)
# progressModal
# 
# rows2[[3]]$clickElement(); rows2[[4]]$clickElement(); rows2[[5]]$clickElement()
# 
# #remDr$findElement("css selector", ".fileinput-button")$clickElement()
# 
# 
# remDr$findElement("css selector", "#fileupload")$clickElement()
# 
# well <- remDr$findElement("css selector", "body > div[class=span4] > form ")
# 
# js=remDr$executeScript("$('a[data-value=panel1]').click()")
# elemScr(e, "1_NewPlotPanel.png")
# js=remDr$executeScript("$('a[data-value=panel3]').click()")
# elemScr(e, "2_TitleAndAxisPanel.png")
# js=remDr$executeScript("$('a[data-value=panel4]').click()")
# elemScr(e, "3_GuideLinePanel.png")
# js=remDr$executeScript("$('a[data-value=panel5]').click()")
# elemScr(e, "4_KeysPanel.png")
# js=remDr$executeScript("$('a[data-value=panel6]').click()")
# elemScr(e, "5_HeatmapPanel.png")
# js=remDr$executeScript("$('a[data-value=panel2]').click()")
# elemScr(e, "6_SaveLoadPanel.png")
# 
# js=remDr$executeScript("$('a[data-value=panel7]').click()")
# elemScr(e, "4_SaveLoadPanel.png")
# 
# elemScr(e)
# 
# 
# pic <- readPNG('test.png')
# writePNG(pic[126:(126+370), 362:(362+370), ], 'test3.png')
# 
# 
# remDr$screenshot(file = 'test.png')
# pic <- readPNG('test.png')
# 
# library(raster)
# 
# remDr$closeWindow()
# 
# webElem <- remDr$findElement(using = "xpath", "//*/input[@id = 'gbqfq']")
# webElem$sendKeysToElement(list("R Cran"))
# webElem$sendKeysToElement(list(key='Enter'))
# 
# remDr$executeScript("alert('zzz')", args = list())
# remDr$maxWindowSize()
# remDr$screenshot(display = TRUE)
# 
# 
# ch <- remoteDriver(browserName = "chrome")
# 
# remDr <- remoteDriver(browserName = "chrome")
# remDr$open()
# 
# remDr$close()
