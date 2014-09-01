# setup ------------------------------------------------------------------------

require(RSelenium)
require(png)

RSelenium::checkForServer()
RSelenium::startServer()

remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "firefox"
)


extraCapabilities <- list("screen-resolution" = "1280x1024")
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "chrome"
                      , extraCapabilities = extraCapabilities
)
remDr$open()
remDr$getStatus()

remDr$getWindowSize()
remDr$setWindowSize(1280, 1024, winHand = "current")
remDr$getWindowSize(windowId = "current")

#require(shiny)
#runApp(paste0(find.package("RSelenium"), "/apps/shinytestapp"), port = 6012)

remDr$navigate("http://127.0.0.1:5051/")

# functions --------------------------------------------------------------------

elemScr <- function(e, out='test.png') {
    point <- e$getElementLocationInView()
    size <- e$getElementSize()
    img <- remDr$screenshot(display = FALSE)
    pic <- readPNG(base64Decode(img, "raw"))
    writePNG(pic[point$y:(point$y+size$height+1), point$x:(point$x+size$width+1), ], out)
}



webElems <- remDr$findElements("css selector", "#ctrlSelect input")
webElems[[1]]$clickElement()
webElems[[2]]$clickElement()
webElems[[3]]$clickElement()
webElems[[4]]$clickElement()

sapply(webElems, function(x){x$getElementAttribute('id')})

scr <- remDr$screenshot(display = TRUE)

# getImages --------------------------------------------------------------------


e <- remDr$findElements("css selector", "body > div[class=span4] > form")[[1]]
z <- remDr$findElement("css selector", 'a[data-value=panel1]')
ev=remDr$mouseMoveToLocation(webElement = z)
remDr$click()
remDr$click(buttonId = 0)
elemScr(e, "1_NewPlotPanel.png")
remDr$findElements("css selector", 'a[data-value=panel3]')[[1]]$clickElement()
elemScr(e, "2_TitleAndAxispanel.png")
#remDr$findElements("css selector", 'a[data-value=panel3]')[[1]]$highlightElement(wait = .1)
e$highlightElement(wait = .1)

js=remDr$executeScript("$('a[data-value=panel1]').click()")
elemScr(e, "1_NewPlotPanel.png")
js=remDr$executeScript("$('a[data-value=panel3]').click()")
elemScr(e, "2_TitleAndAxisPanel.png")
js=remDr$executeScript("$('a[data-value=panel4]').click()")
elemScr(e, "3_GuideLinePanel.png")
js=remDr$executeScript("$('a[data-value=panel5]').click()")
elemScr(e, "4_KeysPanel.png")
js=remDr$executeScript("$('a[data-value=panel6]').click()")
elemScr(e, "5_HeatmapPanel.png")
js=remDr$executeScript("$('a[data-value=panel2]').click()")
elemScr(e, "4_SaveLoadPanel.png")

js=remDr$executeScript("$('a[data-value=panel7]').click()")
elemScr(e, "4_SaveLoadPanel.png")

elemScr(e)


pic <- readPNG('test.png')
writePNG(pic[126:(126+370), 362:(362+370), ], 'test3.png')


remDr$screenshot(file = 'test.png')
pic <- readPNG('test.png')

library(raster)

remDr$closeWindow()

webElem <- remDr$findElement(using = "xpath", "//*/input[@id = 'gbqfq']")
webElem$sendKeysToElement(list("R Cran"))
webElem$sendKeysToElement(list(key='Enter'))

remDr$executeScript("alert('zzz')", args = list())
remDr$maxWindowSize()
remDr$screenshot(display = TRUE)


ch <- remoteDriver(browserName = "chrome")

remDr <- remoteDriver(browserName = "chrome")
remDr$open()
