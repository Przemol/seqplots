require(RSelenium)
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

remDr$navigate("http://127.0.0.1:6012/")
webElems <- remDr$findElements("css selector", "#ctrlSelect input")
webElems[[1]]$clickElement()
webElems[[2]]$clickElement()
webElems[[3]]$clickElement()
webElems[[4]]$clickElement()

sapply(webElems, function(x){x$getElementAttribute('id')})

scr <- remDr$screenshot(display = TRUE)


e <- remDr$findElements("css selector", "#summary")
e <- e[[1]]
point <- e$getElementLocationInView()
size <- e$getElementSize()
e$highlightElement(wait = .1)

img <- remDr$screenshot(display = FALSE)
pic <- readPNG(base64Decode(img, "raw"))
writePNG(pic[point$y:(point$y+size$height+1), point$x:(point$x+size$width+1), ], 'test3.png')

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
