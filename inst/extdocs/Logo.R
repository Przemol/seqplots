
plotLogo <- function(
    cm='black', cax='black', ca='darkgreen', fw='#DD0030', rev='Royalblue', 
    file='LogoSeqPlotsLabeled.pdf', label=TRUE
){
    if(nchar(file)) pdf(file, width=3*4, height=3)
    
    require(grid)
    
    library(showtext)
    font.add.google("Audiowide", "Audiowide")
    showtext.auto()
    
    layout(t(c(1,2,2,2)))
    
    rad=pi/8
    a=2
    b=1.0 #period=pi
    t=seq(-pi*0.9, pi*3, by=0.1)
    #shift ratio: 13/(13+21)
    r=pi *  (13/(13+21))
    
    R <- matrix(c(cos(rad),-sin(rad),sin(rad),cos(rad)),2,2, byrow=T)
    
    z <- R %*% rbind(t*b, (sin(t)*a))
    z2 <- R %*% rbind(t*b+r, (sin(t)*a))
    
    par(pty='s', mar=c(1,1,1,1), oma=c(0,0,0,0))
    
    plot(t(z), lwd=12, type='n', col='Royalblue', asp = 1, xlim=c(-4, 10), ylim=c(-4, 10), axes=F, xlab=NA, ylab=NA)
    arrows(-3.5, -3.5, -3.5 ,10, lwd=10, col=cax, angle = 20)
    arrows(-3.5, -3.5, 10, -3.5, lwd=10, col=cax, angle = 20)
    
    lines(t(z2), lwd=12, col='#DD0030')
    lines(t(z), lwd=12, col='Royalblue')
    
    lines(c(-2, 0.5, 5), c(1, 6.5, 5.5), lwd=15, col=ca)
    arrows(5, 5.5, 9, 10, lwd=15, length = 0.4,  angle = 35, code = 2, col=ca)
    
    
    par(pty='m')
    plot.new()
    
    text(0.5, 0.5, "SeqPlots", cex = 15.5, col=cm, family="Audiowide")
    
    if(nchar(file)) dev.off()
}
#plotLogo()


plotIcon <- function(
    cm='black', cax='black', ca='darkgreen', fw='#DD0030', rev='Royalblue', 
    file='IconSeqPlots.pdf', label=TRUE, sc=3
){
    if(nchar(file)) pdf(file, width=sc*3, height=sc*3)
    rad=pi/8
    a=2
    b=1.0 #period=pi
    t=seq(-pi*0.9, pi*3, by=0.1)
    #shift ratio: 13/(13+21)
    r=pi *  (13/(13+21))
    
    R <- matrix(c(cos(rad),-sin(rad),sin(rad),cos(rad)),2,2, byrow=T)
    
    z <- R %*% rbind(t*b, (sin(t)*a))
    z2 <- R %*% rbind(t*b+r, (sin(t)*a))
    
    par(pty='s', mar=c(0,0,0,0), oma=c(0,0,0,0), bg = 'transparent')
    
    plot(t(z), lwd=12, type='n', col='Royalblue', asp = 1, xlim=c(-4, 10), ylim=c(-4, 10), axes=F, xlab=NA, ylab=NA, bg = 'transparent')
    arrows(-3.5, -3.5, -3.5 ,10, lwd=sc*10, col=cax, angle = 20, length = sc*0.25)
    arrows(-3.5, -3.5, 10, -3.5, lwd=sc*10, col=cax, angle = 20, length = sc*0.25)
    
    lines(t(z2), lwd=sc*12, col='#DD0030')
    lines(t(z), lwd=sc*12, col='Royalblue')
    
    lines(c(-2, 0.5, 5), c(1, 6.5, 5.5), lwd=sc*15, col=ca)
    arrows(5, 5.5, 9, 10, lwd=sc*15, length = 0.4*sc,  angle = 35, code = 2, col=ca)
    
    
    if(nchar(file)) dev.off()
}


plotSticker <- function(filename="seqplots_sticker.png", url = 'seqplots.github.io', type='wt') {
    library(hexSticker)
    if(type=='wt') {
        sticker(
            expression(plotIcon(file = '', sc = .4)), package="SeqPlots", filename=filename, 
            s_width = 1.6, s_height = 1, s_x = 1, s_y = 0.8, p_y = 1.43, p_size = 7.5, spotlight = TRUE, l_y=1.5, 
            url = url, u_size=2.4, u_x = 1.16, u_y = 0.02, u_color = 'black', p_color = 'black', p_family = "Audiowide",
            h_fill = "#A3C1AD", h_color = 'black'
        )
    } else {
        sticker(
            expression(plotIcon(file = '', sc = .4, cax='transparent')), package="SeqPlots", filename=filename, 
            s_width = 1.8, s_height = 1.1, s_x = 1.0, s_y = 0.7, p_y = 1.40, p_size = 8.0, spotlight = TRUE, l_y=1.4, l_x = 1, 
            url = url, u_size=2.7, u_x = 1, u_y = 0.1, u_color = 'white', p_color = 'white', p_family = "Audiowide",
            h_fill = "black", h_color = 'darkgrey'
        )
    }
}
