Installation - App for Mac, Win and Linux
=========================================
  
  > The App bundle is an easy way to use SeqPlots for macOS, Win and Linux. 
  > It contains all R binaries and packages that SeqPlots depends on. 
  > Additionally, test tracks and genome annotations, and reference sequences for *Drosophila melanogaster* and *Caenorhabditis elegans* are included. Sequences for other organisms can be downloaded using a graphical user interface - follow the instructions here: http://przemol.github.io/seqplots/#genomes-managment

**System requirements**
  
  * Mac OS X 10.9 (Mavericks) or higher
  * Windows 7 or higher (64bit)
  * Ubuntu 16.04 (possibly other distributions, not tested)

Installation
------------
  
Downbaload and installer from:
https://github.com/Przemol/seqplots_electron/releases

Follow the installation instructions.

How to start on macOS (bypass Gatekeeper warning)
-------------------------------------------------
  
Start SeqPlots from Applications. If run for the *first time* the operating system security notice might be shown.

<center> ![](img/03_01a.png) </center>

To run the application right-click on SeqPlots icon and choose "Open", followed by clicking "Open" button in popup window.
This need to be done only once, later SeqPlots can be started as usual Mac OS X application.

<center> ![](img/03_01b.png) </center>

Package content
---------------
* The Electron framework (https://github.com/electron/electron)
* Platypus (http://www.sveinbjorn.org/platypus) wrapper
* R (http://www.r-project.org/)
* SeqPlots dependency packages (including Shiny, rtracklayer and BSgenome)
* Full genome sequences
* SeqPlots package
