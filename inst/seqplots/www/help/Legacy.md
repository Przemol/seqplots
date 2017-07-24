Legacy distributions
====================

> Following SeqPlots distributions are no longer supported and updated.

## Old Mac OS X app

> The Mac OS X bundle is an easy way to use SeqPlots for Mac OS X users. It contains all R binaries and packages that SeqPlots depends on. Additionally, test tracks and genome annotations, and reference sequences for *Homo sapiens*, *Drosophila melanogaster* and *Caenorhabditis elegans* are included. Sequences for other organisms can be downloaded using a graphical user interface.

**System requirements**

* Mac OS X 10.6 (Snow Leopard) or higher

### How to install


1. Download the compressed application from https://github.com/Przemol/seqplots_osx/releases/latest
2. Double-click on downloaded file to unzip the contents of archive
3. Drag and drop SeqPlots.app to Applications folder


### How to start

Start SeqPlots from Applications. If run for the *first time* the operating system security notice might be shown.

> ![Seq1](img/03_01a.png)

To run the application right-click on SeqPlots icon and choose "Open", followed by clicking "Open" button in popup window.
This need to be done only once, later SeqPlots can be started as usual Mac OS X application.

> ![Seq2](img/03_01b.png)

After starting SeqPlots the welcome screen displays the software version, the currently installed genomes and the data folder location.

> ![SeqPlots Mac OS X bindle - the welcome screen](img/03_02.png)

This screen allows you to set up following options:

* Change your data location folder - by default your home directory will be used (**`Change data location`** button)
* Exit application (**`Quit`** button)

Press the **`START`** button to initiate SeqPlots. If initiation was successful the user interface should open in your default web browser.

For SeqPlots usage instructions please refer to <b>Quick start demo</b> or specific chapters of this document.

The window in the background allows you to assess if SeqPlots is running properly and exit the application at any moment.

> ![SeqPlots Mac OS X bindle - the diagnostic window](img/03_04.png)

The full progress bar indicates that SeqPlots is running. Press "Cancel" to stop it. Pressing "Details" will reveal a small text window that displays diagnostic and error messages.

### Package content

* Platypus (http://www.sveinbjorn.org/platypus) wrapper
* R (http://www.r-project.org/) branch 3.1 for Snow leopard (http://r.research.att.com/snowleopard/R-3.1-branch/R-3.1-branch-snowleopard-sa-x86_64.tar.gz)
* SeqPlots dependency packages (including Shiny, rtracklayer and BSgenome)
* Full genome sequences for Caenorhabditis elegans (UCSC version ce10) - BSgenome.Celegans.UCSC.ce10 R package
* SeqPlots package



## Docker image


> SeqPlots is available as Docker image, which can be easily run on Mac OS, Windows and Linux systems. The image can be obtained from Docker Hub: https://hub.docker.com/r/przemol/seqplots/

**System requirements**: Windows, Mac OS X, Linux or any other system supporting the Docker software. Refer to https://www.docker.com/ for details.


### How to install Docker

Install the Docker Toolbox/Docker executables:

  * Windows: http://docs.docker.com/windows/step_one
  * Mac OS X: http://docs.docker.com/mac/step_one
  * Linux: http://docs.docker.com/linux/step_one

### How to use on Mac and Windows

1. On Mac or Windows start [Kitematic](https://kitematic.com/). The application will be installed as a part of Docker Toolbox. You may skip the registration/login.

2. Search for "seqplots", then click on `CREATE` button. This will download the container, all it's dependencies and run the application. The SeqPlots web interface should be shown in "WEB PREVIEW" window on right side of Kitematic.

3. To open SeqPlots interface click on `WEB PREVIEW`. This will open the GUI is system default web browser. At this point the SeqPlots is ready to use. 

4. The uploaded files, datasets and downloaded reference genomes will be saved inside the Docker container.
To use local system storage or access files previously added using SeqPlots Mac OS X app or R package click on the cog icon "VOLUMES" section of Kitematic (below `WEB PREVIEW`). There should be one Docker folder (/var/shiny-server/DATA), currently not mapped to local filesystem. Click on `CHANGE` button on the right side and select your data location, e.g. ~/SeqPlots_data. Verify that path is correct in `LOCAL FOLDER` column, restart SeqPlots Docker image if needed.

### How to use Linux

1. After installing Docker and verifying it works properly (`docker run hello-world`) run following commands in terminal:
```
docker pull przemol/seqplots
docker run -p 80:80 przemol/seqplots
```

2. At this point the SeqPlots should be available on port 80 of local web interface. Open the web browser and navigate to [http://localhost](http://localhost). 

    If port 80 is already taken (e.g. by Apache web server), change port mapping in docker command, e.g. `docker run --rm -p 3838:80 przemol/seqplots` and navigate to [http://localhost:3838](http://localhost:3838). 

4. To use local file system for SeqPlots data storage use `-v <host_dir>:<container_dir>` parameter:
```
docker run -p 3838:80 -v ~/SeqPlots_data:/var/shiny-server/DATA przemol/seqplots
```

### Troubleshooting

When using shared folder error messages `The application unexpectedly exited.` or `Error in plot.new() : could not open file 'tmp/sessionID_1b61fb3846.png'` indicate that shared file system is not writeable for SeqPlots in Docker container. Make sure all files and folders have "Read & Write" permission set for everyone (all users). This can be changed using right click followed by "More info" (Mac) or "Properties" (Windows). On Mac and Linux the following command will fix all issues:
```
chmod -R a+w ~/SeqPlots_data
```

### Package content

* R (http://www.r-project.org/) branch 3.2 for Linux
* Shiny Server (https://github.com/rstudio/shiny-server)
* SeqPlots dependency packages (including Shiny, rtracklayer and BSgenome)
* Full genome sequences for Caenorhabditis elegans (UCSC version ce10) - BSgenome.Celegans.UCSC.ce10 R package
* SeqPlots package