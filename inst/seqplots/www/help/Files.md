Adding and managing files
=========================

Supported file formats
----------------------

Tracks:

* BigWig (.bw) - http://genome.ucsc.edu/FAQ/FAQformat.html#format6.1
* Wiggle (.wig) - http://genome.ucsc.edu/goldenPath/help/wiggle.html
* BedGraph (.bdg) - http://genome.ucsc.edu/goldenPath/help/bedgraph.html

Features:

* BED - http://genome.ucsc.edu/FAQ/FAQformat.html#format1
* GFF - http://genome.ucsc.edu/FAQ/FAQformat.html#format3
* GTF (with .gff extension) - http://genome.ucsc.edu/FAQ/FAQformat.html#format4

Files must be formatted according to UCSC guidelines. All widely used chromosome names conventions are accepted, e.g. for human files either 'chr1' or '1' can be used, however these conventions should not be mixed within single files.


Adding files
------------

Press the `Add files` button to bring up the **file upload panel**.

> ![File upload panel](img/05_01.png) 

You can drag and drop files here or press the `Add files...` button to open a file selection menu. Before starting the upload the following mandatory information must be provided about each file: 

* User ID
* Reference genome - drop-down menu containing reference genome package currently installed in R

Comments are optional.

The contents of the text field can be copied to all files by clicking the icon at the left of the field. The default values can be set using `Set defaults...` button. Default values are stored using the browser cookies, and the settings will be remembered across different sessions as long as the same web browser is used. File extensions that are not supported will raise an error.

> ![File upload panel with 4 files selected](img/05_02.png)

Individual files can be uploaded by pressing 'start' next to the file name or all files can be uploaded at once by pressing the `Start upload` button at the top of **file upload panel**. 

During the upload process a progress bar is displayed. After upload SeqPlots gives a message that upload was successful or or gives an error message. Common errors are misformatted file formats or chromosome names do not matched the reference genome. For more information please refer to *Error explained* chapter.

> ![A feedback on successfully upload files](img/05_03.png)

To dismiss the upload window, click on `X` or outside the window.


Downloading and removing files
------------------------------
Clicking the `New plot set` button brings up the **file collection window**. The primary function of this window is to choose signal tracks and feature files to use for calculating the plots. However, it also provides basic file management capabilities. Information on files can be reviewed and files can be downloaded or deleted.  Fields can be searched, filtered and sorted by any column. The red `x` button on the right site of file table removes a single file from the collection, while `Remove selected files` button will erase all selected files.

> ![The file collection window](img/06/06_01.png)

