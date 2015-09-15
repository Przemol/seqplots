Installation - Server deployment
===============================

> The server version allows the sharing of signal and feature files by a group of users and for remote calculation and file storage.  This is the ideal method for providing a shared SeqPlots service to a group.  

**System requirements:**

* R 3.1 or higher
* Shiny Server 1.0 or higher

Installation
------------

1\. Install and configure the Shiny Server by following the instructions on https://github.com/rstudio/shiny-server

2\. Install SeqPlots R package and dependences by following the instruction [here](Installation%20-%20R%20package)

3\. Copy SeqPlots files to Shiny Server application folder:

```{bash eval=FALSE}
cp -r $(Rscript -e "cat(system.file('seqplots', package='seqplots'))") /srv/shiny-server/
```

4\. Set up SeqPlots **data location** by running from R: 

```{r eval=FALSE}
seqplots(root='/path/to/data/location')
```

5\. Edit first line of `/srv/shiny-server/shiny/server_config.R`, so the environment variable `root` matches the **data location**; for example:

```{r eval=FALSE}
Sys.setenv('root'='/var/shiny-server/DATA')
```


Usage
-----

After successful installation the SeqPlost web GUI will be available at `your_server_name:3838/seqplots/`.

For further usage please refer to [quick start guide](Quick%20start) or specific chapters of [documentation](..).

### Additional genome packages ###
Genomic packages can be installed using standard bioconductor installer (Internet connection required). For example, to instal human reference genome (hg19):

```{r eval=FALSE}
source("http://bioconductor.org/biocLite.R")
biocLite("BSgenome.Hsapiens.UCSC.hg19")
```

Corresponding genome packages are required before uploading the files for plotting. 
Full list of supported genomes is avilable here: http://www.bioconductor.org/packages/release/BiocViews.html#___BSgenome


Alternative installation
------------------------

SeqPlots can be directly cloned into Shiny Server application folder from git repository by using `git clone https://github.com/Przemol/seqplots.git`. The data location can be set up with following R code:

```{r eval=FALSE}
root <- "/data/location"
dir.create(root)
setwd(root)
require(RSQLite)
sqlite <- dbDriver("SQLite")
con <- dbConnect(sqlite, dbname = "files.sqlite")
dbGetQuery(con, "CREATE TABLE files (id INTEGER PRIMARY KEY ASC, name TEXT UNIQUE, 
  ctime TEXT, type TEXT, format TEXT, genome TEXT, user TEXT, comment TEXT)")
if (!dbListTables(con) == "files") 
    warning("Database not created!")
dbDisconnect(con)
if (!all(sapply(c("removedFiles", "files", "publicFiles", "tmp"), dir.create))) 
    warning("Folders not created!")
message("\nData loaction: ", root)
```

The following dependencies must be installed in R:

```
              package  version
1              shiny   0.10.0
2             fields      7.1
3           parallel    3.1.0
4          multicore      0.2
5           BSgenome   1.32.0
6      GenomicRanges   1.16.3
7            plotrix    3.5-7
8        rtracklayer   1.24.2
9            RJSONIO  1.2-0.2
10           RSQLite   0.11.4
11           kohonen   2.0.14
12             Cairo    1.5-5
13            digest    0.6.4
14           methods    3.1.0
15             tools    3.1.0
16             utils    3.1.0
17            httpuv    1.3.0
18           caTools     1.17
19            xtable    1.7-3
20         htmltools    0.2.4
21            bitops    1.0-6
22              Rcpp   0.11.2
23              spam   0.41-0
24              maps    2.3-7
25              grid    3.1.0
26         grDevices    3.1.0
27      BiocGenerics   0.10.0
28           IRanges   1.22.9
29        Biostrings   2.32.0
30           XVector    0.4.0
31         Rsamtools   1.16.1
32          graphics    3.1.0
33             stats    3.1.0
34          zlibbioc   1.10.0
35      GenomeInfoDb    1.0.2
36            stats4    3.1.0
37               XML 3.98-1.1
38             RCurl 1.95-4.1
39 GenomicAlignments    1.0.1
40      BiocParallel    0.6.1
41           foreach    1.4.2
42         BatchJobs      1.2
43            BBmisc      1.7
44               DBI    0.2-7
45         sendmailR    1.1-2
46              brew    1.0-6
47              plyr    1.8.1
48           stringr    0.6.2
49              fail      1.2
50         checkmate      1.0
51         codetools    0.2-8
52         iterators    1.0.7
53         base64enc    0.1-1
54             class   7.3-10
55              MASS   7.3-33
```

