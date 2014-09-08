library(testthat)
library(seqplots)

message('Runing only command line tests, skipping GUI')
test_check("seqplots", filter='1')
