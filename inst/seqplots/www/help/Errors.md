# Error messages


```
Problem with line N: "line_text" [internal_error]
``` 

> The import of feature file (GFF or BED) was not successful due to mis-formatted file.

-----------------


```
Chromosome names provided in the file does not match ones defined in reference genome. 
INPUT: [chr3R, chr2L, chr2R, chr3L] 
GENOME: [chrI, chrII, chrIII, chrIV, chrV, ...]
```

> There are unexpected chromosome names in input file. Following genomes: *Arabidopsis thaliana, Caenorhabditis elegans, Cyanidioschyzon_merolae, Drosophila melanogaster, Homo sapiens, Oryza sativa, Populus trichocarpa, Saccharomyces cerevisiae and Zea mays* support chromosome names remapping between different naming conventions, including: AGPv2, ASM9120v1, Ensembl, JGI2_0, MSU6, NCBI, TAIR10 and UCSC. If you see above error in one of these genomes there are still unexpected names after the correction.
The problematic chromosome names are given in the error message. Remove GFF/BED lines corresponding to them or upgrade the genome to one containing proper naming. Alternatively set genome to NA.

-----------------


```
File already exists, change the name or remove old one.
```

> File named like this already exists in the database, it is impossible to have two files sharing same filename.

-----------------


```
ERROR: solving row 300: negative widths are not allowed
```
> The the row 300 have end coordinate smaller than beginning, hence the width in negative. To fix it the start and stop indicates should be swapped. This error often happens when negative strand (-) ranges are misformatted. 

-----------------


