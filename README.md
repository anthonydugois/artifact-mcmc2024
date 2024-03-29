# Code used to analyze the properties of random matrices with control on CV

Python scripts were used with following packages on Python 3.9.6:

- numba 0.54.0
- numpy 1.20.3
- pandas 1.3.2

R scripts were used with following packages on R 4.1.0:

- dplyr 1.0.7
- ggplot2 3.3.5
- readr 2.0.0
- scales 1.1.1
- tidyr 1.1.3
- tikzDevice 0.12.3.1
- zoo 1.8-9

The main Python functions are available in `munu_var.py`. This file defines
the functions used to compute the pre-calculation table and generate random
vectors. It can also be used as a script to compute a table with provided
parameters and save it on disk. Use `python munu_var.py --help` for details.

The other Python scripts generate clean datasets that are needed for building
the figures. Each R script builds the corresponding figure for the article.

## tl;dr

To generate all data and figures, just use the provided Makefile:

```
make
```

