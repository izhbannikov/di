##di - an R package to calculate Deficit Index

## Authors: Ilya Zhbannikov, Konstantin Arbeev, Anatoliy Yashin

## Installation

```
devtools::install_github("izhbannikov/di")
```

## Usage

```
library(di)
dd <- data.frame(subj=seq(1:100), var1=rbinom(100,1,.5), var2=rbinom(100,1,.5), var3=rbinom(100,1,.5))
ddi <- di(dd, c("var1", "var2", "var3"))
```

## References
```
Arnold B. Mitnitski, Alexander J. Mogilner, and Kenneth Rockwood, “Accumulation of Deficits as a Proxy Measure of Aging,” TheScientificWorldJOURNAL, vol. 1, pp. 323-336, 2001. doi:10.1100/tsw.2001.58
```
