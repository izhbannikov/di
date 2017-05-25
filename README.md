#di - an R package to calculate Deficit Index

## Authors: Ilya Zhbannikov, Konstantin Arbeev, Anatoliy Yashin

## Installation

```
devtools::instal_github("izhbannikov/di")
```

## Usage

```
library(di)
dd <- data.frame(subj=seq(1:100), var1=rbinom(100,1,.5), var2=rbinom(100,1,.5), var3=rbinom(100,1,.5))
ddi <- di(dd, c("var1", "var2", "var3"))
```

