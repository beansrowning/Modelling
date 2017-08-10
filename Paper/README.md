# Thesis Paper

## Depends :
- XeLaTeX
- bibTeX
- TiKZ 
- pgfplots
- subfiles
- ragged2e

## Build instructions :
Building the PDF requires three passes of XeLaTeX, and one of Bibtex in the following manner.  
TiKZ images and pgfplots will render as seprate TeX jobs to avoid memory issues.  
1. xelatex -shell-escape -synctex=1 Paper
2. bibtex Paper
3. xelatex -shell-escape -synctex=1 Paper
4. xelatex -shell-escape -synctex=1 Paper
