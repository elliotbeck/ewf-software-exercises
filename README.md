# Empirische Wirtschaftsforschung (EWF) Softwareübungen

This repository serves as official code source for the course EWF ([03SM22BO0002](https://studentservices.uzh.ch/uzh/anonym/vvz/?sap-language=DE&sap-ui-language=DE#/details/2024/003/SM/51110326/50000003/Wirtschaftswissenschaftliche%2520Fakult%25C3%25A4t/51085509/Bachelor%2520of%2520Arts%2520UZH%2520in%2520Wirtschaftswissenschaften%2520(RVO22)/51087264),
AS 24) at the University of Zurich, Department of Economics. All code is written in R. If you find
any bugs or typos, please [open an an
issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/creating-an-issue) and I
will try to take care of it asap. Or even better, fork the repo and [submit a pull
request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork).

For an introduction to R see: https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf 

We use R Markdown (https://rmarkdown.rstudio.com/lesson-1.html) for the software exercises. R
Markdown provides an authoring framework for data science. You can use a single R Markdown file to
both: 
- save and execute code
- generate high quality reports that can be shared with an audience

This repo is organized as follows:
- the data data/ directory contains all the data sets used in the exercises
- the code lies in the exercises.Rmd file. This file can be run to generate both the exercises.md
  as well as the exercises.html
- exercises.md is a Markdown file that can be directly viewed on GitHub
- exercises.html is an .html file which can be viewed here:
  https://html-preview.github.io/?url=https://github.com/elliotbeck/ewf-software-exercises/blob/main/exercises.html 
  (same content as exercises.md, just different format)

## Note

I include some data in this repo (data/), which is not good practice in general. I could try
to make up for it by using [Git LFS](https://git-lfs.com), which replaces large
files such as audio samples, videos, datasets, and graphics with text pointers
inside Git, while storing the file contents on a remote server. However, as this is an introductory
course, we just store the files with git. Keep in mind to consider Git LFS if you need to store 
large files on Git in the future.

## Sources

This course heavily borrows from [Michael
Wolf](https://www.econ.uzh.ch/en/people/faculty/wolf.html)'s and his former TA Marc
Sommer's work.
