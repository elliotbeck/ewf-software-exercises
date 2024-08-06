# Empirische Wirtschaftsforschung (EWF) Softwareübungen

Welcome to the official repository for the Empirische Wirtschaftsforschung (EWF) course
([03SM22BO0002](https://studentservices.uzh.ch/uzh/anonym/vvz/?sap-language=DE&sap-ui-language=DE#/details/2024/003/SM/51110326/50000003/Wirtschaftswissenschaftliche%2520Fakult%25C3%25A4t/51085509/Bachelor%2520of%2520Arts%2520UZH%2520in%2520Wirtschaftswissenschaften%2520(RVO22)/51087264), AS 24) at the University of Zurich, Department of Economics. All code is written in R.

If you encounter any bugs or typos, please 
[open an issue](https://docs.github.com/en/issues/tracking-your-work-with-issues/creating-an-issue),
 and I will address it as soon as possible. Better yet, fork the repository and 
 [submit a pull request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request-from-a-fork).

## Repository Structure
- **data/**: Contains all the datasets used in the exercises.
- **src/exercises.Rmd**: The main file containing all the code for the exercises. Running this file 
will generate both `exercises.md` and `exercises.html`. The file can also be executed directly and 
interactively via RStudio or VSCode.
- **solutions/exercises.md**: A Markdown file that can be viewed directly on GitHub.
- **solutions/exercises.html**: An HTML file viewable [here](https://html-preview.github.io/?url=https://github.com/elliotbeck/ewf-software-exercises/blob/main/solutions/exercises.html) (same content as `exercises.md`, just in a different format).

## Introduction to Git
Git is a powerful version control system that is essential for modern software development. 
It allows multiple people to work on a project simultaneously without interfering with each other’s 
work. Here are some key benefits of using Git:

- **Version Control**: Git tracks changes to files, allowing you to revert to previous versions 
if needed. This is crucial for maintaining a history of your work and for recovering from mistakes.
- **Collaboration**: Git supports branching and merging, enabling multiple developers to work on 
different features or bug fixes concurrently. Changes can be reviewed and integrated systematically.
- **Backup**: By pushing your code to remote repositories (e.g., GitHub, GitLab), you ensure that 
your work is backed up and can be accessed from anywhere.
- **Documentation**: Each change can be annotated with a commit message, providing a log of what 
changes were made and why. This is invaluable for understanding the evolution of a project.
- **Automation**: Git can be integrated with various continuous integration/continuous deployment 
(CI/CD) tools to automate testing, building, and deployment of your code.

For a detailed introduction, refer to the [official Git documentation](https://git-scm.com/doc).

Version controlling systems are used in nearly every company and organization that works extensively 
with code. Therefore, it is essential for students to learn the basics of version control to be 
well-prepared for the job market.

## Introduction to R
For an introduction to R, see the 
[official R manual](https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf).

We use R Markdown for the software exercises. R Markdown provides an authoring framework for data 
science. You can use a single R Markdown file to both:
- Save and execute code.
- Generate high-quality reports that can be shared with an audience.

Learn more about R Markdown [here](https://rmarkdown.rstudio.com/lesson-1.html).

## Choice of Editor
Most often, people use R via an Integrated Development Environment (IDE). An IDE is a software 
application that provides comprehensive facilities to computer programmers for software development. 

Traditionally, the go-to IDE for R is [RStudio](https://posit.co/download/rstudio-desktop/).
However, I recommend using [VSCode](https://code.visualstudio.com/download) with R extensions ([R in VSCode](https://code.visualstudio.com/docs/languages/r)). 

While technically not an IDE, VSCode is great for several reasons:
- Supports multiple programming languages.
- Extensible with various extensions.
- Integrated terminal and debugging support.
- Customizable to fit your development needs.

## How to Run the Code in This Repository
1. **Install Git**:
   - Follow the instructions for your operating system from the [official Git website](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git).
   
2. **Clone the repository**:
   ```bash
   git clone https://github.com/elliotbeck/ewf-software-exercises.git
   ```
   
3. **Open the project** in your preferred IDE (VSCode or RStudio).

4. **Install required R packages**:
   Ensure all required packages are installed by running:
   ```R
   install.packages(c("required_package1", "required_package2", ...))
   ```
   
5. **Run the code**:
   Open `exercises.Rmd` and knit the document to generate `exercises.md` and `exercises.html`.

## Note
This repository includes some data files in the `data/` directory, which is generally not best
practice. Typically, large files should be managed with [Git LFS](https://git-lfs.com), which
replaces large files with text pointers inside Git while storing the file contents on a remote
server. However, as this is an introductory course, we store the files directly with Git. For future
projects, consider using Git LFS for large files (if you really need to store data on Git).

## Sources
This course heavily borrows from the work of 
[Michael Wolf](https://www.econ.uzh.ch/en/people/faculty/wolf.html) and his former TA, Marc Sommer.