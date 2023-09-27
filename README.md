# R-script example for e-learning in the Master thesis

This is a R-script to show the possibilities for statistics in research.
But be aware that more ways lead to the same/similar result.

The used dataset is simulated for species PHOENIX.
*The Phoenix is a sign of death and rebirth in the Greek mythology. 
Feeling drawn to the Phoenix or seeing it in your dreams can be a prophetic symbol 
that difficulties are coming your way but with the strength of the phoenix behind you, 
you will prevail. https://simple.wikipedia.org/wiki/Phoenix_(mythology)*

The elearning for Master thesis can be found at https://ulearning.uu.nl/login/index.php .


## Usage

Within Rstudio your can run this code step by step  and study the result as well as the code used to create the .result

## Project Structure

The project structure distinguishes three kinds of folders:
- read-only (RO): not edited by either code or researcher
- human-writeable (HW): edited by the researcher only.
- project-generated (PG): folders generated when running the code; these folders can be deleted or emptied and will be completely reconstituted as the project is run.


```
.
├── .gitignore
├── LICENSE
├── README.md
├── requirements.txt
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```

## Add a citation file
Create a citation file for your repository using [cffinit](https://citation-file-format.github.io/cff-initializer-javascript/#/)

## License

This project is licensed under the terms of the [MIT License](/LICENSE).
