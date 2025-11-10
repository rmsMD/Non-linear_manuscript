# Non-linear Regression Modelling for Medical Professionals

[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)

## Overview

This repository contains supplementary materials for the manuscript, 
**"Non-linear regression modelling for medical professionals: making curved paths straight-forward"**.

This repository contains R scripts for performing the analyses, and outputting the plots and tables shown in the manuscript. The following files are available:

| File | Description |
|------|-------------|
| `case_study.R` | Main R script demonstrating the complete analysis workflow |
| `simulate_data.R` | Data simulation script |
| `R_markdown_for_Supplementary_PDF.Rmd` | R Markdown document for generating supplementary materials |
| `R_markdown_for_Supplementary_PDF.pdf` | Generated supplementary PDF document |

## Analysis Overview

The case study demonstrates:

- **Data Simulation**: Creating a dataset with known linear (age) and U-shaped (BMI) relationships
- **Model Fitting**: Using restricted cubic splines with logistic regression
- **Model Evaluation**: Testing for non-linearity and model diagnostics
- **Visualization**: Creating publication-ready plots
- **Table Generation**: Producing formatted tables for manuscript submission

## Citation

If you use **rmsMD** in your work, please cite the following article:

> Tingle SJ, Kourounis G, Elliot S, Harrison EM. Non-linear regression modelling for medical professionals: making curved paths straightforward. *Postgrad Med J.* 2025 Nov; qgaf183. DOI: [10.1093/postmj/qgaf183](https://doi.org/10.1093/postmj/qgaf183)

BibTeX entry for reference managers:

```bibtex
@article{10.1093/postmj/qgaf183,
    author = {Tingle, Samuel J and Kourounis, Georgios and Elliot, Sarah and Harrison, Ewen M},
    title = {Non-linear regression modelling for medical professionals; making curved paths straightforward},
    journal = {Postgraduate Medical Journal},
    pages = {qgaf183},
    year = {2025},
    month = {11},
    issn = {0032-5473},
    doi = {10.1093/postmj/qgaf183},
    url = {https://doi.org/10.1093/postmj/qgaf183},
    eprint = {https://academic.oup.com/pmj/advance-article-pdf/doi/10.1093/postmj/qgaf183/65131196/qgaf183.pdf},
}
```
