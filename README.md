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
- **Model Validation**: Testing for non-linearity and model diagnostics
- **Visualization**: Creating publication-ready plots showing odds ratios and predicted probabilities
- **Table Generation**: Producing formatted tables for manuscript submission

## Outputs

The scripts generate:
- **Tables**: Model summaries in Word document format
- **Figures**: Multi-panel plots showing relationships (PDF/JPG)
- **Diagnostics**: Model fit statistics and non-linearity tests

## Citation

TBC
