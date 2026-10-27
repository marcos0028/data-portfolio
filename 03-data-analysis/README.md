# Hospital Length of Stay Analysis

## Overview

This project investigates differences in hospital length of stay among patients undergoing surgery for acute cerebrovascular diseases in New York State.

Using hospital discharge data from 2022 to 2024, the analysis examines factors associated with prolonged hospitalization and evaluates differences in hospital performance after adjusting for patient characteristics.

## Objectives

- Identify patient and admission characteristics associated with prolonged hospital stays.
- Measure variation in hospital performance using multilevel statistical models.
- Compare hospital outcomes using direct and indirect standardization.
- Evaluate the association between carotid endarterectomy or stenting and prolonged hospitalization using propensity score weighting.

## Methodology

The analysis includes the following statistical techniques:

- Exploratory data analysis and descriptive statistics.
- Generalized Estimating Equations (GEE) with a logistic link.
- Multilevel logistic regression with random intercepts.
- Direct and indirect standardization of hospital performance.
- Propensity score estimation and Inverse Probability of Treatment Weighting (IPTW).

## Dataset

The study uses hospital inpatient discharge data from the Statewide Planning and Research Cooperative System (SPARCS), covering 2022–2024.

The final analytical sample contains 54,992 patients, including 8,820 who underwent carotid endarterectomy or stenting.

## Key Findings

- Patient clinical severity and mortality risk were strongly associated with prolonged hospitalization.
- Significant differences in hospital performance remained after accounting for patient characteristics.
- Risk adjustment substantially changed the interpretation of performance at certain hospitals.
- After IPTW adjustment, carotid endarterectomy or stenting was associated with lower odds of prolonged hospitalization.

## Repository Structure

- `Methods_and_tools_data_analysis.pdf`: Full project report.
- `src/`: Source code used for the analysis.

## Limitations

The available dataset lacked detailed clinical characteristics, limiting the adjustment for differences in patient health status and the interpretation of treatment effects.

## Full Report

See 'Methods_and_tools_data_analysis.pdf' for the complete analysis, methodology, results and conclusions.