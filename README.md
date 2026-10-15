# Manufacturing MSME Data Quality Assessment and MEAL Analysis

## Overview

This project applies a practical Monitoring, Evaluation, Accountability and Learning (MEAL) approach to a manufacturing micro, small and medium-sized enterprise (MSME) dataset from Kenya.

Using R, the analysis examines data quality, documents cleaning decisions, and explores descriptive differences in reported revenue and employment between businesses recorded as having received training and those recorded as having received no training.

The project demonstrates how data-quality assessment supports credible monitoring, responsible evaluation, and evidence-informed programme learning.

## Objectives

- Assess missing values, exact duplicates, repeated identifiers, inconsistent county labels, and unusually high observations.
- Document cleaning decisions and maintain an audit trail.
- Compare reported revenue and employment by training status.
- Examine descriptive differences across business-size categories.
- Develop practical recommendations for MEAL systems and data-quality assurance.

## Dataset

The analysis uses the original dataset from the [MEL Quantitative Case Study: Kenya MSME Manufacturing Analysis](https://github.com/Ocholar/mel-case-study-kenya).

The original dataset contains 5,016 records and eight variables covering business identifiers, county, business size, owner's gender, manufacturing classification, training status, revenue, and employment.

The original dataset is not redistributed in this repository. Users should obtain it from the source and confirm any applicable access and redistribution conditions.

## Methods

The workflow includes:

1. Data familiarisation and completeness checks.
2. Identification of exact duplicate records and repeated identifiers.
3. Standardisation of identified county spelling variants.
4. Flagging of unusual revenue and employment values for investigation.
5. Descriptive comparisons by training status and business size.
6. Interpretation of findings and development of MEAL recommendations.

The analytical copy excludes one exact duplicate row. Repeated identifiers and unusual observations are retained pending verification. Missing values are not imputed.

## Key findings

- Original records: 5,016.
- Records in the analytical copy: 5,015.
- Exact duplicate rows excluded: 1.
- Repeated-ID groups requiring review: 519.
- Missing revenue values: 6.
- Missing employment values: 6.
- County labels reduced from 45 to 27 after standardising identified variants.
- Training status: 1,490 records classified as trained and 3,525 as untrained.

The median reported revenue was KES 32,750 among businesses recorded as having received training and KES 22,000 among those recorded as having received no training. Median employment was two and one employees, respectively.

These are descriptive associations, not evidence that training caused improved business performance.

## Limitations

Repeated identifiers remain unresolved, county labels require final verification, and unusually high values require source-level checks. The available data do not establish a causal training effect.

## Tools

- R
- dplyr
- stringr
- tibble
- readr

## Reproducibility

The R scripts and aggregated outputs in this repository document the analytical workflow. To reproduce the analysis, obtain the source dataset, place it in the locally configured data directory, and follow the instructions in the R scripts.

## Related article
https://medium.com/@iamfaradayotieno/assessing-data-quality-and-business-performance-in-a-manufacturing-msme-dataset-a-practical-meal-ee48bed1d012

## Author

Michael Faraday Otieno

This is an independent learning and portfolio project. The analysis and cleaning decisions are independent of the source repository's own analysis.
