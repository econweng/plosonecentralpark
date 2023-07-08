# Reproduction code for: COVID-19 and visitation to Central Park, New York City

## Description
This repository contains the code, scripts, and data required to reproduce the results presented in the paper.

## Computational Requirements
Before running the code, please ensure that you have the following software/libraries installed:
* R
* R studio
* Stata

## Instructions for Data Preparation and Analysis
### Data Preparation:

The weekly patterns of visitation information used in this project were obtained from SafeGraph. Please get in touch with SafeGraph for details: https://www.safegraph.com/. Compiled dataset for the paper could be assessed here: 
* Year 2019: https://uflorida-my.sharepoint.com/:x:/g/personal/conanwwz_ufl_edu/EbFpox-qP85Jpc2NhaE-qtoBrhWqUHPP7wxUM6aeyYgvgQ?e=nfugC5
* Year 2020: https://uflorida-my.sharepoint.com/:x:/g/personal/conanwwz_ufl_edu/EXzOJr2S65JIqNmDHsOj3LQBXgDC7q4b9wPEOycgLE5rng?e=2ddvV8

### Data Analysis

#### Calculation of travel cost
The code used for travel cost calculation can be found at https://github.com/econweng/plosonecentralpark/commit/f30bb72931c2ee7b46561060b1c57dc05b2a4efc#diff-97e94dcbaa593f7e8201598d4261d1552543b584de2d2057e805ff0cf43d5080: Please ensure that you set up your osrm server to enable large data processing.

#### Variable Construction
To construct the variables utilized in the regression analysis, we utilized the R language. The code can be found at: https://github.com/econweng/plosonecentralpark/commit/f30bb72931c2ee7b46561060b1c57dc05b2a4efc#diff-92442b6d0d0c6ba1c547df48d8bd8c0e9fd9894199edeb8a192ff88d04dcfaac

#### Econometric Analysis
For the econometric analysis, we employed Stata, a popular statistical software package widely used for econometric modeling. The specific code used can be found at:https://github.com/econweng/plosonecentralpark/commit/f30bb72931c2ee7b46561060b1c57dc05b2a4efc#diff-ab4540538593df3be5df492dc2e4a84edf59e0c75a7a23d5263778dc172d5b0b

#### Calculation of Consumer Surplus
The coefficients derived from the econometric analysis were used to calculate the related consumer surplus. The process and calculations involved in this step are detailed here: https://github.com/econweng/plosonecentralpark/commit/f30bb72931c2ee7b46561060b1c57dc05b2a4efc#diff-fdb72fd6d2dbda7916d6946cc88455eead35ed588660f0dc29118be970a8e2ab






