# Improving Vaccination Rates: A Technical Report for State Legislators

This repository contains the R code and technical report for a project aimed at improving vaccination rates and providing a comprehensive briefing for a legislator. The project objective was to conduct the necessary analyses and then write up a technical report for a scientifically knowledgeable staff member in a state legislator’s office.

## Data Source

Three datasets were used for this anaysis:
* US Vaccination Rates - time series data from the World Health Organization reporting vaccination rats in the US for five common vaccines.
* Schools Reporting Status - a list of California kindergartens and whether they reported vaccination data to the state in 2013.
* Districts - a sample of California public school districts from the 2013 data collection, along with specific numbers and percentages for each district.
  - DistrictName    : Name of the district
  - WithoutDTP      : Percentage of students without the DTP vaccine
  - WithoutPolio    : Percentage of students without the Polio vaccine
  - WithoutMMR      : Percentage of students without the MMR vaccine
  - WithoutHepB     : Percentage of students without the Hepatitis B vaccine
  - PctUpToDate     : Percentage of all enrolled students with completely up-to-date vaccines
  - DistrictComplete: Boolean indicating whether or not the district’s reporting was complete
  - PctBeliefExempt : Percentage of all enrolled students with belief exceptions
  - PctChildPoverty : Percentage of children in the district living below the poverty line
  - PctFreeMeal     : Percentage of children in the district eligible for free student meals
  - PctFamilyPoverty: Percentage of families in the district living below the poverty line
  - Enrolled        : Total number of enrolled students in the district
  - TotalSchools    : Total number of different schools in the district


## Tools and Technologies

The analyses and graphs were created using R and RStudio.

## Outputs

The technical report is available in PDF format and is designed for a scientifically knowledgeable staff member in a state legislator’s office. The report includes descriptive statistics, graphical representations of the data, and statistical analyses of the relationships between different variables related to vaccination rates.

## Instructions

To run the R code used in this project, clone the repository and open the Vac_rates_project.R file in RStudio. The code is organized into separate .R files for each section of the report.

## Contributing

If you find any issues or have suggestions for improving this project, please submit an issue or pull request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

I hope this helps you create your README.md file for your project. Feel free to customize it to best suit your project's needs.
