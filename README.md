This repository contains the complete workflow for a Energy Conservation project. The project is divided into three main stages: data cleaning, data preparation, and modeling. Each stage is represented by an R script:

1. **Data Cleaning**: `Data_Cleaning_Final.R`
2. **Data Preparation**: `Data_Preparation_Final.R`
3. **Modeling**: `Modelling_Code_v1.R`

## Prerequisites

Before you begin, ensure you have met the following requirements:

- R (version 3.6 or later)
- RStudio (optional but recommended)
- The following R packages:
  - `dplyr`
  - `tidyr`
  - `ggplot2`
  - `caret`
  - `randomForest`
  - Any other packages mentioned in the scripts

## Installation

To get started, clone the repository to your local machine:

```bash
git clone https://github.com/yourusername/your-repo-name.git
```

Navigate to the project directory:

```bash
cd your-repo-name
```
## Usage

### Data Cleaning

The `Data_Cleaning_Final.R` script is responsible for cleaning the raw data. This includes handling missing values, removing duplicates, and other preprocessing steps. To run this script, open it in RStudio or your preferred R environment and execute the code.

### Data Preparation

The `Data_Preparation_Final.R` script prepares the cleaned data for modeling. This includes feature engineering, scaling, and splitting the data into training and testing sets.

### Modeling

The `Modelling_Code_v1.R` script builds and evaluates the machine learning models. This script covers the entire modeling pipeline, including model training, validation, and performance evaluation.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgements

- Special thanks to the authors and contributors of the R packages used in this project.
- Thank you to anyone who reviews and improves this project.

