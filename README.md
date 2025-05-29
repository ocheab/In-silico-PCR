# In Silico PCR Tool

## Overview
The **In Silico PCR Tool** is an R Shiny application designed for performing in silico PCR on **Plasmodium genomes**. This tool enables researchers to analyze primer binding sites, visualize product size distributions, and download amplification results in various formats. It is a user-friendly web-based application suitable for **malaria researchers, molecular biologists, and bioinformaticians**.

## Features
- **Run in silico PCR** with customizable parameters (primers, mismatches, product size, etc.).
- **View and download** amplification results as tables and FASTA sequences.
- **Visualize PCR product size distribution** and binding sites interactively.
- **Support for multiple Plasmodium genome datasets**.
- **Interactive and responsive UI** using the Shiny framework.

## Installation
To install and run the **In Silico PCR Tool**, follow these steps:
  
  ### Prerequisites
  Ensure that you have **R (≥ 4.0.0)** and the following packages installed:
  
  ```r
install.packages(c("shiny", "shinybusy", "shinythemes", "Biostrings", "DT", "shinyWidgets"))
```

### Installing the Package
```r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("your_github_username/InSilicoPCR")
```

## Usage
To launch the app, simply run:
  
  ```r
library(InSilicoPCR)
runApp()
```

Once launched, the web interface will guide you through primer selection and PCR analysis.

## How to Use
1. **Home Tab**: Overview of the application with usage instructions.
2. **PCR Analysis Tab**: Enter primer sequences, select genome, and define parameters.
3. **PCR Product Sequences Tab**: View and download FASTA sequences of amplified products.
4. **Visualization Tab**: Generate plots of product size distribution and primer binding sites.
5. **Help Tab**: Additional guidance and FAQs.

## Input Parameters
- **Plasmodium Species**: Choose from available genomes.
- **Forward Primer**: Enter the forward primer sequence.
- **Reverse Primer**: Enter the reverse primer sequence.
- **Max Mismatches**: Allowed mismatches for primer binding.
- **Product Size Range**: Define the minimum and maximum PCR product size.

## Example Workflow
```r
# Load required library
library(InSilicoPCR)

# Start the application
runApp()

# Select a Plasmodium genome, enter primers, and run analysis
```

## Output
- **CSV File**: Amplification results (sequence names, primer positions, product sizes).
- **FASTA File**: Extracted PCR product sequences.
- **Visual Plots**: Histogram of product size distribution.

## License
This software is licensed under the **MIT License**.

## Author
**George Oche Ambrose**  
  Email: ocheab1@gmail.com  
GitHub: https://github.com/ocheab

## Contributions & Issues
If you encounter any issues or want to contribute, feel free to open an issue or submit a pull request on GitHub.
"# In-silico-PCR" 
"# In-silico-PCR" 
