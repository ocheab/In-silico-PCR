
# 🔬 In Silico PCR Tool (R Package)

![logo](inst/www/pcr.jpg)

## 📌 Overview

**In Silico PCR Tool** is an R-based Shiny application designed for molecular biologists, clinical scientists, and researchers. This tool simulates PCR amplification in silico using uploaded or manually entered primers and DNA sequences. It allows users to evaluate primer binding specificity, detect potential amplicons, and download predicted PCR products—all before synthesis!

> ✅ Developed by the [Centre for Malaria and Other Tropical Diseases Care](https://cemtrod.org), University of Ilorin Teaching Hospital (UITH), Nigeria.

---

## 🎯 Features

- 🧬 **Simulate PCR reactions** on uploaded FASTA sequences
- 📎 **Manually or batch-upload primer pairs**
- 🎯 Filter results by mismatch tolerance and product size
- 📊 Interactive tables to explore amplification results
- 📥 Download predicted amplicons in FASTA or CSV format
- ⚙️ Built as an installable R package (not web-based)

---

## 📸 Interface Snapshots

### 🏠 Home Page
![Home](inst/www/pcr.jpg)

### 🧪 Run Simulation Tab
![Simulation](inst/www/simulation_screenshot.png)

### 📊 Results Table
![Results](inst/app/www/results_table.png)

---

## 🚀 Installation

You can install the development version of this package directly from GitHub:

```r
# install.packages("devtools") # if not already installed
devtools::install_github("ocheab/In-silico-PCR")
````

---

## ▶️ Launch the App

After installing the package, launch the app with:

```r
library(inSilicoPCR)
run_app()
```

---

## 📁 File Requirements

### 1. DNA Sequences

Upload one or more files in **FASTA** format. Each file can contain multiple sequences.

### 2. Primer CSV (Optional)

You may upload a CSV file with two columns: `Forward` and `Reverse`.

```csv
Forward,Reverse
ATGCGTACTA,GGTAGTCGTA
CTTGCATGGC,AACGTCAGTT
```

---

## ⚙️ Functional Highlights

| Functionality         | Description                                  |
| --------------------- | -------------------------------------------- |
| `run_app()`           | Launches the Shiny interface                 |
| Input: `fasta_upload` | Upload one or more FASTA files               |
| Input: `primer_csv`   | Optional CSV for bulk primer entry           |
| Input: Manual Entry   | Add primer pairs by hand                     |
| Filtering Controls    | Max mismatch, min/max product size           |
| Output Table          | View forward/reverse primer positions, sizes |
| Downloads             | Get FASTA or CSV of filtered products        |

---

## 📘 Example Use Case

Researchers validating primer pairs for *Plasmodium knowlesi* 18S rRNA gene can:

1. Upload `.fasta` sequences from *Plasmodium* genome
2. Provide candidate primers
3. Run the tool to ensure:

   * Amplicons are within desired product size range
   * Binding occurs with high specificity
   * No off-target amplifications are found
4. Download selected amplicons for lab validation

---

## 🧪 For Lab Use

This app is ideal for:

* Primer validation before synthesis
* Diagnostic kit development
* Molecular diagnostics training
* High-throughput screening of primer performance

---

## 🤝 Acknowledgments

Developed at the **Centre for Malaria and Other Tropical Diseases Care**, UITH, Nigeria.

For technical assistance: **[cemtrod.ilorin@gmail.com](mailto:cemtrod.ilorin@gmail.com)**

---

## 📄 License

This package is open-source and distributed under the MIT License.

---

## 📌 Citation

If you use this tool in your work, please cite:

```
George Oche, A., (2025). In Silico PCR Tool. Centre for Malaria and Other Tropical Diseases Care, UITH.
https://github.com/ocheab/In-silico-PCR
```

```

---
