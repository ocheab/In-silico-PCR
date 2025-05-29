[![Schematic of the PCR simulator software used in this study. The... | Download Scientific Diagram](https://tse1.mm.bing.net/th/id/OIP.y74EJCheKIsrnK0qNklQ6QHaG1?pid=Api)](https://www.researchgate.net/figure/Schematic-of-the-PCR-simulator-software-used-in-this-study-The-software-includes-adding_fig3_283804088)

Certainly! Here's a comprehensive and visually engaging `README.md` file tailored for your **In Silico PCR** project. This README is designed to provide clear guidance to users and contributors, enhancing the accessibility and appeal of your GitHub repository.

---

# 🧬 In Silico PCR Tool

**Simulate PCR Amplification with Precision and Ease**

![In Silico PCR Banner](https://github.com/ocheab/In-silico-PCR/blob/main/assets/banner.png)

---

## 📖 Overview

The **In Silico PCR Tool** is a user-friendly Shiny application developed by the [Centre for Malaria and Other Tropical Diseases Care, UITH, Ilorin, Nigeria](https://www.uithilorin.org.ng/). It enables researchers to simulate PCR amplification processes computationally, facilitating:

* **Primer Binding Analysis**: Evaluate the binding efficiency of primers to target DNA sequences.
* **Product Size Prediction**: Determine the expected sizes of PCR products.
* **Specificity Assessment**: Analyze the specificity of primer pairs to minimize off-target effects.
* **Result Exportation**: Download simulation results in CSV and FASTA formats for further analysis.

---

## 🚀 Features

* **Multiple Input Options**: Upload multiple FASTA files and primer CSV files or manually input primer sequences.
* **Customizable Parameters**: Set mismatch tolerances and define product size ranges to tailor simulations.
* **Interactive Results Table**: View and filter predicted amplification results with ease.
* **Downloadable Outputs**: Export selected results in CSV and FASTA formats.
* **Responsive UI**: Enjoy a clean and intuitive interface with real-time feedback using `shinybusy`.

---

## 🖥️ Screenshots

### 🏠 Home Page

![Home Page](https://github.com/ocheab/In-silico-PCR/blob/main/assets/home_page.png)

*Welcome screen introducing the tool and its capabilities.*

### 🧪 Run Simulation

![Run Simulation](https://github.com/ocheab/In-silico-PCR/blob/main/assets/run_simulation.png)

*Interface for uploading files, setting parameters, and initiating simulations.*

### 📊 Results Table

![Results Table](https://github.com/ocheab/In-silico-PCR/blob/main/assets/results_table.png)

*Interactive table displaying predicted amplification results.*

---

## 🛠️ Installation

To run the **In Silico PCR Tool** locally, follow these steps:

1. **Clone the Repository**

   ```bash
   git clone https://github.com/ocheab/In-silico-PCR.git
   cd In-silico-PCR
   ```

2. **Install Required Packages**

   Ensure you have R installed. Then, install the necessary R packages:

   ```r
   install.packages(c("shiny", "shinybusy", "shinythemes", "Biostrings", "DT", "shinyWidgets", "readr"))
   ```

3. **Run the Application**

   ```r
   shiny::runApp()
   ```

   The application will launch in your default web browser.

---

## 📂 Project Structure

```
In-silico-PCR/
├── R/                      # R scripts and functions
├── inst/
│   └── www/                # Static resources (images, CSS)
├── man/                    # Documentation files
├── rsconnect/              # Deployment configurations
├── app.R                   # Main Shiny application script
├── DESCRIPTION             # Package metadata
├── LICENSE                 # License information
├── NAMESPACE               # R namespace declarations
├── README.md               # Project README
```

---

## 🤝 Contributing

We welcome contributions from the community! To contribute:

1. **Fork the Repository**

   Click the "Fork" button at the top right of the repository page.

2. **Create a New Branch**

   ```bash
   git checkout -b feature/YourFeatureName
   ```

3. **Commit Your Changes**

   ```bash
   git commit -m "Add your feature"
   ```

4. **Push to Your Fork**

   ```bash
   git push origin feature/YourFeatureName
   ```

5. **Submit a Pull Request**

   Navigate to your forked repository on GitHub and click "New Pull Request".

---

## 📄 License

This project is licensed under the MIT License. See the [LICENSE](https://github.com/ocheab/In-silico-PCR/blob/main/LICENSE) file for details.

---

## 📬 Contact

For questions, suggestions, or collaboration inquiries, please contact:

📧 [cemtrod.ilorin@gmail.com](mailto:cemtrod.ilorin@gmail.com)

---

## 🌐 Acknowledgments

Developed by the [Centre for Malaria and Other Tropical Diseases Care, UITH, Ilorin, Nigeria](https://www.uithilorin.org.ng/), with the aim of advancing malaria research through computational tools.

---

*Empowering researchers with efficient and accurate PCR simulation capabilities.*

---
