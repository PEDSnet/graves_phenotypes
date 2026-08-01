<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->
<a id="readme-top"></a>

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/PEDSnet/graves_phenotypes">
    <img src="images/logo.png" alt="Logo" height="80px" width="80px">
  </a>

<h3 align="center">Graves Disease Phenotypes</h3>

  <p align="center">
    Phenotypic Subgroups of Pediatric Graves' Disease: A Multicenter Cluster Analysis of Clinical Heterogeneity and Disease Severity
    <br />
    <!-- Uncomment and edit below for a documentation hyperlink. -->
    <!-- <a href="https://github.com/PEDSnet/graves_phenotypes"><strong>Explore the docs »</strong></a> -->
    <!-- <br /> -->
  </p>
</div>

<!-- ABOUT THE PROJECT -->
## About The Project

Graves’ disease (GD) is the most common cause of hyperthyroidism in youth. Current literature supports substantial clinical heterogeneity in its presentation in children and adolescents, but there are no clinically meaningful disease subgroups. We examined clinical and biochemical features of GD in US youth between 2013-2024 at time of initial disease presentation. 

<p align="right">(<a href="#readme-top">back to top</a>)</p>

### Built With

<!-- DEPENDENCIES_START -->
**R Version:** 4.4.0

- data.table (1.17.8)
- dbplyr (2.5.1)
- dplyr (1.2.1)
- DT (0.34.0)
- ggplot2 (4.0.1)
- gt (1.1.0)
- kableExtra (1.4.1)
- knitr (1.50)
- purrr (1.2.0)
- readr (2.1.6)
- rlang (1.2.0)
- srcr (1.1.2)
- stringr (1.6.0)
- table1 (1.5.1)
- tibble (3.3.0)
- tidyr (1.3.1)
- tidyverse (2.0.0)
<!-- DEPENDENCIES_END -->

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started


### Installation

> **Note:** The installation section below is an example. Please edit it to reflect the specific installation steps for your project.

1. Create a new repo using "Use this template."
2. Clone the repo:
```sh
git clone https://github.com/PEDSnet/graves_phenotypes.git
```
3. Migrate codebase (including `renv.lock`) into project repository base directory:
```sh
# (Optional) if you haven't already, generate renv.lock:
cd /path/to/project/directory
Rscript -e "renv::snapshot()"

# Copy into cloned graves_phenotypes repo:
cd /path/to/graves_phenotypes
cp -R /path/to/project/directory/* .
```
4. Setup R environment:
```bash
Rscript -e "renv::restore()"
```

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[issues-shield]: https://img.shields.io/github/issues/PEDSnet/graves_phenotypes.svg?style=for-the-badge
[issues-url]: https://github.com/PEDSnet/graves_phenotypes/issues
[license-shield]: https://img.shields.io/github/license/PEDSnet/graves_phenotypes.svg?style=for-the-badge
[license-url]: https://github.com/PEDSnet/graves_phenotypes/blob/master/LICENSE.txt
[product-screenshot]: images/screenshot.png
