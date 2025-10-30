# seedTools. Go-To Toolbox for Seed Biology<img src="man/figures/logo.png" align="right" height="138"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/pgomba/seedTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pgomba/seedTools/actions/workflows/R-CMD-check.yaml) <!-- badges: end -->

# About seedTools

*seedTools* is an R package offering a collection of functions designed to simplify seed biology-related tasks. It aims to be an ever-expanding repository of tools—a "function bazaar"—where users can contribute and share functions that streamline common workflows such as media calculations, data analysis, and visualization. Whether you're looking to accelerate your own seed biology tasks or provide tools to benefit the wider community, *seedTools* is here to help.

# Installing and loading seedTools

To install **seedTools** from GitHub, first ensure you have the **devtools** (or **remotes**) package installed (e.g. `install.packages("devtools")`). Then load devtools with `library(devtools)` (or remotes with `library(remotes)`) and run

`devtools::install_github("pgomba/seedTools")`

(or equivalently `remotes::install_github("pgomba/seedTools")`). This will clone the repository, build the package, and install it locally. Once installation is complete, load the package as usual via `library(seedTools)` and you can begin using its functions.

# How to Cite seedTools

As *seedTools* evolves, incorporating contributions from multiple authors, maintaining a stable citation for the package as a whole can become challenging. Publishing the package in a static format may not adequately recognize future contributors and updates.

Instead, the recommended approach is to cite individual functions directly in your work. For example:

"*We used the `rh_mix()` function from the seedTools package (https://github.com/pgomba/seedTools).*"

In your bibliography, reference the package as:

*seedTools* (2024). *Go-To toolbox for seed Biology*. v.0.1. Available at https://pgomba.github.io/seedTools/.

# Collaborating

To submit a function, feel free to push it to the GitHub repository. Alternatively, you can send me the `.R` file containing the function and, if possible, the relevant roxygen documentation. Whether you're submitting a single function or a family of related functions, please include a short article to guide users and provide a starting point. Whenever possible, keep function dependencies (libraries) to a minimum.
