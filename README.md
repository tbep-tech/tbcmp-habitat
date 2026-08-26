# tbcmp-habitat

<!-- badges: start -->
[![build](https://github.com/tbep-tech/tbcmp-habitat/workflows/build/badge.svg)](https://github.com/tbep-tech/tbcmp-habitat/actions)
<!-- badges: end -->

Materials to identify habitat opportunity areas using a modified workflow to support the Tampa Bay Coastal Master Plan (TBCMP). Reserve areas within the five-foot coastal contour are reclassified as existing/potential native/restorable categories. Methods adapted from the Tampa Bay Estuary Program [2020 Habitat Master Plan Update](https://github.com/tbep-tech/hmpu-workflow), applied to the seven counties in the TBCMP. [Claude Code](https://code.claude.com/docs/en/overview) was used extensively to adapt the original code, with all resulting products and documentation verified by a real human. 

View products and methods descriptions: <https://tbep-tech.github.io/tbcmp-habitat>

## Local Setup

This project uses [renv](https://rstudio.github.io/renv/) for R package management. To run locally after cloning the repo:

1. Open the project in the IDE.
2. Run `renv::restore()` to install all required packages from `renv.lock`.
3. Run the scripts in the `R/` folder in numbered order to reproduce the analysis.