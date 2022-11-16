# FlyBaseInR

The FlyBaseInR package is an R package that provides direct access to
FlyBase API. Users can extract sequences using a FlyBaseID Gene List and
compare proteomic expressions in developmental stages within the group.

# Installation

To download package:

    install.packages("devtools")
    devtools::install_github("CathNiu/FlyBaseInR", build_vignettes = TRUE)
    library("FlyBaseInR")

# Overview

The FlyBaseInR package provides three functions. The getAllSequences()
and getAllExpression output text files or dataframe(for expression) and
could be used later for customized analysis. The main drawing function
is drawExpression().

To see further documentation:

    ls("package:FlyBaseInR")

For more information on the package, call:

    browseVignettes("FlyBaseInR")

# Contributions

The author of the package is Huilin Niu. The api retrieval functions
make use of the `httr` R packages for data retrieval and json parsing.
The plot drawing functions make use of the `plotly` R package in order
to draw and label the plot. Secondary inclusions are the `bind_cols` and
`bind_rows` function from `dyplr` package.

# References

Park T. Bootswatch. (2020). GitHub Repository.
<https://github.com/thomaspark/bootswatch>

Plotly R Open Source Graphing Library. Plotly. Website.
<https://plotly.com/r/>

R Core Team (2017). R: A language and environment for statistical
computing. R Foundation for Statistical Computing, Vienna, Austria.
Website. <https://www.R-project.org/>.

Gramates LS, Agapite J, Attrill H, Calvi BR, Crosby M, dos Santos G
Goodman JL, Goutte-Gattat D, Jenkins V, Kaufman T, Larkin A, Matthews B,
Millburn G, Strelets VB, and the FlyBase Consortium (2022) FlyBase: a
guided tour of highlighted features. Genetics, Volume 220, Issue 4,
April 2022, iyac035. <https://flybase.org/>

Wickham, H. and Bryan, J. (2019). R Packages (2nd edition). Newton,
Massachusetts: O’Reilly Media. <https://r-pkgs.org/>

# Acknowledgements

This package was developed as part of an assessment for 2022BCB410H:
Applied Bioinformatics, University of Toronto, Toronto, CANADA.

For further information see the vignettes.
