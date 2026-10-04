# R environment

The original folder contained no `renv.lock` or package-version record. A historical lockfile therefore cannot be reconstructed reliably.

R 4.5.1 is the version used for the syntax and input checks during repository preparation. The complete package list is declared in `scripts/install_packages.R`. Run it explicitly in R to install missing packages from CRAN. Analysis scripts check dependencies and stop with a clear message if packages are missing; they do not install software during analysis.

Both processed-data scripts also completed a full run with the following package versions. Each historical baseline was executed with the same environment for comparison. These versions describe the validated environment, rather than the unknown environment used to produce the April paper.

| Package | Validated version |
| --- | --- |
| data.table | 1.18.6.1 |
| dplyr | 1.2.1 |
| tidyr | 1.3.2 |
| ggplot2 | 4.0.3 |
| fixest | 0.14.2 |
| broom | 1.0.13 |
| stringr | 1.6.0 |
| scales | 1.4.0 |
| readxl | 1.5.0.1 |
| readr | 2.2.0 |
| haven | 2.5.5 |
| modelsummary | 2.6.0 |

Processed-data scripts:

```text
data.table dplyr tidyr ggplot2 fixest broom stringr scales
```

Additional packages used by the raw-data master script:

```text
readxl readr haven modelsummary
```

Run all commands from the repository root, or set `ENERGY_DID_ROOT`. An existing personal package library may be selected with `R_LIBS_USER` before starting R.

The saved PDFs have no accompanying LaTeX source in the supplied folder. They are delivered as original exports; regenerating their page layout is outside the supplied source material.

