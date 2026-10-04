# Original data and optional raw reconstruction

The processed analyses work with `data/processed/analysis_dataset_v2.csv`, which is included unchanged. The small state-level workbook is also included unchanged. The original raw survey collection is stored separately to keep the repository practical to clone.

## Source and reuse

The household data originate from the U.S. Bureau of Labor Statistics (BLS) Consumer Expenditure Surveys public-use microdata. BLS describes its published material as public domain, with attribution requested: [copyright information](https://www.bls.gov/opub/copyright-information.htm). The [PUMD guide](https://www.bls.gov/cex/pumd-getting-started-guide.htm) explains the public-use disclosure restrictions and the roles of survey identifiers such as `NEWID`.

The renewable-energy workbook is the authors' original summary. The paper attributes its underlying production measure to EIA SEDS; see [EIA SEDS](https://www.eia.gov/state/seds/) and [reuse policy](https://www.eia.gov/about/copyrights_reuse.php). The workbook has state codes, production summaries and population, with no household records. Its averaging years and exact EIA series are not recorded.

## Optional FMLI workflow

1. Open the official [BLS PUMD download page](https://www.bls.gov/cex/pumd_data.htm).
2. Obtain the Interview Survey **SAS** packages needed for 2019Q1-2021Q1 and the **CSV** packages needed for 2021Q2-2024Q1. Consult each package's contents: BLS annual releases can include the following year's first quarter; since the 2020 release, quarter 1 is obtained from the previous year's package.
3. Extract only the exact FMLI filenames in `fmli_manifest.csv` into `data/raw/FMLI/`. Do not add other quarters: the historical script selects all matching filenames in that folder.
4. Compare the file sizes and SHA-256 values with the manifest. It records the authors' local snapshot. Later BLS corrections can make newly retrieved data differ; use the included processed CSV when comparing historical processed-data results.
5. Run `Rscript scripts/master_script.R` from the repository root after installing dependencies.

The raw master workflow needs 22 FMLI files totaling 448,586,679 bytes in the original snapshot. MTBI data, the old `fmli_clean.csv`, and files from 2017-2018 are unused by the supplied master script and are omitted. The original `mtbi_clean.csv` also exceeds GitHub's regular file-size limit; [GitHub large-file documentation](https://docs.github.com/en/repositories/working-with-files/managing-large-files/about-large-files-on-github) describes that limit.

Raw downloads are ignored by Git. The analysis scripts do not download data automatically.

