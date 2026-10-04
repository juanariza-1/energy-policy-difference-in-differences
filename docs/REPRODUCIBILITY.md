# Historical versions and replication boundaries

## Preserved results

`reports/PAPER_DID.pdf` and `reports/paper_energy_did_v2_final.pdf` are the two supplied April 2026 exports. Both credit Juan Pablo Ariza Gallo and Juan Esteban Londoño. The original CSV, renewable-energy workbook, PDFs and 15 saved figures are unchanged; their hashes are recorded in `artifact_checksums.csv`.

## Available analysis versions

| Script | Input | Historical behavior |
| --- | --- | --- |
| `scripts/FINAL_CODE.R` | Included processed CSV | Compact analysis, model tables and 15 figures |
| `scripts/did_unified_analysis.R` | Included processed CSV | Extended analysis, model tables and 15 figures |
| `scripts/master_script.R` | Optional raw FMLI files and included workbook | Constructs a sample in memory; separate model specifications and figures |

The paper's replication appendix describes the unified script as loading raw survey data. The available file of that name actually loads the processed CSV. The raw-loading program is `master_script.R`. These are separate historical versions; the repository keeps this distinction explicit.

## Compatibility changes

- Replaced personal Desktop paths with paths relative to the repository root.
- Created separate output folders under `generated/`, keeping saved report figures intact.
- Moved package installation to an explicit setup script and added clear dependency checks.
- Accepted the workbook's original `x` header as the mean column expected by the raw script.
- Harmonized raw SAS/CSV survey identifier types as character strings so their rows can be combined, preserving leading zeros from CSV `NEWID` values.
- Corrected four event-study plot arguments from `ci_col` to the supported `fixest::iplot` argument `ci.col`, retaining the original colors and model objects.
- Converted the CSV's `Low`, `Medium` and `High` tercile labels to the existing plotting codes 1, 2 and 3. Previously, coercion to a number discarded every label. This compatibility change affects tercile grouping for the plot; no regression formula, sample restriction or treatment definition was changed.

## Differences requiring a separate analytical revision

1. **Energy burden.** The final paper defines severe burden as `1[4 * quarterly energy expenditure / annual before-tax income > 0.10]`. The master script instead thresholds an unannualized ratio; the processed-data scripts use the unannualized expenditure/income ratio as a continuous outcome. They do not implement the same estimand. The original formulas remain intact, so regenerated burden results should not be labeled as a replication of the final paper's corrected binary outcome.
2. **Renewable-energy provenance.** The report names EIA SEDS, but the workbook does not record a series code or the years included in the state mean. Recovering those details would strengthen exact replication.
3. **Treatment timing.** The so-called 75/25 interaction is built using 2022Q2 as the post period, while some formulas include the 2022Q1 post indicator. The split means the upper quartile versus the remaining 75%, rather than the upper versus lower quartiles. The supplied logic is preserved.
4. **State-level thresholds.** Several treatment cutoffs and standardizations are calculated over household rows. A separate methodological review could compare these with cutoffs computed over unique states and document the preferred estimand.
5. **Raw snapshot equivalence.** The master script is a historical raw-data workflow and has different construction choices. Its output is not assumed identical to the supplied processed CSV.
6. **Overlapping first-quarter snapshots.** The raw file selection includes both `fmli191.sas7bdat` and `fmli191x.sas7bdat`. Both contain 5,623 rows and share all 5,623 `NEWID` values, while their values differ. Choosing the appropriate release or revision and avoiding repeated interviews requires a separate provenance decision; both filenames remain in the historical selection.

The paper itself discusses weather controls, electricity-generation shares, household composition, sensitivity to zero expenditures, and stronger inference checks as possible extensions. They are research opportunities, not changes made during repository preparation.

## Validation performed

Both processed-data scripts and their original analytical baselines completed full runs under R 4.5.1. Each version fitted 26 model objects and produced 15 figures. For each script, all estimated coefficients, covariance matrices and sample sizes were identical before and after the compatibility changes. Fourteen of the 15 regenerated figure files were byte-identical to the same-environment baseline; the tercile figure changed because the input labels now retain their intended groups.

All `feols` calls and constructed model formulas in the three scripts match their originals. Input checks confirm the CSV dimensions, positive survey weights, finite outcomes, treatment interactions and complete tercile labels. The workbook's mean-production-per-capita values match all 88,006 rows of the processed snapshot.

Execution also confirms the version distinction: the processed-data scripts estimate a full-sample continuous-treatment coefficient of approximately **-0.011712**, whereas the saved paper reports approximately **-0.006**. The Q4 coefficient is approximately **-0.044708**, consistent with the rounded paper value. The processed scripts' continuous unannualized burden outcome is a different variable from the paper's corrected binary outcome. Published PDFs and saved figures remain the original exports.

The optional raw master script completed a full run on the original 22-file snapshot, fitting **27 models**, generating **11 figures**, and saving its workspace. After the identifier-type and workbook-header compatibility fixes, it constructs **92,565 rows**, 41 states and 21 quarters. Correcting the four confidence-interval plotting argument names left every fitted coefficient, covariance matrix and sample size exactly unchanged. Its sample remains different from the included 88,006-row processed snapshot; these are separate executed historical workflows.

