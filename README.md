Nie et al., Distinct biological ages of organs and systems identiﬁed from a multi-omics study, Cell Reports (2022), https://doi.org/10.1016/j.celrep.2022.110459

[![DOI](https://zenodo.org/badge/453005426.svg)](https://zenodo.org/badge/latestdoi/453005426)

## Description
`calculateKDM.R` is the script for calculating biological ages for individuals in the original data set. The KDM biological ages were calculated by [kdm_calc.R][1] module. 

`CalculatePE.Rmd` is the script for calculating parameters (k, q, s, r) for KDM model in sex-stratified samples. The results of it were merged into ‘BiologicalAgeSummaryStatistics.xlsx’.

`dataPreparation_cox.R` is the script for processing the data from National Health and Nutrition Examination Survey (NHANES) of the USA. The data was downloaded from [cdc.gov][2]. The analyses include calculating biological ages and cox regressions.

[1]: <https://github.com/dayoonkwon/BioAge> "kdm_calc.R"
[2]: <https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&CycleBeginYear=1999> "cdc.gov"
