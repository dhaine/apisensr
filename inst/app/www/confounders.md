---
output: 
  html_document:
    mathjax:"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
runtime: shiny
---
Confounding occurs when the effect or association between an exposure and an
outcome is distorted by the presence of other variables that are causes of the
exposure or that share common causal ancestors with the exposure.

`episensr` has several options for confounding:

1. Unknown or unmeasured confounding without effect modification,
2. Unknown or unmeasured polychotomous (3-level) confounding without effect
  modification,
3. Unknown or unmeasured confounding in the presence of effect modification,
4. Bounding the bias limits of unmeasured confounding,
5. Sensitivity analysis for unmeasured confounders based on confounding imbalance
  among exposed and unexposed,
6. Sensitivity analysis for unmeasured confounders based on external adjustment,
7. E-value to assess bias due to unmeasured confounder.

For the first 3 bias analysis, provide the 2-by-2 table of exposure and outcome and
the following bias parameters in the **Simple analysis tab**:

- The association between the confounder and the outcome among those who were
not exposed (RR, OR, or RD according to choice of implementation),
- The prevalence of the confounder among the exposed (between 0 and 1), and
- The prevalence of the confounder among the unexposed (between 0 and 1).

You also have the choice of implementation between ratio measures (relative
risk - RR; odds ratio - OR) and difference measure (risk difference - RD).

The data for the example provided come from:

- Tyndall M.W., Ronald A.R., Agoki E., Malisa W., Bwayo J.J., Ndinya-Achola J.O.
  et al. Increased risk of infection with human immunodeficiency virus type 1
  among uncircumcised men presenting with genital ulcer disease in Kenya. Clin
  Infect Dis 1996;23:449-53.
