---
output: 
  html_document:
    mathjax:"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
runtime: shiny
---
It performs a simple sensitivity analysis for disease or exposure
misclassification.
Confidence interval for odds ratio is computed as in Chu et al. (2006), for
exposure misclassification.

Estimate of association can be biased if subjects are incorrectly categorized
with respect to their exposure status or outcome, i.e. exposed/diseased subjects
can be classified as non-exposed/non-diseased and vice versa.
Most studies have some degree of misclassification, as there's no perfect
instruments to obtain data (sensitivity and/or specificity are not perfect).
Random error can also cause misclassification (e.g. data entry, missing data).

There are two type of misclassification bias, differential (misclassification is
different in the groups to be compared) and nondifferential (misclassification
is the same across groups to be compared).

In the **Analysis tab**, provide:

1. The 2-by-2 table of exposure and outcome,
2. The type of misclassification: exposure misclassification (corrections using
   sensitivity and specificity: nondifferential and independent errors) or
   outcome misclassification,
3. The following bias parameters:
    - Sensitivity of exposure (for exposure misclassification) or outcome (for
      outcome misclassification) classification among those with the outcome
      (for exposure misclassification) or exposure (for outcome misclassification),
    - Sensitivity of exposure (or outcome) classification among those without
      the outcome (or exposure),
    - Specificity of exposure (or outcome) classification among those with the
      outcome (or exposure), and
    - Specificity of exposure (or outcome) classification among those without
      the outcome (or exposure).

The data for the example come from:

- Fink, A.K., Lash,  T.L. A null association between smoking during pregnancy
  and breast cancer using Massachusetts registry data (United States). Cancer
  Causes Control 2003;14:497-503.

*- Chu, H., Zhaojie, W., Cole, S.R., Greenland, S., Sensitivity analysis of
  misclassification: A graphical and a Bayesian approach, Annals of
  Epidemiology 2006;16:834-841.*
