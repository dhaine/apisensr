---
output: 
  html_document:
    mathjax:"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
runtime: shiny
---
It performs a probabilistic sensitivity analysis for exposure or outcome
misclassification, and random error.
Misclassification can be differential (4 bias parameters) or not (2 bias parameters).

In the **Analysis tab**, provide:

1. The 2-by-2 table of exposure and outcome,
2. The type of misclassification: exposure misclassification or outcome
   misclassification,
3. The number of replications to run,
4. The following bias parameters:
   - Sensitivity of exposure (for exposure misclassification) or outcome (for
      outcome misclassification) classification among those with the outcome
      (for exposure misclassification) or exposure (for outcome misclassification),
    - Sensitivity of exposure (or outcome) classification among those without
      the outcome (or exposure),
    - Specificity of exposure (or outcome) classification among those with the
      outcome (or exposure), and
    - Specificity of exposure (or outcome) classification among those without
      the outcome (or exposure).
5. The correlation between cases and non-cases sensitivities and specificities,
   and
6. If you want to discard draws that lead to negative adjusted count.

Sensitivities and specificities are given probability distributions as constant,
uniform, triangular, trapezoidal, logit-logistic, or logit-normal.
You can check what these distributions look like in their own tab.

The data for the example provided come from:

- Greenland S., Salvan A., Wegman D.H., Hallock M.F., Smith T.J. A case-control
  study of cancer mortality at a transformer-assembly facility. Int Arch Occup
  Environ Health 1994; 66(1):49-54.
