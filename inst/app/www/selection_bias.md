---
output: 
  html_document:
    mathjax:"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
runtime: shiny
---
Selection bias occurs when the study population is not representative of the
target population.
Assessed exposure and outcome influence participation to the study, either from
the start (recruitment of some participants is more or less likely) or during
follow-up (retaining or not participants in the study).

The sensitivity analysis performed by `episensr` for selection bias is based on
selection proportion.

In the **Analysis tab**, provide the 2-by-2 table of exposure and outcome and
the following bias parameters:

- Selection probability among cases exposed (*1*),
- Selection probability among cases unexposed (*2*),
- Selection probability among noncases exposed (*3*), and
- Selection probability among noncases unexposed (*4*).

Alternatively provide a single positive selection-bias factor which is the ratio
of the exposed versus unexposed selection probabilities comparing cases and
noncases [(1 X 4) / (2 X 3) from above].

The data for the example provided come from:

- Stang A., Schmidt-Pokrzywniak A., Lehnert M., Parkin D.M., Ferlay J., Bornfeld
  N. et al. Population-based incidence estimates of uveal melanoma in Germany.
  Supplementing cancer registry data by case-control data. Eur J Cancer Prev
  2006;15:165-70.
