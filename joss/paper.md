---
title: 'nsqipr: An R package for interacting with NSQIP data'
tags:
  - R
  - NSQIP
  - Surgery
  - Quality 
  - Database
authors:
  - name: Dylan Russell, M.D.
    orcid: 0000-0002-9543-9897
    affiliation: 1
affiliations:
 - name: Tripler Army Medical Center, Honolulu HI
   index: 1
citation_author: Russell, Dylan M.D.
date: "27 October, 2021"
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
repository: www.github.com/dylanrussellmd/nsqipr
---

# Summary

The [American College of Surgeons׳ National Surgical Quality Improvement Program (ACS-NSQIP)](https://www.facs.org/quality-programs/acs-nsqip) is a de-identified data set released annually to researchers from participating hospitals. The main data set as of this publication contains 8,735,357 patient records from 700 hospitals across the United States and U.S. military hospitals around the world. The data sets contain individual patient demographics, pre-operative risk factors, pre-operative laboratory values, intra-operative variables, and post-operative outcomes up to 30 days after surgery. ACS-NSQIP is an invaluable source of high-quality data that has generated a large body of hypothesis-generating retrospective studies. Analysis of the data first requires thoughtful cleaning and joining of the individual data sets across the available 14 years of data. This is a repetitive, laborious, and error-prone process that would benefit from a standardized, open-source package.

# Statement of need

``Gala`` is an Astropy-affiliated Python package for galactic dynamics. Python
enables wrapping low-level languages (e.g., C) for speed without losing
flexibility or ease-of-use in the user-interface. The API for ``Gala`` was
designed to provide a class-based and user-friendly interface to fast (C or
Cython-optimized) implementations of common operations such as gravitational
potential and force evaluation, orbit integration, dynamical transformations,
and chaos indicators for nonlinear dynamics. ``Gala`` also relies heavily on and
interfaces well with the implementations of physical units and astronomical
coordinate systems in the ``Astropy`` package [@astropy] (``astropy.units`` and
``astropy.coordinates``).

``Gala`` was designed to be used by both astronomical researchers and by
students in courses on gravitational dynamics or astronomy. It has already been
used in a number of scientific publications [@Pearson:2017] and has also been
used in graduate courses on Galactic dynamics to, e.g., provide interactive
visualizations of textbook material [@Binney:2008]. The combination of speed,
design, and support for Astropy functionality in ``Gala`` will enable exciting
scientific explorations of forthcoming data releases from the *Gaia* mission
[@gaia] by students and experts alike.

# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{ if } x < 0\cr
1\textrm{ else}
\end{array}\right.$$


# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Rendered R Figures

Figures can be plotted like so:


```r
plot(1:10)
```

![](paper_files/figure-latex/unnamed-chunk-1-1.pdf)<!-- --> 

# References
