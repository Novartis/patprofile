---
title: "Wiki"
date: "10/7/2019"
output: html_document
---


# About

This is a demo app to show safety charts from [safetyexploreR](https://github.com/RhoInc/safetyexploreR) with drill-down ability
to patient profiles, provided by the [patprofile](https://github.com/Novartis/patprofile) R package.


# Get started

```r
devtools::install_github('Novartis/patprofile')
devtools::install_github('xni7/safetyexploreR')
showExample('patprofile')
```


# Workflow

Users can either

- Browse patient profiles by patient ID
- or drill down to patient profiles from eDISH, AE Timelines, Shift plots, lab outlier explorers.


