---
abstract: Outliers due to technical errors in water-quality data from \emph{in situ} sensors can reduce data quality and have a direct impact on inference drawn from subsequent data analysis. However, outlier detection through manual monitoring is unfeasible given the volume and velocity of data the sensors produce. Here, we proposed an automated framework that provides early detection of outliers in water-quality data from \emph{in situ} sensors caused by technical issues.The framework was used first to identify the data features that differentiate outlying instances from typical behaviours. Then statistical transformations were applied to make the outlying instances stand out in transformed data space. Unsupervised outlier scoring techniques were then applied to the transformed data space and an approach based on extreme value theory was used to calculate a threshold for each potential outlier. Using two data sets obtained from \emph{in situ} sensors in rivers flowing into the Great Barrier Reef lagoon, Australia, we showed that the proposed framework successfully identified outliers involving abrupt changes in turbidity, conductivity and river level, including sudden spikes, sudden isolated drops and level shifts, while maintaining very low false detection rates. We implemented this framework in the open source R package \texttt{oddwater}.
authors:
- admin
- Rob J. Hyndman
- Catherine Leigh
- Kerrie Mengersen
- Kate Smith-Miles
date: "2019-10-12"
doi: "10.1029/2019WR024906"
featured: false
image:
  caption: 'Top panel (a–c): Bi-variate relationships between original water-quality variables (turbidity (NTU), conductivity (μS/cm) and river level (m)) measured by in situ sensors at Sandy Creek. Bottom panel (d–f): Bi-variate relationships between transformed series (one sided derivative) of turbidity (NTU), conductivity (μS/cm) and river level (m) measured by in situ sensors at Sandy Creek. In each scatter plot, outliers determined by water-quality experts are shown in red, while typical points are shown in black. Neighboring points are marked in green.'
  focal_point: ""
  preview_only: false
projects:
publication: '*Water Resources Research*'
publication_short: ""
publication_types:
- "2"
publishDate: "2019-10-12"
slides: 
summary: This paper develops a method for detecting technical outliers in water-quality data derived from *in situ* sensors.
tags:
- Anomaly Detection
- Outlier Detection
title: A feature‐based procedure for detecting technical outliers in water‐quality data from in situ sensors
url_code: ""
url_dataset: ""
url_pdf: https://agupubs.onlinelibrary.wiley.com/doi/pdf/10.1029/2019WR024906#accessDenialLayout
url_poster: ""
url_project: "https://github.com/pridiltal/oddwater"
url_slides: "https://prital.netlify.com/talks/isf2019/isf2019#1"
url_source: ""
url_video: ""
---

Supplementary notes can be added here, including [code and math](https://github.com/pridiltal/oddwater).
