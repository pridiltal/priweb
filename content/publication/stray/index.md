---
abstract: The HDoutliers algorithm is a powerful unsupervised algorithm for detecting anomalies in high-dimensional data, with a strong theoretical foundation. However, it suffers from some limitations that significantly hinder its performance level, under certain circumstances. In this article, we propose an algorithm that addresses these limitations. We define an anomaly as an observation that deviates markedly from the majority with a large distance gap. An approach based on extreme value theory is used for the anomalous threshold calculation. Using various synthetic and real datasets, we demonstrate the wide applicability and usefulness of our algorithm, which we call the stray algorithm. We also demonstrate how this algorithm can assist in detecting anomalies present in other data structures using feature engineering. We show the situations where the stray algorithm outperforms the HDoutliers algorithm both in accuracy and computational time. This framework is implemented in the open source R package stray
authors:
- admin
- Rob J. Hyndman
- Kate Smith-Miles
date: "2019-08-12"
doi: ""
featured: false
image:
  caption: 'Scatterplots of hourly pedestrian counts at 43 locations in the city Melbourne, Australia, from 1 December to 31 December 2018. Anomalous days detected by the stray algorithm using scagnostics are marked in red colour'
  focal_point: ""
  preview_only: false
projects:
- anomaly-detection
publication: ""
publication_short: ""
publication_types:
- "3"
publishDate: "2019-08-12"
slides: 
summary: The algorithm, stray, which is specially designed for high-dimensional data, addresses the limitations of the state-of-art-method, the HDoutliers algorithm. 
tags:
- Anomaly Detection
- High-dimensional data,
title: Anomaly Detection in High Dimensional Data
url_code: https://github.com/pridiltal/stray
url_dataset: https://github.com/pridiltal/stray
url_pdf: https://www.monash.edu/business/ebs/research/publications/ebs/wp20-2019.pdf
url_poster: ''
url_project: https://github.com/pridiltal/stray
url_slides: "https://prital.netlify.com/talks/user2018/user2018#1" 
url_source: ''
url_video: ''
---
Supplementary notes can be added here, including [code and math](https://github.com/pridiltal/stray).
