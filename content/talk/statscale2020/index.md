---
abstract: The first part of the talk introduces a framework that provides early detection of anomalous series within a large collection of nonstationary streaming time-series data. We define an anomaly as an observation that is very unlikely given the recent distribution of a given system. The proposed framework first calculates a boundary for the system???s typical behaviour using extreme value theory. Then a sliding window is used to test for anomalous series within a newly arrived collection of series. The model uses time series features as inputs, and a density-based comparison to detect any significant changes in the distribution of the features. We show that the proposed algorithm can work well in the presence of noisy nonstationarity data within multiple classes of time series.
The HDoutliers algorithm is a powerful unsupervised algorithm for detecting anomalies in high-dimensional data, with a strong theoretical foundation. However, it suffers from some limitations that significantly hinder its performance level, under certain circumstances. The second part of the talkintroduces an algorithm that addresses these limitations. We define an anomaly as an observation where its k-nearest neighbour distance with the maximum gap is significantly different from what we would expect if the distribution of k-nearest neighbours with the maximum gap is in the maximum domain of attraction of the Gumbel distribution. An approach based on extreme value theory is used for the anomalous threshold calculation. Using various synthetic and real datasets, we demonstrate the wide applicability and usefulness of our algorithms. These frameworks are implemented in the open source R packages oddstream and stray.
address:
  city: StatScale, Lancaster University
  country: United Kingdom
  postcode: ""
  region: ""
  street: ""
all_day: false
authors: []
date: "2020-12-04"
date_end: "2020-12-04"
event: 40th International Symposium on Forecasting, Rio de Janeiro, Brazil.
event_url: https://isf.forecasters.org/
featured: false
image:
  caption: ''
  focal_point: Right
links:
- icon: twitter
  icon_pack: fab
  name: Follow
  url: https://twitter.com/pridiltal
location: Virtual ISF 2020, Rio de Janeiro, Brazil
math: true
projects:
- Anomaly Detection
publishDate: "2020-12-04"
slides: 
summary: 40th International Symposium on Forecasting, Rio de Janeiro, Brazil
tags: []
title: Tensor-based anomaly detection in multivariate spatio-temporal data
url_code: ""
url_pdf: ""
url_slides: "talks/statscale2020/statscale2020.html" 
url_video: ""
---


