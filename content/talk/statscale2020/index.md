---
abstract: Due to rapid progress in hardware technology, it has become possible for many sensors to capture multiple parameters or multiple measurements simultaneously, which ultimately leads to multivariate spatio-temporal data. This work develops a framework for detecting anomalies in data with tensor (multiway) structure which make traditional matrix-based spectral methods for anomaly detection inadequate for such data. An anomaly is defined as an observation that is very unlikely given the forecast distribution for the corresponding time period. This work extends the previous oddstream framework for one-dimensional multivariate streaming data to multidimensional multivariate streaming data context using tensor analysis. Identi- fication of locations or time periods related to anomalous behaviours using all the information obtained from the multiple measurements is the main goal of the appli- cations relate to the topic. This work makes two fundamental contributions. First, it proposes a framework that provides early detection of anomalies in multivariate spatio-temporal data. The proposed framework first derives a feature space from multivariate spatio-temporal data using tensor decomposition. Then it forecasts a boundary for the systems??? typical behavior. A sliding window method is then used to test for anomalous series within the newly arrived collection of multivariate series. An approach based on extreme value theory is used for the typical boundary predic- tion process. Second, it proposes a method to deal with class overlapping problem which in turn allows to detect potential anomalies at their early stages. The wide applicability and usefulness of this proposed framework will be demonstrated using various synthetic and real world datasets. This framework is implemented in the open source R package mask. We show that the proposed algorithm can work well in the presence of noisy non-stationarity data within multiple classes of time series with class imbalance and class overlapping problems.
address:
  city: Rio de Janeiro
  country: Brazil
  postcode: ""
  region: ""
  street: ""
all_day: false
authors: []
date: "2020-12-04"
date_end: "2020-12-04"
event: StatScale Seminar
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
title: Anomaly Detection in Streaming Time Series Data
url_code: ""
url_pdf: ""
url_slides: "talks/statscale2020/statscale2020.html" 
url_video: ""
---


