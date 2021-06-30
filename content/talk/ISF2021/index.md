---
abstract: Accurate load forecasting is vital for effective energy management, as forecast underestimation can result in blackouts, whereas overestimation may result in energy wastage. However, the quality of historical data for load forecasting can be affected in several ways, such as data integrity attacks,  missing values, incorrect readings,  technical aberrations. These issues make data unreliable and untrustworthy and can have a direct impact on forecast accuracy and subsequent decision making. This work develops a framework for detecting anomalies in tidy time series data. An anomaly is defined as an observation that is predicted as very unlikely given the robust time series forecast models. The algorithm works with tidy temporal data provided by the `tsibble` package and produces an  `outstable`, a tsibble with flagged anomalies and their degree of outlierness. An approach based on extreme value theory is applied to residual series in order to calculate a data-driven anomalous threshold. The proposed framework can also provide a cleansed tsibble that closely integrates with the tidy forecasting workflow used in the `fable` package.  A number of different approaches are available for the data cleansing process. The wide applicability and usefulness of this proposed framework in load forecasting will be demonstrated using various synthetic, real-world, and publicly available benchmark datasets including data from Global Energy Forecasting Competitions.  This framework is implemented in the open-source R package `outstable`. 
address:
  city: Virtual Conference
  country: ""
  postcode: ""
  region: ""
  street: ""
all_day: false
authors: []
date: "2021-06-30"
date_end: "2021-06-30"
event: 41st International Symposium on Forecasting, Virtual Conference.
event_url: https://forecasters.org/events/symposium-on-forecasting/
featured: false
image:
  caption: ''
  focal_point: Right
links:
- icon: twitter
  icon_pack: fab
  name: Follow
  url: https://twitter.com/pridiltal
location: Virtual ISF 2021
math: true
projects:
- Anomaly Detection
publishDate: "2021-06-30"
slides: 
summary: 41st International Symposium on Forecasting
tags: []
title: Tidy Time Series Anomaly Detection for Load Forecasting
url_code: ""
url_pdf: ""
url_slides: "talks/ISF2021/ISF-2021.html" 
url_video: ""
---


