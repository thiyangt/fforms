
Let $\mathcal{P}=\{(\mathbf{f_i}, z_i)\}_{i=1}^{N}$ be the
historical data set we use to train a classifier. Consider a
$d$-dimensional feature vector $\mathbf{F}=(F_1, F_2, ..., F_d)$ and a dependent
variable, the best forecasting method for each series $Z$. Let $\mathcal{G}$ be the unknown relationship between $\mathbf{F}$ and
$Z$. @zhao2019causal call this 'law of nature'. Inside the FFORMS framework, the random forest algorithm tries to learn this relationship using
the historical data we provided. We denote the predicted function as
$g$. The second  objective of this paper is to explore the nature of the relationship between features and forecast model selection learned by the FFORMS framework. In the following subsections, we provide a description of tools we use to explore what is happening under the hood of the FFORMS framework.  


## Visualize patterns learned by the meta-learner

**The out-of-bag (OOB) observations:** A useful by-product of random forest model is OOB observations. OOB observations are the observations not included in building a given tree. In general, each tree uses only around one third of observations in its construction and is grown based on different bootstrap samples. Hence, each tree has a different set of OOB observations.  

**The vote matrix:** The vote matrix ($N \times P$; $N$ is the total number of observations; $P$ is the number of classes) contains one row for each observation and one column for each class label. Each cell contains the fraction of trees in  the forest that classified each observation to each class. 

We use a vote matrix calculated based on OOBs to visualise patterns learned by the random forest. The goal is to visualise how well the model captures the the intuitive similarities between class labels. For example, if the best forecast model identified for a time series is random walk with drift, then ARIMA, and ETS models with trend are also likely to assign higher votes while ARMA and white noise process assign low values for votes. The underlying results are shown in \autoref{oobviz}.


## Feature importance


@jiang2002 explain variable importance under three different views: i) causality (change in the value of $Z$ for an increase or decrease in the value of $F_i$), ii) contribution of $F_i$ based on out-of-sample prediction accuracy and iii) face value of $F_i$ on prediction function $g$. For example, in linear regression models estimated coefficients of each predictor can be considered a measure of variable importance. See @jiang2002 for comparable face value interpretation for machine learning models. In this paper we use the first two notions of variable importance. Partial dependency functions and individual conditional expectation curves are used to explore the 'causality' notion of variable importance, while permutation-based variable importance measure is used to capture the second notion of variable importance (feature contribution to the predictive accuracy) [@zhao2019causal]. We introduce each of these variable importance measures below.  


### Permutation-based variable importance measure

The permutation-based variable importance introduced by @breiman2001random measures the prediction
strength of each feature. This measure is calculated based on the OOB observations. The calculation of variable importance is formalised as follows: Let $\bar{\mathcal{B}}^{(k)}$ be the OOB sample for a tree $k$, with $k\in \{1,...,ntree\}$, where $ntree$ is the number of trees in the random forest. Then the variable importance of variable $F_{j}$ in $k^{th}$ tree is:
  \[VI^{(k)}(F_{j})=\frac{\sum_{i\in \bar{\mathcal{B}}^{(k)}}I(\gamma_{i}=\gamma_{i,\pi_{j}}^{k})}{|\bar{\mathcal{B}}^{(k)}|}-\frac{\sum_{i\in \bar{\mathcal{B}}^{(k)}}I(\gamma_{i}=\gamma_{i}^{k})}{|\bar{\mathcal{B}}^{(k)}|},\]
where $\gamma_{i}^{k}$ denotes the predicted class for the $i^{th}$ observation before permuting the values of $F_{j}$ and $\gamma_{i, \pi_{j}}^{k}$ is the predicted class for the $i^{th}$ observation after permuting the values of $F_{j}$. The overall variable importance score is calculated as:
  \[VI(F_{j})=\frac{\sum_{k=1}^{ntree}VI^{(k)}(F_{j})}{ntree}.\]

Permutation-based variable importance measures provide a useful starting point for identifying the relative influence of features on the predicted outcome.  However, they provide little indication of the nature of the relationship between the features and model outcome. To gain further insights into the role of features inside the FFORMS framework we use the partial dependence plot (PDP) introduced by @friedman2008predictive. 


### Partial dependence plot (PDP) and variable importance measure based on PDP

Partial dependence plots can be used to graphically examine how each feature is related to the model prediction while accounting for the average effect of other features in the model. Let $F_s$ be the subset of features we want to examine for partial dependencies and $F_c$ be the remaining set of features in $F$.  Then $g_s$, the partial dependence function on $F_s$ is defined as 
\[g_s(F_s)=E_{f_c}[g(f_s, F_c)]=\int{g(f_s, f_c)dP(f_c).}\] 
In practice, PDP can be estimated from a training data set as 
\[\bar{g_s}(f_s)=\frac{1}{n}\sum_{i=1}^{n}g(f_s, F_{iC}),\]
where $n$ is the number of observations. A partial dependency curve can be created by plotting the pairs of $\{(f_s^k, \bar{g}_s(f_{sk}))\}_{k=1}^{m}$ defined on a grid of points $\{f_{s1}, f_{s2},\dots, f_{sm}\}$ based on $F_s$. The FFORMS framework has treated the forecast model selection problem as a classification problem. Hence, in this paper partial dependency functions display the probability of a certain class occurring given different values of the feature $F_s$.


@Greenwell2018 introduce a variable importance measure based on the partial dependency curves. The idea is to measure the 'flatness' of partial dependence curves for each feature. A feature with a PDP curve that is flat, relative to the other features, does not have much influence on the predicted value. The flatness of the curve is measured using the standard deviation of the values $\{\bar{g}_{s}(f_{sk})\}_{k=1}^{m}$.

### Individual conditional expectation (ICE) curves and variable importance measure based on ICE curves

While partial dependency curves are useful in understanding the estimated relationship between features and the predicted outcome in the presence of substantial interaction between features, they can be misleading.  @goldstein2015peeking propose individual conditional expectation (ICE) curves to address this issue. Instead of
averaging $g(f_s, F_{iC})$ over all observations in the training data, an ICE curve generates the individual response curves by plotting the pairs $\{(f_s^k, g(f_{sk}, F_{iC}))\}_{k=1}^{m}$ defined on grid of points $\{f_{s1}, f_{s2},\dots, f_{sm}\}$ based on $F_s$. In other words, the partial dependency curve is simply the average of all the ICE curves. 


This method is similar to the PDP-based variable importance scores above, but is based on measuring the 'flatness' of the individual conditional expectation curves. We calculate standard deviations of each ICE curve.  We then compute an ICE-based variable importance score, which is simply the average of all the standard deviations. A higher value indicates a higher degree of interactivity with other features. 

### Ranking of features based on feature importance measures

To identify important class-specific features we rank features in three different ways: i) based on permutation-based variable importance, ii) based on partial dependence functions and iii) based on ICE curves. We consider 25 features for yearly data. The feature that shows the highest importance is ranked 25, the second highest is ranked 24, and so on. Finally, for each feature a mean rank is calculated based on the rankings of the three measures. 

## Relationship between most important features and the choice of forecast model selection 

The partial-dependence curves, along with their confidence intervals, are used to visualise the relationship between top features and the choice of forecast model selection.
