---
layout: post
title: Spatial statistics and PageRank
modified: 3/29/2020, 9:20:12
excerpt: "Links between a common statistical model and Google's famous algorithm"
tags: [spatial, pagerank]
comments: true
category: blog
---

This is a *blog*. *Blogs* can be used to write about *things*.
# Network analysis and economics
I have recently been spending a lot of time working with spatial econometric models. For those unfamiliar, spatial econometrics is a growing field that focus on modeling spatial dependence between observations. While early work in this field focused on spatial dependence through traditional measures of geogrpahic proximity, researchers performing cross-country analyses have increasingly put an emphasis on the notion of *economic proximity*, which is typically measured through bilateral trade flows.

There a lot of attractive features to this modeling approach (though there are plenty of issues as well...) -- one of its biggest advantages is that it allows for a straightforward calculation of spillovers. In this framwork, shocks incurred by one country are transferred to its immediate trading partners, and then to those countries' trading partners, and so on, so that shocks propogate through countries by way of the observed trading network. This is pretty cool because it can elucidate trends that would otherwise be difficult to see through the naked eye. 

While I have outlined it here, there has already been plenty of dicussion on the use of trade weights in measuring cross-country spillovers. Interestingly, within the spatial econometrics framework using trade as a spatial weight, researchers rarely call attention to the fact that global trade is a *network*. There is a real opportunity to combine the methods used in network analysis (NA), another long popular field in other discliplines that is now gaining traction in economics, with spatial econometrics. 

One of the main contributions of NA is the development of various criteria for measuring *network centrality* -- how important each particular agent (i.e. node) is within a network. I will not get into all of these in this post, but will instead focus on the measure of *Katz centrality*. Suppose one estimates a simple spatial autoregressive (SAR) model with trade weights $$W$$, hoping to quantify the spillover effects on shocks to GDP ($y$) in various countries:

$y = \rho W y + \beta X + \varepsilon$. 

We can write this in reduced-form:
$y = (I-\rho W)^{-1}X + u$ (where $u=(I-\rho W)^{-1}\varepsilon$)



