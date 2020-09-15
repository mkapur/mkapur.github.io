---
title: "Sablefish Operating Model Overview"
subtitle:  "UW SAFS Think Tank"
date: "26 Oct 2020"
author: "M Sosa Kapur kapurm@uw.edu"
output:
  xaringan::moon_reader:
    lib_dir: libs
    css: [rstudioconf-xaringan-themer.css]
    nature:
      beforeInit: "https://platform.twitter.com/widgets.js"
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
---






layout: true

.header[<!--html_preserve--><i class="fab  fa-github "></i><!--/html_preserve--> @mkapur/sab-mse]

---
# Objectives

+ Provide overview of Operating Model design (beta, obviously)
+ Discuss assumptions for key model features

--

##  Outline
+ General model framework
+ Walk-through details from document  <b>  [link me to OM doc on gDrive!] </b>
+ Discuss as we go (interrupt me :) )


---

# Ground Floor

.pull-left[
+ OM is written in `R`

+ Conditioning +/- estimation happens in `TMB`
+ We are <b>not</b> shooting to replicate Stock Synthesis (etc.) exactly 
]

.pull-right[
![](/_presentations/img/groundfloor.gif)
]
---




# Spatial Structure
+ A little more complicated than expected

![](/_presentations/img/Fig1_strata_panel.png)

--

+ Let's walk through how these came about

---

# Spatial Structure









