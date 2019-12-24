---
title: "4_anova_longitud_Paper2"
author: "Aniko Maraz"
date: "November 12, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
`names(data_final_long_scales_viz_m)

with(data = data_final_long_scales_viz_m, interaction.plot(time, Group_affect, gen_impr))



trial.aov <- aov(gen_impr ~ time  * Group_affect + Error(session), 
                 data=data_final_long_scales_viz_m)

summary(trial.aov)

data_final_long_scales_viz_m$gen_impr[data_final_long_scales_viz_m$time == "1"]


table(data_final_long_scales_viz_m$time, data_final_long_scales_viz_m$gen_impr)

names(data_final_long_scales_viz_m)
