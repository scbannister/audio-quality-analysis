# Audio Quality Analysis - Cadenza CAD1 Challenge (https://cadenzachallenge.org/docs/cadenza1/cc1_summary_task1)
Example code for analysis of perceptual attribute ratings and audio signal features as part of the Cadenza CAD1 Challenge.

This repo contains an R script which imports a dataset containing perceptual attribute ratings for audio quality from participants with hearing impairment, and audio signal features of music samples processed by machine learning systems for those listeners. 

Basic descriptive analyses and visualisations are generated, and linear mixed effects models are developed to answer two key questions:

1) How do participant ratings of basic audio quality (BAQ) differ across the machine learning systems, across different hearing impairment severities, and in relation to any interactions between machine learning system and hearing impairment severity?

2) How do ratings of BAQ relate to audio signal features of the music samples, and are there patterns in the signal features depending on the machine learning systems and how they process music for different hearing impairment severities?

Alongside this R code, there is an R markdown file and corresponding .html document, to give an immediate sense of the outputs from the analysis.

The dataset used in the analysis is publicly available in the following repository: https://zenodo.org/records/13271525.
