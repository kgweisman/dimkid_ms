# Developing representations of mental life: Changes in conceptual structure between 4-9 years among US children

## Kara Weisman ([website](http://kgweisman.github.io/)), Carol S. Dweck, & Ellen M. Markman

## Abstract

Attributions of thoughts, beliefs, desires, intentions, emotions, perceptions, and sensations are at the core of human social life—but “mental life” is a complex concept, encompassing a wide range of experiences and abilities that vary along many dimensions. This makes ordinary people’s representations of mental life a fascinating case study of abstract reasoning and its development: How do children come to represent this complex conceptual space? In this paper we describe a series of studies designed to explore this question among US children (4-9y) and adults, using an empirical approach that unites recent work on the “dimensions of mind perception” with rich traditions of research on the development of the animate-inanimate distinction, lay biology and psychology, and theory of mind. These studies address three ontological questions about ordinary people’s representations of mental life: (1) What are the conceptual units that anchor representations of mental life at different points in development? (2) How are these conceptual units organized in relation to each other, and how does this organization change over development? and (3) How do people of different ages deploy their conceptual representations of mental life to reason about specific entities in the world—namely, animate beings vs. inanimate objects? Results suggest that, over the course of early and middle childhood, US children’s representations of mental life undergo substantial development in all three of these respects. These findings have important implications for children’s social cognitive development.

## Guide to repo

This repo will include all analyses and fully reproducible manuscript files for the paper and supplemental materials.

### Data

Datasets will be available [here](https://github.com/kgweisman/mental-life-conceptual-dev/tree/master/data/deidentified). Scripts for cleaning and anonymization wil be available [here](https://github.com/kgweisman/mental-life-conceptual-dev/tree/master/code).

### Analysis

Analysis scripts will be embedded in the fully reproducible manuscripts for the supplemental materials ([here](https://github.com/kgweisman/mental-life-conceptual-dev/blob/master/supplement/supplement-main.Rmd)) and the main text of the paper ([here](https://github.com/kgweisman/mental-life-conceptual-dev/blob/master/paper/paper.Rmd)).

### Paper

The paper will be available as a PDF [here](https://github.com/kgweisman/mental-life-conceptual-dev/blob/master/paper/paper.pdf), and the supplemental materials [here](https://github.com/kgweisman/mental-life-conceptual-dev/blob/master/supplement/supplement-main.pdf).

### Figures

All figures related to this project will be available [here](https://github.com/kgweisman/mental-life-conceptual-dev/blob/master/outputs/). The key plots presented in the main text of the paper will be displayed below.

## Programming environment

All analyses were conducted in R (version 4.1.0); platform: aarch64-apple-darwin20 (64-bit); running under: macOS Big Sur 11.4.

The analyses were built using the following packages:

- tidyverse (version 1.3.1) 
- psych (version 2.1.6)
- mgcv (version 1.8-36)
- lme4 (version 1.1-27.1)
- parameters (version 0.14.0)
- kableExtra (version 1.3.4)
- cowplot (version 1.1.1)
- langcog (version 0.1.9001; available at https://github.com/langcog/langcog-package)
- knitr (version 1.33)
