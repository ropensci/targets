---
title: 'The targets R package: a dynamic Make-like function-oriented pipeline toolkit for reproducibility and high-performance computing'
tags:
- R
- reproducibility
- high-performance computing
- pipeline
- workflow
- Make
date: "12 January 2021"
output: pdf_document
authors:
- name: William Michael Landau
  orcid: 0000-0003-1878-3253
  email: will.landau@gmail.com
  affiliation: 1
bibliography: paper.bib
affiliations:
- name: Eli Lilly and Company
  index: 1
---

# Summary

The [`targets`](https://github.com/ropensci/targets) R package [@targets] is a pipeline toolkit for computationally intense reproducible research. It reduces the time and effort required to develop a data analysis project and maintain a trustworthy set of results. [`targets`](https://github.com/ropensci/targets) uses static code analysis to detect dependency relationships among interconnected computational tasks and construct a directed acyclic graph (DAG), which researchers can visualize in order to understand and communicate the structure of a complicated workflow. To run the pipeline at scale, [`targets`](https://github.com/ropensci/targets) leverages implicit parallel computing and optional cloud storage. In subsequent runs, [`targets`](https://github.com/ropensci/targets), skips tasks that are already synchronized with their upstream dependencies, which not only reduces the runtime of rapidly developing workflows, but also provides tangible evidence of reproducibility.

In high-performance computing scenarios, [`targets`](https://github.com/ropensci/targets) uses its DAG to discern which targets can run concurrently and which targets are still waiting for other upstream targets to finish processing. As soon as a target's dependency requirements are met, the target is deployed to the next available parallel worker. Internally, [`targets`](https://github.com/ropensci/targets) leverages the [`clustermq`](https://github.com/mschubert/clustermq) package [@clustermq] for persistent workers and the  [`future`](https://github.com/HenrikBengtsson/future) package [@future] for transient workers. Both [`clustermq`](https://github.com/mschubert/clustermq) and [`future`](https://github.com/HenrikBengtsson/future) are powerful and versatile frameworks capable of submitting R workloads not only to multiple cores on a single machine, but also to popular resource managers on shared computing clusters.

[`targets`](https://github.com/ropensci/targets) is the successor to [`drake`](https://github.com/ropensci/drake) [@drake], which in turn originated from [`remake`](https://github.com/richfitz/remake) [@remake], an R package modeled after GNU Make [@Make]. Unlike Make, [`targets`](https://github.com/ropensci/targets) and [`drake`](https://github.com/ropensci/drake) and [`remake`](https://github.com/richfitz/remake) focus on the R language, encourage an idiomatic function-oriented style of programming, and abstract each target as an R object. Relative to [`remake`](https://github.com/richfitz/remake) and  [`drake`](https://github.com/ropensci/drake), [`targets`](https://github.com/ropensci/targets) is friendlier and more efficient, surpassing the permanent architectural limitations of both predecessors. The data storage system of [`targets`](https://github.com/ropensci/targets) is lighter and more transparent, which helps users diagnose issues, move projects to different file systems, work with multiple contributors, and leverage seamless [Metaflow](https://github.com/Netflix/metaflow)-like cloud storage integration [@metaflow]. In addition,  [`targets`](https://github.com/ropensci/targets) supports stronger user-side guardrails, more introspective dependency graph visualizations, parallel efficient dynamic branching, and an interface more amenable to metaprogramming and third-party extensions.

# References
