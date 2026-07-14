# ADR-07: Treatment of Gibson's classifications

**Status**: Accepted
**Date**: 2026-07-13

## Context

Gibson's dataset includes a `Gibson Classification` column with values: `Petra`, `Between`, `Mecca`, `Parallel`, `Jerusalem`, `Unknown`. These represent Gibson's own interpretation of each mosque's intended qibla destination. They are the primary contested claim in the academic debate this package is designed to address.

The risk is that if these labels leak into the analytical pipeline — particularly into unsupervised learning — results will appear to confirm Gibson's hypothesis by construction.

The design doc is unambiguous: "The unsupervised workflow must not use Gibson's Petra, Mecca, 'between,' or other destination labels when fitting clusters" and "never use them as labels when fitting unsupervised models."

## Decision

1. **Preserve as data**: `gibson_classification` is retained in `gibson_qibla` exactly as supplied (after case standardisation). The original value is also retained in `gibson_classification_source`. Neither column is dropped or hidden.

2. **Label as hypothesis, not ground truth**: All documentation, vignettes, reports, and function help pages refer to `gibson_classification` as "Gibson's classification" or "Gibson's hypothesis", never as the "correct" or "true" classification.

3. **Exclude from unsupervised model fitting**: `cluster_qiblas()` and all related clustering functions explicitly exclude `gibson_classification` and `gibson_classification_source` from the feature set, with no option to include them as input features. They may be used only for post-hoc external validation after fitting.

4. **External validation only**: When computing cluster purity, ARI, NMI, or other agreement metrics against Gibson's labels, these are labelled "external validation against Gibson's classification" in all outputs.

5. **No privileged destination**: The package's shipped destination table lists Mecca, Petra, and Jerusalem at equal status. No destination is loaded first, ordered first in output, or treated as the null hypothesis in any test unless the analyst explicitly specifies it.

## Rationale

- Gibson's hypothesis is the object of study, not an input assumption. Using it to fit models would be circular.
- Preserving the column allows analysts to evaluate how well data-driven methods agree or disagree with Gibson's interpretation, which is a legitimate and important question.
- Making the neutral stance explicit in code (exclusion from feature sets) rather than just documentation provides an enforceable guarantee.

## Consequences

- `cluster_qiblas()` raises an error if `gibson_classification` appears in the `features` argument.
- Post-hoc agreement functions clearly label Gibson's labels as one external reference among others (dynasty, period, region).
- The package takes no position on whether Gibson is right or wrong; it provides the tools to test his claims rigorously.
