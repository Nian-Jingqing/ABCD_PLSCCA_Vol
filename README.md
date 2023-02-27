Code and wiki for the manuscript, "Multivariate Analytical Approaches for Investigating Brain-Behavior Relationships"

We used partial least squares canonical (PLSC), canonical correlation analysis, and partial least squares regression (PLSR) to evaluate a relationship between two rich variable sets. A first variable set X was constructed from the cortical and subcortical regional gray matter volumes. A parallel variable set Y was constructed from the psychopathology data for 4 psychopathology dimensions (general psychopathology, conduct problems, internalizing problems, ADHD symptoms). Each method produces 4 components and each component is tested for their statistical significance based on our permutation testing framework.

We have also included a section of the script to construct a heat map of output loading strengths.

main.py constructs the variable sets and runs the analyses for each of the three methods.

The following external libraries are necessary: sklearn, numpy, scipy, nilearn, seaborn
