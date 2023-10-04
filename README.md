# Automated Gating

##### Description

The Automated Gating operator is based on the openCyto R package.

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, measurement value of the first channel
`x-axis`        | numeric, optional, measurement value of the second channel 
`column`        | factor, optional, sample / file identifier

Output data|.
---|---
`Gates`       | 0 / 1 flag per gate

##### Details

The operator is a wrapper of the `gs_add_gating_method()` function from the
`OpenCyto` R/Bioconductor package.
