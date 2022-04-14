# OpenCyto one-step operator

##### Description

The `OpenCyto one-step operator` 

##### Usage

Input projection|.
---|---
`y-axis`        | numeric, measurement value 
`row`           | factor, channel to be used for gating
`column`        | factor, observation

Input parameters|.
---|---
`input_var`        | parameter description

Output relations|.
---|---
`flag`        | pass / fail flag, per column

##### Details

The operator is a wrapper of the `gs_add_gating_method()` function from the
`OpenCyto` R/Bioconductor package.
