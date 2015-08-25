# Scala Interpreters
> Collection of different interpreters to study the concepts of programming languages

## Content

 1. **Package V1**: Introduction
    * **WAEInterp**: Interpreter that supports *With* constructs and *Arithmetic Expressions*. Showing the 
    difference between eager substitution and lazy substitution. 
      
 2. **Package V2**: Taxonomy of Functions, Static vs. Dynamic Scoping
    * **F1WAEImmediateSubstInterp**: *First-Order* interpreter with *Arithmetic Expressions* and *With* constructs using eager substitution.
    * **F1WAEDynamicInterp**: *First-Order* interpreter that introduces the concept of environments and showing its advantage 
    over substitution. The interpreter adapts *dynamic scoping*.
    * **F1WAEDynamicInterp**: *First-Order* interpreter using *static scoping*.
    
    ----------------------------
    
    * **FWAEImmediateSubstInterp**: *First-class* interpreter with *Arithmetic Expressions* and *With* constructs using *lazy substitution*
    * **FWAEDynamicInterp**: *First-class* dynamic scope interpreter with *Arithmetic Expressions* and *With* constructs using environments.
    * **FWAEStaticInterp**: *First-class* interpreter with *Arithmetic Expressions* and *With* constructs using environments. To enforce 
    *static scoping* *closures* are introduced as a basic concept. 
