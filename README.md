# Scala Interpreters
> Collection of different interpreters to study the concepts of programming languages. Repository is used to 
prepare for the exam ["Concepts of programming languages"](http://www.stg.tu-darmstadt.de/teaching/courses/ss_2015/ss_2015_copl/organizational/organization.en.jsp) 
at TU Darmstadt.

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
    
    * **FWAEImmediateSubstInterp**: *First-class* interpreter with *Arithmetic Expressions* and *With* constructs using *lazy substitution*.
    * **FWAEDynamicInterp**: *First-class* dynamically scoped interpreter with *Arithmetic Expressions* and *With* constructs using environments.
    * **FWAEStaticInterp**: *First-class* interpreter with *Arithmetic Expressions* and *With* constructs using environments. To enforce 
    *static scoping* *closures* are introduced as a basic concept. 
 3. **Package V3** Recursion
    * **RCFWAEInterp**: *First-class* statically scoped interpreter with conditionals and recursion. 
    
## Types of Interpreters

* **Syntactic Interpreters**: Uses the *interpreting language* only for the purpose of representing terms of the interpreted language.
    These kind of interpreters implement all the corresponding behaviour explicitly e.g *RCFWAEInterp* for recursion.
    
* **Metainterpreter**: Uses features of the interpreting language to directly implement behaviour of the interpreted language. This
    would be the case, if we implement functions with Scala functions. 
    
* **Metacircular interpreter**: A meta interpreter in which the interpreting and interpreted language are the same. If the interpreted and
    interpreter language match closely, a meta interpreter can be easy to write. However is they don't match it could be hard e.g
    Scala is statically scoped and trying to implement dynamic scoping. 

