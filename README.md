# Scala Interpreters
> Collection of different interpreters to study the concepts of programming languages. Repository is used to 
prepare for the exam ["Concepts of programming languages"](http://www.stg.tu-darmstadt.de/teaching/courses/ss_2015/ss_2015_copl/organizational/organization.en.jsp) 
at TU Darmstadt.

## Content

 1. Package V1: **Introduction**
    * **WAEInterp**: Interpreter that supports *With* constructs and *Arithmetic Expressions*. Showing the 
    difference between eager substitution and lazy substitution. 
      
 2. Package V2: **Taxonomy of Functions, Static vs. Dynamic Scoping**
    * **F1WAEImmediateSubstInterp**: *First-Order* interpreter with *Arithmetic Expressions* and *With* constructs using eager substitution.
    * **F1WAEDynamicInterp**: *First-Order* interpreter that introduces the concept of environments and showing its advantage 
    over substitution. The interpreter adapts *dynamic scoping*.
    * **F1WAEDynamicInterp**: *First-Order* interpreter using *static scoping*.
    
    ----------------------------
    
    * **FWAEImmediateSubstInterp**: *First-class* interpreter with *Arithmetic Expressions* and *With* constructs using *lazy substitution*.
    * **FWAEDynamicInterp**: *First-class* dynamically scoped interpreter with *Arithmetic Expressions* and *With* constructs using environments.
    * **FWAEStaticInterp**: *First-class* interpreter with *Arithmetic Expressions* and *With* constructs using environments. To enforce 
    *static scoping* *closures* are introduced as a basic concept. 
    
    -----------------------------
    
    * **DRCFAEInterp**: *First-class* interpreter with algebraic data type support.
    
 3. Package V3: **Recursion**
    * **RCFWAEInterp**: *First-class* statically scoped interpreter with conditionals and recursion.
     
 4. Package V4: **Stateful languages**
    * **SCFWAEInterp**: Stateful interpreter with sequencing, values and box stores. To preserve static scoping an additional store is introduced. 
    This concept is called *store-passing style*. Function application is implemented using *call-by-value* style. 
    * **RSCFWAEInterp**: Stateful interpreter with recursion support.
    
 5. Package V5: **Memory management**
    * **RefCount_SRCFWAEInterp**: Stateful interpreter with a reference counting store.
    * **GC_SRCFWAEInterp**: Stateful interpreter with mark and sweep garbage collection.
 6. Package V6: **OO Interpreter**
    * **OOInterp**: First-order classes interpreter with predefined classes and local variables in method.
    * **OOWithInheritanceInterp**: First-order classes interpreter with additional inheritance support. 
    
## Types of Interpreters

* **Syntactic Interpreters**: Uses the *interpreting language* only for the purpose of representing terms of the interpreted language.
    These kind of interpreters implement all the corresponding behaviour explicitly e.g *RCFWAEInterp* for recursion.
    
* **Metainterpreter**: Uses features of the interpreting language to directly implement behaviour of the interpreted language. This
    would be the case, if we implement functions with Scala functions. 
    
* **Metacircular interpreter**: A meta interpreter in which the interpreting and interpreted language are the same. If the interpreted and
    interpreter language match closely, a meta interpreter can be easy to write. However, if they don't match it could be hard e.g
    Scala is statically scoped and trying to implement dynamic scoping wouldn't be easy. 

