# Quick way to reproduce a hard-to-reproduce bug in `callNextGeneric`

uniqueMethod and uniqueGeneric are minimalist packages that can be used
to reproduce the bug:

    devtools::install_github("Bioconductor/uniqueMethod")
    devtools::install_github("Bioconductor/uniqueGeneric")

    library(uniqueMethod)
    library(uniqueGeneric)

    setClass("B", slots=c(values="integer"))
    setMethod("unique", "B",
      function(x, incomparables=FALSE, ...) {
        x@values <- unique(x@values, incomparables=incomparables, ...)
        x
      }
    )
    setClass("C", contains="B")
    setMethod("unique", "C",
        function(x, incomparables=FALSE, ...) callNextMethod()
    )
    unique(new("C"))
    #Error in unique.default(x = x) : unique() applies only to vectors

# Details

The `unique()` S4 generic defined in uniqueGeneric is defined with
`setGeneric("unique", signature="x")` so is different from the
`base::unique()` implicit S4 generic defined by uniqueMethod:

    library(uniqueMethod)
    library(uniqueGeneric)

    uniqueMethod::unique
    # standardGeneric for "unique" defined from package "base"
    #
    # function (x, incomparables = FALSE, ...) 
    # standardGeneric("unique")
    # <environment: 0x43e3898>
    # Methods may be defined for arguments: x, incomparables
    # Use  showMethods("unique")  for currently available ones.

    showMethods(uniqueMethod::unique)
    # Function: unique (package base)
    # x="A"
    # x="ANY"

    uniqueGeneric::unique
    # standardGeneric for "unique" defined from package "uniqueGeneric"
    #
    # function (x, incomparables = FALSE, ...) 
    # standardGeneric("unique")
    # <bytecode: 0x46fa718>
    # <environment: 0x46f22a8>
    # Methods may be defined for arguments: x
    # Use  showMethods("unique")  for currently available ones.

    showMethods(uniqueGeneric::unique)
    # Function: unique (package uniqueGeneric)
    # x="ANY"

Since `uniqueGeneric::unique()` masks `uniqueMethod::unique()`, the
following is expected:

    unique(new("A"))
    # Error in unique.default(new("A")) : unique() applies only to vectors

    uniqueMethod:::unique(new("A"))
    # Hi, I'm the uniqueMethod::unique() method for A objects.

Now let's define a method on `uniqueGeneric::unique`. For this we introduce
a new class:

    setClass("B", slots=c(values="integer"))

    setMethod("unique", "B",
      function(x, incomparables=FALSE, ...) {
        x@values <- unique(x@values, incomparables=incomparables, ...)
        x
      }
    )

Note that something is already going wrong with `selectMethod()`:

    showMethods(uniqueGeneric::unique)
    # Function: unique (package uniqueGeneric)
    # x="ANY"
    # x="B"

    selectMethod(uniqueGeneric::unique, "B")  # ok

    showMethods(unique)
    # Function: unique (package uniqueGeneric)
    # x="ANY"
    # x="B"

    selectMethod(unique, "B")  # ok

    showMethods("unique")
    # Function: unique (package uniqueGeneric)
    # x="ANY"
    # x="B"

    selectMethod("unique", "B")  # doesn't find the method!

However, despite this, method dispatch works as expected:

    b <- new("B", values=c(4:1, 1:4))
    unique(b)

But calls to `callNextMethod()` that happen from within a method defined
on the `uniqueGeneric::unique()` generic are broken:

    setClass("C", contains="B")

    setMethod("unique", "C",
        function(x, incomparables=FALSE, ...) callNextMethod()
    )

    showMethods(uniqueMethod::unique)
    # Function: unique (package base)
    # x="A"
    # x="ANY"

    showMethods(uniqueGeneric::unique)
    # Function: unique (package uniqueGeneric)
    # x="ANY"
    # x="B"
    # x="C"

    uniqueGeneric::unique(new("C"))
    # Error in unique.default(x = x) : unique() applies only to vectors

Very surprisingly, the `uniqueMethod::unique()` generic now also seems
to have a method for C objects defined on it:

    showMethods(uniqueMethod::unique)
    # Function: unique (package base)
    # x="A"
    # x="ANY"
    # x="C"

but `selectMethod()` can't find it:

    selectMethod(uniqueMethod::unique, "C")
    # Method Definition (Class "derivedDefaultMethod"):
    #
    # function (x, incomparables = FALSE, ...) 
    # UseMethod("unique")
    # <bytecode: 0x3c7fe60>
    # <environment: namespace:base>
    #
    # Signatures:
    #         x    
    # target  "C"  
    # defined "ANY"

It seems that when we did `uniqueGeneric::unique(new("C"))`, the method was
was found but `callNextMethod()` went looking for the next method in the
wrong generic:

    setClass("D", contains="A")

    setMethod(uniqueGeneric::unique, "D",
        function(x, incomparables=FALSE, ...) callNextMethod()
    )

    showMethods(uniqueMethod::unique)
    # Function: unique (package base)
    # x="A"
    # x="ANY"
    # x="C"

    showMethods(uniqueGeneric::unique)
    # Function: unique (package uniqueGeneric)
    # x="ANY"
    # x="B"
    # x="C"
    # x="D"

    uniqueGeneric::unique(new("D"))
    # Hi, I'm the uniqueMethod::unique() method for A objects.
    # NULL

