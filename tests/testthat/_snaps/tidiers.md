# tidy names unnamed centers and accepts col.names

    Code
      generics::tidy(cl, col.names = "a")
    Condition
      Error in `tidy.gmeans()`:
      ! length(col.names) == ncol(centers) is not TRUE

# augment adds cluster assignments

    Code
      generics::augment(cl, x[1:10, ])
    Condition
      Error:
      ! `data` must have one row per observation used to fit the model

---

    Code
      generics::augment(cl, as.vector(x))
    Condition
      Error:
      ! `data` must be a matrix or data frame

