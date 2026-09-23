# gmeans errors clearly when centers is passed

    Code
      gmeans(matrix(1:20, ncol = 2L), centers = 3L)
    Condition
      Error:
      ! `centers` can't be passed to `gmeans()`, use `k_init` instead

# gmeans errors clearly when k_init is not less than the number of rows

    Code
      gmeans(matrix(1:4, ncol = 2L), k_init = 2L)
    Condition
      Error:
      ! `k_init` must be less than the number of rows in `x`

---

    Code
      gmeans(matrix(1:10, ncol = 2L), k_init = 6L, k_max = 6L)
    Condition
      Error:
      ! `k_init` must be less than the number of rows in `x`

# ad.test works

    Code
      res
    Output
      
      	Anderson-Darling normality test
      
      data:  x
      A = 1.2438, p-value = 0.00293
      

