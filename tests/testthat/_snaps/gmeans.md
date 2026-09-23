# gmeans errors clearly when centers is passed

    Code
      gmeans(matrix(1:20, ncol = 2L), centers = 3L)
    Condition
      Error:
      ! `centers` can't be passed to `gmeans()`, use `k_init` instead

# ad.test works

    Code
      res
    Output
      
      	Anderson-Darling normality test
      
      data:  x
      A = 1.2438, p-value = 0.00293
      

