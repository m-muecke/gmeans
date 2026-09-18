# print.summary.gmeans prints the summary

    Code
      print(summary(cl))
    Output
      G-means clustering with 7 clusters (k_init = 2, k_max = 10, level = 0.05)
      
       cluster size withinss Sepal.Length Sepal.Width Petal.Length Petal.Width
             1   36   11.454        6.247       2.847        4.775       1.575
             2    7    1.263        5.243       2.371        3.443       1.029
             3   21    4.177        5.629       2.724        4.133       1.295
             4   50   15.151        5.006       3.428        1.462       0.246
             5   12    4.655        7.475       3.125        6.300       2.050
             6   18    3.052        6.611       3.000        5.506       2.083
             7    6    1.230        6.283       3.233        5.517       2.400
      
      Total SS: 681.4, within SS: 40.98, between SS: 640.4 (93.99% of total)

# print.summary.gmeans omits the ratio when the total sum of squares is zero

    Code
      print(summary(cl))
    Output
      G-means clustering with 1 cluster (k_init = 1, k_max = 10, level = 0.05)
      
       cluster size withinss x1 x2
             1   20        0  1  1
      
      Total SS: 0, within SS: 0, between SS: 0

