# print.summary.gmeans prints the summary

    Code
      print(summary(cl))
    Output
      G-means clustering with 2 clusters (k_init = 1, k_max = 10, level = 1e-04)
      
       cluster size withinss Sepal.Length Sepal.Width Petal.Length Petal.Width
             1   97   123.80        6.301       2.887        4.959      1.6959
             2   53    28.55        5.006       3.370        1.560      0.2906
      
      Total SS: 681.4, within SS: 152.3, between SS: 529 (77.64% of total)

# print.summary.gmeans omits the ratio when the total sum of squares is zero

    Code
      print(summary(cl))
    Output
      G-means clustering with 1 cluster (k_init = 1, k_max = 10, level = 1e-04)
      
       cluster size withinss x1 x2
             1   20        0  1  1
      
      Total SS: 0, within SS: 0, between SS: 0

