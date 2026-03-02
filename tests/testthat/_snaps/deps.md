# pkg_dependencies works as expected for local package

    Code
      local_deps
    Output
         package     type       name   op version
      1  checked  Depends          R   >= 3, 6, 2
      2  checked  Imports      callr <NA>        
      3  checked  Imports        cli <NA>        
      4  checked  Imports       glue <NA>        
      5  checked  Imports     igraph <NA>        
      6  checked  Imports   jsonlite <NA>        
      7  checked  Imports    memoise <NA>        
      8  checked  Imports    options <NA>        
      9  checked  Imports         R6 <NA>        
      10 checked  Imports  rcmdcheck <NA>        
      11 checked  Imports      rlang <NA>        
      12 checked  Imports      utils   >= 3, 6, 2
      13 checked  Imports      tools <NA>        
      14 checked Suggests    remotes <NA>        
      15 checked Suggests   testthat   >= 3, 0, 0
      16 checked Suggests visNetwork <NA>        
      17 checked Suggests      withr <NA>        

# pkg_dependencies works as expected for cran package

    Code
      df
    Output
              package      type        name   op       version
      1       checked   Imports       callr <NA>              
      2       checked   Imports         cli <NA>              
      3       checked   Imports      igraph <NA>              
      4       checked   Imports    jsonlite <NA>              
      5       checked   Imports     options <NA>              
      6       checked   Imports          R6 <NA>              
      7       checked   Imports   rcmdcheck <NA>              
      8       checked   Imports       utils   >=       3, 6, 2
      9       checked   Imports       tools <NA>              
      10      checked  Suggests    testthat   >=       3, 0, 0
      11      checked  Suggests       withr <NA>              
      12        callr   Depends           R   >=          3, 4
      13          cli   Depends           R   >=          3, 4
      14       igraph   Depends     methods <NA>              
      15       igraph   Depends           R   >=       3, 5, 0
      16     jsonlite   Depends     methods <NA>              
      17           R6   Depends           R   >=          3, 6
      18     testthat   Depends           R   >=       4, 1, 0
      19        withr   Depends           R   >=       3, 6, 0
      20        callr   Imports    processx   >=       3, 6, 1
      21        callr   Imports          R6 <NA>              
      22        callr   Imports       utils <NA>              
      23          cli   Imports       utils <NA>              
      24       igraph   Imports         cli <NA>              
      25       igraph   Imports    graphics <NA>              
      26       igraph   Imports   grDevices <NA>              
      27       igraph   Imports   lifecycle <NA>              
      28       igraph   Imports    magrittr <NA>              
      29       igraph   Imports      Matrix <NA>              
      30       igraph   Imports   pkgconfig   >=       2, 0, 0
      31       igraph   Imports       rlang   >=       1, 1, 0
      32       igraph   Imports       stats <NA>              
      33       igraph   Imports       utils <NA>              
      34       igraph   Imports       vctrs <NA>              
      35      options   Imports       utils <NA>              
      36    rcmdcheck   Imports       callr   >= 3, 1, 1, 9000
      37    rcmdcheck   Imports         cli   >=       3, 0, 0
      38    rcmdcheck   Imports        curl <NA>              
      39    rcmdcheck   Imports        desc   >=       1, 2, 0
      40    rcmdcheck   Imports      digest <NA>              
      41    rcmdcheck   Imports    pkgbuild <NA>              
      42    rcmdcheck   Imports prettyunits <NA>              
      43    rcmdcheck   Imports          R6 <NA>              
      44    rcmdcheck   Imports   rprojroot <NA>              
      45    rcmdcheck   Imports sessioninfo   >=       1, 1, 1
      46    rcmdcheck   Imports       utils <NA>              
      47    rcmdcheck   Imports       withr <NA>              
      48    rcmdcheck   Imports       xopen <NA>              
      49     testthat   Imports        brio   >=       1, 1, 5
      50     testthat   Imports       callr   >=       3, 7, 6
      51     testthat   Imports         cli   >=       3, 6, 5
      52     testthat   Imports        desc   >=       1, 4, 3
      53     testthat   Imports    evaluate   >=       1, 0, 4
      54     testthat   Imports    jsonlite   >=       2, 0, 0
      55     testthat   Imports   lifecycle   >=       1, 0, 4
      56     testthat   Imports    magrittr   >=       2, 0, 3
      57     testthat   Imports     methods <NA>              
      58     testthat   Imports     pkgload   >=       1, 4, 0
      59     testthat   Imports      praise   >=       1, 0, 0
      60     testthat   Imports    processx   >=       3, 8, 6
      61     testthat   Imports          ps   >=       1, 9, 1
      62     testthat   Imports          R6   >=       2, 6, 1
      63     testthat   Imports       rlang   >=       1, 1, 6
      64     testthat   Imports       utils <NA>              
      65     testthat   Imports       waldo   >=       0, 6, 2
      66     testthat   Imports       withr   >=       3, 0, 2
      67        withr   Imports    graphics <NA>              
      68        withr   Imports   grDevices <NA>              
      69       igraph LinkingTo       cpp11   >=       0, 5, 0
      70     processx   Depends           R   >=       3, 4, 0
      71    lifecycle   Depends           R   >=          3, 6
      72     magrittr   Depends           R   >=       3, 4, 0
      73       Matrix   Depends           R   >=              
      74       Matrix   Depends     methods <NA>              
      75        rlang   Depends           R   >=       4, 0, 0
      76        vctrs   Depends           R   >=       4, 0, 0
      77         curl   Depends           R   >=       3, 0, 0
      78         desc   Depends           R   >=          3, 4
      79       digest   Depends           R   >=       3, 3, 0
      80     pkgbuild   Depends           R   >=          3, 5
      81  prettyunits   Depends           R   >=         2, 10
      82    rprojroot   Depends           R   >=       3, 0, 0
      83  sessioninfo   Depends           R   >=          3, 4
      84        xopen   Depends           R   >=          3, 1
      85         brio   Depends           R   >=          3, 6
      86     evaluate   Depends           R   >=       3, 6, 0
      87      pkgload   Depends           R   >=       3, 4, 0
      88           ps   Depends           R   >=          3, 4
      89        waldo   Depends           R   >=          4, 0
      90        cpp11   Depends           R   >=       4, 0, 0
      91     processx   Imports          ps   >=       1, 2, 0
      92     processx   Imports          R6 <NA>              
      93     processx   Imports       utils <NA>              
      94    lifecycle   Imports         cli   >=       3, 4, 0
      95    lifecycle   Imports       rlang   >=       1, 1, 0
      96       Matrix   Imports   grDevices <NA>              
      97       Matrix   Imports    graphics <NA>              
      98       Matrix   Imports        grid <NA>              
      99       Matrix   Imports     lattice <NA>              
      100      Matrix   Imports       stats <NA>              
      101      Matrix   Imports       utils <NA>              
      102   pkgconfig   Imports       utils <NA>              
      103       rlang   Imports       utils <NA>              
      104       vctrs   Imports         cli   >=       3, 4, 0
      105       vctrs   Imports        glue <NA>              
      106       vctrs   Imports   lifecycle   >=       1, 0, 3
      107       vctrs   Imports       rlang   >=       1, 1, 7
      108        desc   Imports         cli <NA>              
      109        desc   Imports          R6 <NA>              
      110        desc   Imports       utils <NA>              
      111      digest   Imports       utils <NA>              
      112    pkgbuild   Imports       callr   >=       3, 2, 0
      113    pkgbuild   Imports         cli   >=       3, 4, 0
      114    pkgbuild   Imports        desc <NA>              
      115    pkgbuild   Imports    processx <NA>              
      116    pkgbuild   Imports          R6 <NA>              
      117 sessioninfo   Imports         cli   >=       3, 1, 0
      118 sessioninfo   Imports       tools <NA>              
      119 sessioninfo   Imports       utils <NA>              
      120       xopen   Imports    processx <NA>              
      121     pkgload   Imports         cli   >=       3, 3, 0
      122     pkgload   Imports        desc <NA>              
      123     pkgload   Imports          fs <NA>              
      124     pkgload   Imports        glue <NA>              
      125     pkgload   Imports   lifecycle <NA>              
      126     pkgload   Imports     methods <NA>              
      127     pkgload   Imports    pkgbuild <NA>              
      128     pkgload   Imports    processx <NA>              
      129     pkgload   Imports       rlang   >=       1, 1, 1
      130     pkgload   Imports   rprojroot <NA>              
      131     pkgload   Imports       utils <NA>              
      132          ps   Imports       utils <NA>              
      133       waldo   Imports         cli <NA>              
      134       waldo   Imports     diffobj   >=       0, 3, 4
      135       waldo   Imports        glue <NA>              
      136       waldo   Imports     methods <NA>              
      137       waldo   Imports       rlang   >=       1, 1, 0
      138     lattice   Depends           R   >=              
      139        glue   Depends           R   >=          3, 6
      140          fs   Depends           R   >=          3, 6
      141     diffobj   Depends           R   >=       3, 1, 0
      142     lattice   Imports        grid <NA>              
      143     lattice   Imports   grDevices <NA>              
      144     lattice   Imports    graphics <NA>              
      145     lattice   Imports       stats <NA>              
      146     lattice   Imports       utils <NA>              
      147        glue   Imports     methods <NA>              
      148          fs   Imports     methods <NA>              
      149     diffobj   Imports      crayon   >=       1, 3, 2
      150     diffobj   Imports       tools <NA>              
      151     diffobj   Imports     methods <NA>              
      152     diffobj   Imports       utils <NA>              
      153     diffobj   Imports       stats <NA>              
      154      crayon   Imports   grDevices <NA>              
      155      crayon   Imports     methods <NA>              
      156      crayon   Imports       utils <NA>              

