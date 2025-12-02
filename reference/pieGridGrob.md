# Make grid of pieGrobs

Make grid of pieGrobs

## Usage

``` r
pieGridGrob(
  radMat,
  wts = NULL,
  fills = NULL,
  labels = NULL,
  showRadSum = FALSE,
  ncol = NULL,
  nrow = NULL,
  byrow = TRUE,
  name = NULL,
  gp = NULL,
  vp = NULL
)

grid.pieGridGrob(
  radMat,
  wts = NULL,
  fills = NULL,
  labels = NULL,
  showRadSum = FALSE,
  ncol = NULL,
  nrow = NULL,
  byrow = TRUE,
  name = NULL,
  gp = NULL,
  vp = NULL
)
```

## Arguments

- radMat:

  `matrix(<numeric>)`, observations by slice radii

- wts:

  `vector(<numeric>)`, relative weights of each slice

- fills:

  Vector of colors to fill slices

- labels:

  `vector(<character>)`, (optional) label for each observation

- showRadSum:

  Logical scalar, when `TRUE` show the weighted sum of slices below the
  label

- nrow, ncol:

  Integer scalar, number of rows and columns for the grid

- byrow:

  Logical scalar, fill the grid by rows when `TRUE`

- name, gp, vp:

  Passed to [grid::gTree](https://rdrr.io/r/grid/grid-defunct.html)

## Value

`pieGrob` [grid::grob](https://rdrr.io/r/grid/grid-defunct.html) object

## Examples

``` r
# \donttest{
library(grid)

s <- seq(0.2, 1, by = 0.1)
smat <- do.call("rbind", replicate(20, s, simplify = FALSE))
grid.newpage()
grid.pieGridGrob(radMat = smat)


rownames(smat) <- sprintf("obs%02d", 1:20)
grid.newpage()
grid.pieGridGrob(radMat = smat, wts = s)

grid.newpage()
grid.pieGridGrob(radMat = smat, wts = s, showRadSum = TRUE, labels = FALSE)

grid.newpage()
grid.pieGridGrob(radMat = smat, labels = "hello")

grid.newpage()
grid.pieGridGrob(radMat = smat, labels = 1:20)


## Can edit like normal grid objects
grid.newpage()
grid.pieGridGrob(radMat = smat, wts = s, showRadSum = TRUE)

grid.ls() ## shows grid elements
#> GRID.gTree.1308
#>   GRID.frame.1309
#>     GRID.cellGrob.1310
#>       pie-1
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1311
#>       pie-1-radSum
#>     GRID.cellGrob.1312
#>       pie-1-label
#>   GRID.frame.1313
#>     GRID.cellGrob.1314
#>       pie-2
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1315
#>       pie-2-radSum
#>     GRID.cellGrob.1316
#>       pie-2-label
#>   GRID.frame.1317
#>     GRID.cellGrob.1318
#>       pie-3
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1319
#>       pie-3-radSum
#>     GRID.cellGrob.1320
#>       pie-3-label
#>   GRID.frame.1321
#>     GRID.cellGrob.1322
#>       pie-4
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1323
#>       pie-4-radSum
#>     GRID.cellGrob.1324
#>       pie-4-label
#>   GRID.frame.1325
#>     GRID.cellGrob.1326
#>       pie-5
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1327
#>       pie-5-radSum
#>     GRID.cellGrob.1328
#>       pie-5-label
#>   GRID.frame.1329
#>     GRID.cellGrob.1330
#>       pie-6
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1331
#>       pie-6-radSum
#>     GRID.cellGrob.1332
#>       pie-6-label
#>   GRID.frame.1333
#>     GRID.cellGrob.1334
#>       pie-7
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1335
#>       pie-7-radSum
#>     GRID.cellGrob.1336
#>       pie-7-label
#>   GRID.frame.1337
#>     GRID.cellGrob.1338
#>       pie-8
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1339
#>       pie-8-radSum
#>     GRID.cellGrob.1340
#>       pie-8-label
#>   GRID.frame.1341
#>     GRID.cellGrob.1342
#>       pie-9
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1343
#>       pie-9-radSum
#>     GRID.cellGrob.1344
#>       pie-9-label
#>   GRID.frame.1345
#>     GRID.cellGrob.1346
#>       pie-10
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1347
#>       pie-10-radSum
#>     GRID.cellGrob.1348
#>       pie-10-label
#>   GRID.frame.1349
#>     GRID.cellGrob.1350
#>       pie-11
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1351
#>       pie-11-radSum
#>     GRID.cellGrob.1352
#>       pie-11-label
#>   GRID.frame.1353
#>     GRID.cellGrob.1354
#>       pie-12
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1355
#>       pie-12-radSum
#>     GRID.cellGrob.1356
#>       pie-12-label
#>   GRID.frame.1357
#>     GRID.cellGrob.1358
#>       pie-13
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1359
#>       pie-13-radSum
#>     GRID.cellGrob.1360
#>       pie-13-label
#>   GRID.frame.1361
#>     GRID.cellGrob.1362
#>       pie-14
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1363
#>       pie-14-radSum
#>     GRID.cellGrob.1364
#>       pie-14-label
#>   GRID.frame.1365
#>     GRID.cellGrob.1366
#>       pie-15
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1367
#>       pie-15-radSum
#>     GRID.cellGrob.1368
#>       pie-15-label
#>   GRID.frame.1369
#>     GRID.cellGrob.1370
#>       pie-16
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1371
#>       pie-16-radSum
#>     GRID.cellGrob.1372
#>       pie-16-label
#>   GRID.frame.1373
#>     GRID.cellGrob.1374
#>       pie-17
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1375
#>       pie-17-radSum
#>     GRID.cellGrob.1376
#>       pie-17-label
#>   GRID.frame.1377
#>     GRID.cellGrob.1378
#>       pie-18
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1379
#>       pie-18-radSum
#>     GRID.cellGrob.1380
#>       pie-18-label
#>   GRID.frame.1381
#>     GRID.cellGrob.1382
#>       pie-19
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1383
#>       pie-19-radSum
#>     GRID.cellGrob.1384
#>       pie-19-label
#>   GRID.frame.1385
#>     GRID.cellGrob.1386
#>       pie-20
#>         slice1
#>         slice2
#>         slice3
#>         slice4
#>         slice5
#>         slice6
#>         slice7
#>         slice8
#>         slice9
#>     GRID.cellGrob.1387
#>       pie-20-radSum
#>     GRID.cellGrob.1388
#>       pie-20-label
grid.edit("pie-20", fills = 1:9)

grid.edit("pie-19-label", gp = gpar(font = 2, col = "red"))

grid.edit("pie-1", wts = rep(1, 9), rads = rep(1, 9))

for (s in sprintf("pie-%d-radSum", 2:4)) {
  grid.edit(s, gp = gpar(font = 2, col = "blue"))
}



# }
```
