csvLoader
======

Instalation
-----------
> $ghc --make set1up

> $./setup configure --prefix=$HOME --user

> $./setup build

> $./setup install

Runing
------

> $./csvLoader -M *markets_symbols* -I *path/to/dir/with/csv* -T *date period* 

 *market_symbol* is a market symbol

 *path/to/dir/with/csv* is a path to directory which contain *.csv files.
If path contains " " (space) you should put path into quote ("path").

 *date period* is period of date in format "DATE DATE"

  Which *.csv files will by searched? csv filename format should be *market*_DATE.csv
  Format of csv should be like on eodate ftp server ()