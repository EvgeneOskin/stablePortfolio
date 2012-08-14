stablePortfolio
===============

Instalation
-----------
> $ghc --make setup

> $./setup configure --prefix=$HOME --user

> $./setup build

> $./setup install

Runing
------

> $./stablePortfolio -M *markets_symbols* -I *path/to/database* -T *date period* 

 *market_symbol* is a market symbol. Don't need now

 *path/to/database* is a path to Sqlite3 database.
If path contains " " (space) you should put path into quote ("path").

 *date period* is period of date in format "DATE DATE"

 Type this:
> $./stablePortfolio -H

  to see all options  
