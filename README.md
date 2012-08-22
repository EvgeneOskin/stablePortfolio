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

> $./stablePortfolio -I *path/to/database* -T *date period* -S *Symdbols* -C *Capital*

 *path/to/database* is a path to Sqlite3 database.
If path contains " " (space) you should put path into quote ("path").

 *date period* is a period of date in format "yyyymmdd yyyymmdd" (with quote)

 *Symdbols* is a symbols of quotes which you already have in your portfolio (expect in format **A1,B1,AA,BC1**)
 
 *Capital* is a amount of money which you can spend new quotes (expect in format floating point **0.0**)
 
 Type this:
> $./stablePortfolio -H

  to see all options  

  ***In my experience, value of the best goondess (fitness) is about 3e-6 (actuale 5.58e-6 and 1.1e-6).*** You should *play* with genetic algorithms paramaters