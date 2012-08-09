import argparse
import sqlalchemy
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Integer, String, DateTime, Float

base = declarative_base()

class Prices(base):
    __tablename__ = 'prices'

    id = sqlalchemy.Column(Integer, primary_key = True)
    ticker = sqlalchemy.Column(String)
    date = sqlalchemy.Column(DateTime)
    openPrice = sqlalchemy.Column(Float)
    highPrice = sqlalchemy.Column(Float)
    lowPrice = sqlalchemy.Column(Float)
    closePrice = sqlalchemy.Column(Float)
    volume = sqlalchemy.Column(Integer)

    def __init__(self, ticker, date, openPrice, highPrice, lowPrice, closePrice, volume):
        self.ticker = ticker
        self.date = date 
        self.openPrice = openPrice
        self.highPrice = highPrice
        self.lowPrice = lowPrice
        self.closePrice =closePrice
        self.volume = self.volume

    def __repr__(self):
        """need for creating a row
        """    
        return '<prices(%s,%s,%s,%s,%s,%s)>' % ( self.ticker, self.date, self.openPrice
                                               , self.highPrice, self.lowPrice, self.closePrice
                                               , self.volume)
        
class DataBase():
    def __init__(self, db_name, db_type, detail = False):
        """For sqlite3:
           db_type = "sqlite"
           db_name = "/relative/path/to/file.db" (or "//absolute/path/to/file.db")
        """
        self.engine = sqlalchemy.create_engine('%s://%s' % (db_type, db_name), echo = detail)
        base.metadata.create_all(self.engine)

def main():
    parser = argparse.ArgumentParser(description = 'Generate BIG database',
                                     epilog='Bye.')
    parser.add_argument('input', action='store', nargs=1,
                        help='path/to/sql/files')
    parser.add_argument('type', action='store', nargs=1,
                        help='type of database')
    arg_data = vars(parser.parse_args(None))
    database = DataBase(arg_data['input'][0], arg_data['type'][0])

if __name__=='__main__':
    main()