from typing import Optional, Any, Dict, List, Union, Iterator

from datetime import datetime, date

import pandas as pd
from math import ceil

import asyncio, aiohttp


MINUTES_PER_TRADINGDAY = 960
HOURS_PER_TRADINGDAY = MINUTES_PER_TRADINGDAY / 60
DAYS_PER_TRADINGDAY = 1



date_mapping = {
    'day': DAYS_PER_TRADINGDAY,
    'hour': HOURS_PER_TRADINGDAY,
    'minute': MINUTES_PER_TRADINGDAY,
}

def get_query_urls(start_time, end_ts, timeframe, multiplier):
    
    all_buisness_days = pd.date_range(start_time, end_ts, freq='B')
    
    window_size = 50_000 // calculate_candels_per_day(timeframe, multiplier)
    
    bountry_days = all_buisness_days[::window_size]
        
    if bountry_days[-1] != pd.to_datetime(end_ts):
        bountry_days = bountry_days.append(pd.DatetimeIndex([pd.to_datetime(end_ts)]))
    
    return bountry_days
    


def calculate_candels_per_day(timeframe, multiplier):
    candels_per_day = date_mapping[timeframe] / multiplier
    
    return ceil(candels_per_day)


def query_urls(tikr, daterange, multiplier, timeframe, base):
    
    res = []
    for from_, to in zip(daterange, daterange[1:]):
        url = f'https://api.polygon.io/v2/aggs/ticker/{tikr}/range/{multiplier}/{timeframe}/{from_}/{to}{base}'
        res.append(url)
        
    return res


async def async_http_request(url):
    
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response:
            data = {}
            if response.status == 200:
                # Parse and print the response JSON content
                data = await response.json()                
                
                if 'results' in data:
                    data = data['results']
                else:
                    data = []
                    
            return data


async def fetch_ticker_details_concurrently(urls):
    tasks = [async_http_request(url) for url in urls]

    results = await asyncio.gather(*tasks)
    return results
    


class RESTClient:
    def __init__(self, api_key):
        self.key = api_key
        
        
        
    def list_aggs(
        self,
        ticker: str,
        multiplier: int,
        timespan: str,
        from_: Union[str, int, datetime, date],
        to: Union[str, int, datetime, date],
        adjusted: bool = True,
        limit = 50000
    ) -> pd.DataFrame:


        base = f'?adjusted={adjusted}&sort=asc&limit=50000&apiKey={self.key}'

        daterange = get_query_urls(from_, to, timespan, multiplier)
        

        urls = query_urls(ticker, daterange.date, multiplier, timespan, base)

        loop = asyncio.get_event_loop()
        res = loop.run_until_complete(fetch_ticker_details_concurrently(urls))
                
        final = [d for sub in res for d in sub]
                
        df = pd.DataFrame(final).sort_values('t').drop_duplicates(subset='t').reset_index(drop=True)
        df.columns = ['volume', 'vwap', 'open', 'close', 'high', 'low', 'timestamp', 'transactions']
        
        df.index = pd.to_datetime(df['timestamp'], unit='ms', utc=True).dt.tz_convert('America/New_York').dt.tz_localize(None)
        
        
        return df[['open', 'high', 'low', 'close', 'volume', 'vwap', 'timestamp', 'transactions']]
    
    
def test():
    tickers = ['AAPL', 'MSFT', 'GOOGL', 'AMZN','TSLA']
    client = RESTClient('RpovRC4_0dsV0iW9wOYVpXYImBqXsY7v')


    for ticker in tickers:
        client.list_aggs(ticker, multiplier = 1,timespan = 'minute', from_ = '2000-01-01', to = '2024-02-01', limit = 50000).to_csv(f'{ticker}.csv')
        
        
        
    
    # client = RESTClient('RpovRC4_0dsV0iW9wOYVpXYImBqXsY7v')
    
    
    # print(client.list_aggs('AAPL', multiplier = 1,timespan = 'minute', from_ = '2000-01-01', to = '2024-02-01', limit = 50000))
    
    
    
if __name__ == '__main__':
    test()