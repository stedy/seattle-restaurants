import BeautifulSoup as bs4
import re
import argparse
import sqlite3
from geopy.geocoders import OpenCage

def main():
    """Script to load data for processing HTML results for NAICS restaurant
    codes"""

    parser = argparse.ArgumentParser(description="set up db for restaurants")
    parser.add_argument('--raw_html', help="full path to raw HTML file")
    parser.add_argument('--db', help="full path to database")
    args = parser.parse_args()

    url = args.raw_html
    db = sqlite3.connect(args.db)
    cur = db.cursor()

    page = file(url).read()
    soup = bs4.BeautifulSoup(page)

    names, address = [], []
    for s in soup.findAll('span', {'id' : re.compile("lblTradeName")}):
        names.append(s.string)
    for a in soup.findAll('span', {'id' : re.compile("lblAddress1")}):
        address.append(a.string)
    NAICStype = soup.find('a', {'id' : re.compile("lnkNAIC")}).string

    with open("api_key.txt", 'rb') as a:
        api = a.read().splitlines()

    geolocator = OpenCage(api[0],
        domain='api.opencagedata.com',
        scheme='https', timeout=10, proxies=None)

    latlong = []

    for name, place in zip(names, address):
        location = geolocator.geocode(place + " Seattle, WA")
        latlong.append(location.latitude)
        print (location.latitude, location.longitude)
        if cur.execute("""SELECT Name FROM Restaurants WHERE Name == ?""",
                [Name]) is None:
            cur.execute("""INSERT into Restaurants (Name, Latitude,
                        Longitude, NAICStype, entrydate) VALUES (?,?,?,?,?)""",
                [name, location.latitude, location.longitude, NAICStype, "2015-01-30"])
            db.commit()
        else:
            pass

    results = zip(names, address, latlong)
    for x in results:
        print x

if __name__ == '__main__':
    main()
