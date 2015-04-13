import BeautifulSoup as bs4
import re
import argparse
import sqlite3
import os
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

    chars_to_remove = ['DP', 'FSR', 'LSR', 'MFS', 'breweries', '.aspx']

    filename = os.path.basename(url)
    filedate = filename.translate(None, ''.join(chars_to_remove)).lstrip('-')

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
        scheme='https', timeout=15, proxies=None)

    latlong = []

    for name, place in zip(names, address):
        query = cur.execute("""SELECT Count(*) FROM
                Addresses WHERE Name = ? AND Address = ?""",
                [str(name), str(place)])
        data = query.fetchone()[0]
        if data == 0:
            location = geolocator.geocode(place + " Seattle, WA")
            latlong.append(location.latitude)
            
            print(name)
            print (location.latitude, location.longitude)
            cur.execute("""INSERT into Addresses (Name, Latitude,
                        Longitude, NAICStype, Address) VALUES (?,?,?,?,?)""",
                [name, location.latitude, location.longitude, NAICStype,
                    place])
            cur.execute("""INSERT INTO Dates (Name, Address,
                    Entrydate) VALUES (?,?,?)""",
                [name, place, filedate])
            db.commit()
        if data > 0:
            cur.execute("""INSERT INTO Dates (Name, Address,
                    entrydate) VALUES (?,?,?)""",
                [name, place, filedate])
            db.commit()

if __name__ == '__main__':
    main()
