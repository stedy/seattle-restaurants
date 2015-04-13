DROP TABLE IF EXISTS Addresses;
CREATE TABLE Addresses(
  ID integer PRIMARY KEY,
  Name text,
  Latitude text,
  Longitude text,
  NAICStype text,
  Address text);

DROP TABLE IF EXISTS Dates;
CREATE TABLE Dates(
  Name text,
  Address text,
  Entrydate text);
