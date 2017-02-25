
import urllib.request
import urllib.error
from time import sleep
import csv
import json
import os


reports = {
    'occupied_housing_units_total':     'H0040001',
    'occupied_housing_units_owned_with_a_mortgage_or_a_loan':     'H0040002',
    'occupied_housing_units_owned_free_and_clear':     'H0040003',
    'occupied_housing_units_renter_occupied':     'H0040004',
    'total_population':     'P0080001',
    'white_population':     'P0080003',
    'black_population':     'P0080004',
    'asian_population':     'P0080006',
    'hispanic_population':  'P0090002'
}


LOCATION_CSV = './state-city-fips.csv'
SHOOTINGS_CSV = './fatal-police-shootings-data.csv'
OUTPUT_CSV = './fatal-police-shootings-data-census.csv'
CACHE_DIRECTORY = 'cache'
REPORT_YEAR = 2010

with open("api-census.key") as file:
    API_KEY = file.read().splitlines()[0]

if not os.path.exists(CACHE_DIRECTORY):
    os.makedirs(CACHE_DIRECTORY)

with open(LOCATION_CSV) as csvfile:
    reader = csv.DictReader(csvfile)
    fips = {}
    for row in reader:
        fips[(row['STATE'].strip(), row['PLACENAME'].strip())] = {
            'STATEFP': int(row['STATEFP']),
            'PLACEFP': int(row['PLACEFP'])
        }
        # note this is an imperfect solution, since this will overwrite
        #  many times over the data set, but catches some county mistakes
        #  in the raw shootings set and allows simpler grouping of obscure
        #  locations
        fips[(row['STATE'].strip(), row['COUNTY'].strip())] = {
            'STATEFP': int(row['STATEFP']),
            'PLACEFP': int(row['PLACEFP'])
        }


misspellings = {
    "St, Louis": "St. Louis",
    "Lakes Charles": "Lake Charles",
    "Ft. Lauderdale": "Fort Lauderdale",
}

obscure_places = {
    ('TX', 'Baytown'): "Chambers County",
    ('OH', 'Rome'): "Adams County",
    ('OH', 'Sylvania Township'): "Lucas County",
    ('MD', 'Chestertown'): "Kent County",
    ('MI', 'Holland Township'): "Ottawa County",
    ('FL', 'Opa-Locka'): "Miami-Dade County",
    ('IL', 'Villa Park'): "DuPage County",
    ('PA', 'Fort Littleton'): "Fulton County"
    # TODO: Add more of the ~180 missing
}


def fix_pair(pair):
    for misspelling, correct in misspellings.items():
        if (pair[1] == misspelling):
            return (pair[0], correct)
    for tup, loc in obscure_places.items():
        if (pair == tup):
            return (pair[0], loc)
    return pair


with open(SHOOTINGS_CSV) as csvfile:
    reader = csv.DictReader(csvfile)
    locations = []
    for row in reader:
        loc_pair = fix_pair((row['state'], row['city']))
        locations.append({
            'pair': loc_pair,
            'row': row
        })

def cache_name(statefp, placefp):
    return os.path.join(CACHE_DIRECTORY, 'cache-{0}.{1}.json'.format(statefp, placefp))

def cache_check(statefp, placefp):
    try:
        with open(cache_name(statefp, placefp)) as file:
            data = file.read().splitlines()[0]
            return json.loads(data)
    except:
        return None

def cache_write(statefp, placefp, data):
    with open(cache_name(statefp, placefp), "w") as f:
        f.write(json.dumps(data))


def get_data(variable, statefp, placefp):
    url = 'http://api.census.gov/data/{0}/{1}?key={2}&get={3}&in=state:{4}&for=place:{5}'.format(
        str(REPORT_YEAR),
        'sf1',
        API_KEY,
        variable,
        statefp,
        placefp
    )
    req = urllib.request.Request(url, None, {'Content-Type': 'application/json'})

    try:
        response = urllib.request.urlopen(req)
    except urllib.error.URLError:
        # Might be going to fast. Sleep a few seconds and give it
        # another shot.
        print(" -> Could not connect to Census Bureau. Sleeping for 5 seconds and trying again.")
        sleep(5)
        response = urllib.request.urlopen(req)

    res = response.read()
    if (len(res) < 1):
        return None
    res = json.loads(res.decode('utf8'))
    # print(res[1][0])
    response.close()
    return int(res[1][0])

def get_data_all(statefp, placefp):
    cache_results = cache_check(statefp, placefp)
    if cache_results is not None:
        return cache_results
    results = {}
    for label, variable in reports.items():
        results[label] = get_data(variable, statefp, placefp)
    cache_write(statefp, placefp, results)
    return results

def get_data_blank():
    di = {}
    for key, value in reports.items():
        di[key] = None
    return di

rows_new = []
for location in locations:
    if location['pair'] not in fips:
        print('Missing: ', location['pair'])
        data = get_data_blank()
    else:
        data = get_data_all(fips[location['pair']]['STATEFP'], fips[location['pair']]['PLACEFP'])
    row_new = {**location['row'], **data}
    rows_new.append(row_new)


with open(OUTPUT_CSV, 'w') as f:
    w = csv.DictWriter(f, rows_new[0].keys())
    w.writeheader()
    for row in rows_new:
        w.writerow(row)
