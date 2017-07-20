human_data = {}
droid_data = {}
data = {}


def setup():
    from .schema import Human, Droid
    global human_data, droid_data
    luke = Human(
        id='1000',
        name='Luke Skywalker',
        friends=['1002', '1003', '2000', '2001'],
        appears_in=[4, 5, 6],
        home_planet='Tatooine',
    )

    vader = Human(
        id='1001',
        name='Darth Vader',
        friends=['1004'],
        appears_in=[4, 5, 6],
        home_planet='Tatooine',
    )

    han = Human(
        id='1002',
        name='Han Solo',
        friends=['1000', '1003', '2001'],
        appears_in=[4, 5, 6],
        home_planet=None,
    )

    leia = Human(
        id='1003',
        name='Leia Organa',
        friends=['1000', '1002', '2000', '2001'],
        appears_in=[4, 5, 6],
        home_planet='Alderaan',
    )

    tarkin = Human(
        id='1004',
        name='Wilhuff Tarkin',
        friends=['1001'],
        appears_in=[4],
        home_planet=None,
    )

    human_data = {
        '1000': luke,
        '1001': vader,
        '1002': han,
        '1003': leia,
        '1004': tarkin,
    }

    c3po = Droid(
        id='2000',
        name='C-3PO',
        friends=['1000', '1002', '1003', '2001'],
        appears_in=[4, 5, 6],
        primary_function='Protocol',
    )

    r2d2 = Droid(
        id='2001',
        name='R2-D2',
        friends=['1000', '1002', '1003'],
        appears_in=[4, 5, 6],
        primary_function='Astromech',
    )

    droid_data = {
        '2000': c3po,
        '2001': r2d2,
    }

    from .schema import Ship, Faction
    global data

    xwing = Ship(
        id='1',
        name='X-Wing',
    )

    ywing = Ship(
        id='2',
        name='Y-Wing',
    )

    awing = Ship(
        id='3',
        name='A-Wing',
    )

    # Yeah, technically it's Corellian. But it flew in the service of the rebels,
    # so for the purposes of this demo it's a rebel ship.
    falcon = Ship(
        id='4',
        name='Millenium Falcon',
    )

    homeOne = Ship(
        id='5',
        name='Home One',
    )

    tieFighter = Ship(
        id='6',
        name='TIE Fighter',
    )

    tieInterceptor = Ship(
        id='7',
        name='TIE Interceptor',
    )

    executor = Ship(
        id='8',
        name='Executor',
    )

    rebels = Faction(
        id='1',
        name='Alliance to Restore the Republic',
        ships=['1', '2', '3', '4', '5']
    )

    empire = Faction(
        id='2',
        name='Galactic Empire',
        ships=['6', '7', '8']
    )

    data = {
        'Faction': {
            '1': rebels,
            '2': empire
        },
        'Ship': {
            '1': xwing,
            '2': ywing,
            '3': awing,
            '4': falcon,
            '5': homeOne,
            '6': tieFighter,
            '7': tieInterceptor,
            '8': executor
        }
    }


def get_character(id):
    return human_data.get(id) or droid_data.get(id)


def get_friends(character):
    return map(get_character, character.friends)


def get_hero(episode):
    if episode == 5:
        return human_data['1000']
    return droid_data['2001']


def get_human(id):
    return human_data.get(id)


def get_droid(id):
    return droid_data.get(id)


def create_ship(ship_name, faction_id):
    from .schema import Ship
    next_ship = len(data['Ship'].keys()) + 1
    new_ship = Ship(
        id=str(next_ship),
        name=ship_name
    )
    data['Ship'][new_ship.id] = new_ship
    data['Faction'][faction_id].ships.append(new_ship.id)
    return new_ship


def get_ship(_id):
    return data['Ship'][_id]


def get_faction(_id):
    return data['Faction'][_id]


def get_rebels():
    return get_faction('1')


def get_empire():
    return get_faction('2')

def get_weather(location):
    import requests
    from .schema import Weather

    url = "http://samples.openweathermap.org/data/2.5/weather?id=2172797&appid=b1b15e88fa797225412429c1c50c122a1"
    raw = requests.get(url)
    response = raw.json()
    weather = Weather(
        location=location,
        description=response['weather'][0]['description'],
        temp=response['main']['temp'],
        pressure=response['main']['pressure'],
        humidity=response['main']['humidity'],
        speed=response['wind']['speed'],
        deg=response['wind']['deg']
    )
    return weather

def get_currentip():
    import requests

    url = "https://api.ipify.org/?format=json"
    raw = requests.get(url)
    response = raw.json()
    currentip = response['ip']
    return currentip
