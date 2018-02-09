
data = {}


def setup():
    global data

    from .schema import Ship, Faction
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
