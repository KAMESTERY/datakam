
from main import process, execute_query

def executeQuery(query :str):
    tasks = [execute_query(query)]
    [result] = process(*tasks)
    return result

respString = executeQuery("{hello}")

print("Cool Response: ", respString)

currentipString = executeQuery("{currentip}")

print("Current IP Response: ", currentipString)

weatherString = executeQuery(
    """
    query CurrentWeather {
        weather {
            location
            description
            temp
            pressure
            humidity
            speed
            deg
          }
    }
    """
)

print("Weather Response: ", weatherString)

heroString = executeQuery(
    """
    query CheckTypeOfLuke {
      hero(episode: EMPIRE) {
        __typename
        name
      }
    }
    """
)

print("Hero Response: ", heroString)

bunchString = executeQuery(
    """
    query CollectMetadata {
        currentip
        weather(location: 'London') {
            location
            description
            temp
            pressure
            humidity
            speed
            deg
        }
    }
    """
)

print("Bunch Response: ", bunchString)
