
from main import process, execute_query

def executeQuery(query :str):
    tasks = [execute_query(query)]
    [result] = process(*tasks)
    return result

respString = executeQuery("{hello}")

print(f"Cool Response: {respString}")

currentipString = executeQuery("{currentip}")

print(f"Current IP Response: {currentipString}")

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

print(f"Weather Response: {weatherString}")

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

print(f"Hero Response: {heroString}")

bunchString = executeQuery(
    """
    query CollectMetadata {
        weather(location: "London") {
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

print(f"Bunch Response: {bunchString}")

from worker import launch, terminate

launch()

terminate()
