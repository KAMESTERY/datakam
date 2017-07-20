
from main import process, execute_query

def executeQuery(query :str):
    tasks = [execute_query(query)]
    [result] = process(*tasks)
    return result

respString = executeQuery("{hello}")

print("Cool Response: ", respString)

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
