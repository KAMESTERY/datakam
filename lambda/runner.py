
from main import process, execute_query

def executeQuery(query :str):
    tasks = [execute_query(query)]
    [result] = process(*tasks)
    return result

respString = executeQuery("{hello}")

print("Cool Response: ", respString)

weatherString = executeQuery(
    """
    weather {
        location
        description
        temp
        pressure
        humidity
        speed
        deg
      }
    """
    # """
    # {
    #     weather(location: "lambert") {
    #         pressure
    #         humidity
    #         speed
    #         deg
    #         location
    #         description
    #         temp
    #       }
    # }
    # """
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
