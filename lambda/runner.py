
from worker import executeQuery

respString = executeQuery("{hello}")

print("Cool Response: ", respString)

weatherString = executeQuery(
    """
    {
        weather(location: "lambert") {
            pressure
            humidity
            speed
            deg
            location
            description
            temp
          }
    }
    """
)

print("Weather Response: ", weatherString)
