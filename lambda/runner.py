
from worker import executeQuery

respString = executeQuery("{hello}")

print("Cool Response: ", respString)

weatherString = executeQuery("{weather}")

print("Weather Response: ", weatherString)
