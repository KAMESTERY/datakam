
import logging

from pynamodb.models import Model
from pynamodb.indexes import (
    GlobalSecondaryIndex,
    AllProjection
)

logging.basicConfig()
log = logging.getLogger("pynamodb")
log.setLevel(logging.DEBUG)
log.propagate = True

################ BASE MODEL AND INDEX

class PartialModel(Model):
    class Meta:
        # Specifies the write capacity
        write_capacity_units = 10
        # Specifies the read capacity
        read_capacity_units = 10

class PartialIndex(GlobalSecondaryIndex):
    class Meta:
        # Specifies the write capacity
        write_capacity_units = 10
        # Specifies the read capacity
        read_capacity_units = 10
        # All attributes are projected
        projection = AllProjection()
