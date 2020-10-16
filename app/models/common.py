
import datetime

from dateutil.parser import parse
from pydantic import (
    BaseConfig,
    BaseModel,
    validator,
)


class DateTimeModelMixin(BaseModel):
    created_at: datetime.datetime = datetime.datetime.now()
    updated_at: datetime.datetime = datetime.datetime.now()

    @validator("created_at", "updated_at", pre=True)
    def default_datetime(
            cls,
            value: datetime.datetime,
    ) -> datetime.datetime:
        return value or datetime.datetime.now()


def convert_string_to_datetime(dts: str) -> datetime.datetime:
    return parse(dts)


def convert_json_to_realworld(dt: datetime.datetime) -> str:
    return dt.replace(tzinfo=datetime.timezone.utc).isoformat().replace("+00:00", "Z")


def convert_field_to_camel_case(string: str) -> str:
    return "".join(
        word if index == 0 else word.capitalize()
        for index, word in enumerate(string.split("_"))
    )


class ModelConfigMixin(BaseModel):
    class Config(BaseConfig):
        allow_population_by_field_name = True
        json_encoders = {datetime.datetime: convert_json_to_realworld}
        alias_generator = convert_field_to_camel_case
