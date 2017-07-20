import asyncio
import graphene

from graphene import relay, resolve_only_args
from graphql.execution.executors.asyncio import AsyncioExecutor

from .data import (
    get_character, get_droid, get_hero, get_human,
    create_ship, get_empire, get_faction, get_rebels,
    get_ship, get_weather
)


class Episode(graphene.Enum):
    NEWHOPE = 4
    EMPIRE = 5
    JEDI = 6


class Character(graphene.Interface):
    id = graphene.ID()
    name = graphene.String()
    friends = graphene.List(lambda: Character)
    appears_in = graphene.List(Episode)

    def resolve_friends(self, args, *_):
        # The character friends is a list of strings
        return [get_character(f) for f in self.friends]


class Human(graphene.ObjectType):
    class Meta:
        interfaces = (Character,)

    home_planet = graphene.String()


class Droid(graphene.ObjectType):
    class Meta:
        interfaces = (Character,)

    primary_function = graphene.String()


class Ship(graphene.ObjectType):
    '''A ship in the Star Wars saga'''

    class Meta:
        interfaces = (relay.Node,)

    name = graphene.String(description='The name of the ship.')

    @classmethod
    def get_node(cls, id, context, info):
        return get_ship(id)


class Faction(graphene.ObjectType):
    '''A faction in the Star Wars saga'''

    class Meta:
        interfaces = (relay.Node,)

    name = graphene.String(description='The name of the faction.')
    ships = relay.ConnectionField(Ship, description='The ships used by the faction.')

    @resolve_only_args
    def resolve_ships(self, **args):
        # Transform the instance ship_ids into real instances
        return [get_ship(ship_id) for ship_id in self.ships]

    @classmethod
    def get_node(cls, id, context, info):
        return get_faction(id)


class IntroduceShip(relay.ClientIDMutation):
    class Input:
        ship_name = graphene.String(required=True)
        faction_id = graphene.String(required=True)

    ship = graphene.Field(Ship)
    faction = graphene.Field(Faction)

    @classmethod
    def mutate_and_get_payload(cls, input, context, info):
        ship_name = input.get('ship_name')
        faction_id = input.get('faction_id')
        ship = create_ship(ship_name, faction_id)
        faction = get_faction(faction_id)
        return IntroduceShip(ship=ship, faction=faction)


class Weather(graphene.ObjectType):
    location = graphene.String(description='The Location')
    description = graphene.String(description='The Description')
    temp = graphene.String(description='The Temperature')
    pressure = graphene.String(description='The Pressure')
    humidity = graphene.String(description='The Humidity')
    speed = graphene.String(description='The Speed')
    deg = graphene.String(description='The Degrees')


class Query(graphene.ObjectType):
    hello = graphene.String()

    # def resolve_hello(self, args, context, info):
    @resolve_only_args
    def resolve_hello(self):
        return 'World'

    weather = graphene.Field(Weather,
                             description='The Weather'
                             )

    @resolve_only_args
    def resolve_weather(self, location=None):
        return get_weather(location)

    hero = graphene.Field(Character,
                          episode=Episode()
                          )
    human = graphene.Field(Human,
                           id=graphene.String()
                           )
    droid = graphene.Field(Droid,
                           id=graphene.String()
                           )

    @resolve_only_args
    def resolve_hero(self, episode=None):
        return get_hero(episode)

    @resolve_only_args
    def resolve_human(self, id):
        return get_human(id)

    @resolve_only_args
    def resolve_droid(self, id):
        return get_droid(id)

    rebels = graphene.Field(Faction)
    empire = graphene.Field(Faction)
    node = relay.Node.Field()

    @resolve_only_args
    def resolve_rebels(self):
        return get_rebels()

    @resolve_only_args
    def resolve_empire(self):
        return get_empire()


class Mutation(graphene.ObjectType):
    introduce_ship = IntroduceShip.Field()


schema = graphene.Schema(
    query=Query,
    mutation=Mutation
)


async def execute_query_async(query: str):
    loop = asyncio.get_event_loop()
    executor = AsyncioExecutor(loop=loop)
    result = schema.execute(query, executor=executor)
    # result = schema.execute(query)
    return result
