
import graphene

from graphene import (
    relay,
    resolve_only_args
)

from .data import (
    create_ship, get_empire,
    get_faction, get_rebels, get_ship,
    get_weather, get_currentip
)


try:
    from slapweb.models.userinfo import (
        User,
        UserProfile
    )
except:
    from models.userinfo import (
        User,
        UserProfile
    )


class Ship(graphene.ObjectType):
    '''A ship in the Star Wars saga'''

    class Meta:
        interfaces = (relay.Node, )

    name = graphene.String(description='The name of the ship.')

    @classmethod
    def get_node(cls, info, id):
        return get_ship(id)


class ShipConnection(relay.Connection):

    class Meta:
        node = Ship


class Faction(graphene.ObjectType):
    '''A faction in the Star Wars saga'''

    class Meta:
        interfaces = (relay.Node, )

    name = graphene.String(description='The name of the faction.')
    ships = relay.ConnectionField(ShipConnection, description='The ships used by the faction.')

    def resolve_ships(self, info, **args):
        # Transform the instance ship_ids into real instances
        return [get_ship(ship_id) for ship_id in self.ships]

    @classmethod
    def get_node(cls, info, id):
        return get_faction(id)


class IntroduceShip(relay.ClientIDMutation):

    class Input:
        ship_name = graphene.String(required=True)
        faction_id = graphene.String(required=True)

    ship = graphene.Field(Ship)
    faction = graphene.Field(Faction)

    @classmethod
    def mutate_and_get_payload(cls, root, info, ship_name, faction_id, client_mutation_id=None):
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


class UserObject(graphene.ObjectType):
    user_id = graphene.String(description='User ID')
    email = graphene.String(description='Email')
    username = graphene.String(description='Username')
    roles = graphene.String(description='Roles')
    confirmed = graphene.String(description='Confirmed')
    last_seen = graphene.String(description='Last Seen')


class CheckPassword(graphene.ObjectType):
    user = graphene.Field(UserObject, description="User")
    valid = graphene.Boolean(description='Correct Password Entered')


class Query(graphene.ObjectType):

    check_password = graphene.Field(
        CheckPassword,
        email=graphene.String(description='Email Address', required=True),
        password=graphene.String(description='Password', required=True)
    )

    @resolve_only_args
    def resolve_check_password(self, email, password):
        valid, user = User.check_password(email, password)
        response = CheckPassword(valid=False)
        if user:
            user=UserObject(
                user_id=user.user_id,
                email=user.email,
                username=user.username,
                roles=user.roles,
                confirmed=user.confirmed,
                last_seen=user.last_seen
            )
            response.user=user
            response.valid=valid
        return response

    currentip = graphene.String(description='The Current IP of the Lambda Function')

    @resolve_only_args
    def resolve_currentip(self):
        return get_currentip()

    hello = graphene.String()

    # def resolve_hello(self, args, context, info):
    @resolve_only_args
    def resolve_hello(self):
        return 'World'

    weather = graphene.Field(Weather,
                             location=graphene.Argument(graphene.String, default_value='London'),
                             description='The Weather'
                             )

    @resolve_only_args
    def resolve_weather(self, location=None):
        return get_weather(location)

    rebels = graphene.Field(Faction)
    empire = graphene.Field(Faction)
    node = relay.Node.Field()

    def resolve_rebels(self, info):
        return get_rebels()

    def resolve_empire(self, info):
        return get_empire()



class Mutation(graphene.ObjectType):
    introduce_ship = IntroduceShip.Field()


schema = graphene.Schema(
    query=Query,
    mutation=Mutation
)
