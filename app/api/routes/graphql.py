

import graphene
from graphql.execution.executors.asyncio import AsyncioExecutor

from starlette.graphql import GraphQLApp

from app.services.graphql.content import DocumentQuery


class Query(
    graphene.ObjectType,
    DocumentQuery
):
    pass


schema = graphene.Schema(query=Query)

gqlApp = GraphQLApp(
    schema=schema,
    executor_class=AsyncioExecutor
)
