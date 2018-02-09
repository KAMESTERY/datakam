
import asyncio
import uvloop

from graphql.execution.executors.asyncio import AsyncioExecutor

from .schema import schema

def execute_query(query: str):
    asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
    loop = asyncio.new_event_loop()
    # loop = asyncio.get_event_loop()
    executor = AsyncioExecutor(loop=loop)
    result = schema.execute(query, executor=executor)
    # result = schema.execute(query)
    return result

async def execute_query_async(query: str):
    asyncio.set_event_loop_policy(uvloop.EventLoopPolicy())
    loop = asyncio.get_event_loop()
    executor = AsyncioExecutor(loop=loop)
    result = schema.execute(query, executor=executor)
    # result = schema.execute(query)
    return result

