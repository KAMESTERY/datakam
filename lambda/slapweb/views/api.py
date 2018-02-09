
from pyramid.view import view_config

try:
    from slapweb.gql import (
        execute_query,
    )
except:
    from gql import (
        execute_query,
    )


@view_config(
    route_name='api',
    renderer='templates/api.jinja2'
)
def api_view(request):
    return {}

@view_config(
    route_name='slapalicious',
    request_method='POST',
    renderer='json'
)
def slapalicious(request):
    result = None
    post = request.json_body

    if 'query' in post:
        query = post.get('query')
        result = execute_query(query)
    elif 'mutation' in post:
        mutation = post.get('mutation')
        result = execute_query(mutation)
    else:
        result = execute_query('{ hello }')

    return dict(data=result.data)
