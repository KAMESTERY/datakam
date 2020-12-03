
from fastapi import FastAPI, HTTPException
from fastapi.exceptions import RequestValidationError

from app.api.errors.http_error import http_error_handler
from app.api.errors.validation_error import http422_error_handler
from app.api.routes.api import router as api_router
# from app.api.routes.graphql import gqlApp
from app.core.events import (
    create_start_app_handler,
    create_stop_app_handler
)


def get_application() -> FastAPI:
    application = FastAPI(title='DataKam', debug=True, version='1.0.0')

    application.add_event_handler("startup", create_start_app_handler(application))
    application.add_event_handler("shutdown", create_stop_app_handler(application))

    application.add_exception_handler(HTTPException, http_error_handler)
    application.add_exception_handler(RequestValidationError, http422_error_handler)

    application.include_router(api_router, prefix="")
    # application.add_route("/gql", gqlApp)

    return application


app = get_application()

def run(debug=True):
    import os
    import uvicorn
    from multiprocessing import cpu_count

    uvicorn.run(
        "app.main:app",
        host='0.0.0.0',
        port=int(os.environ.get("PORT", 1717)),
        workers=cpu_count(),
        loop="uvloop",
        reload=True,
        debug=debug
    )

if __name__ == "__main__":
    run()
