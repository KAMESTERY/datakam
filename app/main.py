from fastapi import FastAPI
from app.api.routes.api import router as api_router


def get_application() -> FastAPI:
    application = FastAPI(title='DataKam', debug=True, version='1.0.0')

    application.include_router(api_router, prefix="")

    return application


app = get_application()

if __name__ == "__main__":
    import uvicorn

    uvicorn.run("app.main:app", host='0.0.0.0', port=1717, reload=True, debug=True, workers=1)
