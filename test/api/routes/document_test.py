import json
import pytest

from urllib.parse import quote

from fastapi.testclient import TestClient
# from starlette.testclient import TestClient
from app.main import app
from test.api.routes import (
    DOCUMENT_DATA,
    DOCUMENT_UPDATE_DATA,
    NAMESPACE,
    CONTENT_ID,
    # dicts_match,
    validate_document
)

client = TestClient(app)


def test_create_document():
    response = client.post(
        "/content/document",
        json=DOCUMENT_DATA
    )
    assert response.status_code == 201
    assert response.json() == {
        "namespace": NAMESPACE,
        "contentId": CONTENT_ID,
        "message": "Content was created."
    }


def test_retrieve_document():
    response = client.get(quote(f"/content/document/{NAMESPACE}/{CONTENT_ID}/"))
    assert response.status_code == 200
    res_doc = response.json()
    validate_document(res_doc)


def test_get_documents_by_topic():
    response = client.get(quote(f"/content/document/{NAMESPACE}"))
    assert response.status_code == 200
    res_doc = response.json()[0]
    # assert dicts_match(res_doc, DOCUMENT_DATA)
    validate_document(res_doc)

def test_update_document():
    response = client.put(
        quote(f"/content/document/{NAMESPACE}/{CONTENT_ID}/"),
        json=DOCUMENT_UPDATE_DATA
    )
    assert response.status_code == 200
    assert response.json()  == {
        "namespace": NAMESPACE,
        "contentId": CONTENT_ID,
        "message": "Content was updated."
    }


def test_retrieve_updated_document():
    response = client.get(quote(f"/content/document/{NAMESPACE}/{CONTENT_ID}/"))
    assert response.status_code == 200
    res_doc = response.json()
    assert res_doc.body == "UPDATE UPDATE UPDATE UPDATE"


def test_patch_document_media():
    DOCUMENT_PATCH_DATA = dict(
        media=[
            dict(
                fileUrl="https://location.of.another.media.file"
            )
        ]
    )

    response = client.patch(
        quote(f"/content/document/{NAMESPACE}/{CONTENT_ID}/"),
        json=DOCUMENT_PATCH_DATA
    )
    assert response.status_code == 200
    assert response.json()  == {
        "namespace": NAMESPACE,
        "contentId": CONTENT_ID,
        "message": "Content was updated."
    }


def test_retrieve_patched_document():
    response = client.get(quote(f"/content/document/{NAMESPACE}/{CONTENT_ID}/"))
    assert response.status_code == 200
    res_doc = response.json()
    assert res_doc.media[0].fileUrl == "https://location.of.another.media.file"


def test_delete_document():
    response = client.delete(quote(f"/content/document/{NAMESPACE}/{CONTENT_ID}/"))
    assert response.status_code == 200
    assert response.json() == {
        "namespace": NAMESPACE,
        "contentId": CONTENT_ID,
        "message": "Content was deleted."
    }
