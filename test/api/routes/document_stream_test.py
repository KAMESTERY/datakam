import json
import pytest

from urllib.parse import quote

from fastapi.testclient import TestClient
# from starlette.testclient import TestClient
from app.main import app
from test.api.routes import (
    DOCSTREAM_DATA,
    # DOCUMENT_UPDATE_DATA,
    NAMESPACE,
    DOCSTREAM_CONTENT_ID,
    # dicts_match,
    validate_document_stream
)

client = TestClient(app)


def test_create_document_stream():
    response = client.post(
        "/content/docstream",
        json=DOCSTREAM_DATA
    )
    assert response.status_code == 201
    assert response.json() == {
        "namespace": NAMESPACE,
        "contentId": DOCSTREAM_CONTENT_ID,
        "message": "Content was created."
    }


def test_retrieve_document_stream():
    response = client.get(quote(f"/content/docstream/{NAMESPACE}/{DOCSTREAM_CONTENT_ID}/"))
    assert response.status_code == 200
    res_doc = response.json()
    validate_document_stream(res_doc)


def test_get_document_streams_by_topic():
    response = client.get(quote(f"/content/docstream/{NAMESPACE}"))
    assert response.status_code == 200
    res_doc = response.json()[0]
    # assert dicts_match(res_doc, DOCUMENT_STREAM_DATA)
    validate_document_stream(res_doc)

# def test_update_document_stream():
#     response = client.put(
#         quote(f"/content/docstream/{NAMESPACE}/{DOCSTREAM_CONTENT_ID}/"),
#         json=DOCUMENT_UPDATE_DATA
#     )
#     assert response.status_code == 200
#     assert response.json()  == {
#         "namespace": NAMESPACE,
#         "contentId": DOCSTREAM_CONTENT_ID,
#         "message": "Content was updated."
#     }
#
#
# def test_retrieve_updated_document_stream():
#     response = client.get(quote(f"/content/docstream/{NAMESPACE}/{DOCSTREAM_CONTENT_ID}/"))
#     assert response.status_code == 200
#     res_doc = response.json()
#     assert res_doc.body == "UPDATE UPDATE UPDATE UPDATE"
#
#
# def test_patch_document_stream_media():
#     DOCUMENT_PATCH_DATA = dict(
#         media=[
#             dict(
#                 fileUrl="https://location.of.another.media.file"
#             )
#         ]
#     )
#
#     response = client.patch(
#         quote(f"/content/docstream/{NAMESPACE}/{DOCSTREAM_CONTENT_ID}/"),
#         json=DOCUMENT_PATCH_DATA
#     )
#     assert response.status_code == 200
#     assert response.json()  == {
#         "namespace": NAMESPACE,
#         "contentId": DOCSTREAM_CONTENT_ID,
#         "message": "Content was updated."
#     }
#
#
# def test_retrieve_patched_document_stream():
#     response = client.get(quote(f"/content/docstream/{NAMESPACE}/{DOCSTREAM_CONTENT_ID}/"))
#     assert response.status_code == 200
#     res_doc = response.json()
#     assert res_doc.media[0].fileUrl == "https://location.of.another.media.file"


def test_delete_document_stream():
    response = client.delete(quote(f"/content/docstream/{NAMESPACE}/{DOCSTREAM_CONTENT_ID}/"))
    assert response.status_code == 200
    assert response.json() == {
        "namespace": NAMESPACE,
        "contentId": DOCSTREAM_CONTENT_ID,
        "message": "Content was deleted."
    }
