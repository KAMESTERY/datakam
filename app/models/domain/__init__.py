
from typing import List

from pydantic import (
    EmailStr,
    Field,
    HttpUrl,
    NameEmail
)

EXAMPLE_TOPIC: str = Field(..., example="com.kamestery.devdata:##:africa")
EXAMPLE_DOCUMENT_ID: str = Field(..., example="com.kamestery.devdata:##:africa:##:some-content")
EXAMPLE_NAMESPACE: str = Field(..., example="com.kamestery.devdata:##:africa")
EXAMPLE_CONTENT_ID: str = Field(..., example="com.kamestery.devdata:##:africa:##:some-content")
EXAMPLE_CONTENT_SLUG: str = Field(..., example="some-content")
EXAMPLE_CONTENT_TITLE: str = Field(..., example="Some Content")
EXAMPLE_CONTENT_TEXT: str = Field(..., example="Vivamus quis nibh metus. Maecenas maximus nunc in quam tristique bibendum. Phasellus semper semper finibus. Curabitur sit amet sodales felis, ac egestas mauris. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aenean efficitur, odio vulputate rutrum vulputate, ipsum nulla tincidunt lectus, quis aliquet ante felis id lorem. Aliquam id tempor magna, nec luctus lacus. Quisque ullamcorper purus vel lectus porttitor aliquam. Maecenas quis varius velit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Pellentesque dapibus ligula non justo interdum, eu rutrum est laoreet. Nulla volutpat nisi nec nisi auctor sagittis. In et blandit eros. Vivamus auctor enim augue, at pulvinar mi venenatis id.")
EXAMPLE_UPDATED_CONTENT_TEXT: str = Field(..., example="UPDATED UPDATED UPDATED UPDATED Vivamus quis nibh metus. Maecenas maximus nunc in quam tristique bibendum. Phasellus semper semper finibus. Curabitur sit amet sodales felis, ac egestas mauris. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aenean efficitur, odio vulputate rutrum vulputate, ipsum nulla tincidunt lectus, quis aliquet ante felis id lorem. Aliquam id tempor magna, nec luctus lacus. Quisque ullamcorper purus vel lectus porttitor aliquam. Maecenas quis varius velit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Pellentesque dapibus ligula non justo interdum, eu rutrum est laoreet. Nulla volutpat nisi nec nisi auctor sagittis. In et blandit eros. Vivamus auctor enim augue, at pulvinar mi venenatis id.")
EXAMPLE_CHILD_ID: str = Field(..., example="com.kamestery.devdata:##:africa:##:some-child-content")
EXAMPLE_OTHER_CHILD_ID: str = Field(..., example="com.kamestery.devdata:##:africa:##:some-other-child-content")
EXAMPLE_EMAIL: EmailStr = Field(..., example="lb@lambert.kmt")
EXAMPLE_AUTHOR: NameEmail = Field(..., example="Lambert Dagobert <lb@lambert.kmt>")
EXAMPLE_TAGS: List[str] = Field(..., example=["dev-tag1", "dev-tag2", "dev-tag3", "dev-tag4"])
EXAMPLE_NUMBER: int = Field(..., example=4)
EXAMPLE_BOOL: bool = Field(..., example=True)
EXAMPLE_MEDIA_URL: HttpUrl = Field(..., example="https://location.of.media.file")
