
NAMESPACE = "com.kamestery.devdata:##:africa"

DOCUMENT_CONTENT_ID = "com.kamestery.devdata:##:africa:##:some-document-content"
DOCUMENT_CHILD_ID = "com.kamestery.devdata:##:africa:##:some-document-child-content"
DOCUMENT_OTHER_CHILD_ID = "com.kamestery.devdata:##:africa:##:some-other-document-child-content"

DOCSTREAM_CONTENT_ID = "com.kamestery.devdata:##:africa:##:some-docstream-content"
DOCSTREAM_CHILD_ID = "com.kamestery.devdata:##:africa:##:some-docstream-child-content"
DOCSTREAM_OTHER_CHILD_ID = "com.kamestery.devdata:##:africa:##:some-other-docstream-child-content"

DOCUMENT_DATA = {
    "createdAt": "2020-10-23T18:30:42.415Z",
    "updatedAt": "2020-10-23T18:30:42.415Z",
    "topic": NAMESPACE,
    "documentId": DOCUMENT_CONTENT_ID,
    "userId": "lb@lambert.kmt",
    "tags": [
        "dev-tag1",
        "dev-tag2",
        "dev-tag3",
        "dev-tag4"
    ],
    "score": 4,
    "slug": "some-content",
    "title": "Some Content",
    "body": "Vivamus quis nibh metus. Maecenas maximus nunc in quam tristique bibendum. Phasellus semper semper finibus. Curabitur sit amet sodales felis, ac egestas mauris. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aenean efficitur, odio vulputate rutrum vulputate, ipsum nulla tincidunt lectus, quis aliquet ante felis id lorem. Aliquam id tempor magna, nec luctus lacus. Quisque ullamcorper purus vel lectus porttitor aliquam. Maecenas quis varius velit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Pellentesque dapibus ligula non justo interdum, eu rutrum est laoreet. Nulla volutpat nisi nec nisi auctor sagittis. In et blandit eros. Vivamus auctor enim augue, at pulvinar mi venenatis id.",
    "publish": True,
    "media": [
        {
            "createdAt": "2020-10-23T18:30:42.415Z",
            "updatedAt": "2020-10-23T18:30:42.415Z",
            "contentId": DOCUMENT_CONTENT_ID,
            "score": 4,
            "mediaId": DOCUMENT_CHILD_ID,
            "userId": "lb@lambert.kmt",
            "tags": [
                "dev-tag1",
                "dev-tag2",
                "dev-tag3",
                "dev-tag4"
            ],
            "fileUrl": "https://location.of.media.file"
        }
    ]
}


DOCUMENT_UPDATE_DATA = {
    "createdAt": "2020-10-23T18:30:42.415Z",
    "updatedAt": "2020-10-23T18:30:42.415Z",
    "topic": NAMESPACE,
    "documentId": DOCUMENT_CONTENT_ID,
    "userId": "lb@lambert.kmt",
    "tags": [
        "dev-tag1",
        "dev-tag2",
        "dev-tag3",
        "dev-tag4"
    ],
    "score": 4,
    "slug": "some-content",
    "title": "Some Content",
    "body": "UPDATE UPDATE UPDATE UPDATE",
    "publish": True,
    "media": [
        {
            "createdAt": "2020-10-23T18:30:42.415Z",
            "updatedAt": "2020-10-23T18:30:42.415Z",
            "contentId": DOCUMENT_CONTENT_ID,
            "score": 4,
            "mediaId": DOCUMENT_CHILD_ID,
            "userId": "lb@lambert.kmt",
            "tags": [
                "dev-tag1",
                "dev-tag2",
                "dev-tag3",
                "dev-tag4"
            ],
            "fileUrl": "https://location.of.media.file"
        }
    ]
}


DOCSTREAM_DATA = {
  "createdAt": "2020-10-26T01:33:23.676Z",
  "updatedAt": "2020-10-26T01:33:23.676Z",
  "namespace": NAMESPACE,
  "contentId": DOCSTREAM_CONTENT_ID,
  "userId": "lb@lambert.kmt",
  "score": 4,
  "version": 4,
  "itemStream": [
    {
      "createdAt": "2020-10-26T01:33:23.676Z",
      "updatedAt": "2020-10-26T01:33:23.676Z",
      "contentId": DOCSTREAM_CONTENT_ID,
      "score": 4,
      "mediaId": DOCSTREAM_CHILD_ID,
      "userId": "lb@lambert.kmt",
      "tags": [
        "dev-tag1",
        "dev-tag2",
        "dev-tag3",
        "dev-tag4"
      ],
      "fileUrl": "https://location.of.media.file"
    },
    {
      "createdAt": "2020-10-26T01:33:23.676Z",
      "updatedAt": "2020-10-26T01:33:23.676Z",
      "contentId": DOCSTREAM_CONTENT_ID,
      "textblockId": DOCSTREAM_OTHER_CHILD_ID,
      "userId": "lb@lambert.kmt",
      "tags": [
        "dev-tag1",
        "dev-tag2",
        "dev-tag3",
        "dev-tag4"
      ],
      "score": 4,
      "body": "Vivamus quis nibh metus. Maecenas maximus nunc in quam tristique bibendum. Phasellus semper semper finibus. Curabitur sit amet sodales felis, ac egestas mauris. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aenean efficitur, odio vulputate rutrum vulputate, ipsum nulla tincidunt lectus, quis aliquet ante felis id lorem. Aliquam id tempor magna, nec luctus lacus. Quisque ullamcorper purus vel lectus porttitor aliquam. Maecenas quis varius velit. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Pellentesque dapibus ligula non justo interdum, eu rutrum est laoreet. Nulla volutpat nisi nec nisi auctor sagittis. In et blandit eros. Vivamus auctor enim augue, at pulvinar mi venenatis id.",
      "author": "Lambert Dagobert <lb@lambert.kmt>"
    }
  ]
}


def validate_document(res_doc: dict):
    assert res_doc['topic'] == DOCUMENT_DATA['topic']
    assert res_doc['documentId'] == DOCUMENT_DATA['documentId']
    assert res_doc['userId'] == DOCUMENT_DATA['userId']
    assert res_doc['media'][0]['contentId'] == DOCUMENT_DATA['media'][0]['contentId']
    assert res_doc['media'][0]['mediaId'] == DOCUMENT_DATA['media'][0]['mediaId']


def validate_document_stream(res_doc: dict):
    assert res_doc['namespace'] == DOCSTREAM_DATA['namespace']
    assert res_doc['contentId'] == DOCSTREAM_DATA['contentId']
    assert res_doc['userId'] == DOCSTREAM_DATA['userId']
    assert res_doc['itemStream'][1]['contentId'] == DOCSTREAM_DATA['itemStream'][0]['contentId']
    assert res_doc['itemStream'][1]['mediaId'] == DOCSTREAM_DATA['itemStream'][0]['mediaId']
    assert res_doc['itemStream'][0]['contentId'] == DOCSTREAM_DATA['itemStream'][1]['contentId']
    assert res_doc['itemStream'][0]['textblockId'] == DOCSTREAM_DATA['itemStream'][1]['textblockId']


def dicts_match(d1: dict, d2: dict, path="") -> bool:
    for k in d1:
        if (k not in d2):
            print (path, ":")
            print (k + " as key not in d2", "\n")
            return False
        else:
            if type(d1[k]) is dict:
                if path == "":
                    path = k
                else:
                    path = path + "->" + k
                dicts_match(d1[k],d2[k], path)
            else:
                if d1[k] != d2[k]:
                    print (path, ":")
                    print (" - ", k," : ", d1[k])
                    print (" + ", k," : ", d2[k])
                    return False
                else:
                    return True


## Check this: https://stackoverflow.com/questions/27265939/comparing-python-dictionaries-and-nested-dictionaries
# d1= {'a':{'b':{'cs':10},'d':{'cs':20}}}
# d2= {'a':{'b':{'cs':30} ,'d':{'cs':20}},'newa':{'q':{'cs':50}}}
#
# def findDiff(d1, d2, path=""):
#     for k in d1:
#         if (k not in d2):
#             print (path, ":")
#             print (k + " as key not in d2", "\n")
#         else:
#             if type(d1[k]) is dict:
#                 if path == "":
#                     path = k
#                 else:
#                     path = path + "->" + k
#                 findDiff(d1[k],d2[k], path)
#             else:
#                 if d1[k] != d2[k]:
#                     print (path, ":")
#                     print (" - ", k," : ", d1[k])
#                     print (" + ", k," : ", d2[k])
#
# print ("comparing d1 to d2:")
# print (findDiff(d1,d2))
# print ("comparing d2 to d1:")
# print (findDiff(d2,d1))
