package com.kamestery.services

import com.kamestery.data.content.Content
import com.kamestery.data.content.ContentRef
import com.kamestery.data.content.Document
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service
import software.amazon.awssdk.services.dynamodb.DynamoDbClient
import kotlin.streams.toList

@Service
class ContentService {

    @Autowired
    lateinit var dynamoDB: DynamoDbClient


    fun createDocument(doc: Document): ContentRef? {
        val request = DynamoRequestBuilder(Content.CONTENT_TBL).putRequest(doc.asInput())
        val res = dynamoDB.putItem(request)
        return Document.from(res.attributes())?.ref()
    }

    fun getDocument(cr: ContentRef): Document? {
        val request = DynamoRequestBuilder(Content.CONTENT_TBL).getRequest(cr.key())
        val res = dynamoDB.getItem(request).item()
        System.out.println(res)
        return Document.from(res)
    }

    fun updateDocument(doc: Document): ContentRef? {
        val request = DynamoRequestBuilder(Content.CONTENT_TBL).putRequest(doc.asInput())
        val res = dynamoDB.putItem(request)
        return Document.from(res.attributes())?.ref()
    }

    fun listDocuments(): Set<Document?> {
        val request = DynamoRequestBuilder(Content.CONTENT_TBL).scanRequest(Document.attributes().toList())
        return dynamoDB.scanPaginator(request).items().stream().map {
            it -> Document.from(it)
        }.toList().toSet()
    }

    fun deleteDocument(cr: ContentRef): ContentRef? {
        val request = DynamoRequestBuilder(Content.CONTENT_TBL).deleteRequest(cr.key())
        val res = dynamoDB.deleteItem(request)
        return Document.from(res.attributes())?.ref()
    }
}
