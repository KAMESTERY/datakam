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
        return if (getDocument(doc.ref()) != null) null else {
            val input = doc.asInput()
            var request = DynamoRequestBuilder(Content.CONTENT_TBL).putRequest(input)
            dynamoDB.putItem(request)
            // doc.media.map {
            //     m -> {
            //         m.parentDocumentID = doc.documentID
            //         val mInput = m.asInput()
            //         var mRequest = DynamoRequestBuilder(Content.CONTENT_TBL).putRequest(mInput)
            //         dynamoDB.putItem(mRequest)
            //     }
            // }
            doc.ref()
        }
    }

    fun getDocument(cr: ContentRef): Document? {
        val request = DynamoRequestBuilder(Content.CONTENT_TBL).getRequest(
                cr.key(),
                Document.attributes()
        )
        val res = dynamoDB.getItem(request).item()
        return Document.from(res)
    }

    fun updateDocument(doc: Document): ContentRef? {
        val ref = getDocument(doc.ref())?.let {
            val input = doc.asInput()
            val request = DynamoRequestBuilder(Content.CONTENT_TBL).putRequest(input)
            dynamoDB.putItem(request)
            doc.ref()
        }
        return ref
    }

    fun listDocuments(): Set<Document?> {
        val request = DynamoRequestBuilder(Content.CONTENT_TBL).scanRequest(Document.attributes())
        return dynamoDB.scanPaginator(request).items().stream().map {
            it -> Document.from(it)
        }.toList().toSet()
    }

    fun deleteDocument(cr: ContentRef): ContentRef? {
        val ref = getDocument(cr)?.let {
            val request = DynamoRequestBuilder(Content.CONTENT_TBL).deleteRequest(cr.key())
            dynamoDB.deleteItem(request)
            cr
        }
        return ref
    }
}
