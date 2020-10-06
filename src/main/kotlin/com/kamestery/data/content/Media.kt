package com.kamestery.data.content

import io.quarkus.runtime.annotations.RegisterForReflection
import software.amazon.awssdk.services.dynamodb.model.AttributeValue
import java.time.Instant

@RegisterForReflection
class Media (
    var parentDocumentID: String = "",
    var type: String = Categorie.IMAGE.name,
    var mediaID: String = "",
    var userID: String = "",
    var tags: List<String> = emptyList(),
    var score: Int = 0,
    var version: Int = 0,
    var position: Int = 0,
    val createdAt: String = Instant.now().toString(),
    val updatedAt: String = Instant.now().toString(),
    var fileUrl: String = ""
) {
    enum class Categorie {
        AUDIO,
        DOCUMENT,
        IMAGE,
        VIDEO
    }

    companion object {
        const val FILEURL = "FileUrl"
        const val TYPE = "Type"
        const val TAGS = "Tags"
        const val POSITION = "Position"

        fun attributes() = setOf(
                FILEURL, TYPE, TAGS, POSITION
        ) + Content.attributes()

        fun from(item: Map<String, AttributeValue>?): Media? {
            return item?.get(Content.CONTENTID)?.let {
                Media(
                        parentDocumentID = item.get(Content.NAMESPACE)?.s() ?: "",
                        type = item.get(TYPE)?.s() ?: "",
                        mediaID = item.get(Content.CONTENTID)?.s() ?: "",
                        userID = item.get(Content.USERID)?.s() ?: "",
                        tags = item.get(TAGS)?.ss() ?: emptyList(),
                        score = item.get(Content.SCORE)?.n()?.toInt() ?: 0,
                        version = item.get(Content.VERSION)?.n()?.toInt() ?: 0,
                        position = item.get(Content.VERSION)?.n()?.toInt() ?: 0,
                        createdAt = item.get(Content.CREATEDAT)?.s() ?: "",
                        updatedAt = item.get(Content.UPDATEDAT)?.s() ?: "",
                        fileUrl = item.get(FILEURL)?.s() ?: ""
                )
            }
        }
    }

    fun asInput(): Map<String, AttributeValue> {
        return mapOf(
            Content.NAMESPACE to AttributeValue.builder().s(this.parentDocumentID).build(),
            TYPE to AttributeValue.builder().s(this.type).build(),
            Content.CONTENTID to AttributeValue.builder().s(this.mediaID).build(),
            Content.USERID to AttributeValue.builder().s(this.userID).build(),
            TAGS to AttributeValue.builder().ss(this.tags).build(),
            Content.SCORE to AttributeValue.builder().n(this.score.toString()).build(),
            Content.VERSION to AttributeValue.builder().n(this.version.toString()).build(),
            POSITION to AttributeValue.builder().n(this.position.toString()).build(),
            Content.CREATEDAT to AttributeValue.builder().s(this.createdAt).build(),
            Content.UPDATEDAT to AttributeValue.builder().s(this.updatedAt).build(),
            FILEURL to AttributeValue.builder().s(this.fileUrl).build()
        )
    }


    fun ref() = ContentRef(
            namespace = this.parentDocumentID,
            contentID = this.mediaID
    )
}