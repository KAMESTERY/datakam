package com.kamestery.data.content

import io.quarkus.runtime.annotations.RegisterForReflection
import software.amazon.awssdk.services.dynamodb.model.AttributeValue
import java.time.Instant

@RegisterForReflection
data class Document(
        var topic: String = "",
        var documentID: String = "",
        var userID: String = "",
        var tags: List<String> = emptyList(),
        var score: Int = 0,
        var version: Int = 0,
        val createdAt: String = Instant.now().toString(),
        val updatedAt: String = Instant.now().toString(),
        var slug: String = "",
        var title: String = "",
        var identifier: String = "",
        var body: String = "",
        var publish: Boolean = false,
        var filtreVisuel: Int = FiltreVisuel.IG_WILLOW.value,
        var langue: Int = Langue.ENGLISH.value,
        var niveau: Int = Niveau.LOW.value,
        var media: List<Media> = emptyList()
) {
    enum class FiltreVisuel(val value: Int) {
        IG_WILLOW(0),
        IG_WALDEN(1),
        IG_VALENCIA(2),
        IG_TOASTER(3),
        IG_SUTRO(4),
        IG_SIERRA(5),
        IG_RISE(6),
        IG_NASHVILLE(7),
        IG_MAYFAIR(8),
        IG_LOFI(9),
        IG_KELVIN(10),
        IG_INKWELL(11),
        IG_HUDSON(12),
        IG_HEFE(13),
        IG_EARLYBIRD(14),
        IG_BRANNAN(15),
        IG_AMARO(16),
        IG_1977(17)
    }

    enum class Langue(val value: Int) {
        ENGLISH(0),
        FRENCH(1),
        PORTUGUESE(2),
        SPANISH(3),
        SWAHILI(4)
    }

    enum class Niveau(val value: Int) {
        LOW(0),
        MEDIUM(1),
        HIGH(2),
        CRITICAL(3)
    }

    companion object {
        const val SLUG: String = "Slug"
        const val TITLE: String = "Title"
        const val IDENTIFIER: String = "Identifier"
        const val BODY: String = "Body"
        const val PUBLISH: String = "Publish"
        const val FILTREVISUEL: String = "FiltreVisuel"
        const val LANGUE: String = "Langue"
        const val NIVEAU: String = "Niveau"
        const val TAGS: String = "Tags"

        fun attributes() = setOf(
                SLUG, TITLE, IDENTIFIER, BODY, PUBLISH, FILTREVISUEL, LANGUE, NIVEAU, TAGS
        ) + Content.attributes()

        fun from(item: Map<String, AttributeValue>?): Document? {
            return item?.get(Content.CONTENTID)?.let {
                Document(
                        topic = item.get(Content.NAMESPACE)?.s() ?: "",
                        documentID = item.get(Content.CONTENTID)?.s() ?: "",
                        userID = item.get(Content.USERID)?.s() ?: "",
                        tags = item.get(TAGS)?.ss() ?: emptyList(),
                        score = item.get(Content.SCORE)?.n()?.toInt() ?: 0,
                        version = item.get(Content.VERSION)?.n()?.toInt() ?: 0,
                        createdAt = item.get(Content.CREATEDAT)?.s() ?: "",
                        updatedAt = item.get(Content.UPDATEDAT)?.s() ?: "",
                        slug = item.get(SLUG)?.s() ?: "",
                        title = item.get(TITLE)?.s() ?: "",
                        identifier = item.get(IDENTIFIER)?.s() ?: "",
                        body = item.get(BODY)?.s() ?: "",
                        publish = item.get(PUBLISH)?.bool() ?: false,
                        filtreVisuel = item.get(FILTREVISUEL)?.n()?.toInt() ?: 0,
                        langue = item.get(LANGUE)?.n()?.toInt() ?: 0,
                        niveau = item.get(NIVEAU)?.n()?.toInt() ?: 0
                )
            }
        }
    }

    fun asInput(): Map<String, AttributeValue> {
        return mapOf(
            Content.NAMESPACE to AttributeValue.builder().s(this.topic).build(),
            Content.CONTENTID to AttributeValue.builder().s(this.documentID).build(),
            Content.USERID to AttributeValue.builder().s(this.userID).build(),
            TAGS to AttributeValue.builder().ss(this.tags).build(),
            Content.SCORE to AttributeValue.builder().n(this.score.toString()).build(),
            Content.VERSION to AttributeValue.builder().n(this.version.toString()).build(),
            Content.CREATEDAT to AttributeValue.builder().s(this.createdAt).build(),
            Content.UPDATEDAT to AttributeValue.builder().s(this.updatedAt).build(),
            SLUG to AttributeValue.builder().s(this.slug).build(),
            TITLE to AttributeValue.builder().s(this.title).build(),
            IDENTIFIER to AttributeValue.builder().s(this.identifier).build(),
            BODY to AttributeValue.builder().s(this.body).build(),
            PUBLISH to AttributeValue.builder().bool(this.publish).build(),
            FILTREVISUEL to AttributeValue.builder().n(this.filtreVisuel.toString()).build(),
            LANGUE to AttributeValue.builder().n(this.langue.toString()).build(),
            NIVEAU to AttributeValue.builder().n(this.niveau.toString()).build()
        )
    }


    fun ref() = ContentRef(
            namespace = this.topic,
            contentID = this.documentID
    )
}