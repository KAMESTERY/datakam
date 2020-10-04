package com.kamestery.data.content

import software.amazon.awssdk.services.dynamodb.model.AttributeValue

data class ContentRef(var namespace: String = "", var contentID: String = "") {
    fun key(): Map<String, AttributeValue> {
        return mapOf(
            Content.NAMESPACE to AttributeValue.builder().s(namespace).build(),
            Content.CONTENTID to AttributeValue.builder().s(contentID).build()
        )
    }
}