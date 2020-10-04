package com.kamestery.data.content

import java.time.Instant

data class Content(
        var namespace: String = "",
        var contentID: String = "",
        var userID: String = "",
        var score: Int = 0,
        var version: Int = 0,
        var createdAt: Instant = Instant.now(),
        var updatedAt: Instant = Instant.now()
) {

    companion object {
        const val CONTENT_TBL: String = "Content"

        const val NAMESPACE = "Namespace"
        const val CONTENTID = "ContentID"
        const val USERID = "UserID"
        const val SCORE = "Score"
        const val VERSION = "Version"
        const val CREATEDAT = "CreatedAt"
        const val UPDATEDAT = "UpdatedAt"

        fun attributes() = setOf(
                NAMESPACE, CONTENTID, USERID, SCORE, VERSION, CREATEDAT, UPDATEDAT
        )
    }
}