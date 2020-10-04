package com.kamestery.services

import software.amazon.awssdk.services.dynamodb.model.AttributeValue
import software.amazon.awssdk.services.dynamodb.model.DeleteItemRequest
import software.amazon.awssdk.services.dynamodb.model.GetItemRequest
import software.amazon.awssdk.services.dynamodb.model.PutItemRequest
import software.amazon.awssdk.services.dynamodb.model.ScanRequest

class DynamoRequestBuilder(val tableName: String) {

    fun scanRequest(selection: List<String> = emptyList()) =
            ScanRequest.builder()
                    .tableName(this.tableName)
                    .attributesToGet(selection).build()

    fun putRequest(item: Map<String, AttributeValue>) =
            PutItemRequest.builder()
                    .tableName(tableName)
                    .item(item).build()

    fun getRequest(key: Map<String, AttributeValue>, selection: List<String> = emptyList()) =
            GetItemRequest.builder()
                    .tableName(tableName)
                    .key(key)
                    .attributesToGet(selection).build()

    fun deleteRequest(key: Map<String, AttributeValue>) =
            DeleteItemRequest.builder()
                    .tableName(tableName)
                    .key(key).build()

    companion object {

        const val USER: String = "User"
        const val USERGROUP: String = "UserGroup"
        const val USERPROFILE: String = "UserProfile"
    }
}