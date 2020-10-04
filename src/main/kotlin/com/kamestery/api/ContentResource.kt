package com.kamestery.api

import com.kamestery.data.content.ContentRef
import com.kamestery.data.content.Document
import com.kamestery.services.ContentService
import org.springframework.beans.factory.annotation.Autowired
import javax.enterprise.context.ApplicationScoped
import javax.ws.rs.*
import javax.ws.rs.core.MediaType

@Path("/content")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
@ApplicationScoped
class ContentResource {

    @Autowired
    lateinit var contentService: ContentService

    @Path("document")
    @POST
    fun createDocument(doc: Document) = contentService.createDocument(doc)

    @Path("document")
    @PUT
    fun updateDocument(doc: Document) = contentService.updateDocument(doc)

    @Path("documents")
    @GET
    fun listDocuments() = contentService.listDocuments()

    @Path("document/{topic}/{documentID}")
    @GET
    fun getDocument(
            @PathParam("topic") topic: String,
            @PathParam("documentID") documentID: String
    ): Document? {
        val cr = ContentRef(
                namespace = topic,
                contentID = documentID
        )
        return contentService.getDocument(cr)
    }

    @Path("document/{topic}/{documentID}")
    @DELETE
    fun deleteDocument(
            @PathParam("topic") topic: String,
            @PathParam("documentID") documentID: String
    ): ContentRef? {
        val cr = ContentRef(
                namespace = topic,
                contentID = documentID
        )
        return contentService.deleteDocument(cr)
    }
}