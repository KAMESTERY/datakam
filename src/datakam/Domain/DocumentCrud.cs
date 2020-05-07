using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using datakam.ValueObjects;
using static datakam.DataAccessLayer.DAL;

namespace datakam.Domain
{
    public static partial class Domain
    {
        public static async Task<Document> GetDocument(string topic, string document_id)
        {
            var docThing = await GetThing(topic, document_id);

            var document = await AsDocument(docThing);

            return document;
        }
        
        public static async IAsyncEnumerable<Document> GetDocumentsByTopic(string topic)
        {
            await foreach(var docThing in GetThingByName(topic))
            {
                yield return await AsDocument(docThing);
            }
        }

        #region AsDocument

        private static async Task<Document> AsDocument(Thing docThing)
        {
            var document = new Document();
            document.Topic = docThing.Name;
            document.DocumentID = docThing.ThingID;
            document.UserID = docThing.UserID;
            document.Tags = docThing.Tags;
            document.Score = docThing.Score;
            document.Version = docThing.Version;
            document.CreatedAt = docThing.CreatedAt;
            document.UpdatedAt = docThing.UpdatedAt;

            await foreach (var docData in GetData(docThing.ThingID))
            {
                if (Document.SLUG.Equals(docData.Key))
                    document.Slug = docData.Value;
                if (Document.TITLE.Equals(docData.Key))
                    document.Title = docData.Value;
                if (Document.IDENTIFIER.Equals(docData.Key))
                    document.Identifier = docData.Value;
                if (Document.BODY.Equals(docData.Key))
                    document.Body = docData.Value;
                if (Document.PUBLISH.Equals(docData.Key))
                    document.Publish = Convert.ToBoolean(docData.Value);
                if (Document.FILTREVISUEL.Equals(docData.Key))
                    document.FiltreVisuel = Convert.ToInt32(docData.Value);
                if (Document.LANGUE.Equals(docData.Key))
                    document.Langue = Convert.ToInt32(docData.Value);
                if (Document.NIVEAU.Equals(docData.Key))
                    document.Niveau = Convert.ToInt32(docData.Value);
            }

            document.Media = new List<Media>();
            await foreach (var mediaThing in GetThingByName(docThing.ThingID))
            {
                var media = new Media();
                media.ParentDocumentID = mediaThing.Name;
                media.MediaID = mediaThing.ThingID;
                media.UserID = mediaThing.UserID;
                media.Tags = mediaThing.Tags;
                media.Score = mediaThing.Score;
                media.Version = mediaThing.Version;
                media.CreatedAt = mediaThing.CreatedAt;
                media.UpdatedAt = mediaThing.UpdatedAt;
                await foreach (var docData in GetData(docThing.ThingID))
                {
                    if (Media.FILEURL.Equals(docData.Key))
                        media.FileUrl = docData.Value;
                }

                document.Media.Add(media);
            }

            return document;
        }

        #endregion
    }
}