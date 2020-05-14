using datakam.ValueObjects;
using GraphQL.Types;
using Microsoft.AspNetCore.Components.Forms;

namespace datakam.MiddleWare
{
    #region TextBlockType

    public class InputTextBlockType : InputObjectGraphType<TextBlock>
    {
        public InputTextBlockType()
        {
            Name = "InputTextBlock";
            Field(x => x.ParentDocumentID, type: typeof(StringGraphType)).Description("ParentDocumentID");
            Field(x => x.Type, type: typeof(StringGraphType)).Description("Type");
            Field(x => x.TextBlockID, type: typeof(StringGraphType)).Description("TextBlockID");
            Field(x => x.UserID, type: typeof(StringGraphType)).Description("UserID");
            Field(x => x.Tags, type: typeof(ListGraphType<StringGraphType>)).Description("Tags");
            Field(x => x.Text, type: typeof(StringGraphType)).Description("Text");
            Field(x => x.Author, type: typeof(StringGraphType)).Description("Author");
            Field(x => x.Position, type: typeof(IntGraphType)).Description("Position");
        }
    }
    
    #endregion

    #region TextBlockType

    public class TextBlockType : ObjectGraphType<TextBlock>
    {
        public TextBlockType()
        {
            Name = "TextBlock";
            Field(x => x.ParentDocumentID, type: typeof(StringGraphType)).Description("ParentDocumentID");
            Field(x => x.Type, type: typeof(StringGraphType)).Description("Type");
            Field(x => x.TextBlockID, type: typeof(StringGraphType)).Description("TextBlockID");
            Field(x => x.UserID, type: typeof(StringGraphType)).Description("UserID");
            Field(x => x.Tags, type: typeof(ListGraphType<StringGraphType>)).Description("Tags");
            Field(x => x.Score, type: typeof(IntGraphType)).Description("Score");
            Field(x => x.Version, type: typeof(IntGraphType)).Description("Version");
            Field(x => x.Position, type: typeof(IntGraphType)).Description("Position");
            Field(x => x.CreatedAt, type: typeof(DateTimeGraphType)).Description("CreatedAt");
            Field(x => x.UpdatedAt, type: typeof(DateTimeGraphType)).Description("UpdatedAt");
            Field(x => x.Text, type: typeof(StringGraphType)).Description("Text");
            Field(x => x.Author, type: typeof(StringGraphType)).Description("Author");
        }
    }
    

    #endregion
    
    #region InputMediaType

    public class InputMediaType : InputObjectGraphType<Media>
    {
        public InputMediaType()
        {
            Name = "InputMedia";
            Field(x => x.ParentDocumentID, type: typeof(StringGraphType)).Description("ParentDocumentID");
            Field(x => x.Type, type: typeof(StringGraphType)).Description("Type");
            Field(x => x.MediaID, type: typeof(StringGraphType)).Description("MediaID");
            Field(x => x.UserID, type: typeof(StringGraphType)).Description("UserID");
            Field(x => x.Tags, type: typeof(ListGraphType<StringGraphType>)).Description("Tags");
            Field(x => x.FileUrl, type: typeof(StringGraphType)).Description("FileUrl");
            Field(x => x.Position, type: typeof(IntGraphType)).Description("Position");
        }
    }

    #endregion
    
    #region MediaType

    public class MediaType : ObjectGraphType<Media>
    {
        public MediaType()
        {
            Name = "Media";
            Field(x => x.ParentDocumentID, type: typeof(StringGraphType)).Description("ParentDocumentID");
            Field(x => x.Type, type: typeof(StringGraphType)).Description("Type");
            Field(x => x.MediaID, type: typeof(StringGraphType)).Description("MediaID");
            Field(x => x.UserID, type: typeof(StringGraphType)).Description("UserID");
            Field(x => x.Tags, type: typeof(ListGraphType<StringGraphType>)).Description("Tags");
            Field(x => x.Score, type: typeof(IntGraphType)).Description("Score");
            Field(x => x.Version, type: typeof(IntGraphType)).Description("Version");
            Field(x => x.Position, type: typeof(IntGraphType)).Description("Position");
            Field(x => x.CreatedAt, type: typeof(DateTimeGraphType)).Description("CreatedAt");
            Field(x => x.UpdatedAt, type: typeof(DateTimeGraphType)).Description("UpdatedAt");
            Field(x => x.FileUrl, type: typeof(StringGraphType)).Description("FileUrl");
        }
    }

    #endregion
    
    #region DocStreamItemType

    public class DocStreamItemType : UnionGraphType
    {
        public DocStreamItemType()
        {
            Type<MediaType>();
            Type<TextBlockType>();
        }
    }

    #endregion
    
    #region InputDocStreamType

    public class InputDocStreamType : InputObjectGraphType<DocStream>
    {
        public InputDocStreamType()
        {
            Name = "InputDocStream";
            Field(x => x.Namespace, type: typeof(StringGraphType)).Description("Namespace");
            Field(x => x.ContentID, type: typeof(StringGraphType)).Description("ContentID");
            Field(x => x.UserID, type: typeof(StringGraphType)).Description("UserID");
            Field(x => x.Tags, type: typeof(ListGraphType<StringGraphType>)).Description("Tags");
            Field(x => x.Media, type: typeof(ListGraphType<InputMediaType>)).Description("Media");
            Field(x => x.TextBlocks, type: typeof(ListGraphType<InputTextBlockType>)).Description("TextBlocks");
            // Field(x => x.ItemsStream, type: typeof(ListGraphType<DocStreamItemType>)).Description("ItemsStream");
        }
    }

    #endregion
    
    #region InputDocStreamType

    public class DocStreamType : ObjectGraphType<DocStream>
    {
        public DocStreamType()
        {
            Name = "DocStream";
            Field(x => x.Namespace, type: typeof(StringGraphType)).Description("Namespace");
            Field(x => x.ContentID, type: typeof(StringGraphType)).Description("ContentID");
            Field(x => x.UserID, type: typeof(StringGraphType)).Description("UserID");
            Field(x => x.Tags, type: typeof(ListGraphType<StringGraphType>)).Description("Tags");
            Field(x => x.Score, type: typeof(IntGraphType)).Description("Score");
            Field(x => x.Version, type: typeof(IntGraphType)).Description("Version");
            Field(x => x.CreatedAt, type: typeof(DateTimeGraphType)).Description("CreatedAt");
            Field(x => x.UpdatedAt, type: typeof(DateTimeGraphType)).Description("UpdatedAt");
            Field(x => x.ItemsStream, type: typeof(ListGraphType<DocStreamItemType>)).Description("ItemsStream");
        }
    }

    #endregion
    
    #region InputDocumentType

    public class InputDocumentType : InputObjectGraphType<Document>
    {
        public InputDocumentType()
        {
            Name = "InputDocument";
            Field(x => x.Topic, type: typeof(StringGraphType)).Description("Topic");
            Field(x => x.DocumentID, type: typeof(StringGraphType)).Description("DocumentID");
            Field(x => x.UserID, type: typeof(StringGraphType)).Description("UserID");
            Field(x => x.Tags, type: typeof(ListGraphType<StringGraphType>)).Description("Tags");
            Field(x => x.Slug, type: typeof(StringGraphType)).Description("Slug");
            Field(x => x.Title, type: typeof(StringGraphType)).Description("Title");
            Field(x => x.Body, type: typeof(StringGraphType)).Description("Body");
            Field(x => x.Identifier, type: typeof(StringGraphType)).Description("Identifier");
            Field(x => x.Publish, type: typeof(BooleanGraphType)).Description("Publish");
            Field(x => x.FiltreVisuel, type: typeof(IntGraphType)).Description("FiltreVisuel");
            Field(x => x.Langue, type: typeof(IntGraphType)).Description("Langue");
            Field(x => x.Niveau, type: typeof(IntGraphType)).Description("Niveau");
            Field(x => x.Media, type: typeof(ListGraphType<InputMediaType>)).Description("Media");
        }
    }

    #endregion

    #region DocumentType

    public class DocumentType : ObjectGraphType<Document>
    {
        public DocumentType()
        {
            Name = "Document";
            Field(x => x.Topic, type: typeof(StringGraphType)).Description("Topic");
            Field(x => x.DocumentID, type: typeof(StringGraphType)).Description("DocumentID");
            Field(x => x.UserID, type: typeof(StringGraphType)).Description("UserID");
            Field(x => x.Tags, type: typeof(ListGraphType<StringGraphType>)).Description("Tags");
            Field(x => x.Score, type: typeof(IntGraphType)).Description("Score");
            Field(x => x.Version, type: typeof(IntGraphType)).Description("Version");
            Field(x => x.CreatedAt, type: typeof(DateTimeGraphType)).Description("CreatedAt");
            Field(x => x.UpdatedAt, type: typeof(DateTimeGraphType)).Description("UpdatedAt");
            Field(x => x.Slug, type: typeof(StringGraphType)).Description("Slug");
            Field(x => x.Title, type: typeof(StringGraphType)).Description("Title");
            Field(x => x.Body, type: typeof(StringGraphType)).Description("Body");
            Field(x => x.Identifier, type: typeof(StringGraphType)).Description("Identifier");
            Field(x => x.Publish, type: typeof(BooleanGraphType)).Description("Publish");
            Field(x => x.FiltreVisuel, type: typeof(IntGraphType)).Description("FiltreVisuel");
            Field(x => x.Langue, type: typeof(IntGraphType)).Description("Langue");
            Field(x => x.Niveau, type: typeof(IntGraphType)).Description("Niveau");
            Field(x => x.Media, type: typeof(ListGraphType<MediaType>)).Description("Media");
        }
    }

    #endregion
    
    #region ContentType

    public class ContentType : UnionGraphType
    {
        public ContentType()
        {
            Type<DocumentType>();
            Type<DocStreamType>();
        }
    }

    #endregion

    #region User

    public class UserType : ObjectGraphType<User>
    {
        public UserType()
        {
            Name = "User";
            Field(x => x.UserID, type: typeof(StringGraphType)).Description("UserID");
            Field(x => x.Email, type: typeof(StringGraphType)).Description("Email");
            Field(x => x.Username, type: typeof(StringGraphType)).Description("Username");
            Field(x => x.Role, type: typeof(IntGraphType)).Description("Role");
            Field(x => x.Confirmed, type: typeof(BooleanGraphType)).Description("Confirmed");
            Field(x => x.PasswordHash, type: typeof(StringGraphType)).Description("PasswordHash");
            Field(x => x.LastSeen, type: typeof(DateTimeGraphType)).Description("LastSeen");
        }
    }

    #endregion
    
    #region UserProfile

    public class UserProfileType : ObjectGraphType<UserProfile>
    {
        public UserProfileType()
        {
            Name = "UserProfile";
            Field(x => x.UserID, type: typeof(StringGraphType)).Description("UserID");
            Field(x => x.Name, type: typeof(StringGraphType)).Description("Name");
            Field(x => x.Age, type: typeof(IntGraphType)).Description("Age");
            Field(x => x.AvatarHash, type: typeof(StringGraphType)).Description("AvatarHash");
            Field(x => x.Location, type: typeof(StringGraphType)).Description("Location");
            Field(x => x.MemberSince, type: typeof(DateTimeGraphType)).Description("MemberSince");
            Field(x => x.AboutMe, type: typeof(StringGraphType)).Description("AboutMe");
        }
    }

    #endregion

    #region PutResult

    public class PutResultType : ObjectGraphType<PutResult>
    {
        public PutResultType()
        {
            Name = "PutResult";
            Field(x => x.Count, type: typeof(StringGraphType)).Description("Count");
            Field(x => x.Keys, type: typeof(ListGraphType<StringGraphType>)).Description("Keys");
        }
    }

    #endregion
}