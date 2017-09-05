package model

// PageBase Page Base
type PageBase struct {
	Title               string `db:"title" json:"title"`
	Slug                string `db:"slug" json:"slug"`
	Publish             bool   `db:"publish" json:"publish"`
	Body                string `db:"body" json:"body"`
	Language            string `db:"language" json:"language"`
	TileSize            uint64 `db:"tile_size" json:"title_size"`
	InstagramFilterTile string `db:"instagram_filter_tile" json:"instagram_filter_tile"`
	AutoIncr
}

// MediaBase Media Base
type MediaBase struct {
	Name        string `db:"name" json:"name"`
	Audio       string `db:"audio" json:"audio"`
	Document    string `db:"document" json:"document"`
	Image       string `db:"image" json:"image"`
	Video       string `db:"video" json:"video"`
	AudioURL    string `db:"audio_url" json:"audio_url"`
	DocumentURL string `db:"document_url" json:"document_url"`
	ImageURL    string `db:"image_url" json:"image_url"`
	VideoURL    string `db:"video_url" json:"video_url"`
	Description string `db:"description" json:"description"`
	//	TimeStamps
	AutoIncr
}

//CREATE TABLE education_articlemedia
//(
//id INT PRIMARY KEY NOT NULL AUTO_INCREMENT,
//name VARCHAR(200) NOT NULL,
//audio VARCHAR(200),
//document VARCHAR(200),
//image VARCHAR(200),
//video VARCHAR(200),
//audio_url VARCHAR(200),
//document_url VARCHAR(200),
//image_url VARCHAR(200),
//video_url VARCHAR(200),
//body LONGTEXT NOT NULL,
//created DATETIME NOT NULL,
//updated DATETIME NOT NULL,
//article_id INT NOT NULL,
//FOREIGN KEY (article_id) REFERENCES Education_article (id)
//);
//CREATE INDEX Education_ar_article_id_27f93267a86b6b02_fk_Education_article_id ON Education_articlemedia (article_id);
//CREATE INDEX Education_articlemedia_b068931c ON Education_articlemedia (name);
