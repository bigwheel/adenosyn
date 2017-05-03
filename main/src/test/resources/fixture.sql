drop table if exists artist;
drop table if exists artist_kana;
drop table if exists music;
drop table if exists content;
drop table if exists tieup;

create table artist (
  id   INT  NOT NULL PRIMARY KEY AUTO_INCREMENT,
  name TEXT NOT NULL
);
create table artist_kana (
  artist_id INT  NOT NULL PRIMARY KEY,
  kana      TEXT NOT NULL
);
create table music (
  id        INT  NOT NULL PRIMARY KEY AUTO_INCREMENT,
  artist_id INT  NOT NULL,
  name      TEXT NOT NULL,
  INDEX index_artist_id(artist_id)
);
create table content (
  id       INT  NOT NULL PRIMARY KEY AUTO_INCREMENT,
  music_id INT  NOT NULL,
  name     TEXT NOT NULL,
  INDEX index_music_id(music_id)
);
create table tieup (
  id       INT  NOT NULL PRIMARY KEY AUTO_INCREMENT,
  music_id INT  NOT NULL,
  name     TEXT NOT NULL,
  INDEX index_music_id(music_id)
);

INSERT INTO artist(id, name)             VALUES (1, "水樹奈々");
INSERT INTO artist_kana(artist_id, kana) VALUES (1, "みずきなな");
INSERT INTO music(id, artist_id, name)   VALUES (11, 1, "深愛");
INSERT INTO music(id, artist_id, name)   VALUES (12, 1, "innocent starter");
INSERT INTO content(id, music_id, name)  VALUES (111, 11, "深愛 - ショートVer.");
INSERT INTO content(id, music_id, name)  VALUES (112, 11, "深愛 - ロングVer.");
INSERT INTO content(id, music_id, name)  VALUES (121, 12, "innocent starter(inst)");
INSERT INTO tieup(id, music_id, name)    VALUES (1101, 11, "White Album 2");
INSERT INTO tieup(id, music_id, name)    VALUES (1201, 12, "魔法少女リリカルなのは");
