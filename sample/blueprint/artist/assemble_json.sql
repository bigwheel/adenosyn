SELECT *
FROM artist
  JOIN artist_kana
    ON artist.id = artist_kana.artist_id
  JOIN music
    ON artist.id = music.artist_id
  JOIN content
    ON music.id = content.music_id
;


SELECT *
FROM music
  JOIN content
    ON music.id = content.music_id
;

SELECT music.id,
  CONCAT(
      '{',
      '"name": "', music.name, '", ',
      '"content": [', GROUP_CONCAT(CONCAT('"', content.name, '"') SEPARATOR ', '), ']',
      '}'
  ) AS json
FROM music
JOIN content
ON music.id = content.music_id
GROUP BY music.id
;

SELECT
  music.*,
  CONCAT(
      '{',
      '"name":"', music.name, '",',
      '"contents":[', GROUP_CONCAT(contents.json SEPARATOR ','), ']',
      '}'
  ) AS json
FROM music
JOIN
(
  SELECT
    content.*,
    CONCAT(
        '{',
        '"name":"', name, '"',
        '}'
    ) AS json
  FROM content
) AS contents
ON music.id = contents.music_id
GROUP BY music.id
;


SELECT
  artist.*,
  CONCAT(
      '{',
      '"name":"', artist.name, '"',
      '}'
  ) AS json
FROM
  artist
;

EXPLAIN SELECT
  artist.*,
  CONCAT(
      '{',
      '"name":', '"', artist.name, '"', ',',
      '"musics":', '[', GROUP_CONCAT('"', music.name, '"' SEPARATOR ','), ']',
      '}'
  ) AS json
FROM artist
JOIN music
ON artist.id = music.artist_id
GROUP BY artist.id;

EXPLAIN SELECT
  artist.*,
  CONCAT(
      '{',
      '"name":', '"', artist.name, '"', ',',
      '"musics":', '[', musics.names, ']',
      '}'
  ) AS json
FROM artist
JOIN (
    SELECT
      music.artist_id,
      GROUP_CONCAT('"', music.name, '"' SEPARATOR ',') AS names
    FROM music
    GROUP BY music.artist_id
) as musics
ON artist.id = musics.artist_id
;



CREATE VIEW v_musics AS
SELECT
  music.artist_id,
  CONCAT('[', GROUP_CONCAT('"', music.name, '"' SEPARATOR ','), ']') AS names
FROM music
GROUP BY music.artist_id
;

SELECT * FROM v_musics;

SELECT
  artist.*,
  CONCAT(
      '{',
      '"name":', '"', artist.name, '"', ',',
      '"musics":', v_musics.names,
      '}'
  ) AS json
FROM artist
JOIN v_musics
ON artist.id = v_musics.artist_id
;

DROP VIEW v_musics;
