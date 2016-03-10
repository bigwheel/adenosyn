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
