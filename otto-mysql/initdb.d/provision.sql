CREATE USER 'youseibox'@'%' IDENTIFIED BY 'youseibox' ;
CREATE DATABASE youseibox;
CREATE DATABASE youseibox_test;
GRANT ALL ON youseibox.*      TO 'youseibox'@'%' ;
GRANT ALL ON youseibox_test.* TO 'youseibox'@'%' ;
FLUSH PRIVILEGES ;
