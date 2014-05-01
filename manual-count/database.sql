--
-- Database: `urenloop`
--

DROP TABLE IF EXISTS `teams`, `laps`;

CREATE TABLE  `teams` (
  `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY ,
  `name` VARCHAR( 255 ) NOT NULL ,
  `laps` INT NOT NULL
) ENGINE = INNODB CHARACTER SET utf8 COLLATE utf8_general_ci;

CREATE TABLE `laps` (
  `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY ,
  `team_id` INT NOT NULL ,
  `value` INT NOT NULL ,
  `time` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ,
  KEY `index` (`team_id`,`time`)
) ENGINE = INNODB CHARACTER SET utf8 COLLATE utf8_general_ci;

-- Sample teams

INSERT INTO `teams` (`id`, `name`, `laps`) VALUES
(1, 'KVHV', 0),
(2, 'Kofschipclubs', 0),
(3, 'Wetenschappen & VLAK', 0),
(4, 'Blandinia', 0),
(5, 'VEK', 0),
(6, 'VLK', 0),
(7, 'VTK', 0),
(8, 'HILOK', 0),
(9, 'VBK', 0),
(10, 'VGK', 0),
(11, 'Politeia', 0),
(12, 'VPPK', 0),
(13, 'Veto, Moeder Lies, LILA & Hermes', 0),
(14, 'VRG & Farma', 0),
(15, 'SeniorenKonvent', 0),
(16, 'Home Konvent', 0),
(17, 'Urgent & Schamper', 0)
