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
(1, 'HILOK', 0),
(2, 'VGK', 0),
(3, 'VTK', 0),
(4, 'VLK', 0),
(5, 'VRG', 0),
(6, 'VEK & Moeder Lies', 0),
(7, 'VPPK', 0),
(8, 'Hermes & LILA', 0),
(9, 'Wetenschappen & VLAK', 0),
(10, 'VBK', 0),
(11, 'HK', 0),
(12, 'SK', 0),
(13, 'Zeus WPI', 0)
