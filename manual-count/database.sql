--
-- Database: `urenloop`
--

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
(2, 'VRG', 0),
(3, 'VEK', 0),
(4, 'Blandinia', 0),
(5, 'Wetenschappen & VLAK', 0),
(6, 'VLK', 0),
(7, 'VTK', 0),
(8, 'VPPK', 0),
(9, 'HILOK', 0),
(10, 'GFK/VGK/VBK/Dentalia', 0),
(11, 'SK', 0),
(12, 'HK', 0),
(13, 'Politeia', 0),
(14, 'Kofschipclubs', 0),
(15, 'Curatio', 0)
