--
-- Database: `urenloop`
--

CREATE TABLE  `teams` (
  `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY ,
  `name` VARCHAR( 255 ) NOT NULL ,
  `laps` INT NOT NULL
) ENGINE = INNODB CHARACTER SET utf8 COLLATE utf8_general_ci;

CREATE TABLE  `urenloop`.`laps` (
  `id` INT NOT NULL AUTO_INCREMENT PRIMARY KEY ,
  `team_id` INT NOT NULL ,
  `value` INT NOT NULL ,
  `time` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ,
  KEY `index` (`team_id`,`time`)
) ENGINE = INNODB CHARACTER SET utf8 COLLATE utf8_general_ci;

-- Sample teams

INSERT INTO `teams` (`id`, `name`, `laps`) VALUES
(1, 'HomeKonvent', 0),
(2, 'SeniorenKonvent', 0),
(3, 'Kofschipclubs', 0),
(4, 'VEK', 0),
(5, 'VTK', 0),
(6, 'VLK', 0),
(7, 'Blandinia', 0),
(8, 'Politeia', 0),
(9, 'VRG', 0),
(10, 'Wetenschappen & VLAK', 0),
(11, 'VPPK', 0),
(12, 'VGK & GFK & VBK', 0),
(13, 'KVHV', 0),
(14, 'HILOK', 0)