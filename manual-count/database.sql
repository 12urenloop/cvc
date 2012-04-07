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
(1, 'FK 1', 13),
(2, 'FK 2', 14),
(3, 'FK 3', 15),
(4, 'FK 4', 13),
(5, 'Samson', 11),
(6, 'Gert', 12),
(7, 'De burgemeester', 14),
(8, 'Albertoooo', 12),
(9, 'Octaaf', 9),
(10, 'Marlene', 11),
(11, 'Bobientje', 11),
(12, 'Van Leemhuyzen', 9);
