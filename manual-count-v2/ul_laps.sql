--
-- Database: `urenloop`
--

-- --------------------------------------------------------

--
-- Tabelstructuur voor tabel `ul_laps`
--

CREATE TABLE IF NOT EXISTS `ul_laps` (
  `id` int(2) NOT NULL AUTO_INCREMENT,
  `naam` varchar(255) NOT NULL,
  `laps` int(5) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=13 ;

--
-- Gegevens worden uitgevoerd voor tabel `ul_laps`
--

INSERT INTO `ul_laps` (`id`, `naam`, `laps`) VALUES
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