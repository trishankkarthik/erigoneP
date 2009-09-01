-- Copyright 2008-9 by  Mordechai (Moti) Ben-Ari. See version.ads
--
--  File names and extensions
--
package Files is
  Source_Extension:   constant String := ".pml";
  LTL_Extension:      constant String := ".prp";
  Trail_Extension:    constant String := ".trl";
  Automata_Extension: constant String := ".aut";

  Root_File_Name:     access   String;
  Source_File_Name:   access   String;
  LTL_File_Name:      access   String;
  Trail_File_Name:    access   String;
  Automata_File_Name: access   String;
end Files;
