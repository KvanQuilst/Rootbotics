with Ada.Text_IO; use Ada.Text_IO;

with Root; use Root;
with Root.Map;
with Root.Eyrie;

procedure Rootbotics is
  VERSION : String := "v0.1";

  OL : Option_List (1..4);
begin
  Put_Line ("Welcome to the Rootbotics Logic Tool " & VERSION & "!");
  New_Line;
  Put_Line ("Which of the following clockwork factions will you be playing with:");
  Put_Line ("--------------------");
  Put_Line (" a. Mechanical Marquise 2.0");
  Put_Line (" b. Electric Eyrie");
  Put_Line (" c. Automated Alliance");
  Put_Line (" d. Vagabot");
  Put_Line ("    No specified options to quit");
  New_Line;
  Put_Line ("More factions to get added later!");
  Put_Line ("--------------------");

  OL := Get_List (4);

  if OL (1) = Character'Val (0) then
    return;
  end if;

  while not Root.Eyrie.Setup (3, Default) loop null; end loop;
  Root.Eyrie.Take_Turn (Rabbit, Root.Map.Fall_Map);
end Rootbotics;
