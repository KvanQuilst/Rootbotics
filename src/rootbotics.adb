with Ada.Text_IO; use Ada.Text_IO;

with Root; use Root;
with Root.Map;
with Root.Eyrie;

procedure Rootbotics is
  VERSION : constant String := "v0.1";

  OL : Option_List (1..4);
  
  -- In order of setup priority --
  type Faction is (Marquise, Eyrie, Alliance, Vagabot);
  Playing : array (Faction'Range) of Boolean := (others => False);
begin
  Put_Line ("Welcome to the Rootbotics Logic Tool " & VERSION & "!");
  New_Line;
  Put_Line ("Which of the following clockwork factions will you be playing with:");
  Put_Line ("-------------------------");
  Put_Line (" a. Mechanical Marquise 2.0");
  Put_Line (" b. Electric Eyrie");
  Put_Line (" c. Automated Alliance");
  Put_Line (" d. Vagabot");
  Put_Line ("    No specified options to quit");
  New_Line;
  Put_Line ("More factions to get added later!");
  Put_Line ("-------------------------");

  Get_List (OL);

  if OL (1) = Character'Val (0) then
    return;
  end if;

  for I in OL'Range loop
    if OL (I) /= Character'Val (0) then
      Playing (Faction'Val (Character'Pos (OL (I)) - 97)) := True;
    end if;
  end loop;

  -- Faction Setup in Order --
  for I in Playing'Range loop
    if Playing (I) then
      case I is
        when Marquise =>
          Put_Line ("The Mechanical Marquise 2.0 is unimplemented!");

        when Eyrie =>
          declare
            Corner : Integer range 1..4;
          begin
            Put ("Which corner clearing will the Electric Eyrie start in: ");
            Get_Input (Corner, 1, 4);
            while not Root.Eyrie.Setup (Corner, Default) loop
              Put ("Which corner clearing will the Electric Eyrie start in: ");
              Get_Input (Corner, 1, 4);
            end loop;
          end;

        when Alliance =>
          Put_Line ("The Automated Alliance is unimplmented!");

        when Vagabot =>
          Put_Line ("The Vagabot is unimplemented!");

      end case;
    end if;
  end loop;

  loop
    Root.Eyrie.Take_Turn (Rabbit, Root.Map.Fall_Map);
  end loop;
end Rootbotics;
