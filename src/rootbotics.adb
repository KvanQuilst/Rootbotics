with Ada.Text_IO; use Ada.Text_IO;

with Root; use Root;
with Root.Map;
with Root.Eyrie;

procedure Rootbotics is
  IL : Input_List (1..12);
begin
  IL := Get_List ("Values");
  for I in IL'Range loop
    Put_Line (IL (I)'Image);
  end loop;

  --while not Root.Eyrie.Setup (3, Default) loop null; end loop;
  --Root.Eyrie.Take_Turn (Rabbit, Root.Map.Fall_Map);
end Rootbotics;
