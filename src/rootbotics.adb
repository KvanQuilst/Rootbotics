with Ada.Text_IO; use Ada.Text_IO;

with Root; use Root;
with Root.Map;
with Root.Eyrie;

procedure Rootbotics is
begin
  while not Root.Eyrie.Setup (3, Default) loop null; end loop;
  Root.Eyrie.Take_Turn (Rabbit, Root.Map.Fall_Map);
end Rootbotics;
