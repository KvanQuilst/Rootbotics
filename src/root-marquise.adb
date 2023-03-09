with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Root.Marquise is

   procedure Put_Logo is
      Length : constant := 21;
   begin
      Set_Style (Yellow);
      Put_Line_Centered ("      / \     / \      ");
      Put_Line_Centered ("    / /\  \ /  /\ \    ");
      Put_Line_Centered ("  /  /  \  ^  /  \  \  ");
      Put_Line_Centered (" |   __        __    | ");
      Put_Line_Centered (" | / __ \    / __ \  | ");
      Put_Line_Centered ("_|| |__| |  | |__| | |_");
      Put_Line_Centered ("\  \ __ /    \ __ /   /");
      Put_Line_Centered ("\    ..   /\   ..     /");
      Reset_Style;
      Put (To_String (((WIDTH - Length) / 2 + 2) * "-"));
      Set_Style (Yellow);
      Put                 ("\_________________/");
      Reset_Style;
      Put_Line (To_String (((WIDTH - Length) / 2 - 1) * "-"));
   end Put_Logo;

   -------------------
   -- Faction Setup --
   -------------------

   procedure Setup (M : Map) is
      Corner : Integer range 1 .. 4;
   begin
      Put_Line ("Which corner clearing will the " & Name & " start in: ");
      Corner := Get_Integer (1, 4);

      -- Place starting pieces --
      for I in Meeples'Range loop
         Meeples (I) := 1;
      end loop;
      Meeples (Corner) := Meeples (Corner) + 1;

      if M.Name = Lake then
         case Corner is
            when 1 => Meeples (2) := 0;
            when 2 => Meeples (1) := 0;
            when 3 => Meeples (4) := 0;
            when 4 => Meeples (3) := 0;
         end case;
      else
         case Corner is
            when 1 => Meeples (3) := 0;
            when 2 => Meeples (4) := 0;
            when 3 => Meeples (1) := 0;
            when 4 => Meeples (2) := 0;
         end case;
      end if;

   end Setup;

   ---------------
   -- Take Turn --
   ---------------
   procedure Warriors_Lost;
   procedure Battle  (S : Suit; M : Map);
   procedure Recruit (S : Suit; M : Map);
   function  Build   (S : Suit; M : Map) return Boolean;
   procedure Move    (S : Suit; M : Map);

   procedure Take_Turn (Order : Suit; M : Map) is
      Expand : Boolean;
   begin
      for I in Rule'Range loop
         Rule (I) := False;
      end loop;

      -- Mechanical Marquise 2.0 State --
      Put_Logo;
      New_Line;
      Set_Style (Yellow);
      Put_Line_Centered ("Mechanical Marquise 2.0");
      Reset_Style;
      New_Line;
      Put_Line ("    Meeple Supply:" & Meeple_Supply'Image);
      Put_Line ("   Sawmill Supply:" & Sawmill_Supply'Image);
      Put_Line ("  Workshop Supply:" & Workshop_Supply'Image);
      Put_Line (" Recruiter Supply:" & Recruiter_Supply'Image);
      New_Line;
      Put (" Current Order: ");
      case Order is
         when Fox    => Put_Line (Root.IO.Fox);
         when Mouse  => Put_Line (Root.IO.Mouse);
         when Rabbit => Put_Line (Root.IO.Rabbit);
         when Bird   => Put_Line (Root.IO.Bird);
      end case;
      New_Line;
      Separator;

      Warriors_Lost;

      -- Have the marquise lost? --
      if Meeple_Supply = MEEPLE_MAX then
         New_Line;
         Put_Line ("The " & Name & " cannot do anything!");
         return;
      end if;

      -- Birdsong --
      Put_Birdsong;
      Put_Line ("Craft order card for (+ 1) if it has an available item.");
      Continue;

      -- Daylight --
      Put_Daylight;

      loop

         -- Battle --
         Put_Line ("--  Battle");
         Battle (Order, M);
         Continue;

         -- Recruit --
         Put_Line ("--  Recruit");
         Recruit (Order, M);
         Continue;

         -- Build --
         Put_Line ("--  Build");
         Expand := not Build (Order, M);
         Continue;

         -- Move --
         Put_Line ("--  Move");
         Move (Order, M);
         Continue;

         exit when not Expand;

         Put_Line ("No buildings were placed, the " &
                   Name & " expand!");
         New_Line;

      end loop;

      -- Evening --
      Put_Evening;
      Put ("Score  (+");
      declare
         P : Integer;
      begin
         case Order is
            when Fox =>
               P := (if Sawmill_Supply /= 6
                     then 6 - Sawmill_Supply - 1 else 0);
            when Mouse =>
               P := (if Workshop_Supply /= 6
                     then 6 - Sawmill_Supply - 1 else 0);
            when Rabbit =>
               P := (if Recruiter_Supply /= 6
                     then 6 - Sawmill_Supply - 1 else 0);
            when Bird =>
               -- Find building track with least remaining buildings --
               P := (if Sawmill_Supply < Workshop_Supply
                     then Sawmill_Supply else Workshop_Supply);
               P := (if P < Recruiter_Supply then P else Recruiter_Supply);

               -- Score for that track --
               P := (if P /= 6 then 6 - P - 1 else 0);
         end case;
      end;
      Put_Line (") points for the " & Name);

   end Take_Turn;

   function Check_Rule (Clearing : Priority) return Boolean is
   begin
      if Rule (Clearing) then
         return True;
      end if;

      Put_Line ("Do the " & Name & " rule clearing" & Clearing'Image & "?");
      if Get_Yes_No then
         Rule (Clearing) := True;
      end if;

      return Rule (Clearing);
   end Check_Rule;

   procedure Warriors_Lost is
   begin
      -- Determine lost warriors --
      for I in Priority'Range loop

         -- Warriors --
         if Meeples (I) > 0 then
            Put_Line ("Do the " & Name & " still have" & Meeples (I)'Image &
                      " warrior(s) in clearing" & I'Image & "? (y/n)");
            if not Get_Yes_No then
               Put_Line ("How many warriors remain?");
               declare
                  Val : Integer;
               begin
                  Val := Get_Integer (0, Meeples (I));
                  Meeples (I) := Val;
               end;
            end if;
         end if;

         -- Sawmills --
         if Sawmill (I) > 0 then
            Put_Line ("Do the " & Name & " still have" & Sawmill (I)'Image &
                      " sawmill(s) in clearing" & I'Image & "? (y/n)");
            if not Get_Yes_No then
               Put_Line ("How many sawmills remain?");
               declare
                  Val : Integer;
               begin
                  Val := Get_Integer (0, Sawmill (I));
                  Sawmill (I) := Val;
               end;
            end if;
         end if;

         -- Workshop --
         if Workshops (I) > 0 then
            Put_Line ("Do the " & Name & " still have" & Workshops (I)'Image &
                      " workshop(s) in clearing" & I'Image & "? (y/n)");
            if not Get_Yes_No then
               Put_Line ("How many workshops remain?");
               declare
                  Val : Integer;
               begin
                  Val := Get_Integer (0, Workshops (I));
                  Workshops (I) := Val;
               end;
            end if;
         end if;

         -- Recruiter --
         if Recruiter (I) > 0 then
            Put_Line ("Do the " & Name & " still have" & Recruiter (I)'Image &
                      " recruiter(s) in clearing" & I'Image & "? (y/n)");
            if not Get_Yes_No then
               Put_Line ("How many recruiters remain?");
               declare
                  Val : Integer;
               begin
                  Val := Get_Integer (0, Recruiter (I));
                  Recruiter (I) := Val;
               end;
            end if;
         end if;
      end loop;
   end Warriors_Lost;

   -- Battle in each ordered clearing --
   procedure Battle (S : Suit; M : Map) is
      Lost : Integer;
   begin
      for I in Priority'Range loop
         -- Check matching clearing or escalation --
         if (M.Clearings (I).C_Suit = S or else S = Bird) and then
            Meeples (I) > 0
         then
            Put_Line ("Battle in clearing" & I'Image & " the enemy with " &
                      "the most pieces, then the most points.");
            Put_Line ("How many pieces were lost?");
            Lost := Get_Integer (0, Meeples (I));
            Meeples (I) := Meeples (I) - Lost;
            New_Line;
         end if;
      end loop;
   end Battle;

   -- RECRUIT four warriors evenly among ordered clearings you rule --
   procedure Recruit (S : Suit; M : Map) is
      Rule : array (Integer range 1 .. 4) of Integer := (others => 0);
      Count  : Integer range 0 .. 4 := 0;
   begin
      if S /= Bird then
         for I in Priority'Range loop
            if M.Clearings (I).C_Suit = S and then
               Meeples (I) > 0            and then
               Check_Rule (I)
            then
               Count := Count + 1;
               Rule (Count) := I;
            end if;
         end loop;
      else -- Escalation --
         for I in reverse Priority'Range loop
            if Count < 2       and then
               Meeples (I) > 0 and then
               Check_Rule (I)
            then
               Count := Count + 1;
               Rule (Count) := I;
            end if;
         end loop;
      end if;
      New_Line;

      case Count is
         when 0 =>
            Put_Line ("The " & Name & " cannot place any warriors!");
         when 1 =>
            Put_Line ("Place 4 warriors in clearing" & Rule (1)'Image);
            Meeples (Rule (1)) := Meeples (Rule (1)) + 4;
         when 2 =>
            Put_Line ("Place 2 warriors in clearing" & Rule (1)'Image);
            Put_Line ("Place 2 warriors in clearing" & Rule (2)'Image);
            Meeples (Rule (1)) := Meeples (Rule (1)) + 2;
            Meeples (Rule (2)) := Meeples (Rule (2)) + 2;
         when 3 =>
            Put_Line ("Place 2 warriors in clearing" & Rule (1)'Image);
            Put_Line ("Place 1 warrior in clearing" & Rule (2)'Image);
            Put_Line ("Place 1 warrior in clearing" & Rule (3)'Image);
            Meeples (Rule (1)) := Meeples (Rule (1)) + 2;
            Meeples (Rule (2)) := Meeples (Rule (2)) + 1;
            Meeples (Rule (3)) := Meeples (Rule (3)) + 1;
         when 4 =>
            for I in Rule'Range loop
               Put_Line ("Place 1 warrior in clearing" & Rule (I)'Image);
               Meeples (Rule (I)) := Meeples (Rule (I)) + 1;
            end loop;
      end case;

      if Count /= 0 then
         Meeple_Supply := Meeple_Supply - 4;
      end if;

   end Recruit;

   -- BUILD a building the clearing you rule --
   -- with the most Marquise warriors        --
   function Build (S : Suit; M : Map) return Boolean is
      Max : Integer := 0;
      Max_Idx : Integer;
   begin
      for I in Priority'Range loop
         -- Check matching clearing or escalation --
         if (M.Clearings (I).C_Suit = S or else S = Bird) and then
            Meeples (I) > 0 and then Check_Rule (I)
         then
            Put_Line ("Are there available building slots in clearing" &
              I'Image & "? (y/n)");
            if Get_Yes_No then
               Max := Meeples (I);
               Max_Idx := I;
            end if;
         end if;
      end loop;
      New_Line;

      if Max = 0 then
         Put_Line ("The " & Name & " cannot place any buildings.");
         return False;
      end if;

      case S is
         when Fox =>
            if Sawmill_Supply /= 0 then
               Put ("Place a SAWMILL in clearing" & Max_Idx'Image);
               Sawmill_Supply := Sawmill_Supply - 1;
               Sawmill (Max_Idx) := Sawmill (Max_Idx) + 1;
            else
               Put_Line ("The " & Name & " cannot place any buildings.");
               return False;
            end if;
         when Rabbit =>
            if Workshop_Supply /= 0 then
               Put ("Place a WORKSHOP in clearing" & Max_Idx'Image);
               Workshop_Supply := Workshop_Supply - 1;
               Workshops (Max_Idx) := Workshops (Max_Idx) + 1;
            else
               Put_Line ("The " & Name & " cannot place any buildings.");
               return False;
            end if;
         when Mouse =>
            if Recruiter_Supply /= 0 then
               Put_Line ("Place a RECRUITER in clearing" & Max_Idx'Image);
               Recruiter_Supply := Recruiter_Supply - 1;
               Recruiter (Max_Idx) := Recruiter (Max_Idx) + 1;
            else
               Put_Line ("The " & Name & " cannot place any buildings.");
               return False;
            end if;
         when Bird =>
            if Sawmill_Supply <= Workshop_Supply and then
               Sawmill_Supply <= Recruiter_Supply
            then
               if Sawmill_Supply > 0 then
                  Put_Line ("Place a SAWMILL in clearing" & Max_Idx'Image);
                  Sawmill_Supply := Sawmill_Supply - 1;
                  Sawmill (Max_Idx) := Sawmill (Max_Idx) + 1;
               end if;
            elsif Recruiter_Supply <= Workshop_Supply then
               if Recruiter_Supply > 0 then
                  Put_Line ("Place a RECRUITER in clearing" & Max_Idx'Image);
                  Recruiter_Supply := Recruiter_Supply - 1;
                  Recruiter (Max_Idx) := Recruiter (Max_Idx) + 1;
               end if;
            else
               if Workshop_Supply > 0 then
                  Put_Line ("Place a WORKSHOP in clearing" & Max_Idx'Image);
                  Workshop_Supply := Workshop_Supply - 1;
                  Workshops (Max_Idx) := Workshops (Max_Idx) + 1;
               end if;
            end if;
      end case;

      return True;
   end Build;

   -- MOVE all but three of the warriors from each ordered    --
   -- clearing to the adjacent clearing with the most enemies --
   procedure Move (S : Suit; M : Map) is
      Options : String_Arr (1 .. Neighbor_Arr'Length);
      Count   : Integer range Neighbor_Arr'Range;
   begin
      -- Get Suit Clearings --
      for I in Priority'Range loop
         -- Check matching clearing or escalation --
         if (M.Clearings (I).C_Suit = S or else S = Bird) and then
            Meeples (I) > 3
         then
            Count := 1;
            while Count < Neighbor_Arr'Length and then
                  M.Clearings (I).Neighbors (Count) /= 0
            loop
               Options (Count) :=
                 Trim (To_Unbounded_String
                 (M.Clearings (I).Neighbors (Count)'Image), Ada.Strings.Left);
               Count := Count + 1;
            end loop;

            Put_Line ("Which clearing has the most enemies?");
            declare
               Opts : constant Char_Arr :=
                  Get_Options (Options (1 .. Count - 1));
               Num_Move : constant Integer := Meeples (I) - 3;
               Max : Integer := 0;
            begin
               New_Line;

               if Opts'Length = 0 then
                  Put_Line ("Move" & Num_Move'Image & " " & Name &
                            " warriors from clearing" & I'Image &
                            " to clearing" &
                            M.Clearings (I).Neighbors (1)'Image);
                  Meeples (M.Clearings (I).Neighbors (1)) :=
                    Meeples (M.Clearings (I).Neighbors (1)) + Num_Move;
                  Meeples (I) := 3;
               else
                  for J in Opts'Range loop
                     Max := (if Character'Pos (Opts (J)) - 96 > Max then
                       Character'Pos (Opts (J)) - 96 else Max);
                  end loop;
                  Put_Line ("Move" & Num_Move'Image & " " & Name &
                            " warriors from clearing" & I'Image &
                            " to clearing" &
                            M.Clearings (I).Neighbors (Max)'Image);
                  Meeples (M.Clearings (I).Neighbors (Max)) :=
                    Meeples (M.Clearings (I).Neighbors (Max)) + Num_Move;
                  Meeples (I) := 3;
               end if;
               New_Line;

               -- Battle if in escalation --
               if S = Bird then
                  declare
                     Lost : Integer;
                  begin
                     Put_Line ("Battle in clearing" &
                               M.Clearings (I).Neighbors (Max)'Image &
                               " the enemy with the most pieces, " &
                               "then the most points.");
                     Put_Line ("How many pieces were lost?");
                     Lost := Get_Integer
                       (0, Meeples (M.Clearings (I).Neighbors (Max)));
                     Meeples (M.Clearings (I).Neighbors (Max)) :=
                       Meeples (M.Clearings (I).Neighbors (Max)) - Lost;
                     New_Line;
                  end;
               end if;
            end;
         end if;
      end loop;
   end Move;

end Root.Marquise;
