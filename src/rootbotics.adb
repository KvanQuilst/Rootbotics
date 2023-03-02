with Ada.Text_IO; use Ada.Text_IO;

with Root; use Root;
with Root.Color; use Root.Color;
with Root.Maps; use Root.Maps;

with Root.Eyrie;
with Root.Marquise;
with Root.Alliance;
with Root.Vagabot; use Root.Vagabot;

procedure Rootbotics is
  VERSION : constant String := "v0.1";

  -- In order of setup priority --
  type Faction is (Marquise, Eyrie, Alliance, Vagabot);
                   --Lizards, Riverfolk, Corvids, Duchy);
  Playing : array (Faction'Range) of Boolean := (others => False);
  Num_Playing : Integer := 0;

  M : Map;

begin
  Put_Line ("Welcome to the Rootbotics Logic Tool " & VERSION & "!");
  New_Line;

  -----------------------
  -- Faction Selection --
  -----------------------
  Put_Line ("Which of the following clockwork factions will you be playing with:");
  Separator;
  Put      (" a. ");
  Root.Marquise.Put_Name (True);
  Put      (" b. ");
  Root.Eyrie.Put_Name (True);
  Put      (" c. ");
  Root.Alliance.Put_Name (True);
  Put      (" d. ");
  Root.Vagabot.Put_Name (True);
  Put_Line (" e. Logical Lizards  (unimplemented)");
  Put_Line (" f. Riverfolk Robots (unimplemented)");
  Put_Line (" g. Cogwheel Corvids (unimplemented)");
  Put_Line (" h. Drillbit Duchy   (unimplemented)");
  New_Line;
  Put_Line ("    No specified options to quit");
  Separator;

  -- Handle faction input --
  declare
    OL  : Option_List (1..Faction'Pos (Faction'Last));
  begin
    Get_List (OL, OL'Length);

    if OL (1) = Character'Val (0) then
      return;
    end if;

    for I in OL'Range loop
      if OL (I) /= Character'Val (0) then
        Playing (Faction'Val (Character'Pos (OL (I)) - 97)) := True;
        Num_Playing := Num_Playing + 1;
      end if;
    end loop;
  end;
  New_Line;

  -------------------
  -- Map Selection --
  -------------------
  Put_Line ("Which map will you be playing on:");
  Separator;

  Put      (" a. ");
  Set_Color (Green);
  Put_Line ("Fall");
  Reset_Style;

  Put      (" b. ");
  Set_Color (Teal);
  Put_Line ("Winter");
  Reset_Style;

  Put      (" c. ");
  Set_Color (Blue);
  Put_Line ("Lake");
  Reset_Style;

  Put      (" d. ");
  Set_Color (Orange);
  Put_Line ("Mountain (unimplemented)");
  Reset_Style;

  Separator;

  -- Handle map input --
  declare
    Option : Character;
  begin
    -- TODO Implement other maps --
    Get_Option (Option, 3);

    case Option is
      when 'a' => M := Fall_Map;
      when 'b' => M := Winter_Map;
      when 'c' => M := Lake_Map;
      --when 'd' => M := Mountain_Map;
      when others => 
        Put_Line ("ERROR: Should never reach here!");
        return;
    end case;
  end;
  New_Line;
  
  ----------------------------
  -- Faction Setup in Order --
  ----------------------------
  for I in Playing'Range loop
    if Playing (I) then
      case I is
        when Marquise => Root.Marquise.Setup (M);
        when Eyrie => Root.Eyrie.Setup;
        when Alliance => Put_Line ("The Automated Alliance is unimplmented!");
        when Vagabot => Root.Vagabot.Setup;
        --TODO Clockwork Expansion 2
        --when Lizards => Put_Line ("The Logical Lizards are unimplemented!");
        --when Riverfolk => Put_Line ("The Riverfolk Robots are unimplemented!");
        --when Corvids => Put_Line ("The Cogwheel Corvids are unimplemented!");
        --when Duchy => Put_Line ("The Drillbit Duchy is unimplemented!");
      end case;
    end if;
  end loop;
  New_Line;

  ---------------------
  -- Manage the Game --
  ---------------------
  loop
    declare
      P_Idx : Integer := 0;
      F_Opt, Order : Character;
      F : Faction;
    begin

      if Num_Playing > 1 then
        -- Choose Faction Turn --
        Put_Line ("Whose turn will you take:");
        Separator;
        for I in Playing'Range loop
          if Playing (I) then
            Put (" " & Character'Val (97 + P_Idx) & ". "); -- 'a' + P_Idx --
            P_Idx := P_Idx + 1;
            case I is
              when Marquise => Root.Marquise.Put_Name (True);
              when Eyrie    => Root.Eyrie.Put_Name    (True);
              when Alliance => Root.Alliance.Put_Name (True);
              when Vagabot  => Root.Vagabot.Put_Name  (True);
              --when Lizards => Root.Lizards.Put_Name (True);
              --when Riverfolk => Root.Riverfolk.Put_Name (True);
              --when Corvids => Root.Corvids.Put_Name (True);
              --when Duchy => Root.Duchy.Put_Name (True);
            end case;
          end if;
        end loop;
        Separator;

        Get_Option (F_Opt, P_Idx);
        P_Idx := Character'Pos (F_Opt) - 96;
      else
        P_Idx := 1;
      end if;


      F := Marquise;
      while P_Idx /= 0 loop
        if Playing (F) then
          P_Idx := P_Idx - 1;
        end if;
        F := Faction'Succ (F);
      end loop;
      F := Faction'Pred (F);

      New_Line;

      -- What's the Order? --
      Put_Line ("What is the order of this turn:");
      Put_Suits_Options;

      Get_Option (Order, 4);
      New_Line;
      
      -- Handle faction turn --
      case F is
        when Marquise => 
          Root.Marquise.Take_Turn (Suit'Val (Character'Pos (Order) - 97), M);
        when Eyrie    => 
          Root.Eyrie.Take_Turn    (Suit'Val (Character'Pos (Order) - 97), M);
        when Alliance => 
          Root.Alliance.Take_Turn (Suit'Val (Character'Pos (Order) - 97), M);
        when Vagabot  => 
          Root.Vagabot.Take_Turn  (Suit'Val (Character'Pos (Order) - 97), M);
        --when Lizards => Root.Lizards.Take_Turn (Order, M);
        --when Riverfok => Root.Riverfolk.Take_Turn (Order, M);
        --when Corvids => Root.Corvids.Take_Turn (Order, M);
        --when Duchy => Root.Duchy.Take_Turn (Order, M);
      end case;
    end;
  end loop;
end Rootbotics;
