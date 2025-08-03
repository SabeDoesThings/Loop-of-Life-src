with Ada.Text_IO;
with SDL.Events.Events;
with SDL.Timers; use SDL.Timers;
with SDL.Video.Palettes;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;

with Level; use Level;
with Camera;
with Mini_Game;
with Sleeping;
with Switching_Days;
with Working;
with Menu;

package body Game is
   Level_ID : Level.Level_Index := 1;

   procedure Run is
      Game_Win : SDL.Video.Windows.Window;
      Game_Ren : SDL.Video.Renderers.Renderer;
      Event : SDL.Events.Events.Events;
      Running : Boolean := True;
      Win_Size : constant SDL.Positive_Sizes := (800, 640);

      Target_FPS : constant := 150.0;
      Milliseconds : constant := 1000.0;
      Frame_Target_Time : constant := Milliseconds / Target_FPS;
      Last_Frame_Time : SDL.Timers.Milliseconds;
      Delta_Time : Float := 0.0;
      Time_To_Wait : SDL.Timers.Milliseconds;

      Current_Level : Level.Object;

      Camera_Obj : Camera.Object;

      Prev_Player_X, Prev_Player_Y : Float := 0.0;
   begin
      --  Initializing
      SDL.Video.Windows.Makers.Create (
         Game_Win,
         "Loop of Life",
         SDL.Video.Windows.Centered_Window_Position,
         Win_Size
      );

      SDL.Video.Renderers.Makers.Create (Game_Ren, Game_Win);

      Last_Frame_Time := SDL.Timers.Ticks;

      Game_Mode := Main_Menu;

      Menu.Init (Game_Ren);
      -- Level_ID := 28;
      Level.Init (Current_Level, Game_Ren, Level.Levels (Level_ID));

      Camera.Init (Camera_Obj, Win_Size);

      while Running loop
         --  Update
         while SDL.Events.Events.Poll (Event) loop
            case Event.Common.Event_Type is
               when SDL.Events.Quit =>
                  Running := False;

               when others =>
                  null;
            end case;
         end loop;

         Time_To_Wait :=
           SDL.Timers.Milliseconds (Long_Integer (Frame_Target_Time))
           - (SDL.Timers.Ticks - Last_Frame_Time);

         if Time_To_Wait > 0 and then
            Time_To_Wait <= SDL.Timers.Milliseconds (Frame_Target_Time)
         then
            SDL.Timers.Wait_Delay (Time_To_Wait);
         end if;

         Delta_Time := Float (SDL.Timers.Ticks - Last_Frame_Time) / 1000.0;
         Last_Frame_Time := SDL.Timers.Ticks;

         if Game_Mode = Playing_Level then
            declare
               Should_Reset : Boolean := False;
            begin
               Level.Update (Current_Level, Delta_Time, Game_Ren, Should_Reset);

               if Should_Reset then
                  Level.Destroy (Current_Level);
                  Level_ID := 1;
                  Game_Mode := Main_Menu;
                  Level.Init (Current_Level, Game_Ren, Level.Levels (Level_ID));
                  Menu.Init (Game_Ren);
                  Camera.Init (Camera_Obj, Win_Size);
               end if;
            end;

            if (Level_ID = 3 or else
               Level_ID = 9 or else
               Level_ID = 15 or else
               Level_ID = 22 or else
               Level_ID = 28) and then
               Current_Level.Goal_Reached
            then
               Game_Mode := In_Mini_Game;
               Mini_Game.Init (Game_Ren, Integer (Level_ID));
            end if;

            if (Level_ID = 6 or else
               Level_ID = 12 or else
               Level_ID = 19 or else
               Level_ID = 25) and then
               Current_Level.Goal_Reached
            then
               Game_Mode := Is_Sleeping;
               Sleeping.Init (Game_Ren);
            end if;

            if (Level_ID = 1 or else
               Level_ID = 7 or else
               Level_ID = 13 or else
               Level_ID = 26) and then
               Current_Level.Goal_Reached
            then
               Game_Mode := Is_Switching_Days;
               Switching_Days.Init (Game_Ren, Current_Level);
            end if;

         elsif Game_Mode = In_Mini_Game then
            Mini_Game.Update (Delta_Time, Running, Current_Level);

            if Game_Mode = Is_Working then
               Working.Init (Game_Ren);
            end if;
         elsif Game_Mode = Is_Sleeping then
            Sleeping.Update (Delta_Time);
         elsif Game_Mode = Is_Switching_Days then
            Switching_Days.Update (Delta_Time);
         elsif Game_Mode = Is_Working then
            Working.Update (Delta_Time);
         elsif Game_Mode = Main_Menu then
            Menu.Update (Delta_Time, Running);
         end if;

         Camera.Update (
            Camera_Obj,
            Current_Level.The_Player.X,
            Current_Level.The_Player.Y,
            Current_Level.BG_Width,
            Current_Level.BG_Height
         );

         if Current_Level.Goal_Reached then
            Prev_Player_X := Current_Level.The_Player.X;
            Prev_Player_Y := Current_Level.The_Player.Y;

            Level.Destroy (Current_Level);

            if Level_ID < Level.Levels'Last then
               Level_ID := Level_ID + 1;

               if Level_ID = 18 then
                  Level.Init (
                     Current_Level,
                     Game_Ren,
                     Level.Levels (Level_ID),
                     Prev_Player_Y
                  );
               else
                  Level.Init (
                     Current_Level,
                     Game_Ren,
                     Level.Levels (Level_ID)
                  );
               end if;
            else
               Running := False;
               exit;
            end if;
         end if;

         if Current_Level.Act_End_Triggered then
            Camera_Obj.X := Camera_Obj.X + 70.0;
            Current_Level.Act_End_Triggered := False;
         end if;

         --  Rendering
         Game_Ren.Set_Draw_Colour (
            SDL.Video.Palettes.Colour'(0, 0, 0, 0)
         );
         Game_Ren.Clear;

         if Game_Mode = Playing_Level then
            Level.Render (Current_Level, Game_Ren, Camera_Obj);
         elsif Game_Mode = In_Mini_Game then
            Mini_Game.Render (Game_Ren);
         elsif Game_Mode = Is_Sleeping then
            Sleeping.Render (Game_Ren);
         elsif Game_Mode = Is_Switching_Days then
            Switching_Days.Render (Game_Ren);
         elsif Game_Mode = Is_Working then
            Working.Render (Game_Ren);
         elsif Game_Mode = Main_Menu then
            Menu.Render (Game_Ren);
         end if;

         Game_Ren.Present;
      end loop;

      Level.Destroy (Current_Level);
      Game_Win.Finalize;
      Game_Ren.Finalize;
   end Run;

   procedure Reset_Game_State is
   begin
      Level_ID := 1;
      Game_Mode := Main_Menu;
   end Reset_Game_State;
end Game;