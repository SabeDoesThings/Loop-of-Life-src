with Ada.Numerics.Float_Random;
with Ada.Containers.Vectors;
with Game; use Game;
with SDL.Events.Mice;
with SDL.Images.IO;
with SDL.Video.Rectangles;
with SDL.Events.Events;
with SDL.Video.Surfaces;
with SDL.Video.Textures.Makers;
with Interfaces.C; use Interfaces.C;
with Working;

package body Mini_Game is
   Max_Popups : constant := 6;

   type Popup is record
      Rect    : SDL.Video.Rectangles.Rectangle;
      Open    : Boolean := True;
   end record;

   Popups : array (1 .. Max_Popups) of Popup;

   Computer_Tex : SDL.Video.Textures.Texture;

   type Texture_Array is array (1 .. Max_Popups) of SDL.Video.Textures.Texture;
   Popup_Textures : Texture_Array;

   Gen : Ada.Numerics.Float_Random.Generator;

   Timer_Started : Boolean := False;
   Timer : Float := 0.0;
   Timer_Duration : constant Float := 2.0;

   procedure Init (Ren : SDL.Video.Renderers.Renderer; Level_Number : Integer) is
      Computer_Surf : SDL.Video.Surfaces.Surface;
      Popup_Surf   : SDL.Video.Surfaces.Surface;
      Screen_Width  : Integer := 800;
      Screen_Height : Integer := 600;
      Popup_Width  : constant := 640;
      Popup_Height : constant := 360;
   begin
      Ada.Numerics.Float_Random.Reset (Gen);

      SDL.Images.IO.Create (Computer_Surf, "res/bg0.png");
      SDL.Video.Textures.Makers.Create (Computer_Tex, Ren, Computer_Surf);

      for I in 1 .. Max_Popups loop
         declare
            File_Name : constant String := "res/popup" & Integer'Image(I) & ".png";
         begin
            SDL.Images.IO.Create (Popup_Surf, File_Name);
            SDL.Video.Textures.Makers.Create (Popup_Textures(I), Ren, Popup_Surf);
         end;
      end loop;

      if Level_Number = 28 then
         declare
            X : Integer := Integer (Ada.Numerics.Float_Random.Random (Gen) * Float (Screen_Width - Popup_Width));
            Y : Integer := Integer (Ada.Numerics.Float_Random.Random (Gen) * Float (Screen_Height - Popup_Height));
         begin
            for I in 1 .. Max_Popups loop
               Popups (I).Open := (I = 6);
               Popups (I).Rect := (SDL.Coordinate (X), SDL.Coordinate (Y), SDL.Dimension (Popup_Width), SDL.Dimension (Popup_Height));
            end loop;
         end;
      else
         for I in 1 .. 5 loop
            declare
               X : Integer := Integer (Ada.Numerics.Float_Random.Random (Gen) * Float (Screen_Width - Popup_Width));
               Y : Integer := Integer (Ada.Numerics.Float_Random.Random (Gen) * Float (Screen_Height - Popup_Height));
            begin
               Popups (I).Rect := (SDL.Coordinate (X), SDL.Coordinate (Y), SDL.Dimension (Popup_Width), SDL.Dimension (Popup_Height));
               Popups (I).Open := True;
            end;
         end loop;

         Popups (6).Open := False;
      end if;
   end Init;

   procedure Update (Dt : Float; Running : in out Boolean; Current_Level : in out Level.Object) is
      use SDL.Events;
      use SDL.Events.Events;
      Evt : SDL.Events.Events.Events;

      Mouse_X, Mouse_Y : Integer;
      Hit : Boolean := False;
   begin
      while SDL.Events.Events.Poll (Evt) loop
         case Evt.Common.Event_Type is
            when SDL.Events.Quit =>
               Running := False;

            when SDL.Events.Mice.Button_Down =>
               Mouse_X := Integer (Evt.Mouse_Button.X);
               Mouse_Y := Integer (Evt.Mouse_Button.Y);

               for I in reverse 1 .. Max_Popups loop
                  if Popups (I).Open then
                     if Mouse_X >= Integer (Popups (I).Rect.X) and Mouse_X <= Integer (Popups (I).Rect.X) + Integer (Popups (I).Rect.Width) and
                        Mouse_Y >= Integer (Popups (I).Rect.Y) and Mouse_Y <= Integer (Popups (I).Rect.Y) + Integer (Popups (I).Rect.Height) then
                        Popups (I).Open := False;
                        Hit := True;
                        exit;
                     end if;
                  end if;
               end loop;
         when others =>
            null;
         end case;
      end loop;

      declare
         All_Closed : Boolean := True;
      begin
         for I in 1 .. Max_Popups loop
            if Popups (I).Open then
               All_Closed := False;
               exit;
            end if;
         end loop;

         if All_Closed then
            if not Timer_Started then
               Timer_Started := True;
               Timer := 0.0;
            else
               Timer := Timer + Dt;
               if Timer >= Timer_Duration then
                  Game.Game_Mode := Game.Is_Working;
                  Timer_Started := False;
                  Timer := 0.0;
               end if;
            end if;
         else
            Timer_Started := False;
            Timer := 0.0;
         end if;
      end;
   end Update;

   procedure Render (Ren : in out SDL.Video.Renderers.Renderer) is
   begin
      Ren.Set_Draw_Colour ((0, 0, 0, 255));
      Ren.Clear;

      Ren.Copy (Computer_Tex);

      for I in 1 .. Max_Popups loop
         if Popups (I).Open then
            Ren.Copy_To (Popup_Textures(I), Popups (I).Rect);
         end if;
      end loop;

      Ren.Present;
   end Render;
end Mini_Game;