with Ada.Text_IO;
with SDL.Images.IO;
with SDL.TTFs.Makers;
with SDL.Video.Palettes;
with SDL.Video.Textures.Makers;
with SDL.Video.Surfaces;
with Interfaces.C; use Interfaces.C;
with Game; use Game;
with SDL.Mixer.Music; use SDL.Mixer.Music;

package body Level is
   Day_Text_Surf : SDL.Video.Surfaces.Surface;
   Day_Text_Text : SDL.Video.Textures.Texture;
   Task_Text_Surf : SDL.Video.Surfaces.Surface;
   Task_Text_Text : SDL.Video.Textures.Texture;
   Font : SDL.TTFs.Fonts;

   Music : Music_Type;

   procedure Init (
      L : in out Object;
      Ren : SDL.Video.Renderers.Renderer;
      Data : Level_Data;
      Start_Player_Y : Float := -1.0
   ) is
   begin
      SDL.Images.IO.Create (L.BG_Surf, Data.BG_Path);

      L.BG_Width  := Integer (L.BG_Surf.Size.Width);
      L.BG_Height := Integer (L.BG_Surf.Size.Height);

      SDL.Video.Textures.Makers.Create (L.BG, Ren, L.BG_Surf);
      SDL.Video.Surfaces.Finalize (L.BG_Surf);

      L.Text_For_Day := Data.Day_Text;

      SDL.TTFs.Makers.Create (Font, "res/Monocraft.ttf", 36);
      Day_Text_Surf := Font.Render_Solid (Text => L.Text_For_Day, Colour => SDL.Video.Palettes.Colour'(Red => 255, Green => 255, Blue => 255, Alpha => 255));
      SDL.Video.Textures.Makers.Create (Day_Text_Text, Ren, Day_Text_Surf);

      SDL.TTFs.Makers.Create (Font, "res/Monocraft.ttf", 36);
      Task_Text_Surf := Font.Render_Solid (Text => Data.Task_Text, Colour => SDL.Video.Palettes.Colour'(Red => 255, Green => 255, Blue => 255, Alpha => 255));
      SDL.Video.Textures.Makers.Create (Task_Text_Text, Ren, Task_Text_Surf);

      Load_MUS ("res/song2.wav", Music);

      L.Goal.X := SDL.Coordinate (Data.Goal_X);
      L.Goal.Y := SDL.Coordinate (Data.Goal_Y);
      L.Goal.Width := SDL.Natural_Dimension (16);
      L.Goal.Height := SDL.Natural_Dimension (16);
      L.Goal_Reached := False;

      L.Bounds.X := SDL.Coordinate(Data.Bounds_X);
      L.Bounds.Y := SDL.Coordinate(Data.Bounds_Y);
      L.Bounds.Width := SDL.Natural_Dimension (Data.Bounds_Width);
      L.Bounds.Height := SDL.Natural_Dimension (Data.Bounds_Height);

      L.The_Player.X := Data.Player_X;

      if Start_Player_Y < 0.0 then
         L.The_Player.Y := Data.Player_Y;
      else
         L.The_Player.Y := Start_Player_Y;
      end if;

      Player.Init (L.The_Player, Ren);

      L.Act_End := (X => 0, Y => 0, Width => 0, Height => 0);
      L.Act_End_Triggered := False;
      L.Show_Eye := False;

      if Data = Level.Levels (Level.Levels'Last) then
         Play (Music, Loop_Forever);

         L.Act_End := (X => 345, Y => 60, Width => 32, Height => 32);

         SDL.Images.IO.Create (L.Eye_Surf, "res/eye.png");
         SDL.Video.Textures.Makers.Create (L.Eye_Sprite.Texture, Ren, L.Eye_Surf);
         SDL.Video.Surfaces.Finalize (L.Eye_Surf);

         L.Eye_Sprite.Frame_Width := 160;
         L.Eye_Sprite.Frame_Height := 160;
         L.Eye_Sprite.Frame_Count := 2;

         L.Eye_Anim.Current_Frame := 0;
         L.Eye_Anim.Timer := 0.0;
         L.Eye_Anim.Frame_Delay := 2.0;
         L.Eye_Anim.Start_Frame := 0;
         L.Eye_Anim.End_Frame := 1;

         L.Eye_Rect := (
            X => 383,
            Y => 30,
            Width => 80,
            Height => 80
         );

         L.Last_Level_Messages (1) := "Hello.                         ";
         L.Last_Level_Messages (2) := "I know this is very strange    ";
         L.Last_Level_Messages (3) := "But what you see is truth.     ";
         L.Last_Level_Messages (4) := "An eye opener I guess.. get it?";
         L.Last_Level_Messages (5) := "Let me ask you something.      ";
         L.Last_Level_Messages (6) := "Are you happy?                 ";
         L.Last_Level_Messages (7) := "Like truly happy?              ";
         L.Last_Level_Messages (8) := "Doing the same loop of life?   ";
         L.Last_Level_Messages (9) := "Why?                           ";
         L.Last_Level_Messages (10) := "Oh who am I kidding?           ";
         L.Last_Level_Messages (11) := "No matter how much I try.      ";
         L.Last_Level_Messages (12) := "You'll never change.           ";
         L.Last_Level_Messages (13) := "                               ";

         L.Message_Interval := 3.5;

         declare
            Msg : String := L.Last_Level_Messages (L.Current_Message_Index);
         begin
            L.Last_Level_Text_Surface := Font.Render_Solid (
               Text => Msg,
               Colour => SDL.Video.Palettes.Colour'(Red => 255, Green => 255, Blue => 255, Alpha => 255)
            );
            SDL.Video.Textures.Makers.Create (
               L.Last_Level_Text_Texture,
               Ren,
               L.Last_Level_Text_Surface
            );
         end;
      end if;
   end Init;

   procedure Update (L : in out Object; Dt : Float; Ren : SDL.Video.Renderers.Renderer; Should_Reset : out Boolean) is
   begin
      Should_Reset := False;

      Player.Update (L.The_Player, Dt, L.Bounds);

      if Rects_Overlap (
            Integer (L.The_Player.Collider.X),
            Integer (L.The_Player.Collider.Y),
            Integer (L.The_Player.Collider.Width),
            Integer (L.The_Player.Collider.Height),

            Integer (L.Goal.X),
            Integer (L.Goal.Y),
            Integer (L.Goal.Width),
            Integer (L.Goal.Height)
         )
      then
         L.Goal_Reached := True;
      end if;

      if L.Act_End.Width > 0 and then L.Act_End.Height > 0 then
         if Rects_Overlap (
            Integer (L.The_Player.Collider.X),
            Integer (L.The_Player.Collider.Y),
            Integer (L.The_Player.Collider.Width),
            Integer (L.The_Player.Collider.Height),

            Integer (L.Act_End.X),
            Integer (L.Act_End.Y),
            Integer (L.Act_End.Width),
            Integer (L.Act_End.Height)
         )
         then
            L.The_Player.Speed := 0.0;

            if not L.Act_End_Triggered then
               L.Act_End_Triggered := True;
               L.Show_Eye := True;
            end if;
         end if;
      end if;

      if L.Show_Eye then
         Animation.Update (L.Eye_Anim, Dt);
      end if;

      if L.Show_Eye then
         Animation.Update (L.Eye_Anim, Dt);

         L.Message_Timer := L.Message_Timer + Dt;

         if not L.All_Messages_Shown then
            if L.Message_Timer > L.Message_Interval then
               L.Message_Timer := 0.0;

               if L.Current_Message_Index < L.Last_Level_Messages'Last then
                  L.Current_Message_Index := L.Current_Message_Index + 1;
                  declare
                     Msg : String := L.Last_Level_Messages (L.Current_Message_Index);
                  begin
                     L.Last_Level_Text_Surface := Font.Render_Solid (
                        Text => Msg,
                        Colour => SDL.Video.Palettes.Colour'(Red => 255, Green => 255, Blue => 255, Alpha => 255)
                     );
                     SDL.Video.Textures.Makers.Create (
                        L.Last_Level_Text_Texture,
                        Ren,
                        L.Last_Level_Text_Surface
                     );
                  end;
               else
                  L.All_Messages_Shown := True;
                  L.Return_Timer := 0.0;
               end if;
            end if;
         else
            L.Return_Timer := L.Return_Timer + Dt;
            if L.Return_Timer > 3.0 then
               Should_Reset := True;
               Pause;
               Free (Music);
               return;
            end if;
         end if;
      end if;
   end Update;

   procedure Render (
      L : in out Object;
      Ren : in out SDL.Video.Renderers.Renderer;
      Cam : in out Camera.Object
   ) is
      Dest : SDL.Video.Rectangles.Rectangle;
      Text_Dest : SDL.Video.Rectangles.Rectangle;
      Task_Dest : SDL.Video.Rectangles.Rectangle;
   begin
      -- Background
      Dest := Camera.To_Screen (Cam, 0, 0, Integer (L.BG_Width), Integer (L.BG_Height));
      Ren.Copy_To (L.BG, Dest);

      -- Player
      Player.Render (L.The_Player, Ren, Cam);

      Text_Dest := (0, 0, Day_Text_Text.Get_Size.Width, Day_Text_Text.Get_Size.Height);
      Ren.Copy_To (Day_Text_Text, Text_Dest);

      Task_Dest := (0, 53, Task_Text_Text.Get_Size.Width, Task_Text_Text.Get_Size.Height);
      Ren.Copy_To (Task_Text_Text, Task_Dest);

      -- Eye animation
      if L.Show_Eye then
         declare
            Dest : SDL.Video.Rectangles.Rectangle := Camera.To_Screen (
               Cam,
               Integer (L.Eye_Rect.X),
               Integer (L.Eye_Rect.Y),
               Integer (L.Eye_Rect.Width),
               Integer (L.Eye_Rect.Height)
            );
         begin
            Animation.Render (
               L.Eye_Anim,
               L.Eye_Sprite,
               Ren,
               Dest
            );
         end;

         declare
            Text_Dest : SDL.Video.Rectangles.Rectangle :=
              (X => 20, Y => 120,
               Width => L.Last_Level_Text_Texture.Get_Size.Width,
               Height => L.Last_Level_Text_Texture.Get_Size.Height);
         begin
            Ren.Copy_To (L.Last_Level_Text_Texture, Text_Dest);
         end;
      end if;
   end Render;

   procedure Destroy (L : in out Object) is
   begin
      L.BG.Finalize;
      Player.Destroy (L.The_Player);
      L.Goal := (X => 0, Y => 0, Width => 0, Height => 0);
      L.Goal_Reached := False;
   end Destroy;

   function Rects_Overlap (
      X1, Y1, W1, H1 : Integer;
      X2, Y2, W2, H2 : Integer
   ) return Boolean is
   begin
      return
         X1 < X2 + W2 and then
         X1 + W1 > X2 and then
         Y1 < Y2 + H2 and then
         Y1 + H1 > Y2;
   end Rects_Overlap;
end Level;