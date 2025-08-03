with SDL.Events.Keyboards;
with SDL.Images.IO;
with SDL.Inputs.Keyboards;
with SDL.Video.Surfaces;
with SDL.Video.Textures.Makers;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body Player is
   procedure Init (P : in out Object; Ren : SDL.Video.Renderers.Renderer) is
      Surface : SDL.Video.Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Surface, "res\player.png");
      SDL.Video.Textures.Makers.Create (P.Sheet.Texture, Ren, Surface);
      SDL.Video.Surfaces.Finalize (Surface);

      P.Sheet.Frame_Width := 16;
      P.Sheet.Frame_Height := 16;
      P.Sheet.Frame_Count := 4;

      P.Idle_Anim.Start_Frame := 0;
      P.Idle_Anim.End_Frame := 1;
      P.Idle_Anim.Current_Frame := P.Idle_Anim.Start_Frame;
      P.Idle_Anim.Frame_Delay := 0.6;

      P.Walk_Anim.Start_Frame := 2;
      P.Walk_Anim.End_Frame := 3;
      P.Walk_Anim.Current_Frame := P.Walk_Anim.Start_Frame;
      P.Walk_Anim.Frame_Delay := 0.3;

      P.Collider := (SDL.Coordinate (P.X + P.Collider_Offset), SDL.Coordinate (P.Y) + 7, 16 / 3, 9);

      P.Speed := 60.0;
   end Init;

   procedure Update (P : in out Object; Dt : Float; Bounds : SDL.Video.Rectangles.Rectangle) is
      procedure Input is
         Keys : SDL.Inputs.Keyboards.Key_State_Access;
      begin
         Keys := SDL.Inputs.Keyboards.Get_State;

         P.Is_Moving := False;

         if Keys (SDL.Events.Keyboards.Scan_Code_W) then
            P.Y := P.Y - P.Speed * Dt;
            P.Is_Moving := True;
         end if;
         if Keys (SDL.Events.Keyboards.Scan_Code_A) then
            P.X := P.X - P.Speed * Dt;
            P.Is_Moving := True;
            P.Facing_Left := True;
         end if;
         if Keys (SDL.Events.Keyboards.Scan_Code_S) then
            P.Y := P.Y + P.Speed * Dt;
            P.Is_Moving := True;
         end if;
         if Keys (SDL.Events.Keyboards.Scan_Code_D) then
            P.X := P.X + P.Speed * Dt;
            P.Is_Moving := True;
            P.Facing_Left := False;
         end if;
      end Input;
   begin
      Input;

      if P.Is_Moving then
         Animation.Update (P.Walk_Anim, Dt);
      else
         Animation.Update (P.Idle_Anim, Dt);
      end if;

      P.Collider.X := SDL.Coordinate (P.X + P.Collider_Offset);
      P.Collider.Y := SDL.Coordinate (P.Y) + 7;

      declare
         Offset_X : constant Float := Float (P.Collider.X) - P.X;
         Offset_Y : constant Float := Float (P.Collider.Y) - P.Y;
         Collider_W : constant Float := Float (P.Collider.Width);
         Collider_H : constant Float := Float (P.Collider.Height);
         Min_X : constant Float := Float (Bounds.X);
         Min_Y : constant Float := Float (Bounds.Y);
         Max_X : constant Float := Float (Bounds.X + Bounds.Width);
         Max_Y : constant Float := Float (Bounds.Y + Bounds.Height);
      begin
         if P.X + Offset_X < Min_X then
            P.X := Min_X - Offset_X;
         elsif P.X + Offset_X + Collider_W > Max_X then
            P.X := Max_X - Offset_X - Collider_W;
         end if;

         if P.Y + Offset_Y < Min_Y then
            P.Y := Min_Y - Offset_Y;
         elsif P.Y + Offset_Y + Collider_H > Max_Y then
            P.Y := Max_Y - Offset_Y - Collider_H;
         end if;
      end;
      -- end;
   end Update;

   procedure Render (
      P : in out Object;
      Ren : in out SDL.Video.Renderers.Renderer;
      Cam : in out Camera.Object
   ) is
      Dest : SDL.Video.Rectangles.Rectangle :=
        Camera.To_Screen (
          Cam,
          Integer (P.X),
          Integer (P.Y),
          P.Sheet.Frame_Width,
          P.Sheet.Frame_Height
        );
      Flip : Animation.Flip_Type := (if P.Facing_Left then Animation.Horizontal else Animation.None);
   begin
      if P.Is_Moving then
         Animation.Render (P.Walk_Anim, P.Sheet, Ren, Dest, Flip);
      else
         Animation.Render (P.Idle_Anim, P.Sheet, Ren, Dest, Flip);
      end if;
   end Render;

   procedure Destroy (P : in out Object) is
   begin
      P.Sheet.Texture.Finalize;
   end Destroy;
end Player;