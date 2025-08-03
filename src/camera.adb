package body Camera is
   procedure Init (C : in out Object; Scr_Size : SDL.Positive_Sizes) is
   begin
      C.Width := Integer (Scr_Size.Width);
      C.Height := Integer (Scr_Size.Height);
   end Init;

   procedure Update (
      C         : in out Object;
      Target_X  : Float;
      Target_Y  : Float;
      BG_Width  : Integer;
      BG_Height : Integer
   ) is
      Half_Width  : constant Float := Float (C.Width) / (2.0 * C.Zoom);
      Half_Height : constant Float := Float (C.Height) / (2.0 * C.Zoom);

      Min_X : constant Float := 0.0;
      Min_Y : constant Float := 0.0;
      Max_X : constant Float := Float (BG_Width) - 2.0 * Half_Width;
      Max_Y : constant Float := Float (BG_Height) - 2.0 * Half_Height;
   begin
      C.X := Target_X - Half_Width;
      C.Y := Target_Y - Half_Height;

      if C.X < Min_X then
         C.X := Min_X;
      elsif C.X > Max_X then
         C.X := Max_X;
      end if;

      if C.Y < Min_Y then
         C.Y := Min_Y;
      elsif C.Y > Max_Y then
         C.Y := Max_Y;
      end if;
   end Update;

   function To_Screen (C : in out Object; World_X, World_Y, W, H : Integer) return SDL.Video.Rectangles.Rectangle is
      Z : constant Float := C.Zoom;
   begin
      return SDL.Video.Rectangles.Rectangle'(
         X => SDL.Coordinate ((Float (World_X) - C.X) * Z),
         Y => SDL.Coordinate ((Float (World_Y) - C.Y) * Z),
         Width => SDL.Natural_Dimension (Float (W) * Z),
         Height => SDL.Natural_Dimension (Float (H) * Z)
      );
   end To_Screen;
end Camera;