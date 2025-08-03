with SDL.Video.Rectangles;
with SDL.Video.Renderers;

package Camera is
   type Object is record
      X, Y : Float := 0.0;
      Zoom : Float := 5.0;
      Width, Height : Integer;
   end record;

   procedure Init (C : in out Object; Scr_Size : SDL.Positive_Sizes);
   procedure Update (C : in out Object; Target_X, Target_Y : Float; BG_Width, BG_Height : Integer);
   function To_Screen (C : in out Object; World_X, World_Y, W, H : Integer) return SDL.Video.Rectangles.Rectangle;
end Camera;