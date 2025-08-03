with Interfaces.C; use Interfaces.C;

package body Animation is
   procedure Update (A : in out Sprite_Animation; Dt : Float) is
   begin
      A.Timer := A.Timer + Dt;
      if A.Timer >= A.Frame_Delay then
         A.Timer := A.Timer - A.Frame_Delay;
         A.Current_Frame := A.Current_Frame + 1;
         if A.Current_Frame > A.End_Frame then
            A.Current_Frame := A.Start_Frame;
         end if;
      end if;
   end Update;

   procedure Render (
      A : Sprite_Animation;
      S : Sprite_Sheet;
      Ren : in out SDL.Video.Renderers.Renderer;
      Dest : in out SDL.Video.Rectangles.Rectangle;
      Flip : Flip_Type := None
   ) is
      Src : SDL.Video.Rectangles.Rectangle;
      Spr_Flip : SDL.Video.Renderers.Renderer_Flip := SDL.Video.Renderers.None;

      use type SDL.Video.Renderers.Renderer_Flip;
   begin
      Src.X := SDL.Coordinate (A.Current_Frame * S.Frame_Width);
      Src.Y := 0;
      Src.Width := SDL.Natural_Dimension (S.Frame_Width);
      Src.Height := SDL.Natural_Dimension (S.Frame_Height);

      case Flip is
         when Horizontal =>
            Spr_Flip := SDL.Video.Renderers.Horizontal;
         when None =>
            Spr_Flip := SDL.Video.Renderers.None;
      end case;

      if Spr_Flip = SDL.Video.Renderers.Horizontal then
         Dest.X := SDL.Coordinate (Integer (Dest.X) + 5);
      end if;

      Ren.Copy (S.Texture, Src, Dest, 0.0, SDL.Video.Rectangles.Point'(Dest.Width / 2, Dest.Height / 2), Spr_Flip);
   end Render;
end Animation;