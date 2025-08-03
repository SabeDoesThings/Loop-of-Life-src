with SDL.Video.Renderers;

package Sleeping is
   procedure Init (Ren : SDL.Video.Renderers.Renderer);
   procedure Update (Dt : Float);
   procedure Render (Ren : in out SDL.Video.Renderers.Renderer);
end Sleeping;