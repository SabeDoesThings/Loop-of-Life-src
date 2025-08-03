with SDL.Video.Renderers;
with Level;

package Switching_Days is
   procedure Init (Ren : SDL.Video.Renderers.Renderer; Current_Level : Level.Object);
   procedure Update (Dt : Float);
   procedure Render (Ren : in out SDL.Video.Renderers.Renderer);
end Switching_Days;