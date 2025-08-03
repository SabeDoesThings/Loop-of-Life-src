with SDL.Video.Renderers;
with Level;

package Menu is
   procedure Init (Ren : SDL.Video.Renderers.Renderer);
   procedure Update (Dt : Float; Running : in out Boolean);
   procedure Render (Ren : in out SDL.Video.Renderers.Renderer);
end Menu;