function love.load()
   dir =  arg[2]
   waters = love.audio.newSource("audio/water.mp3","static")
   points = love.graphics.points
   circle = love.graphics.circle
   line = love.graphics.line
   rectangle = love.graphics.rectangle
   love.graphics.setFont(love.graphics.newFont(100))
   font = love.graphics.getFont()
   setFont = love.graphics.setFont
   setColor = love.graphics.setColor
   love.window.setMode(1200,800)
   lprint = love.graphics.print
   local screen_width, screen_height = love.graphics.getDimensions()
   X = screen_width/2-300
   Y = screen_height/2
   n = 0
   B = 20.0
   D = 10.0
   N = 5
   delta = 1.0/N
   sigma = delta/N
   WW = 1.0*sigma
   DT = 0.1
   rsigma = math.sqrt(sigma)
   state = "run"
   scale = 40
   X0 = 200
   Y0 = 100
   rblob = 7.0
   vortices = {}
   vortices = loadVortices(dir .. "/vortices_00000.dat")
end


function loadVortices(fname)
   newvortices = {}
   file = io.open(fname, "r")
   for line in file:lines() do
      line = line:gsub("^[%s]+","")
      line = line:gsub("[%s]+$","")
      if(line ~= "") then
	 local row = {}
	 for item in string.gmatch(line, "([^%s]+)") do
	    table.insert(row,item)
	 end
	 local duv = {0, 0}
	 if(#vortices > 0) then
	    local inx = row[1]+0
	    local xp = vortices[inx][1]
	    local yp = vortices[inx][2]
	    duv = {tonumber(row[2])-xp, tonumber(row[3])-yp}
	    if(inx == 25) then
	       --print("inx1:",inx,row[1],row[2],vortices[inx][1],row[3],vortices[inx][2],duv[1],duv[2])
	    end
	    newvortices[inx]={row[2],row[3],row[4],duv}
	 else
	    local inx = row[1]+0
	    newvortices[inx]={row[2],row[3],row[4],duv}
	 end
      end
   end
   file:close()
   return newvortices
end

function love.draw()
   drawchannel()
   multicircle(vortices)
end

function drawchannel()
   rectangle("line",X0,Y0,scale*B,scale*D) 
end

function love.update(dt)
   waters:play()
   time = os.date("*t")
   local sec = time.sec
   --print(n,sec)
   if(state == "run" and n < 500) then
      n = n + 1
      print(n)      
      vortices = updateVortices(dt,n)
   else
      love.audio.stop(waters)
   end
end


function updateVortices(dt,n)
   fname = dir .. "/vortices_0000" .. n
   if(n > 9 and n < 100) then
      fname = dir .. "/vortices_000" .. n
   elseif (n > 99 and n < 1000) then
      fname = dir .. "/vortices_00" .. n      
   elseif (n > 999) then
      fname = dir .. "/vortices_0" .. n      
   end
   newvortices = loadVortices(fname .. ".dat")
   return newvortices
end


function love.keypressed(key)
   if key == "down" then
      state = "stop"
   end
   if key == "up" then
      state = "run"
   end
end

function rgba(v)
   v = math.abs(v)
   local r = 1-v
   local g = 1-2*math.abs(v-0.5)
   local b = v
   local a = 1
   return {r,g,b,a}
end


function fs(c)
   s = tostring(c)
   s = string.gsub(s,"^([0-9]+)[.]([0-9][0-9]).*$","%1.%2")
   return s
end

function multicircle(points)
   local U = {}
   local V = {}
   local NU = {}
   for i = 1,10 do
      U[i] = 0.0
      V[i] = 0.0
      NU[i] = 0
   end
   for i,p in pairs(points) do
      color = rgba(p[3]/WW)
      setColor(color)
      local dx = tonumber(p[4][1])
      local dy = tonumber(p[4][2])
      if(dx < -0.5*B) then
	 dx = dx + B
      end
      if(dx > 0.5*B) then
	 dx = dx - B
      end
      local u = dx/DT
      local v = dy/DT
      for j = 1,10 do
	 if(j > tonumber(p[2]) and tonumber(p[2]) > j-1) then
	    U[j] = U[j] + u
	    V[j] = V[j] + v*v
	    NU[j] = NU[j] + 1
	 end
      end
      local xp1 = scale*p[1]+X0
      local yp1 = scale*p[2]+Y0
      circle("fill",xp1,yp1,rblob)
      --arrow(xp1,yp1,u,v)
   end
   setColor({0,1,0,0.5+0.5*n/100})
   local Um = 0.0
   local Vm = 0.0
   for j = 1,10 do
      if(NU[j] > 0) then
	 U[j] = U[j]/NU[j]
	 Um = Um + U[j]/10
	 V[j] = math.sqrt(V[j]/NU[j])
	 Vm = Vm + V[j]/10
      end
      if(j > 1) then
	 Uj1 = X0 + scale*(B+U[j-1]/5)
	 Yj1 = Y0 + scale*D*(j-1-0.5)/10
	 Uj = X0 + scale*(B+U[j]/5)
	 Yj = Y0 + scale*D*(j-0.5)/10
	 Vj1 = X0 + scale*(B+V[j-1]*10)
	 Vj = X0 + scale*(B+V[j]*10)
	 line(Uj1, Yj1, Uj, Yj)
	 line(Vj1, Yj1, Vj, Yj)
      end
   end
   text = love.graphics.newText(font)
   text:add( {{0,1,0}, string.format("Um: %.2f,v-sigma: %.4f", Um,Vm)})
   love.graphics.draw(text, X0+B, Y0+500, 0.0, 0.2)
   --print("U,v:",Um,Vm)
end

function arrow(x1,y1,du,dv)
   local scale = 100
   local duv = math.sqrt(du^2+dv^2)
   local x2 = x1-scale*du
   local y2 = y1-scale*dv
   line(x1,y1,x2,y2)
end
