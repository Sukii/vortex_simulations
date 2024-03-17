function love.load()
   dir =  arg[2]
   waters = love.audio.newSource("audio/water.mp3","static")
   points = love.graphics.points
   circle = love.graphics.circle
   line = love.graphics.line
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
   rsigma = math.sqrt(sigma)
   state = "run"
   scale = 30
   X0 = 200
   Y0 = 200
   rblob = 5.0
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
	 row = {}
	 for item in string.gmatch(line, "([^%s]+)") do
	    table.insert(row,item)
	 end
	 local duv = {0, 0}
	 if(#vortices > 0) then
	    local inx = row[1]+0
	    local xp = vortices[inx][1]
	    local yp = vortices[inx][2]
	    duv = {tonumber(row[2])-xp, tonumber(row[3])-yp}
	    if(inx == 1) then
	       --print("inx0:",inx,row[1],row[2],vortices[inx][1],row[3],vortices[inx][2],duv[1],duv[2])
	    end
	    if(inx == 1) then
	       --print("inx1:",row[1],row[2],vortices[inx][1],row[3],vortices[inx][2],duv[1],duv[2])
	    end
	 end
	 local inx = row[1]+0
	 newvortices[inx]={row[2],row[3],row[4],duv}
      end
   end
   file:close()
   return newvortices
end

function love.draw()
   multicircle(vortices)
end

function love.update(dt)
   waters:play()
   time = os.date("*t")
   local sec = time.sec
   --print(n,sec)
   if(state == "run" and n < 244) then
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
   if(n > 2) then
      n = n - 2
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
   for i,p in pairs(points) do
      color = rgba(p[3]/WW)
      setColor(color)
      local du = p[4][1]
      local dv = p[4][2]
      if(du < -0.5*B) then
	 du = du + B
      end
      if(du > 0.5*B) then
	 du = du - B
      end
      local xp1 = scale*p[1]+X0
      local yp1 = scale*p[2]+Y0
      circle("fill",xp1,yp1,rblob)
      --arrow(xp1,yp1,du,dv)
   end
end

function arrow(x1,y1,du,dv)
   local scale = 100
   local duv = math.sqrt(du^2+dv^2)
   local x2 = x1-scale*du
   local y2 = y1-scale*dv
   line(x1,y1,x2,y2)
end
