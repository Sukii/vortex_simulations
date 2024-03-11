function love.load()
   dir =  arg[2]
   print("cpus:",love.system.getProcessorCount())
   ran = math.random
   sin = math.sin
   cos = math.cos
   asin = math.asin
   exp = math.exp
   points = love.graphics.points
   circle = love.graphics.circle
   setFont = love.graphics.setFont
   setColor = love.graphics.setColor
   love.window.setMode(1000,800)
   lprint = love.graphics.print
   local screen_width, screen_height = love.graphics.getDimensions()
   M = 10000  -- number of vortices
   W = 1.0
   eps = 0.000001
   sigma = 0.01
   PI = math.pi
   R = 100
   R1 = 0.3
   R2 = 0.4
   THETA1 = 0
   THETA2 = PI/6
   DT = 1
   X = screen_width/2-300
   Y = screen_height/2
   XT = 500
   X0 = X - 50
   Y0 = Y + 200
   t = 0.0
   n = 0
   mu = 1.0
   nu = 0.999999
   tmu = 1.0
   tnu = 0.999
   font = love.graphics.newFont(20)
   state = "run"
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
	 table.insert(newvortices,{row[2]+200,row[3],row[4]})
      end
   end
   file:close()
   return newvortices
end

function love.draw()
   multicircle(vortices)
end

function love.update(dt)
   time = os.date("*t")
   local sec = time.sec
   --print(n,sec)
   if(state == "run" and n < 1000) then
      n = n + 1
      print(n)
      vortices = updateVortices(dt,n,dir)
   end
end


function updateVortices(dt,n,dir)
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
      local r = 1-v
      local g = 1-2*math.abs(v-0.5)
      local b = v
      local a = 1
      return {r,g,b,a}
end

function count_points(vort,r1,r2,flag)
   local count = 0
   for i,p in pairs(vortices) do
      local vx = p[1] - X
      if(flag) then
	 vx = vx - XT
      end
      vx = vx/R
      local vy = (p[2] - Y)/R
      local vr = math.sqrt(vx^2 + vy^2)
      if(vr < eps) then
	 vr = eps
      end
      local vtheta = math.asin(vx/vr)
      if(vtheta < 0) then
	 vtheta = vtheta + 2*PI
      end
      if(i < M+1 and vtheta > THETA1 and vtheta < THETA2 and vr > r1 and vr < r2) then
	 count = count + 1
      end
   end
   return count
end


function fs(c)
   s = tostring(c)
   s = string.gsub(s,"^([0-9]+)[.]([0-9][0-9]).*$","%1.%2")
   return s
end

function multicircle(points)
   for i,p in pairs(points) do
      color = rgba(p[3]/W)
      setColor(color)
      circle("fill",p[1],p[2],2)
   end
end
