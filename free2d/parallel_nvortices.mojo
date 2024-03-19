#== nvortices in motion ===
from algorithm import sum
from algorithm import vectorize, parallelize, unroll
from sys.info import num_performance_cores
from sys import argv
from collections.vector import InlinedFixedVector
import math

alias PI = 3.141592653589793
alias D = 2
alias B = 4
alias N = 10
alias delta = 1.0/N
alias NUM_VORTICES = N*N*D*B
alias eps = 0.0000001

@register_passable("trivial")
struct Vortex:
    var pos: SIMD[DType.float16, 2]
    var mass: Float16
    
    fn __init__(
        pos: SIMD[DType.float16, 2],
        mass: Float16,
    ) -> Self:
        return Self {
            pos: pos,
            mass: mass,
        }



#@parameter(inout vors: InlinedFixedVector[Vortex,NUM_VORTICES])
fn advance(inout vors: InlinedFixedVector[Vortex,NUM_VORTICES], dt: Float16):
# Clear the original vector
    var temp_vors: InlinedFixedVector[Vortex,NUM_VORTICES] = vors
    for i in range(NUM_VORTICES):
        temp_vors.__setitem__(i,vors.__getitem__(i))
    for i in range(NUM_VORTICES):
        var vortex_i = temp_vors[i]
        #print("i:",i,vortex_i.pos,vortex_i.mass)
        for j in range(NUM_VORTICES):
            if(i != j):
                var vortex_j = temp_vors[j]
                let diff = vortex_i.pos - vortex_j.pos
                let cdiff = SIMD[DType.float16, 2](diff[1],-diff[0])
                var diff_sqr = (diff * diff).reduce_add()
                if(diff_sqr < eps):
                    diff_sqr = eps
                let mag = dt / (diff_sqr)
                vortex_i.pos -= cdiff * vortex_j.mass * mag
        vors[i].pos = vortex_i.pos
        #print("ix:",i,vors[i].pos,vors[i].mass)

fn run():
    var vortex_00 = Vortex(
                 SIMD[DType.float16, 2](
                   0.0,
                   0.0,
                 ),
           1.0,
        )
    var vors:InlinedFixedVector[Vortex,NUM_VORTICES]
    print("initializing ... ", NUM_VORTICES, " vortices")
    vors.__init__(NUM_VORTICES)
    print("initialized ... with delta: ", delta)
    for i in range(N*B):
      for j in range(N*D):
        var ij:Int = i*N*D+j
        var x:Float16 = (i+0.5)*delta
        var y:Float16 = (j+0.5)*delta
        var w:Float16 = (y/D)*(1.0-y/D)*delta*delta
        var vortex_ij = Vortex(
                 SIMD[DType.float16, 2](
                   x,
                   y,
                 ),
           w,
        )
        vors.__setitem__(ij,vortex_ij)
    #--print init values--
    var t:Float16 = 0
    let dt:Float16 = 0.01
    write_vec(vors,t)
    #parallelize[advance](4,10)
    for i in range(10):
        t = t + dt
        print("advance:",i)
        advance(vors, dt)
        write_vec(vors,t)

fn strFloat(s:String) -> String:
   try:
      let L:Int = s.split(".")[0].__len__()
      var LF:Int = s.__len__()
      if(LF > L+7):
         LF = L+7
      return s.split(".")[0] + s[L:LF]
   except:
      return "Error!"

fn write_vec(vors: InlinedFixedVector[Vortex,NUM_VORTICES], t: Float16):
    var fp:String = "outx/vortices_" + strFloat(t) + ".data"
    
    try:
      var f = open(fp, "w")
      f.write("time:" + str(t) + "\n")
      for k in range(NUM_VORTICES):
        let v = vors.__getitem__(k)
        let sep:String = " "
        var data:String = str(k) + sep +  strFloat(str(v.pos[0])) + sep + strFloat(str(v.pos[1])) + sep + strFloat(str(v.mass)) + "\n"
        f.write(data)
      f.close()
    except IOError:
      print("Error: File does not appear to exist.")

fn main():
    #workers = num_performance_cores()
    print("Starting nvortices...")
    run()
