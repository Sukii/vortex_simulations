#== nvortices in motion ===
from collections.vector import InlinedFixedVector
import math

alias PI = 3.141592653589793
alias D = 2
alias B = 5
alias N = 2
alias delta = 0.5
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



fn advance(inout vors: InlinedFixedVector[Vortex,NUM_VORTICES], dt: Float16):
# Clear the original vector
    @unroll
    for i in range(NUM_VORTICES):
        var vortex_i = vors[i]
        @unroll(NUM_VORTICES - 1)
        for j in range(NUM_VORTICES - i - 1):
           var vortex_j = vors[j + i + 1]
           let diff = vortex_i.pos - vortex_j.pos
           let cdiff = SIMD[DType.float16, 2](diff[1],-diff[0])
           var diff_sqr = (diff * diff).reduce_add()
           if(diff_sqr < eps):
              diff_sqr = eps
           let mag = dt / (diff_sqr)
           vortex_i.pos -= cdiff * vortex_j.mass * mag
           vortex_j.pos += cdiff * vortex_i.mass * mag
           vors[j + i + 1] = vortex_j
           
        vors[i] = vortex_i

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
    print("initialized ...")
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
    for i in range(10):
        t = t + dt
        print("advance:",i)
        advance(vors, dt)
        write_vec(vors,t)

fn write_vec(vors: InlinedFixedVector[Vortex,NUM_VORTICES], t: Float16):
    var fp:String = "outx/vortices_" + str(math.round(t*100)) + ".data"
    try:
      var f = open(fp, "w")
      f.write("time:" + str(t) + "\n")
      for k in range(NUM_VORTICES):
        let v = vors.__getitem__(k)
        var data:String = str(k) + "," +  str(v.pos[0]) + "," + str(v.pos[2]) + "," + str(v.mass) + "\n"
        f.write(data)
      f.close()
    except IOError:
      print("Error: File does not appear to exist.")

fn main():
    print("Starting nvortices...")
    run()
