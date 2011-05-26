#!/usr/local/bin/ruby
#
# Problem Statement: A parachutist of mass 68.1 kg jumps out of a stationary hot
# air balloon. Use Eq. (1.12) to compute the velocity prior to opening the chut.
# The drag coefficient is equal to 12.5kg/s
#
# Eq. 1.12:
#    v(t_i+1) = v(t_i) + [g - (c/m)v(t_i)](t_i+1 - t_i)
#

$drag    = 12.5
$g       = 9.8
$max_rep = 5

def calc(v,mass,t,t1)
   return v + (($g - (($drag/mass) * v)) * (t1 - t))
end

# v(t) mass t step_size n
def main
    v         = ARGV[0].to_f
    mass      = ARGV[1].to_f
    t         = ARGV[2].to_i
    step_size = ARGV[3].to_i
    n         = ARGV[4].to_i
    t1        = t + step_size

    rep_count = 0;

    print "\nv(t):\t",v,"\n"
    print "mass:\t",mass,"\n"
    print "   t:\t",t,"\n"

    print "\nt,s\t\t\tv,m/s\n"
    print t, "\t\t\t", v, "\n"
    for i in 0...n
        v1 = calc(v,mass,t,t1)

        if (v1 - v).abs < 0.01
            rep_count += 1
        end
        
        v = v1
        t = t1
        t1 = t + step_size
       

        if rep_count >= $max_rep
            puts "Inf:\t\t\t%0.03f" % v
            break
        else
            puts "%i\t\t\t%0.03f\n" % [t,v]
        end
    end
end

main
