"""
rokit.py is used to model, predict and optimise 
the flight of a bottle rocket filled with water
and pressurised with air. 
"""


from __future__ import division


def flight(rangled):    

    """
    Flight declares the variables and initial conditions. The loop logic defines
    the three stages of the flight and calls various other functions to calculate
    thrust, pressure, position, velocity and acceleration.
    
    The output of flight is the distanced travelled in the x direction in metres.
    """
    
    
    # Declaring variables and constants
    CoD = 0.2 # Coefficient of drag
    k = 1.4 # ratio of specific heat capacities of air 
    row = 998.0 # Density of water at standard conditions (kg/m^3)
    roa = 1.20  # Density of air at standard conditions (kg/m^3)
    g = 9.81    # Acceleration caused due to gravity    (m/s^2)
    Patm = 101.3 * 10**3 # Atmospheric pressure           (Pa)
    Pg = 275 * 10**3 # Gauge Pressure when valve releases  (Pa)
    Dn = 0.022 # diameter of bottle nozzle
    Db = 0.08 # projected area of bottle normal to direction of flight (m^2)
    Astar = pi * Dn**2/4.0  # area of nozzle at bottle opening in (m^2)
    Adrag = pi * Db**2/4.0  # area of bottle at base (m^2)
    Lramp = 0.5    # length of launch ramp in metres
    Tatm = 273.15 + 22 # temperature of the day (K)
    Plaunch = Patm + Pg # Absolute pressure of rocket at launch    (Pa)         
    Vrkt = 1.05 * 10**-3   # total volume inside the rocket (m^3)
    
    Vairo = Vrkt - Vfuelo   # initial volume of air inside rocket
    Vfuel = Vfuelo   # setting initial conditions
    Vair = Vairo    # setting initial conditions
    Ma = Plaunch*Vairo/(288.0*Tatm) # Mass of enclosed air can be estimated
    Mr = 0.080 # mass of rocket with no fuel (kg)
    Mfuel = row * Vfuel  # mass of fuel in rocket (kg)
    Mtot = Mr + Ma + Mfuel  # total mass of rocekt (kg)
    
    wi = 0.0   # Initial conditions for Runge Kutta
    ti = 0.0   # Initial condition for flight time (seconds)
    h = 0.001  # step size (seconds)
    
    sx = 0.0 # Displacement in x is initially zero
    sxlist = [] # Array to hold x displacements 
    sxlist.append(sx) # Stores initial x-position
    sy = 0.0 # Displacement in y is initially zero
    sylist = [] # Array to hold y displacements 
    sylist.append(sy) # Stores initial y-position
    vx = 0.0 # Velocity in x is initially zero
    vxlist = [] # Array to hold x displacements 
    vxlist.append(vx) # Stores initial x velocity
    vy = 0.0 # Velocity in y is initially zero
    vylist = [] # Array to hold y displacements 
    vylist.append(vy) # Stores initial y velocity
    tilist = [] # Array to hold time 
    tilist.append(ti) # Stores initial time of 0
    y = array([sx, vx, sy, vy])
    
    magnvelo = sqrt(y[1]**2 + y[3]**2) # magnitude of the velocity in (m/s)
    rangler = rangled * pi / 180.0  # angle of launch ramp in radians
    theta = rangler  #  angle once rocket leaves ramp and is no longer fixed.
    
    
    def dydtlaunch(t, y):
        """
        ODEs describing the rockets motion during the launch phase, 
        Returns velocity and acceleration in an array ydot.    
        """        
        
        sx, vx, sy, vy = y
       
        ax = ((thrust * cos(rangler))-(0.5*roa*CoD*Adrag*magnvelo*vx*cos(rangler)) - Mtot*g*sin(rangler)*cos(rangler))/Mtot
    
        ay = ((thrust * sin(rangler))-(0.5*roa*CoD*Adrag*magnvelo*vy*sin(rangler)) - Mtot*g*sin(rangler)*sin(rangler))/Mtot

        ydot = array([vx, ax, vy, ay])
    
        return ydot
    
    
    def dydtfuel(t, y):
        """
        ODEs describing the the rockets motion after it leaves the ramp.
        Returns velocity and acceleration in an array ydot.
        """
        
        sx, vx, sy, vy = y
        
        ax = ((thrust * cos(theta))-(0.5*roa*CoD*Adrag*magnvelo*vx*cos(theta)))/Mtot
        
        ay = ((thrust * sin(theta))-(0.5*roa*CoD*Adrag*magnvelo*vy*sin(theta)) - Mtot*g)/Mtot
        
        ydot = array([vx, ax, vy, ay])
        
        return ydot
    
       
    
    def rk45_step(f, ti, wi, h):
        """ 
        Code from 2700 demo code notes.        
        Integrate one step using the Runge-Kutta fourth order
        
        f(t,y) is a user supplied function giving dy/dt.
        ti: starting value for independent variable.
        wi: starting values(s) for the solution variable
        h: step-size in t
        
        returns the approximate integral of f(t,y)
        """
       
        k1 = h * f(ti, wi)
        k2 = h * f(ti+h/2, wi+k1/2)
        k3 = h * f(ti+h/2, wi+k2/2)
        k4 = h * f(ti+h, wi+k3)
        wip = wi + 1.0/6*(k1 + 2*k2 + 2*k3 + k4)
        return wip
        
        
    def Vairkt(Vair):
        """
        Function is used to calculate the volume of air in the rocket. This is
        needed to determine the pressure in the rocket to calculate the thrust
        """
        if Pthrust > Patm:
            return Vair + h * Astar * sqrt(2.0*(Plaunch*(Vairo/Vair)**1.4-Patm)/row) # m^3
        else:
            return Vrkt
            
    
    def Prkt(Vair):
        """
        This function is used to calculate the pressure inside the rocket 
        using isentropic gas law. The pressure calculated is used to calculate 
        thrust.
        """
        
        return Plaunch * (Vairo/Vair)**1.4  # pressure in Pa
           
        
    def Thrustrkt(Pthrust):
        """
        Function is used to calculate the thrust produced by water
        until water is zero and then thrust caused by air rocket in Newtons
        """
            
        return 2.0*Astar*(Pthrust-Patm) # thrust of rocket in newtons
        
            
    def compair(Mtot, h):
        """
        Function is used to calculate thrust during compressed air stage
        
        First it calculates the density of the air in the bottle.
        It then uses this to calculate the pressure in the bottle.
        From this it can calculate the Mach number.
        It also uses exit density to calculate the exit temperature.
        """
        
        if Mtot - Mr > 0.002 :
            roab = (Mtot-Mr)/Vrkt  # calculates density of the air in the rocket (kg/m^3)
            Pb = Patm*(roab/roa)**1.4 # calculates the pressure in the bottle (Pa)
            Mach = (((Pb/Patm)**(1/3.5) - 1.0)/0.2)**0.5  # calculates Mach number
            
            Tb = Tatm*((Pb/Patm)**1.4)**(1.0/(1.4/0.4))      # calculates Temp in bottle (K)
            Te = Tb/(1.0 + 0.2 * Mach**2)                 # calculates temp at exit (K)
            roaex = roab / (((Tb/Te)**(3.5))**(1.0/1.4)) # calculates density of air at exit (kg/m^3)
            
            sos = sqrt(1.4*287*Te)  # calculates speed of sound at exit  (m/s)
            Ve = Mach*sos        # calculates exit velocity, Ma = V/a   (m/s)        
            Mdot = roaex * Astar * Ve  # calculates mass flow rate   (kg/s)
            thrust = Mdot * Ve         # calculates thrust          (N)
            """print "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f" % \
                    (thrust, Pb, Mach, Mdot, Mtot, Mr)"""
            
            if Pb > Patm * 1.2**3.5:   # checks to determine pressure is above choke
                Mach = 1.0
                Te = 0.8333*Tb       # Temperature relation when choking occurs (K)
                sos = sqrt(1.4*287*Te) # calculates the speed of sound at throat (m/s)
                Ve = Mach*sos        # calculates exit velocity, Ma = V/a   (m/s)        
                Mdot = roaex * Astar * Ve  # calculates mass flow rate   (kg/s)
                thrust = Mdot * Ve         # calculates thrust          (N)
                
                return thrust, Mdot
            else:
                
                return thrust, Mdot
        else:
            
            return 0.0 ,0.0     
    
    
    """
    While loop contains the all three stages of the flight, 
    launch stage there is thrust from water and the angle is fixed. Stage finishes
    when the rocket has traveled the distance of the ramp.
    
    Stage two there is thrust from water and angle is not fixed. Stage finishes 
    when water runs out. ie total mass of the rocket is equal mass of empty
    rocket plus the mass pumped in initially.
    
    Stage three is when there is thrust from air or no thrust at all.
    """
    
    
    while y[2] > -0.0001:
        
        
        if Mtot > Mr + Ma:
            
            """Stage 1"""
            if sqrt(y[0]**2+ y[2]**2) <= Lramp: # if rocket is on ramp 
                
                Pthrust = Prkt(Vair)   # calls function to calculate pressure in bottle which is needed to calculate thrust
                Vair = Vairkt(Vair)     # calls function to calculate/upate volume of air in rocket
                thrust = Thrustrkt(Pthrust) # calls function to calculate the thrust
                y = rk45_step(dydtlaunch, ti, y, h) # calls function to calculate position and velocity using Runge Kutta
                ti = ti + h                         # increases time by timestep
                magnvelo = sqrt(y[1]**2 + y[3]**2)  # calculates/updates the magnitude of the velocity
                Mtot = Mr + Ma + row*(Vrkt - Vair)
                sxlist.append(y[0])
                vxlist.append(y[1])
                sylist.append(y[2])
                vylist.append(y[3])
                tilist.append(ti)
                
                """print "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f" % \
                (Vair, Pthrust, thrust, Mtot, Mr+Ma, sqrt(y[0]**2+ y[2]**2) )"""
                
            else:
               
                Pthrust = Prkt(Vair)   # calls function to calculate pressure in bottle which is needed to calculate thrust
                Vair = Vairkt(Vair)     # calls function to calculate/upate volume of air in rocket                
                thrust = Thrustrkt(Pthrust) # calls function to calculate the thrust
                y = rk45_step(dydtfuel, ti, y, h) # calls function to calculate position and velocity using Runge Kutta
                ti = ti + h
                theta = atan(y[3]/y[1])            # calculates angle of flight by using theta = inverse tan( vy/vx)
                magnvelo = sqrt(y[1]**2 + y[3]**2)  # calculates/updates the magnitude of the velocity
                Mtot = Mr + Ma + row*(Vrkt - Vair)
                sxlist.append(y[0])
                vxlist.append(y[1])
                sylist.append(y[2])
                vylist.append(y[3])
                tilist.append(ti)
                
                """print "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f" % \
                (Vair, Pthrust, thrust, Mtot, Mr + Ma, magnvelo)"""
          
        else:
                
            """
            Stage 2 and 3
            
            The next loop is for the third stage which is ballistic flight. There is no
            thrust from water. Thrust is from compressed air or there is not thrust. 
            In this loop choking is considered. 
            Stage ends when y position is less than 0.0. 
            """
            
            if sqrt(y[0]**2+ y[2]**2) <= Lramp: # if rocket is on ramp 
                
                thrust, Mdot = compair(Mtot, h)       # calls function to calculate thrust and flow rate
                y = rk45_step(dydtlaunch, ti, y, h)  # calls function to calculate position and velocity using Runge Kutta
                Mtot = Mtot - Mdot * h
                ti = ti + h
                        
                magnvelo = sqrt(y[1]**2 + y[3]**2) # calculates magnitude of velocity needed for drag calcs
                
                sxlist.append(y[0])
                vxlist.append(y[1])
                sylist.append(y[2])
                vylist.append(y[3])
                tilist.append(ti)
                
                
                """print "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f" % \
                (Vair, Pthrust, thrust, Mtot, Mr+Ma, sqrt(y[0]**2+ y[2]**2) )"""
                
            else:
               
                thrust, Mdot = compair(Mtot, h)       # calls function to calculate thrust and flow rate
                y = rk45_step(dydtfuel, ti, y, h)  # calls function to calculate position and velocity using Runge Kutta
                Mtot = Mtot - Mdot * h
                ti = ti + h
                theta = atan(y[3]/y[1])            # calculates angle of flight by using theta = inverse tan( vy/vx)
                magnvelo = sqrt(y[1]**2 + y[3]**2) # calculates magnitude of velocity needed for drag calcs
                
                sxlist.append(y[0])
                vxlist.append(y[1])
                sylist.append(y[2])
                vylist.append(y[3])
                tilist.append(ti)
            
                
            
          
    return y[0]  # returns the x-distance travelled
    

def maximize(f, a, b, tolerance):
    """
    Finding a minimum using golden search. 
    
    Returns the bracket xL,xR containing the minimum of the function f.
    However we are searching for a maximum so by feeding in -f(fuel) the 
    maximum will be found.

    f is a user supplied function which calculates distance based on fuel Vol
    a,b is the original bracket containing a maximum, 
    tolerance is the difference between xR and xL.
    
    """
    r = 0.618034
    xL = a + (1-r)*(b-a)
    xR = a + r*(b-a)
    FL = -1*f(xL)
    FR = -1*f(xR)

    while (xR - xL) > tolerance:
        if FR > FL:
            
            b = xR
            xR = xL
            FR = FL
            xL = a + (1-r)*(b-a)
            FL = -1*f(xL)
            """print "xL=%g  FL=%g" % (xL, FL)"""
        else:
            a = xL
            xL = xR
            FL = FR
            xR = a + r*(b-a)
            FR = -1*f(xR)
            """print " xR=%g    FR=%g" % (xR, FR)"""
    return xL, FR

def shooting(beta, g0 ,g1):
    """
    Shooting method, takes in beta value for x(1), i.e the distance
    that wants to be achieved along with two intial guesses g0 and g1.
    Shooting returns the volume of fuel that should be used to achieve this
    distance.
    """
    
    # Find error for initial guesses and plot IVP solutions
    xsoln = flight(g0*10**-6) 
    e0=xsoln - beta
    xsoln = flight(g1*10**-6)
    e1= xsoln - beta
    
    # Now iterate
    for n in range(0,40):
        # Find next guess
        g2= g1-(g1-g0)/(e1-e0)*e1
        """print "%10.6f %10.6f %10.6f %10.6f %10.6f %10.6f" % \
        (xsoln, g0, g1, g2, e1, e2) """     
        
        xsoln = flight(g2*10**-6)
        e2= xsoln - beta
        # Check if solution good enough
          
        
        if abs(e2)<0.5:  # if the error is less than 1 metre
            return g2
        # Increment variables
        g0=g1
        e0=e1
        g1=g2
        e1=e2  
             
if __name__ == '__main__':
    print 'Begin rocket simulation'
    
    from math import *
    from pylab import *
    from numpy import *
    
    Vfuelarray = [] # fuel array for graphing
    xdist = []  # stores the maximum distances achieved
    arrangle = []   # stores the angle for graphing 
    Vfuelo = 0.000175  #inital volume of fuel  m^3  
    Vrkt = 1.05 * 10**-3   # total volume inside the rocket (m^3)
     
    """ 
    loop performs golden search for maximum distance by increments of 25 mL.
    For each volume the ramp angle is calculated using golden search to find
    max distance. These values are stored in arrays.
    
    From testing it was known the maximum occurs between 100 mL to 500 mL. 
    """
    while 0.000375 - Vfuelo > 0.00001:
        
        a = 25   # optimum ramp angle between 0.1 to 90 degrees
        b = 60
        xL, FL = maximize(flight, a, b, 0.5)  #calls optimization function, stores max distance, FL,  and corresponding ramp angle xL 
        Vfuelarray.append(Vfuelo)
        xdist.append(-1.0*FL)
        arrangle.append(xL)
        Vfuelo = Vfuelo + 0.0000250  # increments fuel volume by 25 mL, accuracy of measuring jug.       
        
    """ 
    loop finds the maximum max distance stored in the xdist array and 
    finds its position in the array. It then stores the corrosponding 
    fuel volume and ramp angle that gave that distance
    """   
    maxdist = 0   
    n = len(xdist)     # the number of elments in xi array 
    for i in range(n):
        if xdist[i] > maxdist:    
            maxdist = xdist[i]
            maxfuel = Vfuelarray[i]
            maxangle = arrangle[i]
             
    print 'The maximum distance the rocket can achieve is ', maxdist, ' metres.'
    print 'This distance is achieved by using ',maxfuel*1000.0, ' L of water '
    print 'and a ramp angle of ', maxangle, ' degrees.'
    plot(Vfuelarray, xdist)
    show() 
     
    """ 
    rangled = 25 # angle of launch ramp in degrees  
    text = raw_input("Enter the required distance: ")
    beta = float(text)    
    p = shooting(beta ,g0 = 85, g1 = 150)  # guesses of 300 mL and 600 mL
    print 'The volume of fuel necessary to travel ', text, ' metres at a ramp '\
        'angle of', rangled, ' degrees is ', "%3.1f" % (p), ' mL.'
    """
