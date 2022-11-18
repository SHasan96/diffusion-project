----------------------------------
--CSC 330
--SIMPLIFIED 3D DIFFUSION MODEL
----------------------------------
with ada.text_io, ada.integer_text_io, ada.long_float_text_io, 
     ada.command_line, ada.strings.bounded;
     
use ada.text_io, ada.integer_text_io, ada.long_float_text_io,
    ada.command_line;

procedure diffusion is  
    type array_3D is array(Integer range <>, Integer range <>, Integer range <>) of Long_Float; 
    
    -- Read arguments from command line
    maxsize: Integer := Integer'Value(Argument(Number => 1));
    p_flag : String := Argument(Number => 2);
     
    diffusion_coefficient : Long_Float := 0.175;           -- Note: Ada variables are strongly typed.
    room_dimension : Long_Float := 5.0;                    -- 5 Meters
    speed_of_gas_molecules : Long_Float := 250.0;          -- Based on 100 g/mol gas at RT

    timestep : Long_Float  := 0.0; -- h in seconds
    distance_between_blocks : Long_Float := 0.0;
    DTerm : Long_Float := 0.0;
    
    time : Long_Float := 0.0;
    ratio : Long_Float := 0.0;

begin 
    timestep := (room_dimension / speed_of_gas_molecules) / Long_float(maxsize); -- h in seconds
    distance_between_blocks := room_dimension / Long_Float(maxsize);
    DTerm := diffusion_coefficient * timestep / (distance_between_blocks*distance_between_blocks);

    declare 
        cube : array_3D(0..maxsize-1, 0..maxsize-1, 0..maxsize-1) := (others => (others => (others => 0.0))); 
        change : Long_Float := 0.0;
        minval : Long_Float;
        maxval : Long_Float;
        sumval : Long_Float := 0.0;
                
    begin
        cube(0,0,0) := 1.0e21; -- Initiazlize first cell 
        
        -- Add partition if user entered 'y'
        declare
           px : Integer := Integer(Float'Ceiling(Float(maxsize)*0.5)-1.0); -- use lower median for x 
           py : Integer := Integer(Float'Ceiling(Float(maxsize)*(1.0-0.75))-1.0); -- partition height as (1 - percent height)
       
        begin
           if p_flag = "y" then
               for j in py..maxsize-1 loop
                   for k in 0..maxsize-1 loop
                       cube(px,j,k) := -1.0; -- Mark partition spaces with -1
                   end loop;
               end loop;
           end if; 
        end;
    
        while ratio <= 0.99 loop       
           for i in 0..maxsize-1 loop
               for j in 0..maxsize-1 loop
                   for k in 0..maxsize-1 loop
                       for l in 0..maxsize-1 loop
                           for m in 0..maxsize-1 loop
                               for n in 0..maxsize-1 loop
                                     if cube(i,j,k) = -1.0 or cube (l,m,n) = -1.0 then -- encounters partition
                                         null;
                                     elsif ((i = l  and j = m and  k = n+1) or 
                                        (i = l  and j = m and  k = n-1 ) or 
                                        (i = l  and j = m+1 and k = n  ) or 
                                        (i = l  and j = m-1 and k = n  ) or 
                                        (i = l+1 and j = m and  k = n  ) or 
                                        (i = l-1 and j = m and  k = n  )) then
                                            change := ( cube(i,j,k) - cube(l,m,n) ) * DTerm;                                              
                                            cube(i,j,k) := cube(i,j,k) - change;                             
                                            cube(l,m,n) := cube(l,m,n) + change;
                                     end if;
                               end loop;
                           end loop;
                       end loop;
                   end loop;
               end loop;
           end loop;
        
           time := time + timestep;
        
           sumval := 0.0;
           minval := cube(0,0,0);
           maxval := cube(0,0,0);

           for i in 0..maxsize-1 loop
               for j in 0..maxsize-1 loop
                   for k in 0..maxsize-1 loop
                       if cube(i,j,k) = -1.0 then -- encounters partition
                           null;
                       else
                           maxval := Long_Float'Max(cube(i,j,k),maxval);
                           minval := Long_Float'Min(cube(i,j,k),minval);
                           sumval := sumval + cube(i,j,k);
                       end if;
                    end loop;
               end loop;
           end loop;

           ratio := minval / maxval;
           
           --put(ratio);
           --put("  ");
           --put(time);
           --put("  ");
           --put(cube(maxsize-1,0,0));
           --put("  ");
           --put(cube(maxsize-1,maxsize-1, 0));
           --put("  ");
           --put(cube(maxsize-1,maxsize-1,maxsize-1));
           --put("  ");
           --put(sumval);
           --new_line;
        end loop; -- ending of while loop 

        put("Box equilibrated in "); 
        put(time, Exp => 0); 
        put( " seconds of simulated time.");
        new_line;
     end;    

end diffusion;
       


