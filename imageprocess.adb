with Ada.Numerics;                       use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;  use  Ada.Numerics.Elementary_Functions;

package body imagePROCESS is

   -- image Inversion function
   function imageINV (inputImage : ImageMatrix) return ImageMatrix is   
      maxCol : Integer := inputImage'Length(1);
      maxRow : Integer := inputImage'Length(2);
      outputImage : ImageMatrix(1 .. maxCol , 1 .. maxRow);
      
   begin
      
      for i in 1 .. maxRow loop
         for j in 1 .. maxCol loop
            outputImage(j,i) := abs(255 - inputImage(j,i));
         end loop;
      end loop;     
      
      return outputImage;
   end imageINV;
   
   -- Image Logarithm function
   function imageLOG (inputImage : ImageMatrix) return ImageMatrix is   
      maxCol : Integer := inputImage'Length(1);
      maxRow : Integer := inputImage'Length(2);
      outputImage : ImageMatrix(1 .. maxCol , 1 .. maxRow);
      value01 : Float;  -- temporary variable
      value02 : Float;  -- temporary variable
      
   begin      
      
      -- imageLOG(I)
      value01 := 255.0 / Log (255.0, 10.0);
      for i in 1 .. maxRow loop
         for j in 1 .. maxCol loop
            value02 := Float(inputImage(j,i));
            outputImage(j,i) := Integer(Log(value02,10.0) * value01);
         end loop;
      end loop;
      
      return outputImage;
   end imageLOG;
   
   -- Image grayscale Streaching function based on input iMin and iMax as minimum and maximum intensities in the image   
   function imageSTRETCH (inputImage : ImageMatrix; iMin : Integer ; iMax : Integer) return ImageMatrix is   
      maxCol : Integer := inputImage'Length(1);
      maxRow : Integer := inputImage'Length(2);
      outputImage : ImageMatrix(1 .. maxCol , 1 .. maxRow);
      
   begin            
      
      for i in 1 .. maxRow loop
         for j in 1 .. maxCol loop      
            outputImage(j,i) := 255 * (inputImage(j,i) - iMin) / (iMax - iMin);
         end loop;
      end loop;
      
      return outputImage;
   end imageSTRETCH;
   
   -- This function calculates a histogram of an image and returns 1D array of integers 
   function makeHIST (inputImage : ImageMatrix) return Histogram is
      output : Histogram(1 .. 255);
      maxCol : Integer := inputImage'Length(1);
      maxRow : Integer := inputImage'Length(2);
   begin
      
      -- init
      for h in 1 .. 255 loop
         output(h) := 0;
      end loop;
      
      for h in 1 .. 255 loop              -- histogram intensity array (1~255)
         for i in 1 .. maxRow loop        -- pixels in row
            for j in 1 .. maxCol loop     -- pixels in column
               if inputImage(j,i) = h  then
                  output(h) := output(h) + 1;
               end if;
            end loop; 
         end loop; 
      end loop; 
      return output;
   end makeHIST;
   
   -- Histogram equalization function
   function histEQUAL (inputImage : ImageMatrix) return ImageMatrix is   
      maxCol : Integer := inputImage'Length(1);
      maxRow : Integer := inputImage'Length(2);
      outputImage : ImageMatrix(1 .. maxCol , 1 .. maxRow);
      hist : Histogram(1 .. 255);
      pdf : Histofloat(1 .. 255);
      ch : Histofloat(1 .. 255);
      MN : Float;
   begin
      
      -- init
      for h in 1 .. 255 loop
         hist(h) := 0;
      end loop;
      
      -- STEP II
      for h in 1 .. 255 loop              -- histogram intensity array (1~255)
         for i in 1 .. maxRow loop        -- pixels in row
            for j in 1 .. maxCol loop     -- pixels in column
               if inputImage(j,i) = h  then
                  hist(h) := hist(h) + 1;
               end if;
            end loop; 
         end loop; 
      end loop; 
            
      -- STEP II
      MN := Float(maxRow*maxCol);
      for h in 1 .. 255 loop     -- intensities
         pdf(h) := Float(hist(h))/MN;
      end loop;
      
      -- STEP III
      ch(1) := pdf(1);
      for h in 2 .. 255 loop     -- intensities
         ch(h) := ch(h-1) + pdf(h);         
      end loop;
      
      -- STEP IV
      for h in 1 .. 255 loop     -- intensities
         ch(h) := ch(h) * 255.0;         
      end loop;
      
      -- STEP V
      for i in 1 .. maxRow loop        -- pixels in row
         for j in 1 .. maxCol loop     -- pixels in column
            outputImage(j,i) := Integer(ch(inputImage(j,i)));
         end loop; 
      end loop;            
            
      return outputImage;
   end histEQUAL;
   
end imagePROCESS;
