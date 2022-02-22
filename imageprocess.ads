with imagePGM;                           use imagePGM;
with Ada.Text_IO;                        use Ada.Text_IO;
with Ada.Integer_Text_IO;                use Ada.Integer_Text_IO;
with Ada.Numerics;                       use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;  use  Ada.Numerics.Elementary_Functions;

package imagePROCESS is   

   -- Function's headers
   function imageINV (inputImage : ImageMatrix) return ImageMatrix;
   function imageLOG (inputImage : ImageMatrix) return ImageMatrix;
   function imageSTRETCH (inputImage : ImageMatrix; iMin : Integer ; iMax : Integer) return ImageMatrix;
   function makeHIST (inputImage : ImageMatrix) return Histogram;

end imagePROCESS;
