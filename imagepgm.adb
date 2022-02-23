with Ada.Text_IO, Ada.Strings.Unbounded,Ada.Integer_Text_IO; 
use Ada.Text_IO, Ada.Strings.Unbounded,Ada.Integer_Text_IO;
with Ada.Text_IO.Text_Streams;  use Ada.Text_IO.Text_Streams;
with Ada.Task_Identification;  use Ada.Task_Identification;

package body imagePGM is           
      
   -- read image function: it reads PGM-images
   function readPGM (ImageSrc : String; maxCol : Integer; maxRow : Integer; maxValue : Integer) return ImageMatrix is      
      matrix01 : ImageMatrix(1 .. maxCol , 1 .. maxRow);
      File : File_Type;               
      tempInt : Integer;
      tempInt3 : Natural;
      tempStr : String(1 .. 2);
   begin
      begin
      
         Open (File => File,
               Mode => In_File,
               Name => imageSrc);
      
         -- Skip headers of Image
         Get(File, tempStr);
         for s in 1 .. 3 loop
            Get(File, tempInt);       
         end loop;      
      
         for i in 1 .. maxRow loop
            for j in 1 .. maxCol loop            
               Get (File,tempInt3);               
               matrix01(j,i) := tempInt3;               
            end loop;
         end loop;    
      exception
         when others =>
            Put_Line ("Error! Input image is not a correct PGM-file.");
            Abort_Task (Current_Task);
      end;
      
      return matrix01;
      
   end readPGM;
     
   -- write image function: it writes proccessed image into a PGM-image
   procedure writePGM (saveAddress : String ; inputImage : ImageMatrix ; maxCol : Integer ; maxRow : Integer ; maxValue : Integer) is
      F         : File_Type;
      File_Name : String := saveAddress(saveAddress'First .. saveAddress'last-4) & "_output.pgm"; -- save same file name + _output.pgm
      lineString : Unbounded_String;
   begin
      
      Create (F, Out_File, File_Name);
      -- Create file hedaers
      Put_Line (F, "P2"); -- file type
      Put_Line (F, Integer'Image(maxCol) & Integer'Image(maxRow)); -- column & rows value
      Put_Line (F, Integer'Image(maxValue)); -- Maximum intensity of the image such as 255
              
      -- writing image pixels
      for i in 1 .. maxRow loop
         for j in 1 .. maxCol loop            
            lineString := (lineString & Integer'Image(inputImage(j,i)));                          
         end loop;
         Put_Line(F,Ada.Strings.Unbounded.To_String(lineString));
         lineString := Ada.Strings.Unbounded.Null_Unbounded_String;
      end loop;
      Close (F); 
   end writePGM;

   end imagePGM;
