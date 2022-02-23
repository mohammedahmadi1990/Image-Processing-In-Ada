with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Exceptions;                use Ada.Exceptions;
with imagePGM;                      use imagePGM;
with imagePROCESS;                  use imagePROCESS;

procedure Image is

   --fileName : String := "d:\test3_p2.pgm";
   option : String (1 .. 1);
   Last : Natural;
   fn   : Unbounded_String;
   maxCol : integer;
   maxRow : integer;
   maxValue : Integer;

   function getFilename (flag : String) return Unbounded_String is

      userFileName : Unbounded_String;
      -- Last : Natural;
      imageType : String (1 .. 2);
      File : File_Type;

   begin
      if flag = "r" then
         --  Read user input (PGM Image File)
         Put_Line("Please enter the file to read: ");
         Get_Line(userFileName);
         begin
            -- Open file
            Open (File => File,
             Mode => In_File,
                Name => To_String(userFileName));

            -- Read file header to validate
            imageType := Get_Line (File);
            if(imageType /= "P2") then
               raise PROGRAM_ERROR with "PGM Image file is not valid!";
            end if;

            -- Read PGM image details
            Get (File,maxCol);
            Get (File,maxRow);
            Get (File,maxValue);
            Close (File);
         exception
            when Name_Error =>
               -- check
               Put_Line ("File does not exist!");
            when others =>
               Put_Line ("Error while processing input file");
        end;
      elsif flag = "w" then
         --  Write user input (PGM Image File)
            Put("File is saved Automatically after image processing. ");
         begin
            Create (File, Out_File, To_String (userFileName));
            Delete (File);
         exception
            when Name_Error =>
               Put_Line ("File exist!");
            when others =>
               Put_Line ("Error while processing input file");
        end;
      else
         Put("Invalid input!");
      end if;
      return userFileName;
   end getFilename;

begin

   -- Ask user to start
   Put_Line ("WELCOME TO MY IMAGE PROCCESSING APPLICATION");
   Put("Enter r (Read), w (Write): ");
   Ada.Text_IO.Get_Line(option, Last);
   Skip_Line;
   fn := getFilename(option);

   -- Dynamic decalration of 2D matrix
   declare
      matrix00 : ImageMatrix(1 .. maxCol , 1 .. maxRow);
      matrix01 : ImageMatrix(1 .. maxCol , 1 .. maxRow);
      matrix02 : ImageMatrix(1 .. maxCol , 1 .. maxRow);
      matrix03 : ImageMatrix(1 .. maxCol , 1 .. maxRow);
      hist : Histogram(1 .. 255);
      matrix04 : ImageMatrix(1 .. maxCol , 1 .. maxRow);
   begin

      -- READ FILE
      matrix00 := imagePGM.readPGM(To_String(fn),maxCol,maxRow,maxValue);

      --  Image Proccessing
      matrix01 := imagePROCESS.imageINV(matrix00);             -- Image inversion
      matrix02 := imagePROCESS.imageLOG(matrix00);             -- Log transform
      matrix03 := imagePROCESS.imageSTRETCH(matrix00,64,251);  -- Contrast stretching
      hist := imagePROCESS.makeHIST(matrix00);                 -- Histogram
      matrix04 := imagePROCESS.histEQUAL(matrix00);            -- Histogram Equalization

      -- Save Image
      imagePGM.writePGM(To_String(fn),matrix04,maxCol,maxRow,maxValue);
      Put("File saved!");
   end;

end Image;


