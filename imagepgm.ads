package imagePGM is

   -- Field defination
   type ImageMatrix is array(Integer  range <>, Integer range <>) of Integer;
   type Histogram is array(Integer  range <>) of Integer;
   type Histofloat is array(Integer  range <>) of Float;

   -- Functions header
   function readPGM (ImageSrc : String; maxCol : Integer; maxRow : Integer; maxValue : Integer) return ImageMatrix;
   procedure writePGM (saveAddress : String ; inputImage : ImageMatrix ; maxCol : Integer ; maxRow : Integer ; maxValue : Integer);

end imagePGM;
