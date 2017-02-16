%%BeginProlog
%%BeginProcSet: patternfill 1.0 0
% Dots Per Inch
% width height matrix proc key cache
% definepattern -\> font
/definepattern { %def
   7 dict begin
      /FontDict 9 dict def
      FontDict begin
         /cache exch def
         /key exch def
         /proc exch cvx def
         /mtx exch matrix invertmatrix def
         /height exch def
         /width exch def
         /ctm matrix currentmatrix def
         /ptm matrix identmatrix def
         /str
         (12345678901234567890123456789012)
         def
      end
      /FontBBox [ %def
         0 0 FontDict /width get
         FontDict /height get
      ] def
      /FontMatrix FontDict /mtx get def
      /Encoding StandardEncoding def
      /FontType 3 def
      /BuildChar { %def
         pop begin
         FontDict begin
            width 0 cache { %ifelse
               0 0 width height setcachedevice
            }{ %else
               setcharwidth
            } ifelse
            0 0 moveto width 0 lineto
            width height lineto 0 height lineto
            closepath clip newpath
            gsave proc grestore
         end end
      } def
      FontDict /key get currentdict definefont
   end
} bind def
% dict patternpath -
% dict matrix patternpath -
/patternpath { %def
   dup type /dicttype eq { %ifelse
      begin FontDict /ctm get setmatrix
   }{ %else
      exch begin FontDict /ctm get setmatrix
      concat
   } ifelse
   currentdict setfont
   FontDict begin
      FontMatrix concat
      width 0 dtransform
      round width div exch round width div exch
      0 height dtransform
      round height div exch
      round height div exch
      0 0 transform round exch round exch
      ptm astore setmatrix
      
      pathbbox
      height div ceiling height mul 4 1 roll
      width div ceiling width mul 4 1 roll
      height div floor height mul 4 1 roll
      width div floor width mul 4 1 roll
      
      2 index sub height div ceiling cvi exch
      3 index sub width div ceiling cvi exch
      4 2 roll moveto
      
      FontMatrix ptm invertmatrix pop
      { %repeat
         gsave
            ptm concat
            dup str length idiv { %repeat
               str show
            } repeat
            dup str length mod str exch
            0 exch getinterval show
         grestore
         0 height rmoveto
      } repeat
      pop
   end end
} bind def

% dict patternfill -
% dict matrix patternfill -
/patternfill { %def
   gsave
   clip patternpath
   grestore
   newpath
} bind def

% dict patterneofill -
% dict matrix patterneofill -
/patterneofill { %def
   gsave
   eoclip patternpath
   grestore
   newpath
} bind def

% dict patternstroke -
% dict matrix patternstroke -
/patternstroke { %def
   gsave
   strokepath clip patternpath
   grestore
   newpath
} bind def

% dict ax ay string patternashow -
% dict matrix ax ay string patternashow -
/patternashow { %def
   (0) exch { %forall
      2 copy 0 exch put pop dup
      false charpath 
      currentpoint
      5 index type /dicttype eq { %ifelse
         5 index patternfill
      }{ %else
         6 index 6 index patternfill
      } ifelse
      moveto
      3 copy pop rmoveto
   } forall
   pop pop pop
   dup type /dicttype ne { pop } if pop
} bind def

% dict string patternshow -
% dict matrix string patternshow -
/patternshow { %def
   0 exch 0 exch patternashow
} bind def

/opaquepatternfill { %def
   gsave
   1 setgray
   fill
   grestore
   patternfill
} bind def

15 15 [300 72 div 0 0 300 72 div 0 0]
{ %definepattern
   2 setlinecap
   7.5 0 moveto 15 7.5 lineto
   0 7.5 moveto 7.5 15 lineto
   2 setlinewidth stroke
} bind
/RIGHTdiagonal true definepattern pop

20 20 [200 72 div 0 0 200 72 div 0 0]
{ %definepattern
   2 setlinecap
   5 0 moveto 5 20 lineto
   15 0 moveto 15 20 lineto
   2 setlinewidth stroke
} bind
/VERTpattern true definepattern pop

20 20 [200 72 div 0 0 200 72 div 0 0]
{ %definepattern
   2 setlinecap
   0 5 moveto 20 5 lineto
   0 15 moveto 20 15 lineto
   2 setlinewidth stroke
} bind
/HORIZpattern true definepattern pop

/sfactor {3 sqrt} bind def

15 15 sfactor mul [300 72 div 0 0 300 72 div 0 0]
{ %definepattern
        1 sfactor scale
   2 setlinecap
        0 7.5 moveto 7.5 15 lineto
   7.5 0 moveto 15 7.5 lineto
   2 setlinewidth stroke
} bind
/TRIADdiagonal true definepattern pop

30 30 sfactor mul [300 72 div 0 0 300 72 div 0 0]
{ %definepattern
        1 sfactor scale
        2 2  scale
   2 setlinecap
        0 7.5 moveto 7.5 15 lineto
   7.5 0 moveto 15 7.5 lineto
        15 7.5 moveto 7.5 15 lineto
   7.5 0 moveto 0 7.5 lineto
   0 0 moveto 15 0 lineto
   0 7.5 moveto 15 7.5 lineto
   0 15 moveto 15 15 lineto
   0.5 setlinewidth stroke
} bind
/HEXdiagonal true definepattern pop
%
% PSGraph drawing conventions
%
/dpi 1200 def
/setlabelfont {/TimesItalic findfont 
                dpi 4 div scalefont 
                setfont } def
/setdatafont {/Helvetica findfont 
                dpi 10 div scalefont 
                setfont } def
setlabelfont
/PathCount 0 def
/ResetPathCount {/PathCount 0 def} bind def
/AddPathCount {/PathCount PathCount 1 add def} bind def
/TestPathLimit {PathCount 1400 eq} bind def
/NP {newpath ResetPathCount} bind def
/ST {stroke ResetPathCount} bind def
/NonZeroPathCount {0 PathCount lt} bind def
/IP {NonZeroPathCount {ST} if} bind def
/M {moveto AddPathCount
   TestPathLimit {
   currentpoint /ypos exch def /xpos exch def
   ST xpos ypos moveto AddPathCount 
   } if
} bind def
/L {lineto AddPathCount
   TestPathLimit {
   currentpoint /ypos exch def /xpos exch def
   ST xpos ypos moveto AddPathCount 
   } if
} bind def
/P {moveto 0 0 rlineto stroke} bind def
% Tiles
/T {pop} def
/ET {pop} def
% Tiles with Color Choosing
%/T {CC} def
%/ET {pop} def
% Circles and Arcs
/C {IP 0 360 arc stroke} bind def
% Next assumes a color or gray has been set
/D {IP 0 360 arc fill stroke} bind def
% Sets a color by last argument = level
/Dc {pop CC IP 0 360 arc fill stroke}  def
% Sets a color by last tag 
% /Dc {CC pop IP 0 360 arc fill stroke}  def
% Creates a bubble color effect based on level
% /Dc {pop CC /rad exch def /y exch def /x exch def
%   1  -.005  0  % start incr end
%   { /fac exch def
%     fac sqrt sqrt sqrt sqrt sqrt /z exch def
%     [ currentcmykcolor ]
%     {z mul} forall 
%     setcmykcolor
%     newpath x y fac rad mul 0 360 arc fill stroke} for }  def
/rainbow 1 def
/raindir 1 def
/Drainbow {rainbow CC
  IP 0 360 arc fill stroke
  /rainbow rainbow raindir add def}  def
/E {IP gsave 5 -2 roll translate rotate exch dup
   /radius exch def div 1 exch scale
   0 0 radius 0 360 arc stroke grestore
} bind def
/N {IP arcn stroke} bind def
/A {IP arc stroke} bind def
/AT {arct} bind def
/CT {curveto} bind def
/FS {eofill stroke} bind def
%/FS {/RIGHTdiagonal findfont
%/FS {/VERTpattern findfont
%/FS {/HORIZpattern findfont
%/FS {/TRIADdiagonal findfont
%/FS {/HEXdiagonal findfont
%     gsave stroke grestore patterneofill} bind def
%  Arrows for Labels
/arrowtipoffset {.04 dpi mul} bind def
/tiplength {.07 dpi mul} bind def
/tiphalfwidth {.03 dpi mul} bind def
/tipwidth {tiphalfwidth 2 mul} bind def
% angle arrowhead
/arrowhead {
   gsave 
   currentpoint translate
   rotate
   currentlinewidth 4 div sqrt dup scale
   newpath  
   0 0 moveto
% DRAW TIP OF ARROW
   tiplength neg tiphalfwidth lineto
   0 tipwidth neg rlineto
   closepath fill stroke
   grestore
} def
%  x y len angle label putarrow
/putarrow {/alabel exch def
% ANGLE -> (cos, sin)
   dup cos /cangle exch def
       sin /sangle exch def
   /alength exch def
% ARROW TIP LOCATION
   arrowtipoffset sangle mul add /ay exch def
   arrowtipoffset cangle mul add /ax exch def
   gsave 20 setlinewidth newpath  
   ax ay moveto
% DRAW TIP OF ARROW
   tiplength cangle mul tiphalfwidth sangle mul add
   tiplength sangle mul tiphalfwidth cangle mul sub
   rlineto
   tipwidth neg sangle mul
   tipwidth cangle mul
   rlineto
   closepath fill stroke
% ARROW BODY
   ax ay moveto
   alength dpi mul dup cangle mul exch sangle mul
   rlineto
   gsave
   stroke
   grestore
% LABEL POSITIONING
   /ydif {0.04 dpi mul} def
   sangle 0.0 lt {/ydif {-0.2 dpi mul} def} if
   sangle 0.0 eq {/ydif {-0.1 dpi mul} def} if
   /xdif {cangle 1 sub .5 mul alabel length dpi 18 div mul mul
         .04 dpi mul cangle mul add } def
   xdif ydif rmoveto
   alabel show
   grestore
} def
% Arrow traces along lines
%     x y squarenorm -> sqrt(x^2 + y^2)
/norm { dup mul exch dup mul add sqrt } bind def
%     x y x0 y0 polar ->   r theta  = polar coords. of (x-x0,y-y0)
/polar { exch 
    4 1 roll sub
    3 1 roll exch sub exch
    2 copy
    norm
    3 1 roll exch
    atan } def
%   x y ang label V  -> lineto with arrow trace and label
/Vxoff { 0.1 dpi mul } bind def
/Vyoff { 0.05 dpi mul } bind def
/Vdelta { -0.05 dpi mul -0.03 dpi mul } bind def
/V { 5 copy Blue putarrow
    pop pop pop
    2 copy 
    currentpoint
    polar
    gsave
    Red
    rotate
    2 div
    Vxoff sub Vyoff neg rmoveto
    Vxoff 2 mul 0 rlineto
    Vdelta rlineto
    stroke
    grestore 
    Black
    lineto } def
/Vno { %% No labelling
    pop pop pop
    2 copy
    currentpoint
    polar
    gsave
    Red
    rotate
    2 div
    Vxoff sub Vyoff neg rmoveto
    Vxoff 2 mul 0 rlineto
    Vdelta rlineto
    stroke
    grestore
    Black
    lineto } def
%
% Raster output (1 or 8 bit depth)
%
/grayproc { 0 gt {0} {1} ifelse} def
/rasterimage {         % jdepth iused jused ss xmin ymin rasterimage <data> grestore
    /inout exch def    % Boolean parameter specifies inside or outside
    translate          % Set lower-left corner
    dup scale          % Set scale
    /jused exch def
    /iused exch def
    1 eq              % Compare jdepth against 1-bit (imagemask)
      {/lused {iused 8 idiv} def
        iused jused inout
        [ iused 0 0 jused 0 0]
        {currentfile lused string readhexstring pop}
        imagemask}
      {{grayproc} settransfer
        iused jused 8
        [ iused 0 0 jused 0 0]
        {currentfile iused string readhexstring pop}
        image}
      ifelse
} def
% raster image needs to be followed by raster data and then grestore
%
% Color Images
/mycolorimage {  % iused jused ss xmin ymin mycolorimage <data> grestore
    translate          % Set lower-left corner (from xmin ymin)
    scale              % Set scale (from ssx ssy)
    /jused exch def    % Vertical pixels
    /iused exch def    % Horizontal pixels
    /picstr iused 3 mul string def   % String for image data
    iused jused 8
    [ iused 0 0 jused 0 0]
    {currentfile picstr readhexstring pop}
    false 3
    colorimage
} def
% mycolorimage needs to be followed by raster data and then grestore
%
%%Begin Colors (from dvips)
%
% Sets up setcmykcolor if necessary
/setcmykcolor where{pop}{/setcmykcolor{dup 10 eq{pop
setrgbcolor}{1 sub 4 1 roll 3{3 index add neg dup 0 lt{pop 0}if 3 1 roll
}repeat setrgbcolor pop}ifelse} bind def}ifelse
%
% Preset Colors
/GreenYellow{0.15 0 0.69 0 setcmykcolor} bind def 
/Yellow{0 0 1 0 setcmykcolor} bind def 
/Goldenrod{0 0.10 0.84 0 setcmykcolor} bind def 
/Dandelion{0 0.29 0.84 0 setcmykcolor} bind def 
/Apricot{0 0.32 0.52 0 setcmykcolor} bind def 
/Peach{0 0.50 0.70 0 setcmykcolor} bind def 
/Melon{0 0.46 0.50 0 setcmykcolor} bind def 
/YellowOrange{0 0.42 1 0 setcmykcolor} bind def 
/Orange{0 0.61 0.87 0 setcmykcolor} bind def 
/BurntOrange{0 0.51 1 0 setcmykcolor} bind def 
/Bittersweet{0 0.75 1 0.24 setcmykcolor} bind def 
/RedOrange {0 0.77 0.87 0 setcmykcolor} bind def 
/Mahogany{0 0.85 0.87 0.35 setcmykcolor} bind def
/Maroon{0 0.87 0.68 0.32 setcmykcolor} bind def 
/BrickRed{0 0.89 0.94 0.28 setcmykcolor} bind def 
/Red{0 1 1 0 setcmykcolor} bind def 
/OrangeRed{0 1 0.50 0 setcmykcolor} bind def 
/RubineRed{0 1 0.13 0 setcmykcolor} bind def 
/WildStrawberry{0 0.96 0.39 0 setcmykcolor} bind def 
/Salmon{0 0.53 0.38 0 setcmykcolor} bind def 
/CarnationPink{0 0.63 0 0 setcmykcolor} bind def 
/LightPink{0 0.42 0 0 setcmykcolor} bind def 
/Magenta{0 1 0 0 setcmykcolor} bind def 
/VioletRed{0 0.81 0 0 setcmykcolor} bind def 
/Rhodamine{0 0.82 0 0 setcmykcolor} bind def 
/Mulberry{0.34 0.90 0 0.02 setcmykcolor} bind def 
/RedViolet{0.07 0.90 0 0.34 setcmykcolor} bind def 
/Fuchsia{ 0.47 0.91 0 0.08 setcmykcolor} bind def 
/Lavender{0 0.48 0 0 setcmykcolor} bind def 
/Thistle {0.12 0.59 0 0 setcmykcolor} bind def 
/Orchid{0.32 0.64 0 0 setcmykcolor} bind def
/DarkOrchid{0.40 0.80 0.20 0 setcmykcolor} bind def 
/Purple{0.45 0.86 0 0 setcmykcolor} bind def 
/Plum{0.50 1 0 0 setcmykcolor} bind def 
/Violet{0.79 0.88 0 0 setcmykcolor} bind def 
/RoyalPurple{0.75 0.90 0 0 setcmykcolor} bind def 
/BlueViolet{0.86 0.91 0 0.04 setcmykcolor} bind def 
/Periwinkle{0.57 0.55 0 0 setcmykcolor} bind def
/CadetBlue{0.62 0.57 0.23 0 setcmykcolor} bind def 
/CornflowerBlue{0.65 0.13 0 0 setcmykcolor} bind def 
/MidnightBlue{0.98 0.13 0 0.43 setcmykcolor} bind def 
/NavyBlue{0.94 0.54 0 0 setcmykcolor} bind def 
/RoyalBlue{1 0.50 0 0 setcmykcolor} bind def 
/Blue{1 1 0 0 setcmykcolor} bind def 
/Cerulean{0.94 0.11 0 0 setcmykcolor} bind def 
/Cyan{1 0 0 0 setcmykcolor} bind def 
/ProcessBlue{0.96 0 0 0 setcmykcolor} bind def 
/SkyBlue{0.62 0 0.12 0 setcmykcolor} bind def 
/LightBlue{0.42 0 0.05 0 setcmykcolor} bind def 
/Turquoise{0.85 0 0.20 0 setcmykcolor} bind def 
/TealBlue{0.86 0 0.34 0.02 setcmykcolor} bind def 
/Aquamarine{0.82 0 0.30 0 setcmykcolor} bind def 
/BlueGreen{ 0.85 0 0.33 0 setcmykcolor} bind def 
/Emerald{1 0 0.50 0 setcmykcolor} bind def 
/JungleGreen {0.99 0 0.52 0 setcmykcolor} bind def 
/SeaGreen{0.69 0 0.50 0 setcmykcolor} bind def 
/Green{ 1 0 1 0 setcmykcolor} bind def 
/ForestGreen{0.91 0 0.88 0.12 setcmykcolor} bind def
/PineGreen{0.92 0 0.59 0.25 setcmykcolor} bind def 
/LimeGreen{0.50 0 1 0 setcmykcolor } bind def 
/YellowGreen{0.44 0 0.74 0 setcmykcolor} bind def 
/SpringGreen{0.26 0 0.76 0 setcmykcolor} bind def 
/OliveGreen{0.64 0 0.95 0.40 setcmykcolor} bind def 
/RawSienna{0 0.72 1 0.45 setcmykcolor} bind def 
/Sepia{0 0.83 1 0.70 setcmykcolor} bind def 
/Brown{0 0.81 1 0.60 setcmykcolor} bind def 
/Tan{0.14 0.42 0.56 0 setcmykcolor} bind def 
/LightGray{0 0 0 0.10 setcmykcolor} bind def 
/Gray{0 0 0 0.50 setcmykcolor} bind def 
/DarkGray{0 0 0 0.90 setcmykcolor} bind def 
/Black{0 0 0 1 setcmykcolor} bind def 
/White{0 0 0 0 setcmykcolor} bind def
%%End Colors
% Color Choosing Routines
% Color Maps
/cmapA [{RoyalPurple} {Red} {Orange} {Yellow} {Green} {Blue} {Cyan}] def
/cmapFour [{Green} {Red} {Cyan} {Yellow}] def
/cmapFive [{White} {Green} {Red} {Cyan} {Yellow}] def
/cmapB [{Bittersweet}
        {Peach} 
        {Orange} 
        {BurntOrange}
        {YellowOrange} 
        {Dandelion} 
        {Goldenrod}
        {Yellow} 
        {GreenYellow}] def
/cmapC [
{0     1     1     0 setcmykcolor}
{0    0.9    1     0 setcmykcolor}
{0    0.8    1     0 setcmykcolor}
{0    0.7    1     0 setcmykcolor}
{0    0.6    1     0 setcmykcolor}
{0    0.5    1     0 setcmykcolor}
{0    0.4    1     0 setcmykcolor}
{0    0.3    1     0 setcmykcolor}
{0    0.2    1     0 setcmykcolor}
{0    0.1    1     0 setcmykcolor}
{0     0     1     0 setcmykcolor}
{0.1   0     0.9   0 setcmykcolor}
{0.2   0     0.8   0 setcmykcolor}
{0.3   0     0.7   0 setcmykcolor}
{0.4   0     0.6   0 setcmykcolor}
{0.5   0     0.5   0 setcmykcolor}
{0.6   0     0.4   0 setcmykcolor}
{0.7   0     0.3   0 setcmykcolor}
{0.8   0     0.2   0 setcmykcolor}
{0.9   0     0.1   0 setcmykcolor}
{1     0     0     0 setcmykcolor}
{0.9   0.1   0     0 setcmykcolor}
{0.8   0.2   0     0 setcmykcolor}
{0.7   0.3   0     0 setcmykcolor}
{0.6   0.4   0     0 setcmykcolor}
{0.5   0.5   0     0 setcmykcolor}
{0.4   0.6   0     0 setcmykcolor}
{0.3   0.7   0     0 setcmykcolor}
{0.2   0.8   0     0 setcmykcolor}
{0.1   0.9   0     0 setcmykcolor}
{0     1     0     0 setcmykcolor}
] def
/cmapD [
{0.00 1.00 1.00 0 setcmykcolor}
{0.00 .95 1.00 0 setcmykcolor}
{0.00 .90 1.00 0 setcmykcolor}
{0.00 .85 1.00 0 setcmykcolor}
{0.00 .80 1.00 0 setcmykcolor}
{0.00 .75 1.00 0 setcmykcolor}
{0.00 .70 1.00 0 setcmykcolor}
{0.00 .65 1.00 0 setcmykcolor}
{0.00 .60 1.00 0 setcmykcolor}
{0.00 .55 1.00 0 setcmykcolor}
{0.00 .50 1.00 0 setcmykcolor}
{0.00 .45 1.00 0 setcmykcolor}
{0.00 .40 1.00 0 setcmykcolor}
{0.00 .35 1.00 0 setcmykcolor}
{0.00 .30 1.00 0 setcmykcolor}
{0.00 .25 1.00 0 setcmykcolor}
{0.00 .20 1.00 0 setcmykcolor}
{0.00 .15 1.00 0 setcmykcolor}
{0.00 .10 1.00 0 setcmykcolor}
{0.00 .05 1.00 0 setcmykcolor}
{0.00 0.00 1.00 0 setcmykcolor}
{.05 0.00 .95 0 setcmykcolor}
{.10 0.00 .90 0 setcmykcolor}
{.15 0.00 .85 0 setcmykcolor}
{.20 0.00 .80 0 setcmykcolor}
{.25 0.00 .75 0 setcmykcolor}
{.30 0.00 .70 0 setcmykcolor}
{.35 0.00 .65 0 setcmykcolor}
{.40 0.00 .60 0 setcmykcolor}
{.45 0.00 .55 0 setcmykcolor}
{.50 0.00 .50 0 setcmykcolor}
{.55 0.00 .45 0 setcmykcolor}
{.60 0.00 .40 0 setcmykcolor}
{.65 0.00 .35 0 setcmykcolor}
{.70 0.00 .30 0 setcmykcolor}
{.75 0.00 .25 0 setcmykcolor}
{.80 0.00 .20 0 setcmykcolor}
{.85 0.00 .15 0 setcmykcolor}
{.90 0.00 .10 0 setcmykcolor}
{.95 0.00 .05 0 setcmykcolor}
{1.00 0.00 0.00 0 setcmykcolor}
{.95 .05 0.00 0 setcmykcolor}
{.90 .10 0.00 0 setcmykcolor}
{.85 .15 0.00 0 setcmykcolor}
{.80 .20 0.00 0 setcmykcolor}
{.75 .25 0.00 0 setcmykcolor}
{.70 .30 0.00 0 setcmykcolor}
{.65 .35 0.00 0 setcmykcolor}
{.60 .40 0.00 0 setcmykcolor}
{.55 .45 0.00 0 setcmykcolor}
{.50 .50 0.00 0 setcmykcolor}
{.45 .55 0.00 0 setcmykcolor}
{.40 .60 0.00 0 setcmykcolor}
{.35 .65 0.00 0 setcmykcolor}
{.30 .70 0.00 0 setcmykcolor}
{.25 .75 0.00 0 setcmykcolor}
{.20 .80 0.00 0 setcmykcolor}
{.15 .85 0.00 0 setcmykcolor}
{.10 .90 0.00 0 setcmykcolor}
{.05 .95 0.00 0 setcmykcolor}
{0.00 1.00 0.00 0 setcmykcolor}
] def
/cmapE [
{0.00 1.00 1.00 0 setcmykcolor}
{0.00 .98 1.00 0 setcmykcolor}
{0.00 .96 1.00 0 setcmykcolor}
{0.00 .94 1.00 0 setcmykcolor}
{0.00 .92 1.00 0 setcmykcolor}
{0.00 .90 1.00 0 setcmykcolor}
{0.00 .88 1.00 0 setcmykcolor}
{0.00 .86 1.00 0 setcmykcolor}
{0.00 .84 1.00 0 setcmykcolor}
{0.00 .82 1.00 0 setcmykcolor}
{0.00 .80 1.00 0 setcmykcolor}
{0.00 .78 1.00 0 setcmykcolor}
{0.00 .76 1.00 0 setcmykcolor}
{0.00 .74 1.00 0 setcmykcolor}
{0.00 .72 1.00 0 setcmykcolor}
{0.00 .70 1.00 0 setcmykcolor}
{0.00 .68 1.00 0 setcmykcolor}
{0.00 .66 1.00 0 setcmykcolor}
{0.00 .64 1.00 0 setcmykcolor}
{0.00 .62 1.00 0 setcmykcolor}
{0.00 .60 1.00 0 setcmykcolor}
{0.00 .58 1.00 0 setcmykcolor}
{0.00 .56 1.00 0 setcmykcolor}
{0.00 .54 1.00 0 setcmykcolor}
{0.00 .52 1.00 0 setcmykcolor}
{0.00 .50 1.00 0 setcmykcolor}
{0.00 .48 1.00 0 setcmykcolor}
{0.00 .46 1.00 0 setcmykcolor}
{0.00 .44 1.00 0 setcmykcolor}
{0.00 .42 1.00 0 setcmykcolor}
{0.00 .40 1.00 0 setcmykcolor}
{0.00 .38 1.00 0 setcmykcolor}
{0.00 .36 1.00 0 setcmykcolor}
{0.00 .34 1.00 0 setcmykcolor}
{0.00 .32 1.00 0 setcmykcolor}
{0.00 .30 1.00 0 setcmykcolor}
{0.00 .28 1.00 0 setcmykcolor}
{0.00 .26 1.00 0 setcmykcolor}
{0.00 .24 1.00 0 setcmykcolor}
{0.00 .22 1.00 0 setcmykcolor}
{0.00 .20 1.00 0 setcmykcolor}
{0.00 .18 1.00 0 setcmykcolor}
{0.00 .16 1.00 0 setcmykcolor}
{0.00 .14 1.00 0 setcmykcolor}
{0.00 .12 1.00 0 setcmykcolor}
{0.00 .10 1.00 0 setcmykcolor}
{0.00 .08 1.00 0 setcmykcolor}
{0.00 .06 1.00 0 setcmykcolor}
{0.00 .04 1.00 0 setcmykcolor}
{0.00 .02 1.00 0 setcmykcolor}
{0.00 0.00 1.00 0 setcmykcolor}
{.02 0.00 .98 0 setcmykcolor}
{.04 0.00 .96 0 setcmykcolor}
{.06 0.00 .94 0 setcmykcolor}
{.08 0.00 .92 0 setcmykcolor}
{.10 0.00 .90 0 setcmykcolor}
{.12 0.00 .88 0 setcmykcolor}
{.14 0.00 .86 0 setcmykcolor}
{.16 0.00 .84 0 setcmykcolor}
{.18 0.00 .82 0 setcmykcolor}
{.20 0.00 .80 0 setcmykcolor}
{.22 0.00 .78 0 setcmykcolor}
{.24 0.00 .76 0 setcmykcolor}
{.26 0.00 .74 0 setcmykcolor}
{.28 0.00 .72 0 setcmykcolor}
{.30 0.00 .70 0 setcmykcolor}
{.32 0.00 .68 0 setcmykcolor}
{.34 0.00 .66 0 setcmykcolor}
{.36 0.00 .64 0 setcmykcolor}
{.38 0.00 .62 0 setcmykcolor}
{.40 0.00 .60 0 setcmykcolor}
{.42 0.00 .58 0 setcmykcolor}
{.44 0.00 .56 0 setcmykcolor}
{.46 0.00 .54 0 setcmykcolor}
{.48 0.00 .52 0 setcmykcolor}
{.50 0.00 .50 0 setcmykcolor}
{.52 0.00 .48 0 setcmykcolor}
{.54 0.00 .46 0 setcmykcolor}
{.56 0.00 .44 0 setcmykcolor}
{.58 0.00 .42 0 setcmykcolor}
{.60 0.00 .40 0 setcmykcolor}
{.62 0.00 .38 0 setcmykcolor}
{.64 0.00 .36 0 setcmykcolor}
{.66 0.00 .34 0 setcmykcolor}
{.68 0.00 .32 0 setcmykcolor}
{.70 0.00 .30 0 setcmykcolor}
{.72 0.00 .28 0 setcmykcolor}
{.74 0.00 .26 0 setcmykcolor}
{.76 0.00 .24 0 setcmykcolor}
{.78 0.00 .22 0 setcmykcolor}
{.80 0.00 .20 0 setcmykcolor}
{.82 0.00 .18 0 setcmykcolor}
{.84 0.00 .16 0 setcmykcolor}
{.86 0.00 .14 0 setcmykcolor}
{.88 0.00 .12 0 setcmykcolor}
{.90 0.00 .10 0 setcmykcolor}
{.92 0.00 .08 0 setcmykcolor}
{.94 0.00 .06 0 setcmykcolor}
{.96 0.00 .04 0 setcmykcolor}
{.98 0.00 .02 0 setcmykcolor}
{1.00 0.00 0.00 0 setcmykcolor}
{.98 .02 0.00 0 setcmykcolor}
{.96 .04 0.00 0 setcmykcolor}
{.94 .06 0.00 0 setcmykcolor}
{.92 .08 0.00 0 setcmykcolor}
{.90 .10 0.00 0 setcmykcolor}
{.88 .12 0.00 0 setcmykcolor}
{.86 .14 0.00 0 setcmykcolor}
{.84 .16 0.00 0 setcmykcolor}
{.82 .18 0.00 0 setcmykcolor}
{.80 .20 0.00 0 setcmykcolor}
{.78 .22 0.00 0 setcmykcolor}
{.76 .24 0.00 0 setcmykcolor}
{.74 .26 0.00 0 setcmykcolor}
{.72 .28 0.00 0 setcmykcolor}
{.70 .30 0.00 0 setcmykcolor}
{.68 .32 0.00 0 setcmykcolor}
{.66 .34 0.00 0 setcmykcolor}
{.64 .36 0.00 0 setcmykcolor}
{.62 .38 0.00 0 setcmykcolor}
{.60 .40 0.00 0 setcmykcolor}
{.58 .42 0.00 0 setcmykcolor}
{.56 .44 0.00 0 setcmykcolor}
{.54 .46 0.00 0 setcmykcolor}
{.52 .48 0.00 0 setcmykcolor}
{.50 .50 0.00 0 setcmykcolor}
{.48 .52 0.00 0 setcmykcolor}
{.46 .54 0.00 0 setcmykcolor}
{.44 .56 0.00 0 setcmykcolor}
{.42 .58 0.00 0 setcmykcolor}
{.40 .60 0.00 0 setcmykcolor}
{.38 .62 0.00 0 setcmykcolor}
{.36 .64 0.00 0 setcmykcolor}
{.34 .66 0.00 0 setcmykcolor}
{.32 .68 0.00 0 setcmykcolor}
{.30 .70 0.00 0 setcmykcolor}
{.28 .72 0.00 0 setcmykcolor}
{.26 .74 0.00 0 setcmykcolor}
{.24 .76 0.00 0 setcmykcolor}
{.22 .78 0.00 0 setcmykcolor}
{.20 .80 0.00 0 setcmykcolor}
{.18 .82 0.00 0 setcmykcolor}
{.16 .84 0.00 0 setcmykcolor}
{.14 .86 0.00 0 setcmykcolor}
{.12 .88 0.00 0 setcmykcolor}
{.10 .90 0.00 0 setcmykcolor}
{.08 .92 0.00 0 setcmykcolor}
{.06 .94 0.00 0 setcmykcolor}
{.04 .96 0.00 0 setcmykcolor}
{.02 .98 0.00 0 setcmykcolor}
{0.00 1.00 0.00 0 setcmykcolor}
] def
%
% Color choose based on 0 <= x <=1
/CCC { /x exch def
  x 6 mul /y exch def
  y 1 lt
  { 1 y 0 0 setcmykcolor }
  { y 2 lt 
    { 2 y sub 1 0 0 setcmykcolor }
    { y 3 lt 
      { 0 1 y 2 sub 0 setcmykcolor }
      { y 4 lt 
        { 0 4 y sub 1 0 setcmykcolor }
        { y 5 lt 
          { y 4 sub 0 1 0 setcmykcolor }
          { 1 0 6 y sub 0 setcmykcolor }
          ifelse }
        ifelse }
      ifelse }
    ifelse }
  ifelse }   def 
% Choose the color map
/cmap cmapA def
/lcmap {cmap length} def
/cfac lcmap def
/CC {abs lcmap mod cmap exch get exec} def
%% Color choosing based on two parameters
%/CCB {pop abs cmap length mod cmap exch get exec} def
%/CCB {abs exch pop cmap length mod cmap exch get exec} def
/CCB {exch cfac mul abs exch idiv
      lcmap mod cmap exch get exec} def
%%EndProlog
% Round Caps: for plotting isolated points
1 setlinecap
2 setlinejoin
%begin(plot)
%%Pages: 0 1
0.06 0.06 scale
