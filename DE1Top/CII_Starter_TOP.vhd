use work.types.all;
use work.all;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity CII_Starter_TOP is
     port (HEX0 : out std_logic_vector(0 to 6);
           HEX1 : out std_logic_vector(0 to 6);
           HEX2 : out std_logic_vector(0 to 6);
           HEX3 : out std_logic_vector(0 to 6);
           LEDG : out std_logic_vector(0 to 3);
           I2C_SCLK : out std_logic;
           I2C_SDAT : inout std_logic;
           AUD_DACLRCK : in std_logic;
           AUD_DACDAT : out std_logic;
           AUD_XCK : out std_logic;
           AUD_BCLK : in std_logic;
           AUD_ADCLRCK : in std_logic;
           AUD_ADCDAT : in std_logic;
           CLOCK_50 : in std_logic;
           CLOCK_24 : in std_logic_vector(0 to 0);
           KEY : in std_logic_vector(0 to 1);
           PS2_CLK : in std_logic;
           PS2_DAT : in std_logic;
           VGA_R   : out std_logic_vector(0 to 3);
           VGA_G   : out std_logic_vector(0 to 3);
           VGA_B   : out std_logic_vector(0 to 3);
           VGA_HS  : out std_logic;
           VGA_VS  : out std_logic;
           SW      : in std_logic_vector(0 to 0);
           SRAM_ADDR : out std_logic_vector (0 to 17);
           SRAM_DQ : inout std_logic_vector (0 to 15);
           SRAM_CE_N : out std_logic;
           SRAM_LB_N : out std_logic;
           SRAM_OE_N : out std_logic;
           SRAM_UB_N : out std_logic;
           SRAM_WE_N : out std_logic);
end entity CII_Starter_TOP;

architecture structural of CII_Starter_TOP is
  signal rstkbdatasdaIsramInaudioClks : Z5TZLz2cUz2cUz2cUz2cUZRbooleanZ2TZLz2cUZRstd_logicstd_logicstd_logicvectorzmstd_logiczm0_to_16Z3TZLz2cUz2cUZRstd_logicstd_logicstd_logic;
  signal rst  : boolean;
  signal kbdata : Z2TZLz2cUZRstd_logicstd_logic;
  signal sdai : std_logic;
  signal sramIn : vectorzmstd_logiczm0_to_16;
  signal audioClks : Z3TZLz2cUz2cUZRstd_logicstd_logicstd_logic;
  signal donefaultdacDati2cOhexdispsvgaOutsramOut : Z7TZLz2cUz2cUz2cUz2cUz2cUz2cUZRbooleanbooleanstd_logicZ4TZLz2cUz2cUz2cUZRstd_logicbooleanstd_logicbooleanZ4TZLz2cUz2cUz2cUZRvectorzmstd_logiczm0_to_7vectorzmstd_logiczm0_to_7vectorzmstd_logiczm0_to_7vectorzmstd_logiczm0_to_7Z5TZLz2cUz2cUz2cUz2cUZRstd_logicstd_logicvectorzmstd_logiczm0_to_4vectorzmstd_logiczm0_to_4vectorzmstd_logiczm0_to_4Z7TZLz2cUz2cUz2cUz2cUz2cUz2cUZRvectorzmstd_logiczm0_to_16vectorzmstd_logiczm0_to_18std_logicstd_logicstd_logicstd_logicstd_logic;
  signal done : boolean;
  signal fault : boolean;
  signal audDacDat : std_logic;
  signal i2co : Z4TZLz2cUz2cUz2cUZRstd_logicbooleanstd_logicboolean;
  signal sclo : std_logic;
  signal scloEn : boolean;
  signal sdao : std_logic;
  signal sdaoEn : boolean;
  signal hexdisps : Z4TZLz2cUz2cUz2cUZRvectorzmstd_logiczm0_to_7vectorzmstd_logiczm0_to_7vectorzmstd_logiczm0_to_7vectorzmstd_logiczm0_to_7;
  signal vgaOut : Z5TZLz2cUz2cUz2cUz2cUZRstd_logicstd_logicvectorzmstd_logiczm0_to_4vectorzmstd_logiczm0_to_4vectorzmstd_logiczm0_to_4;
  signal sramOut : Z7TZLz2cUz2cUz2cUz2cUz2cUz2cUZRvectorzmstd_logiczm0_to_16vectorzmstd_logiczm0_to_18std_logicstd_logicstd_logicstd_logicstd_logic;
  signal sramDataOut : vectorzmstd_logiczm0_to_16;
  signal sramAddress : vectorzmstd_logiczm0_to_18;
  signal sramWE : std_logic;
  signal sramOE : std_logic;
  signal sramUB : std_logic;
  signal sramLB : std_logic;
  signal sramCE : std_logic;
  signal audioClk12 : std_logic;
  signal sysclock50 : std_logic;
  signal fftClk10 : std_logic;
  signal audioClkLocked : std_logic;
  signal systemClkLocked : std_logic;
begin
  audiopll_inst : audiopll PORT MAP (
        areset   => not KEY(0),
        inclk0   => CLOCK_24(0),
        c0       => audioClk12,
        locked   => audioClkLocked
  );

  fftpll_inst : fftpll PORT MAP (
        areset   => not KEY(0),
        inclk0   => CLOCK_50,
        c0   => sysclock50,
        c1   => fftClk10,
        locked   => systemClkLocked
    );

  AUD_XCK <= audioClk12;

  rst <= true when KEY(1) = '0' else false;
  
  kbdata.A <= PS2_DAT;
  kbdata.B <= PS2_CLK;

  sdai <= I2C_SDAT;
  
  sramIn <= vectorzmstd_logiczm0_to_16(SRAM_DQ);

  audioClks.A <= AUD_ADCLRCK;
  audioClks.B <= AUD_DACLRCK;
  audioClks.C <= AUD_ADCDAT;

  rstkbdatasdaIsramInaudioClks.A <= rst;
  rstkbdatasdaIsramInaudioClks.B <= kbdata;
  rstkbdatasdaIsramInaudioClks.C <= sdai;
  rstkbdatasdaIsramInaudioClks.D <= sramIn;
  rstkbdatasdaIsramInaudioClks.E <= audioClks;
  
  component0 : entity de1Component_0
                    port map (inputHooks2047615916 => rstkbdatasdaIsramInaudioClks,
                              arrowHooksOut2047615907 => donefaultdacDati2cOhexdispsvgaOutsramOut,
                              clock1 => sysclock50,
                              clock2 => fftClk10,
                              clock3 => AUD_BCLK,
                              resetn => (audioClkLocked AND systemClkLocked));
  
  done      <= donefaultdacDati2cOhexdispsvgaOutsramOut.A;
  fault     <= donefaultdacDati2cOhexdispsvgaOutsramOut.B;
  audDacDat <= donefaultdacDati2cOhexdispsvgaOutsramOut.C;
  i2co      <= donefaultdacDati2cOhexdispsvgaOutsramOut.D;
  hexdisps  <= donefaultdacDati2cOhexdispsvgaOutsramOut.E;
  vgaOut    <= donefaultdacDati2cOhexdispsvgaOutsramOut.F;
  sramOut   <= donefaultdacDati2cOhexdispsvgaOutsramOut.G;
  
  AUD_DACDAT <= audDacDat;

  sclo   <= i2co.A;
  scloEn <= i2co.B;
  sdao   <= i2co.C;
  sdaoEn <= i2co.D;
  
  I2C_SDAT <= sdao when sdaoEn = false else 'Z';
  I2C_SCLK <= '0'  when scloEn = false else '1';
  
  HEX0 <= std_logic_vector(hexdisps.D);
  HEX1 <= std_logic_vector(hexdisps.C);
  HEX2 <= std_logic_vector(hexdisps.B);
  HEX3 <= std_logic_vector(hexdisps.A);
  
  LEDG(0) <= '1' when (done = true)    else '0';
  LEDG(1) <= '1' when (rst = true)     else '0';
  LEDG(2) <= not (audioClkLocked AND systemClkLocked);
  LEDG(3) <= '1' when (fault = true)   else '0';

  VGA_R <= std_logic_vector(vgaOut.C);
  VGA_G <= std_logic_vector(vgaOut.D);
  VGA_B <= std_logic_vector(vgaOut.E);

  VGA_HS <= vgaOut.A;
  VGA_VS <= vgaOut.B;
  
  sramDataOut <= sramOut.A;
  sramAddress <= sramOut.B;
  sramWE <= sramOut.C;
  sramOE <= sramOut.D;
  sramUB <= sramOut.E;
  sramLB <= sramOut.F;
  sramCE <= sramOut.G;
  
  SRAM_DQ <= (others => 'Z') when sramWE = '1' else (std_logic_vector(sramDataOut));
  SRAM_ADDR <= std_logic_vector(sramAddress);
  SRAM_CE_N <= sramCE;
  SRAM_LB_N <= sramLB;
  SRAM_OE_N <= sramOE;
  SRAM_UB_N <= sramUB;
  SRAM_WE_N <= sramWE;

end architecture structural;
