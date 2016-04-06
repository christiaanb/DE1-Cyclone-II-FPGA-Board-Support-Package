use work.all;
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity DE1TOP is
  port(KEY1        : in boolean;
       PS2_DAT     : in std_logic_vector(0 downto 0);
       PS2_CLK     : in std_logic_vector(0 downto 0);
       I2C_SDAT    : inout std_logic_vector(0 downto 0);
       SRAM_DQ     : inout std_logic_vector(15 downto 0);
       AUD_ADCLRCK : in std_logic_vector(0 downto 0);
       AUD_DACLRCK : in std_logic_vector(0 downto 0);
       AUD_ADCDAT  : in std_logic_vector(0 downto 0);
       SW0         : in boolean;
       SW1         : in boolean;
       CLOCK_50    : in std_logic_vector(0 downto 0);
       CLOCK_24    : in std_logic_vector(0 downto 0);
       AUD_BCLK    : in std_logic_vector(0 downto 0);
       KEY0        : in std_logic_vector(0 downto 0);
       LEDG0       : out boolean;
       LEDG1       : out boolean;
       AUD_DACDAT  : out std_logic_vector(0 downto 0);
       I2C_SCLK    : out boolean;
       HEX3        : out std_logic_vector(6 downto 0);
       HEX2        : out std_logic_vector(6 downto 0);
       HEX1        : out std_logic_vector(6 downto 0);
       HEX0        : out std_logic_vector(6 downto 0);
       VGA_HS      : out std_logic_vector(0 downto 0);
       VGA_VS      : out std_logic_vector(0 downto 0);
       VGA_R       : out std_logic_vector(3 downto 0);
       VGA_G       : out std_logic_vector(3 downto 0);
       VGA_B       : out std_logic_vector(3 downto 0);
       SRAM_ADDR   : out std_logic_vector(17 downto 0);
       SRAM_WE_N   : out std_logic_vector(0 downto 0);
       SRAM_OE_N   : out std_logic_vector(0 downto 0);
       SRAM_UB_N   : out std_logic_vector(0 downto 0);
       SRAM_LB_N   : out std_logic_vector(0 downto 0);
       SRAM_CE_N   : out std_logic_vector(0 downto 0);
       AUD_XCK     : out std_logic_vector(0 downto 0);
       LEDR        : out std_logic_vector(1 downto 0));
end entity DE1TOP;

architecture structural of DE1TOP is
  signal audiopll_locked : std_logic;
  signal sdao            : std_logic_vector(0 downto 0);
  signal sdaoEn          : boolean;
  signal sramDataOut     : std_logic_vector(15 downto 0);
  signal sramWe_n        : std_logic_vector(0 downto 0);
begin
  audiopll_inst : entity audiopll
    port map
      (inclk0 => CLOCK_24(0)
      ,c0     => AUD_XCK(0)
      ,areset => NOT KEY0(0)
      ,locked => audiopll_locked);

  de1_inst : entity de1
    port map
      (KEY1          => NOT KEY1
      ,PS2_DAT       => PS2_DAT
      ,PS2_CLK       => PS2_CLK
      ,I2C_SDAT      => I2C_SDAT
      ,sramDataIn    => SRAM_DQ
      ,AUD_ADCLRCK   => AUD_ADCLRCK
      ,AUD_DACLRCK   => AUD_DACLRCK
      ,AUD_ADCDAT    => AUD_ADCDAT
      ,SW0           => SW0
      ,SW1           => SW1
      ,KEY0          => KEY0
      ,CLOCK_50      => CLOCK_50
      ,bclk3000      => AUD_BCLK(0)
      ,bclk3000_rstn => audiopll_locked
      ,LEDG0         => LEDG0
      ,LEDG1         => LEDG1
      ,AUD_DACDAT    => AUD_DACDAT
      -- ,sclo          => sclo
      ,I2C_SCLK      => I2C_SCLK
      ,sdao          => sdao
      ,sdaoEn        => sdaoEn
      ,HEX3          => HEX3
      ,HEX2          => HEX2
      ,HEX1          => HEX1
      ,HEX0          => HEX0
      ,VGA_HS        => VGA_HS
      ,VGA_VS        => VGA_VS
      ,VGA_R         => VGA_R
      ,VGA_G         => VGA_G
      ,VGA_B         => VGA_B
      ,sramDataOut   => sramDataOut
      ,SRAM_ADDR     => SRAM_ADDR
      ,sramWe_n      => sramWe_n
      ,SRAM_OE_N     => SRAM_OE_N
      ,SRAM_UB_N     => SRAM_UB_N
      ,SRAM_LB_N     => SRAM_LB_N
      ,SRAM_CE_N     => SRAM_CE_N);

  LEDR(0 downto 0) <= "1" when SW0 else "0";
  LEDR(1 downto 1) <= "1" when SW1 else "0";

  I2C_SDAT <= "Z" when sdaoEn else sdao;

  SRAM_DQ   <= (others => 'Z') when sramWe_n = "1" else sramDataOut;
  SRAM_WE_N <= sramWe_n;

end;
