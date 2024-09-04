-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.Experiments_Simple_topEntity_types.all;

entity topEntity is
  port(-- clock
       clk    : in Experiments_Simple_topEntity_types.clk_System;
       -- reset
       rst    : in Experiments_Simple_topEntity_types.rst_System;
       -- enable
       en     : in Experiments_Simple_topEntity_types.en_System;
       carg_0 : in signed(63 downto 0);
       carg_1 : in signed(63 downto 0);
       result : out signed(63 downto 0));
end;

architecture structural of topEntity is
  -- Simple.hs:6:1-39
  signal acc            : signed(63 downto 0) := (to_signed(0,64));
  -- Simple.hs:6:1-39
  signal x              : signed(63 downto 0);
  -- Simple.hs:6:1-39
  signal y              : signed(63 downto 0);
  signal x_0            : signed(63 downto 0);
  signal y_0            : signed(63 downto 0);
  signal x_1            : signed(63 downto 0);
  signal y_1            : signed(63 downto 0);
  signal carg           : Experiments_Simple_topEntity_types.Tuple2;
  signal y_projection_0 : signed(63 downto 0);

begin
  carg <= ( Tuple2_sel0_signed_0 => carg_0
          , Tuple2_sel1_signed_1 => carg_1 );

  -- register begin
  acc_register : process(clk,rst)
  begin
    if rst =  '1'  then
      acc <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        acc <= ((x_0 + y_0));
      end if;
    end if;
  end process;
  -- register end

  x <= carg.Tuple2_sel0_signed_0;

  y <= carg.Tuple2_sel1_signed_1;

  x_0 <= acc;

  y_projection_0 <= (resize(x_1 * y_1,64));

  y_0 <= y_projection_0;

  x_1 <= x;

  y_1 <= y;

  result <= acc;


end;

