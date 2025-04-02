-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.Mixed_topEntity_types.all;

entity topEntity is
  port(-- clock
       clk      : in Mixed_topEntity_types.clk_System;
       -- reset
       rst      : in Mixed_topEntity_types.rst_System;
       -- enable
       en       : in Mixed_topEntity_types.en_System;
       a        : in signed(31 downto 0);
       result_0 : out signed(31 downto 0);
       result_1 : out signed(31 downto 0);
       result_2 : out signed(31 downto 0));
end;

architecture structural of topEntity is
  -- mixed.hs:65:1-9
  signal eta1          : Mixed_topEntity_types.en_System;
  -- mixed.hs:56:1-122
  signal x1            : signed(31 downto 0);
  -- mixed.hs:56:1-122
  signal x2            : signed(31 downto 0);
  -- mixed.hs:56:1-122
  signal x3            : signed(31 downto 0);
  -- mixed.hs:56:1-122
  signal x4            : signed(31 downto 0);
  signal result_6      : signed(31 downto 0) := to_signed(0,32);
  -- mixed.hs:35:1-126
  signal past0         : signed(31 downto 0) := to_signed(0,32);
  -- mixed.hs:35:1-126
  signal past1         : signed(31 downto 0) := to_signed(0,32);
  signal result_7      : Mixed_topEntity_types.Tup4;
  signal \c$app_arg\   : signed(31 downto 0) := to_signed(0,32);
  -- mixed.hs:12:1-92
  signal b             : boolean;
  signal result_8      : boolean;
  -- mixed.hs:8:1-92
  signal b_0           : boolean;
  -- mixed.hs:8:1-92
  signal t             : unsigned(63 downto 0);
  signal result_9      : unsigned(63 downto 0);
  signal result_10     : unsigned(63 downto 0) := to_unsigned(0,64);
  signal result_11     : boolean := false;
  signal \c$app_arg_0\ : signed(31 downto 0) := to_signed(0,32);
  signal result_12     : signed(31 downto 0) := to_signed(0,32);
  signal result_13     : signed(31 downto 0) := to_signed(0,32);
  signal result        : Mixed_topEntity_types.Tup3;

begin
  result <= ( Tup3_sel0_signed_0 => result_13
            , Tup3_sel1_signed_1 => result_12
            , Tup3_sel2_signed_2 => result_6 );

  eta1 <= result_11;

  x1 <= result_7.Tup4_sel0_signed_0;

  x2 <= result_7.Tup4_sel1_signed_1;

  x3 <= result_7.Tup4_sel2_signed_2;

  x4 <= result_7.Tup4_sel3_signed_3;

  -- register begin
  result_6_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_6 <= to_signed(0,32);
    elsif rising_edge(clk) then
      if eta1 then
        result_6 <= (((x1 + x2) + x3) + x4);
      end if;
    end if;
  end process;
  -- register end

  -- register begin
  past0_register : process(clk,rst)
  begin
    if rst =  '1'  then
      past0 <= to_signed(0,32);
    elsif rising_edge(clk) then
      if eta1 then
        past0 <= result_13;
      end if;
    end if;
  end process;
  -- register end

  -- register begin
  past1_register : process(clk,rst)
  begin
    if rst =  '1'  then
      past1 <= to_signed(0,32);
    elsif rising_edge(clk) then
      if eta1 then
        past1 <= past0;
      end if;
    end if;
  end process;
  -- register end

  result_7 <= ( Tup4_sel0_signed_0 => result_13
              , Tup4_sel1_signed_1 => past0
              , Tup4_sel2_signed_2 => past1
              , Tup4_sel3_signed_3 => \c$app_arg\ );

  -- register begin
  capp_arg_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg\ <= to_signed(0,32);
    elsif rising_edge(clk) then
      if eta1 then
        \c$app_arg\ <= past1;
      end if;
    end if;
  end process;
  -- register end

  b <= result_10 = to_unsigned(1,64);

  result_8 <= true when b else
              false;

  b_0 <= result_10 < to_unsigned(1,64);

  t <= result_10 + to_unsigned(1,64);

  result_9 <= t when b_0 else
              to_unsigned(0,64);

  -- register begin
  result_10_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_10 <= to_unsigned(0,64);
    elsif rising_edge(clk) then
      if en then
        result_10 <= result_9;
      end if;
    end if;
  end process;
  -- register end

  -- register begin
  result_11_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_11 <= false;
    elsif rising_edge(clk) then
      if en then
        result_11 <= result_8;
      end if;
    end if;
  end process;
  -- register end

  -- register begin
  capp_arg_0_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_0\ <= to_signed(0,32);
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_0\ <= result_13;
      end if;
    end if;
  end process;
  -- register end

  -- register begin
  result_12_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_12 <= to_signed(0,32);
    elsif rising_edge(clk) then
      if en then
        result_12 <= \c$app_arg_0\;
      end if;
    end if;
  end process;
  -- register end

  -- register begin
  result_13_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_13 <= to_signed(0,32);
    elsif rising_edge(clk) then
      if en then
        result_13 <= a;
      end if;
    end if;
  end process;
  -- register end

  result_0 <= result.Tup3_sel0_signed_0;

  result_1 <= result.Tup3_sel1_signed_1;

  result_2 <= result.Tup3_sel2_signed_2;


end;

