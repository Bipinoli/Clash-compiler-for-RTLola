library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package Mixed_topEntity_types is
  subtype clk_System is std_logic;
  subtype rst_System is std_logic;
  subtype en_System is boolean;



  type Tup4 is record
    Tup4_sel0_signed_0 : signed(31 downto 0);
    Tup4_sel1_signed_1 : signed(31 downto 0);
    Tup4_sel2_signed_2 : signed(31 downto 0);
    Tup4_sel3_signed_3 : signed(31 downto 0);
  end record;
  type Tup3 is record
    Tup3_sel0_signed_0 : signed(31 downto 0);
    Tup3_sel1_signed_1 : signed(31 downto 0);
    Tup3_sel2_signed_2 : signed(31 downto 0);
  end record;
  function toSLV (sl : in std_logic) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic;
  function toSLV (b : in boolean) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector) return boolean;
  function tagToEnum (s : in signed) return boolean;
  function dataToTag (b : in boolean) return signed;
  function toSLV (u : in unsigned) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return unsigned;
  function toSLV (s : in signed) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return signed;
  function toSLV (p : Tup4) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tup4;
  function toSLV (p : Tup3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tup3;
end;

package body Mixed_topEntity_types is
  function toSLV (sl : in std_logic) return std_logic_vector is
  begin
    return std_logic_vector'(0 => sl);
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic is
    alias islv : std_logic_vector (0 to slv'length - 1) is slv;
  begin
    return islv(0);
  end;
  function toSLV (b : in boolean) return std_logic_vector is
  begin
    if b then
      return "1";
    else
      return "0";
    end if;
  end;
  function fromSLV (sl : in std_logic_vector) return boolean is
  begin
    if sl = "1" then
      return true;
    else
      return false;
    end if;
  end;
  function tagToEnum (s : in signed) return boolean is
  begin
    if s = to_signed(0,64) then
      return false;
    else
      return true;
    end if;
  end;
  function dataToTag (b : in boolean) return signed is
  begin
    if b then
      return to_signed(1,64);
    else
      return to_signed(0,64);
    end if;
  end;
  function toSLV (u : in unsigned) return std_logic_vector is
  begin
    return std_logic_vector(u);
  end;
  function fromSLV (slv : in std_logic_vector) return unsigned is
    alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return unsigned(islv);
  end;
  function toSLV (s : in signed) return std_logic_vector is
  begin
    return std_logic_vector(s);
  end;
  function fromSLV (slv : in std_logic_vector) return signed is
    alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return signed(islv);
  end;
  function toSLV (p : Tup4) return std_logic_vector is
  begin
    return (toSLV(p.Tup4_sel0_signed_0) & toSLV(p.Tup4_sel1_signed_1) & toSLV(p.Tup4_sel2_signed_2) & toSLV(p.Tup4_sel3_signed_3));
  end;
  function fromSLV (slv : in std_logic_vector) return Tup4 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 31)),fromSLV(islv(32 to 63)),fromSLV(islv(64 to 95)),fromSLV(islv(96 to 127)));
  end;
  function toSLV (p : Tup3) return std_logic_vector is
  begin
    return (toSLV(p.Tup3_sel0_signed_0) & toSLV(p.Tup3_sel1_signed_1) & toSLV(p.Tup3_sel2_signed_2));
  end;
  function fromSLV (slv : in std_logic_vector) return Tup3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 31)),fromSLV(islv(32 to 63)),fromSLV(islv(64 to 95)));
  end;
end;

