library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package Spec_topEntity_types is
  subtype rst_TestDomain is std_logic;

  subtype en_TestDomain is boolean;

  type ValidInt is record
    ValidInt_sel0_value : signed(63 downto 0);
    ValidInt_sel1_valid : boolean;
  end record;
  type Inputs_3 is record
    Inputs_3_sel0_input0 : ValidInt;
    Inputs_3_sel1_input1 : ValidInt;
  end record;
  type Outputs is record
    Outputs_sel0_output0 : ValidInt;
    Outputs_sel1_output1 : ValidInt;
    Outputs_sel2_output2 : ValidInt;
    Outputs_sel3_output3 : ValidInt;
    Outputs_sel4_output4 : ValidInt;
    Outputs_sel5_output5 : ValidInt;
    Outputs_sel6_output6 : ValidInt;
  end record;
  type PacingOut3 is record
    PacingOut3_sel0_boolean_0 : boolean;
    PacingOut3_sel1_boolean_1 : boolean;
  end record;
  type array_of_PacingOut3 is array (integer range <>) of PacingOut3;
  subtype clk_TestDomain is std_logic;

  type Tags is record
    Tags_sel0_input0_0 : unsigned(7 downto 0);
    Tags_sel1_input1_0 : unsigned(7 downto 0);
    Tags_sel2_output0_0 : unsigned(7 downto 0);
    Tags_sel3_output1_0 : unsigned(7 downto 0);
    Tags_sel4_output2_0 : unsigned(7 downto 0);
    Tags_sel5_output3_0 : unsigned(7 downto 0);
    Tags_sel6_output4_0 : unsigned(7 downto 0);
    Tags_sel7_output5_0 : unsigned(7 downto 0);
    Tags_sel8_output6_0 : unsigned(7 downto 0);
  end record;
  type Tuple2_1 is record
    Tuple2_1_sel0_unsigned : unsigned(7 downto 0);
    Tuple2_1_sel1_signed : signed(63 downto 0);
  end record;
  type Pacings_1 is record
    Pacings_1_sel0_pacingIn0 : boolean;
    Pacings_1_sel1_pacingIn1 : boolean;
    Pacings_1_sel2_pacingOut0 : boolean;
    Pacings_1_sel3_pacingOut1 : boolean;
    Pacings_1_sel4_pacingOut2 : boolean;
    Pacings_1_sel5_pacingOut3_0 : PacingOut3;
    Pacings_1_sel6_pacingOut4 : boolean;
    Pacings_1_sel7_pacingOut5 : boolean;
    Pacings_1_sel8_pacingOut6 : boolean;
  end record;
  type Tuple2_2 is record
    Tuple2_2_sel0_Inputs_3 : Inputs_3;
    Tuple2_2_sel1_Pacings_1 : Pacings_1;
  end record;
  type Tuple2 is record
    Tuple2_sel0_boolean : boolean;
    Tuple2_sel1_Tuple2_2 : Tuple2_2;
  end record;
  type array_of_Tuple2_2 is array (integer range <>) of Tuple2_2;
  type Tuple2_3 is record
    Tuple2_3_sel0_array_of_Tuple2_2_0 : array_of_Tuple2_2(0 to 3);
    Tuple2_3_sel1_array_of_Tuple2_2_1 : array_of_Tuple2_2(0 to 0);
  end record;
  type Tuple3 is record
    Tuple3_sel0_boolean_0 : boolean;
    Tuple3_sel1_boolean_1 : boolean;
    Tuple3_sel2_Tuple2_2 : Tuple2_2;
  end record;
  type Tuple2_0 is record
    Tuple2_0_sel0_boolean : boolean;
    Tuple2_0_sel1_Outputs : Outputs;
  end record;
  type array_of_Tags is array (integer range <>) of Tags;
  type array_of_boolean is array (integer range <>) of boolean;
  function toSLV (sl : in std_logic) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic;
  function toSLV (b : in boolean) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector) return boolean;
  function tagToEnum (s : in signed) return boolean;
  function dataToTag (b : in boolean) return signed;
  function toSLV (s : in signed) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return signed;
  function toSLV (p : ValidInt) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return ValidInt;
  function toSLV (p : Inputs_3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Inputs_3;
  function toSLV (p : Outputs) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Outputs;
  function toSLV (p : PacingOut3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return PacingOut3;
  function toSLV (value :  array_of_PacingOut3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_PacingOut3;
  function toSLV (u : in unsigned) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return unsigned;
  function toSLV (p : Tags) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tags;
  function toSLV (p : Tuple2_1) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_1;
  function toSLV (p : Pacings_1) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Pacings_1;
  function toSLV (p : Tuple2_2) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_2;
  function toSLV (p : Tuple2) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2;
  function toSLV (value :  array_of_Tuple2_2) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple2_2;
  function toSLV (p : Tuple2_3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_3;
  function toSLV (p : Tuple3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple3;
  function toSLV (p : Tuple2_0) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_0;
  function toSLV (value :  array_of_Tags) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Tags;
  function toSLV (value :  array_of_boolean) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_boolean;
end;

package body Spec_topEntity_types is
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
  function toSLV (s : in signed) return std_logic_vector is
  begin
    return std_logic_vector(s);
  end;
  function fromSLV (slv : in std_logic_vector) return signed is
    alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return signed(islv);
  end;
  function toSLV (p : ValidInt) return std_logic_vector is
  begin
    return (toSLV(p.ValidInt_sel0_value) & toSLV(p.ValidInt_sel1_valid));
  end;
  function fromSLV (slv : in std_logic_vector) return ValidInt is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 63)),fromSLV(islv(64 to 64)));
  end;
  function toSLV (p : Inputs_3) return std_logic_vector is
  begin
    return (toSLV(p.Inputs_3_sel0_input0) & toSLV(p.Inputs_3_sel1_input1));
  end;
  function fromSLV (slv : in std_logic_vector) return Inputs_3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 64)),fromSLV(islv(65 to 129)));
  end;
  function toSLV (p : Outputs) return std_logic_vector is
  begin
    return (toSLV(p.Outputs_sel0_output0) & toSLV(p.Outputs_sel1_output1) & toSLV(p.Outputs_sel2_output2) & toSLV(p.Outputs_sel3_output3) & toSLV(p.Outputs_sel4_output4) & toSLV(p.Outputs_sel5_output5) & toSLV(p.Outputs_sel6_output6));
  end;
  function fromSLV (slv : in std_logic_vector) return Outputs is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 64)),fromSLV(islv(65 to 129)),fromSLV(islv(130 to 194)),fromSLV(islv(195 to 259)),fromSLV(islv(260 to 324)),fromSLV(islv(325 to 389)),fromSLV(islv(390 to 454)));
  end;
  function toSLV (p : PacingOut3) return std_logic_vector is
  begin
    return (toSLV(p.PacingOut3_sel0_boolean_0) & toSLV(p.PacingOut3_sel1_boolean_1));
  end;
  function fromSLV (slv : in std_logic_vector) return PacingOut3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)));
  end;
  function toSLV (value :  array_of_PacingOut3) return std_logic_vector is
    alias ivalue    : array_of_PacingOut3(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 2);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 2) + 1 to i*2) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_PacingOut3 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_PacingOut3(0 to slv'length / 2 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 2 to (i+1) * 2 - 1));
    end loop;
    return result;
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
  function toSLV (p : Tags) return std_logic_vector is
  begin
    return (toSLV(p.Tags_sel0_input0_0) & toSLV(p.Tags_sel1_input1_0) & toSLV(p.Tags_sel2_output0_0) & toSLV(p.Tags_sel3_output1_0) & toSLV(p.Tags_sel4_output2_0) & toSLV(p.Tags_sel5_output3_0) & toSLV(p.Tags_sel6_output4_0) & toSLV(p.Tags_sel7_output5_0) & toSLV(p.Tags_sel8_output6_0));
  end;
  function fromSLV (slv : in std_logic_vector) return Tags is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 7)),fromSLV(islv(8 to 15)),fromSLV(islv(16 to 23)),fromSLV(islv(24 to 31)),fromSLV(islv(32 to 39)),fromSLV(islv(40 to 47)),fromSLV(islv(48 to 55)),fromSLV(islv(56 to 63)),fromSLV(islv(64 to 71)));
  end;
  function toSLV (p : Tuple2_1) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_1_sel0_unsigned) & toSLV(p.Tuple2_1_sel1_signed));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_1 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 7)),fromSLV(islv(8 to 71)));
  end;
  function toSLV (p : Pacings_1) return std_logic_vector is
  begin
    return (toSLV(p.Pacings_1_sel0_pacingIn0) & toSLV(p.Pacings_1_sel1_pacingIn1) & toSLV(p.Pacings_1_sel2_pacingOut0) & toSLV(p.Pacings_1_sel3_pacingOut1) & toSLV(p.Pacings_1_sel4_pacingOut2) & toSLV(p.Pacings_1_sel5_pacingOut3_0) & toSLV(p.Pacings_1_sel6_pacingOut4) & toSLV(p.Pacings_1_sel7_pacingOut5) & toSLV(p.Pacings_1_sel8_pacingOut6));
  end;
  function fromSLV (slv : in std_logic_vector) return Pacings_1 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)),fromSLV(islv(2 to 2)),fromSLV(islv(3 to 3)),fromSLV(islv(4 to 4)),fromSLV(islv(5 to 6)),fromSLV(islv(7 to 7)),fromSLV(islv(8 to 8)),fromSLV(islv(9 to 9)));
  end;
  function toSLV (p : Tuple2_2) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_2_sel0_Inputs_3) & toSLV(p.Tuple2_2_sel1_Pacings_1));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_2 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 129)),fromSLV(islv(130 to 139)));
  end;
  function toSLV (p : Tuple2) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_sel0_boolean) & toSLV(p.Tuple2_sel1_Tuple2_2));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 140)));
  end;
  function toSLV (value :  array_of_Tuple2_2) return std_logic_vector is
    alias ivalue    : array_of_Tuple2_2(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 140);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 140) + 1 to i*140) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple2_2 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Tuple2_2(0 to slv'length / 140 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 140 to (i+1) * 140 - 1));
    end loop;
    return result;
  end;
  function toSLV (p : Tuple2_3) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_3_sel0_array_of_Tuple2_2_0) & toSLV(p.Tuple2_3_sel1_array_of_Tuple2_2_1));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 559)),fromSLV(islv(560 to 699)));
  end;
  function toSLV (p : Tuple3) return std_logic_vector is
  begin
    return (toSLV(p.Tuple3_sel0_boolean_0) & toSLV(p.Tuple3_sel1_boolean_1) & toSLV(p.Tuple3_sel2_Tuple2_2));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)),fromSLV(islv(2 to 141)));
  end;
  function toSLV (p : Tuple2_0) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_0_sel0_boolean) & toSLV(p.Tuple2_0_sel1_Outputs));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_0 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 455)));
  end;
  function toSLV (value :  array_of_Tags) return std_logic_vector is
    alias ivalue    : array_of_Tags(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 72);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 72) + 1 to i*72) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Tags is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Tags(0 to slv'length / 72 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 72 to (i+1) * 72 - 1));
    end loop;
    return result;
  end;
  function toSLV (value :  array_of_boolean) return std_logic_vector is
    alias ivalue    : array_of_boolean(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 1);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 1) + 1 to i*1) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_boolean is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_boolean(0 to slv'length / 1 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 1 to (i+1) * 1 - 1));
    end loop;
    return result;
  end;
end;

