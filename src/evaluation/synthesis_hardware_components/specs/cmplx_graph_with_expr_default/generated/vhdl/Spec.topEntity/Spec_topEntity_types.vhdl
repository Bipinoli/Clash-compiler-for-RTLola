library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package Spec_topEntity_types is
  subtype rst_TestDomain is std_logic;

  type PacingOut6 is record
    PacingOut6_sel0_boolean_0 : boolean;
    PacingOut6_sel1_boolean_1 : boolean;
  end record;
  type array_of_PacingOut6 is array (integer range <>) of PacingOut6;
  type PacingOut7 is record
    PacingOut7_sel0_boolean_0 : boolean;
    PacingOut7_sel1_boolean_1 : boolean;
  end record;
  type array_of_PacingOut7 is array (integer range <>) of PacingOut7;
  type PacingOut5 is record
    PacingOut5_sel0_boolean_0 : boolean;
    PacingOut5_sel1_boolean_1 : boolean;
  end record;
  subtype en_TestDomain is boolean;

  type ValidInt is record
    ValidInt_sel0_value : signed(63 downto 0);
    ValidInt_sel1_valid : boolean;
  end record;
  type Outputs is record
    Outputs_sel0_output0 : ValidInt;
    Outputs_sel1_output1 : ValidInt;
    Outputs_sel2_output2 : ValidInt;
    Outputs_sel3_output3 : ValidInt;
    Outputs_sel4_output4 : ValidInt;
    Outputs_sel5_output5 : ValidInt;
    Outputs_sel6_output6 : ValidInt;
    Outputs_sel7_output7 : ValidInt;
    Outputs_sel8_output8 : ValidInt;
    Outputs_sel9_output9 : ValidInt;
    Outputs_sel10_output10 : ValidInt;
    Outputs_sel11_output11 : ValidInt;
  end record;
  type Tuple2_0 is record
    Tuple2_0_sel0_boolean : boolean;
    Tuple2_0_sel1_Outputs : Outputs;
  end record;
  type Inputs_3 is record
    Inputs_3_sel0_input0 : ValidInt;
    Inputs_3_sel1_input1 : ValidInt;
  end record;
  subtype index_3 is unsigned(1 downto 0);
  subtype Maybe_0 is std_logic_vector(2 downto 0);
  type array_of_PacingOut5 is array (integer range <>) of PacingOut5;
  type array_of_Maybe_0 is array (integer range <>) of Maybe_0;
  type array_of_signed_64 is array (integer range <>) of signed(63 downto 0);
  type Tuple2_5 is record
    Tuple2_5_sel0_array_of_signed_64_0 : array_of_signed_64(0 to 1);
    Tuple2_5_sel1_array_of_signed_64_1 : array_of_signed_64(0 to 0);
  end record;
  subtype clk_TestDomain is std_logic;
  subtype index_2 is unsigned(0 downto 0);
  subtype Maybe is std_logic_vector(1 downto 0);
  type array_of_Maybe is array (integer range <>) of Maybe;

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
    Tags_sel9_output7_0 : unsigned(7 downto 0);
    Tags_sel10_output8_0 : unsigned(7 downto 0);
    Tags_sel11_output9_0 : unsigned(7 downto 0);
    Tags_sel12_output10_0 : unsigned(7 downto 0);
    Tags_sel13_output11_0 : unsigned(7 downto 0);
    Tags_sel14_slide0_0 : unsigned(7 downto 0);
    Tags_sel15_slide1_0 : unsigned(7 downto 0);
  end record;
  type Tuple2_2 is record
    Tuple2_2_sel0_unsigned : unsigned(7 downto 0);
    Tuple2_2_sel1_signed : signed(63 downto 0);
  end record;
  type Tuple2_4 is record
    Tuple2_4_sel0_Tuple2_2 : Tuple2_2;
    Tuple2_4_sel1_Maybe_0 : Maybe_0;
  end record;
  type array_of_Tuple2_4 is array (integer range <>) of Tuple2_4;
  type array_of_Tuple2_2 is array (integer range <>) of Tuple2_2;
  type Tuple2_7 is record
    Tuple2_7_sel0_array_of_Tuple2_2_0 : array_of_Tuple2_2(0 to 0);
    Tuple2_7_sel1_array_of_Tuple2_2_1 : array_of_Tuple2_2(0 to 2);
  end record;
  type Tuple2_1 is record
    Tuple2_1_sel0_Tuple2_2 : Tuple2_2;
    Tuple2_1_sel1_Maybe : Maybe;
  end record;
  type array_of_Tuple2_1 is array (integer range <>) of Tuple2_1;
  type Slides_1 is record
    Slides_1_sel0_slide0 : boolean;
    Slides_1_sel1_slide1 : boolean;
  end record;
  type array_of_boolean is array (integer range <>) of boolean;
  type Tuple2_6 is record
    Tuple2_6_sel0_array_of_Tuple2_2_0 : array_of_Tuple2_2(0 to 0);
    Tuple2_6_sel1_array_of_Tuple2_2_1 : array_of_Tuple2_2(0 to 1);
  end record;
  type Tuple2_3 is record
    Tuple2_3_sel0_unsigned : unsigned(7 downto 0);
    Tuple2_3_sel1_array_of_signed_64 : array_of_signed_64(0 to 1);
  end record;
  type array_of_Tags is array (integer range <>) of Tags;
  type Pacings_1 is record
    Pacings_1_sel0_pacingIn0 : boolean;
    Pacings_1_sel1_pacingIn1 : boolean;
    Pacings_1_sel2_pacingOut0 : boolean;
    Pacings_1_sel3_pacingOut1 : boolean;
    Pacings_1_sel4_pacingOut2 : boolean;
    Pacings_1_sel5_pacingOut3 : boolean;
    Pacings_1_sel6_pacingOut4 : boolean;
    Pacings_1_sel7_pacingOut5_0 : PacingOut5;
    Pacings_1_sel8_pacingOut6_0 : PacingOut6;
    Pacings_1_sel9_pacingOut7_0 : PacingOut7;
    Pacings_1_sel10_pacingOut8 : boolean;
    Pacings_1_sel11_pacingOut9 : boolean;
    Pacings_1_sel12_pacingOut10 : boolean;
    Pacings_1_sel13_pacingOut11 : boolean;
  end record;
  type Tuple3_0 is record
    Tuple3_0_sel0_Inputs_3 : Inputs_3;
    Tuple3_0_sel1_Slides_1 : Slides_1;
    Tuple3_0_sel2_Pacings_1 : Pacings_1;
  end record;
  type Tuple2 is record
    Tuple2_sel0_boolean : boolean;
    Tuple2_sel1_Tuple3_0 : Tuple3_0;
  end record;
  type array_of_Tuple3_0 is array (integer range <>) of Tuple3_0;
  type Tuple2_8 is record
    Tuple2_8_sel0_array_of_Tuple3_0_0 : array_of_Tuple3_0(0 to 7);
    Tuple2_8_sel1_array_of_Tuple3_0_1 : array_of_Tuple3_0(0 to 0);
  end record;
  type Tuple3 is record
    Tuple3_sel0_boolean_0 : boolean;
    Tuple3_sel1_boolean_1 : boolean;
    Tuple3_sel2_Tuple3_0 : Tuple3_0;
  end record;
  function toSLV (sl : in std_logic) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic;
  function toSLV (b : in boolean) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector) return boolean;
  function tagToEnum (s : in signed) return boolean;
  function dataToTag (b : in boolean) return signed;
  function toSLV (p : PacingOut6) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return PacingOut6;
  function toSLV (value :  array_of_PacingOut6) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_PacingOut6;
  function toSLV (p : PacingOut7) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return PacingOut7;
  function toSLV (value :  array_of_PacingOut7) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_PacingOut7;
  function toSLV (p : PacingOut5) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return PacingOut5;
  function toSLV (s : in signed) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return signed;
  function toSLV (p : ValidInt) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return ValidInt;
  function toSLV (p : Outputs) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Outputs;
  function toSLV (p : Tuple2_0) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_0;
  function toSLV (p : Inputs_3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Inputs_3;
  function toSLV (u : in unsigned) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return unsigned;
  function toSLV (slv : in std_logic_vector) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector;
  function toSLV (value :  array_of_PacingOut5) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_PacingOut5;
  function toSLV (value :  array_of_Maybe_0) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Maybe_0;
  function toSLV (value :  array_of_signed_64) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_signed_64;
  function toSLV (p : Tuple2_5) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_5;
  function toSLV (value :  array_of_Maybe) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Maybe;
  function toSLV (p : Tags) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tags;
  function toSLV (p : Tuple2_2) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_2;
  function toSLV (p : Tuple2_4) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_4;
  function toSLV (value :  array_of_Tuple2_4) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple2_4;
  function toSLV (value :  array_of_Tuple2_2) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple2_2;
  function toSLV (p : Tuple2_7) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_7;
  function toSLV (p : Tuple2_1) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_1;
  function toSLV (value :  array_of_Tuple2_1) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple2_1;
  function toSLV (p : Slides_1) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Slides_1;
  function toSLV (value :  array_of_boolean) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_boolean;
  function toSLV (p : Tuple2_6) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_6;
  function toSLV (p : Tuple2_3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_3;
  function toSLV (value :  array_of_Tags) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Tags;
  function toSLV (p : Pacings_1) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Pacings_1;
  function toSLV (p : Tuple3_0) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple3_0;
  function toSLV (p : Tuple2) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2;
  function toSLV (value :  array_of_Tuple3_0) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple3_0;
  function toSLV (p : Tuple2_8) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple2_8;
  function toSLV (p : Tuple3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return Tuple3;
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
  function toSLV (p : PacingOut6) return std_logic_vector is
  begin
    return (toSLV(p.PacingOut6_sel0_boolean_0) & toSLV(p.PacingOut6_sel1_boolean_1));
  end;
  function fromSLV (slv : in std_logic_vector) return PacingOut6 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)));
  end;
  function toSLV (value :  array_of_PacingOut6) return std_logic_vector is
    alias ivalue    : array_of_PacingOut6(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 2);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 2) + 1 to i*2) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_PacingOut6 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_PacingOut6(0 to slv'length / 2 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 2 to (i+1) * 2 - 1));
    end loop;
    return result;
  end;
  function toSLV (p : PacingOut7) return std_logic_vector is
  begin
    return (toSLV(p.PacingOut7_sel0_boolean_0) & toSLV(p.PacingOut7_sel1_boolean_1));
  end;
  function fromSLV (slv : in std_logic_vector) return PacingOut7 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)));
  end;
  function toSLV (value :  array_of_PacingOut7) return std_logic_vector is
    alias ivalue    : array_of_PacingOut7(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 2);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 2) + 1 to i*2) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_PacingOut7 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_PacingOut7(0 to slv'length / 2 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 2 to (i+1) * 2 - 1));
    end loop;
    return result;
  end;
  function toSLV (p : PacingOut5) return std_logic_vector is
  begin
    return (toSLV(p.PacingOut5_sel0_boolean_0) & toSLV(p.PacingOut5_sel1_boolean_1));
  end;
  function fromSLV (slv : in std_logic_vector) return PacingOut5 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)));
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
  function toSLV (p : Outputs) return std_logic_vector is
  begin
    return (toSLV(p.Outputs_sel0_output0) & toSLV(p.Outputs_sel1_output1) & toSLV(p.Outputs_sel2_output2) & toSLV(p.Outputs_sel3_output3) & toSLV(p.Outputs_sel4_output4) & toSLV(p.Outputs_sel5_output5) & toSLV(p.Outputs_sel6_output6) & toSLV(p.Outputs_sel7_output7) & toSLV(p.Outputs_sel8_output8) & toSLV(p.Outputs_sel9_output9) & toSLV(p.Outputs_sel10_output10) & toSLV(p.Outputs_sel11_output11));
  end;
  function fromSLV (slv : in std_logic_vector) return Outputs is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 64)),fromSLV(islv(65 to 129)),fromSLV(islv(130 to 194)),fromSLV(islv(195 to 259)),fromSLV(islv(260 to 324)),fromSLV(islv(325 to 389)),fromSLV(islv(390 to 454)),fromSLV(islv(455 to 519)),fromSLV(islv(520 to 584)),fromSLV(islv(585 to 649)),fromSLV(islv(650 to 714)),fromSLV(islv(715 to 779)));
  end;
  function toSLV (p : Tuple2_0) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_0_sel0_boolean) & toSLV(p.Tuple2_0_sel1_Outputs));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_0 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 780)));
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
  function toSLV (u : in unsigned) return std_logic_vector is
  begin
    return std_logic_vector(u);
  end;
  function fromSLV (slv : in std_logic_vector) return unsigned is
    alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return unsigned(islv);
  end;
  function toSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function toSLV (value :  array_of_PacingOut5) return std_logic_vector is
    alias ivalue    : array_of_PacingOut5(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 2);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 2) + 1 to i*2) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_PacingOut5 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_PacingOut5(0 to slv'length / 2 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 2 to (i+1) * 2 - 1));
    end loop;
    return result;
  end;
  function toSLV (value :  array_of_Maybe_0) return std_logic_vector is
    alias ivalue    : array_of_Maybe_0(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 3);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 3) + 1 to i*3) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Maybe_0 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Maybe_0(0 to slv'length / 3 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 3 to (i+1) * 3 - 1));
    end loop;
    return result;
  end;
  function toSLV (value :  array_of_signed_64) return std_logic_vector is
    alias ivalue    : array_of_signed_64(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 64);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 64) + 1 to i*64) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_signed_64 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_signed_64(0 to slv'length / 64 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 64 to (i+1) * 64 - 1));
    end loop;
    return result;
  end;
  function toSLV (p : Tuple2_5) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_5_sel0_array_of_signed_64_0) & toSLV(p.Tuple2_5_sel1_array_of_signed_64_1));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_5 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 127)),fromSLV(islv(128 to 191)));
  end;
  function toSLV (value :  array_of_Maybe) return std_logic_vector is
    alias ivalue    : array_of_Maybe(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 2);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 2) + 1 to i*2) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Maybe is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Maybe(0 to slv'length / 2 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 2 to (i+1) * 2 - 1));
    end loop;
    return result;
  end;
  function toSLV (p : Tags) return std_logic_vector is
  begin
    return (toSLV(p.Tags_sel0_input0_0) & toSLV(p.Tags_sel1_input1_0) & toSLV(p.Tags_sel2_output0_0) & toSLV(p.Tags_sel3_output1_0) & toSLV(p.Tags_sel4_output2_0) & toSLV(p.Tags_sel5_output3_0) & toSLV(p.Tags_sel6_output4_0) & toSLV(p.Tags_sel7_output5_0) & toSLV(p.Tags_sel8_output6_0) & toSLV(p.Tags_sel9_output7_0) & toSLV(p.Tags_sel10_output8_0) & toSLV(p.Tags_sel11_output9_0) & toSLV(p.Tags_sel12_output10_0) & toSLV(p.Tags_sel13_output11_0) & toSLV(p.Tags_sel14_slide0_0) & toSLV(p.Tags_sel15_slide1_0));
  end;
  function fromSLV (slv : in std_logic_vector) return Tags is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 7)),fromSLV(islv(8 to 15)),fromSLV(islv(16 to 23)),fromSLV(islv(24 to 31)),fromSLV(islv(32 to 39)),fromSLV(islv(40 to 47)),fromSLV(islv(48 to 55)),fromSLV(islv(56 to 63)),fromSLV(islv(64 to 71)),fromSLV(islv(72 to 79)),fromSLV(islv(80 to 87)),fromSLV(islv(88 to 95)),fromSLV(islv(96 to 103)),fromSLV(islv(104 to 111)),fromSLV(islv(112 to 119)),fromSLV(islv(120 to 127)));
  end;
  function toSLV (p : Tuple2_2) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_2_sel0_unsigned) & toSLV(p.Tuple2_2_sel1_signed));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_2 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 7)),fromSLV(islv(8 to 71)));
  end;
  function toSLV (p : Tuple2_4) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_4_sel0_Tuple2_2) & toSLV(p.Tuple2_4_sel1_Maybe_0));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_4 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 71)),fromSLV(islv(72 to 74)));
  end;
  function toSLV (value :  array_of_Tuple2_4) return std_logic_vector is
    alias ivalue    : array_of_Tuple2_4(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 75);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 75) + 1 to i*75) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple2_4 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Tuple2_4(0 to slv'length / 75 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 75 to (i+1) * 75 - 1));
    end loop;
    return result;
  end;
  function toSLV (value :  array_of_Tuple2_2) return std_logic_vector is
    alias ivalue    : array_of_Tuple2_2(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 72);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 72) + 1 to i*72) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple2_2 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Tuple2_2(0 to slv'length / 72 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 72 to (i+1) * 72 - 1));
    end loop;
    return result;
  end;
  function toSLV (p : Tuple2_7) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_7_sel0_array_of_Tuple2_2_0) & toSLV(p.Tuple2_7_sel1_array_of_Tuple2_2_1));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_7 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 71)),fromSLV(islv(72 to 287)));
  end;
  function toSLV (p : Tuple2_1) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_1_sel0_Tuple2_2) & toSLV(p.Tuple2_1_sel1_Maybe));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_1 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 71)),fromSLV(islv(72 to 73)));
  end;
  function toSLV (value :  array_of_Tuple2_1) return std_logic_vector is
    alias ivalue    : array_of_Tuple2_1(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 74);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 74) + 1 to i*74) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple2_1 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Tuple2_1(0 to slv'length / 74 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 74 to (i+1) * 74 - 1));
    end loop;
    return result;
  end;
  function toSLV (p : Slides_1) return std_logic_vector is
  begin
    return (toSLV(p.Slides_1_sel0_slide0) & toSLV(p.Slides_1_sel1_slide1));
  end;
  function fromSLV (slv : in std_logic_vector) return Slides_1 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)));
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
  function toSLV (p : Tuple2_6) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_6_sel0_array_of_Tuple2_2_0) & toSLV(p.Tuple2_6_sel1_array_of_Tuple2_2_1));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_6 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 71)),fromSLV(islv(72 to 215)));
  end;
  function toSLV (p : Tuple2_3) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_3_sel0_unsigned) & toSLV(p.Tuple2_3_sel1_array_of_signed_64));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 7)),fromSLV(islv(8 to 135)));
  end;
  function toSLV (value :  array_of_Tags) return std_logic_vector is
    alias ivalue    : array_of_Tags(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 128);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 128) + 1 to i*128) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Tags is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Tags(0 to slv'length / 128 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 128 to (i+1) * 128 - 1));
    end loop;
    return result;
  end;
  function toSLV (p : Pacings_1) return std_logic_vector is
  begin
    return (toSLV(p.Pacings_1_sel0_pacingIn0) & toSLV(p.Pacings_1_sel1_pacingIn1) & toSLV(p.Pacings_1_sel2_pacingOut0) & toSLV(p.Pacings_1_sel3_pacingOut1) & toSLV(p.Pacings_1_sel4_pacingOut2) & toSLV(p.Pacings_1_sel5_pacingOut3) & toSLV(p.Pacings_1_sel6_pacingOut4) & toSLV(p.Pacings_1_sel7_pacingOut5_0) & toSLV(p.Pacings_1_sel8_pacingOut6_0) & toSLV(p.Pacings_1_sel9_pacingOut7_0) & toSLV(p.Pacings_1_sel10_pacingOut8) & toSLV(p.Pacings_1_sel11_pacingOut9) & toSLV(p.Pacings_1_sel12_pacingOut10) & toSLV(p.Pacings_1_sel13_pacingOut11));
  end;
  function fromSLV (slv : in std_logic_vector) return Pacings_1 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)),fromSLV(islv(2 to 2)),fromSLV(islv(3 to 3)),fromSLV(islv(4 to 4)),fromSLV(islv(5 to 5)),fromSLV(islv(6 to 6)),fromSLV(islv(7 to 8)),fromSLV(islv(9 to 10)),fromSLV(islv(11 to 12)),fromSLV(islv(13 to 13)),fromSLV(islv(14 to 14)),fromSLV(islv(15 to 15)),fromSLV(islv(16 to 16)));
  end;
  function toSLV (p : Tuple3_0) return std_logic_vector is
  begin
    return (toSLV(p.Tuple3_0_sel0_Inputs_3) & toSLV(p.Tuple3_0_sel1_Slides_1) & toSLV(p.Tuple3_0_sel2_Pacings_1));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple3_0 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 129)),fromSLV(islv(130 to 131)),fromSLV(islv(132 to 148)));
  end;
  function toSLV (p : Tuple2) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_sel0_boolean) & toSLV(p.Tuple2_sel1_Tuple3_0));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 149)));
  end;
  function toSLV (value :  array_of_Tuple3_0) return std_logic_vector is
    alias ivalue    : array_of_Tuple3_0(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 149);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 149) + 1 to i*149) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return array_of_Tuple3_0 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : array_of_Tuple3_0(0 to slv'length / 149 - 1);
  begin
    for i in result'range loop
      result(i) := fromSLV(islv(i * 149 to (i+1) * 149 - 1));
    end loop;
    return result;
  end;
  function toSLV (p : Tuple2_8) return std_logic_vector is
  begin
    return (toSLV(p.Tuple2_8_sel0_array_of_Tuple3_0_0) & toSLV(p.Tuple2_8_sel1_array_of_Tuple3_0_1));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple2_8 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 1191)),fromSLV(islv(1192 to 1340)));
  end;
  function toSLV (p : Tuple3) return std_logic_vector is
  begin
    return (toSLV(p.Tuple3_sel0_boolean_0) & toSLV(p.Tuple3_sel1_boolean_1) & toSLV(p.Tuple3_sel2_Tuple3_0));
  end;
  function fromSLV (slv : in std_logic_vector) return Tuple3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)),fromSLV(islv(2 to 150)));
  end;
end;

