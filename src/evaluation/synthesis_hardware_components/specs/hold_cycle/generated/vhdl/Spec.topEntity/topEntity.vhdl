-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.Spec_topEntity_types.all;

entity topEntity is
  port(-- clock
       clk        : in Spec_topEntity_types.clk_TestDomain;
       -- reset
       rst        : in Spec_topEntity_types.rst_TestDomain;
       -- enable
       en         : in Spec_topEntity_types.en_TestDomain;
       inputs_0_0 : in signed(63 downto 0);
       inputs_0_1 : in boolean;
       inputs_1_0 : in signed(63 downto 0);
       inputs_1_1 : in boolean;
       result_0_0 : out signed(63 downto 0);
       result_0_1 : out boolean;
       result_1_0 : out signed(63 downto 0);
       result_1_1 : out boolean;
       result_2_0 : out signed(63 downto 0);
       result_2_1 : out boolean;
       result_3_0 : out signed(63 downto 0);
       result_3_1 : out boolean;
       result_4_0 : out signed(63 downto 0);
       result_4_1 : out boolean;
       result_5_0 : out signed(63 downto 0);
       result_5_1 : out boolean;
       result_6_0 : out signed(63 downto 0);
       result_6_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_7                          : signed(63 downto 0);
  signal x                                 : signed(63 downto 0);
  signal result_8                          : signed(63 downto 0) := (to_signed(0,64));
  signal result_9                          : signed(63 downto 0);
  signal x_0                               : signed(63 downto 0);
  signal result_10                         : signed(63 downto 0) := (to_signed(0,64));
  signal result_11                         : Spec_topEntity_types.Tuple2;
  -- spec.hs:(265,1)-(267,31)
  signal pIn0                              : boolean;
  -- spec.hs:(265,1)-(267,31)
  signal pIn1                              : boolean;
  -- spec.hs:(265,1)-(267,31)
  signal timer0Over                        : boolean;
  -- spec.hs:(265,1)-(267,31)
  signal timer1Over                        : boolean;
  signal result_12                         : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                       : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_13                         : boolean;
  signal result_14                         : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(690,1)-(694,28)
  signal result_15                         : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:(690,1)-(694,28)
  signal b                                 : boolean;
  -- spec.hs:(690,1)-(694,28)
  signal t                                 : Spec_topEntity_types.Tuple2_1;
  signal x_1                               : signed(63 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal \c$ds2_case_alt\                  : signed(63 downto 0);
  -- spec.hs:345:1-53
  signal tag                               : unsigned(7 downto 0);
  -- spec.hs:345:1-53
  signal tagToMatch                        : unsigned(7 downto 0);
  signal \c$app_arg_0\                     : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_16                         : boolean;
  signal \c$app_arg_1\                     : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_17                         : boolean;
  signal \c$app_arg_2\                     : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_18                         : boolean;
  signal \c$app_arg_3\                     : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_19                         : boolean;
  signal \c$app_arg_4\                     : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_20                         : boolean;
  signal \c$app_arg_5\                     : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_21                         : boolean;
  signal \c$app_arg_6\                     : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_22                         : boolean;
  -- spec.hs:354:1-46
  signal tag_0                             : unsigned(7 downto 0);
  signal result_23                         : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(623,1)-(628,28)
  signal result_24                         : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:(623,1)-(628,28)
  signal b_0                               : boolean;
  -- spec.hs:(623,1)-(628,28)
  signal t_0                               : Spec_topEntity_types.Tuple2_1;
  signal x_2                               : signed(63 downto 0);
  signal y                                 : signed(63 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal \c$out1_case_alt\                 : signed(63 downto 0);
  -- spec.hs:354:1-46
  signal dta                               : signed(63 downto 0);
  -- spec.hs:354:1-46
  signal tag_1                             : unsigned(7 downto 0);
  signal result_25                         : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:108:34-42
  signal x0                                : boolean;
  -- spec.hs:108:34-42
  signal x1                                : boolean;
  -- spec.hs:(651,1)-(655,28)
  signal result_26                         : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:(651,1)-(655,28)
  signal t_1                               : Spec_topEntity_types.Tuple2_1;
  signal x_3                               : signed(63 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal \c$out3_case_alt\                 : signed(63 downto 0);
  -- spec.hs:354:1-46
  signal dta_0                             : signed(63 downto 0);
  -- spec.hs:354:1-46
  signal tag_2                             : unsigned(7 downto 0);
  signal result_27                         : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(637,1)-(642,28)
  signal result_28                         : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:(637,1)-(642,28)
  signal b_1                               : boolean;
  -- spec.hs:(637,1)-(642,28)
  signal t_2                               : Spec_topEntity_types.Tuple2_1;
  signal x_4                               : signed(63 downto 0);
  signal y_0                               : signed(63 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal \c$out2_case_alt\                 : signed(63 downto 0);
  -- spec.hs:354:1-46
  signal dta_1                             : signed(63 downto 0);
  -- spec.hs:354:1-46
  signal tag_3                             : unsigned(7 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal \c$out2_case_alt_0\               : signed(63 downto 0);
  -- spec.hs:354:1-46
  signal dta_2                             : signed(63 downto 0);
  signal \c$app_arg_7\                     : Spec_topEntity_types.array_of_PacingOut3(0 to 2);
  signal result_29                         : Spec_topEntity_types.PacingOut3;
  -- spec.hs:(376,1)-(378,33)
  signal \c$out1_case_alt_0\               : signed(63 downto 0);
  -- spec.hs:354:1-46
  signal dta_3                             : signed(63 downto 0);
  -- spec.hs:354:1-46
  signal tag_4                             : unsigned(7 downto 0);
  signal result_30                         : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(599,1)-(603,28)
  signal result_31                         : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:(599,1)-(603,28)
  signal b_2                               : boolean;
  -- spec.hs:354:1-46
  signal \c$ds_app_arg\                    : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_8\                     : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_32                         : boolean;
  signal \c$app_arg_9\                     : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_33                         : boolean;
  signal \c$app_arg_10\                    : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_34                         : boolean;
  signal result_35                         : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:105:34-42
  signal x_5                               : boolean;
  -- spec.hs:(610,1)-(614,28)
  signal result_36                         : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:(610,1)-(614,28)
  signal t_3                               : Spec_topEntity_types.Tuple2_1;
  signal x_6                               : signed(63 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal \c$out0_case_alt\                 : signed(63 downto 0);
  -- spec.hs:345:1-53
  signal dta_4                             : signed(63 downto 0);
  -- spec.hs:345:1-53
  signal tag_5                             : unsigned(7 downto 0);
  -- spec.hs:345:1-53
  signal tagToMatch_0                      : unsigned(7 downto 0);
  signal result_37                         : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(589,1)-(593,28)
  signal result_38                         : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:(589,1)-(593,28)
  signal b_3                               : boolean;
  -- spec.hs:345:1-53
  signal \c$ds_app_arg_0\                  : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(376,1)-(378,33)
  signal inputs_2                          : Spec_topEntity_types.Inputs_3;
  signal \c$app_arg_11\                    : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_39                         : boolean;
  signal \c$app_arg_12\                    : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_40                         : boolean;
  signal \c$app_arg_13\                    : Spec_topEntity_types.array_of_Tags(0 to 3);
  signal result_41                         : Spec_topEntity_types.Tags;
  signal result_42                         : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(677,1)-(681,28)
  signal result_43                         : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:(677,1)-(681,28)
  signal b_4                               : boolean;
  -- spec.hs:(677,1)-(681,28)
  signal t_4                               : Spec_topEntity_types.Tuple2_1;
  signal x_7                               : signed(63 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal \c$out5_case_alt\                 : signed(63 downto 0);
  -- spec.hs:345:1-53
  signal tagToMatch_1                      : unsigned(7 downto 0);
  signal \c$app_arg_14\                    : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_44                         : boolean;
  signal \c$app_arg_15\                    : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_45                         : Spec_topEntity_types.Tags;
  -- spec.hs:339:1-55
  signal winTag                            : unsigned(7 downto 0);
  signal result_46                         : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(664,1)-(668,28)
  signal result_47                         : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:(664,1)-(668,28)
  signal b_5                               : boolean;
  -- spec.hs:(664,1)-(668,28)
  signal t_5                               : Spec_topEntity_types.Tuple2_1;
  signal x_8                               : signed(63 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal \c$out4_case_alt\                 : signed(63 downto 0);
  -- spec.hs:339:1-55
  signal winData                           : signed(63 downto 0);
  signal result_48                         : unsigned(7 downto 0);
  signal \c$app_arg_16\                    : unsigned(7 downto 0);
  signal \c$app_arg_17\                    : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_49                         : boolean;
  signal \c$app_arg_18\                    : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_50                         : Spec_topEntity_types.Tags;
  -- spec.hs:(568,9)-(570,29)
  signal t_6                               : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_51                         : unsigned(7 downto 0);
  -- spec.hs:(568,9)-(570,29)
  signal b_6                               : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal f1                                : unsigned(7 downto 0);
  signal result_52                         : unsigned(7 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal pOut6                             : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal t_7                               : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_53                         : unsigned(7 downto 0);
  -- spec.hs:(568,9)-(570,29)
  signal b_7                               : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal f1_0                              : unsigned(7 downto 0);
  signal result_54                         : unsigned(7 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal pOut5                             : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal t_8                               : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_55                         : unsigned(7 downto 0);
  -- spec.hs:(568,9)-(570,29)
  signal b_8                               : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal f1_1                              : unsigned(7 downto 0);
  signal result_56                         : unsigned(7 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal pOut4                             : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal t_9                               : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_57                         : unsigned(7 downto 0);
  -- spec.hs:(568,9)-(570,29)
  signal b_9                               : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal f1_2                              : unsigned(7 downto 0);
  signal result_58                         : unsigned(7 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal \c$ds7_case_alt\                  : boolean;
  -- spec.hs:108:34-42
  signal x1_0                              : boolean;
  -- spec.hs:108:34-42
  signal x0_0                              : boolean;
  -- spec.hs:(376,1)-(378,33)
  signal pOut3                             : Spec_topEntity_types.PacingOut3;
  -- spec.hs:(568,9)-(570,29)
  signal t_10                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_59                         : unsigned(7 downto 0);
  -- spec.hs:(568,9)-(570,29)
  signal b_10                              : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal f1_3                              : unsigned(7 downto 0);
  signal result_60                         : unsigned(7 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal pOut2                             : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal t_11                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_61                         : unsigned(7 downto 0);
  -- spec.hs:(568,9)-(570,29)
  signal b_11                              : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal f1_4                              : unsigned(7 downto 0);
  signal result_62                         : unsigned(7 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal pOut1                             : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal t_12                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_63                         : unsigned(7 downto 0);
  -- spec.hs:(568,9)-(570,29)
  signal b_12                              : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal f1_5                              : unsigned(7 downto 0);
  signal result_64                         : unsigned(7 downto 0);
  -- spec.hs:105:34-42
  signal x_9                               : boolean;
  -- spec.hs:(376,1)-(378,33)
  signal pOut0                             : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal t_13                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_65                         : unsigned(7 downto 0);
  -- spec.hs:(568,9)-(570,29)
  signal b_13                              : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal f1_6                              : unsigned(7 downto 0);
  signal result_66                         : unsigned(7 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal pIn1_0                            : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal t_14                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_67                         : unsigned(7 downto 0);
  -- spec.hs:(568,9)-(570,29)
  signal b_14                              : boolean;
  -- spec.hs:(568,9)-(570,29)
  signal f1_7                              : unsigned(7 downto 0);
  signal result_68                         : unsigned(7 downto 0);
  -- spec.hs:(376,1)-(378,33)
  signal pIn0_0                            : boolean;
  -- spec.hs:(376,1)-(378,33)
  signal pacings                           : Spec_topEntity_types.Pacings_1;
  -- spec.hs:(577,1)-(579,22)
  signal toWait                            : signed(63 downto 0) := (to_signed(0,64));
  signal result_69                         : boolean;
  signal result_70                         : signed(63 downto 0);
  -- spec.hs:(577,1)-(579,22)
  signal b_15                              : boolean;
  signal result_71                         : signed(63 downto 0);
  signal x_10                              : signed(63 downto 0);
  signal result_72                         : boolean;
  -- spec.hs:(376,1)-(378,33)
  signal b_16                              : boolean;
  signal result_73                         : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_19\                    : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false ) )
, Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingIn1 => false
, Pacings_1_sel2_pacingOut0 => false
, Pacings_1_sel3_pacingOut1 => false
, Pacings_1_sel4_pacingOut2 => false
, Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
, PacingOut3_sel1_boolean_1 => false )
, Pacings_1_sel6_pacingOut4 => false
, Pacings_1_sel7_pacingOut5 => false
, Pacings_1_sel8_pacingOut6 => false ) );
  signal \c$case_alt\                      : Spec_topEntity_types.Tuple2_2;
  signal \c$case_alt_0\                    : Spec_topEntity_types.Tuple2_2;
  signal \c$case_alt_1\                    : Spec_topEntity_types.Tuple2_2;
  signal \c$case_alt_2\                    : Spec_topEntity_types.Tuple2_2;
  signal \c$case_alt_3\                    : Spec_topEntity_types.Tuple2_2;
  signal \c$case_alt_4\                    : Spec_topEntity_types.Tuple2_2;
  signal \c$app_arg_20\                    : boolean := false;
  signal \c$case_alt_5\                    : boolean;
  signal \c$case_alt_6\                    : boolean;
  signal \c$case_alt_7\                    : boolean;
  signal \c$case_alt_8\                    : boolean;
  signal \c$case_alt_9\                    : boolean;
  signal \c$app_arg_21\                    : boolean := false;
  signal \c$case_alt_10\                   : boolean;
  signal \c$case_alt_11\                   : boolean;
  signal \c$case_alt_12\                   : boolean;
  signal \c$app_arg_22\                    : signed(63 downto 0);
  -- spec.hs:(182,1)-(184,25)
  signal \buffer\                          : Spec_topEntity_types.array_of_Tuple2_2(0 to 3) := Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                                       , PacingOut3_sel1_boolean_1 => false )
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5 => false
                                       , Pacings_1_sel8_pacingOut6 => false ) )
                                       , ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                                       , PacingOut3_sel1_boolean_1 => false )
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5 => false
                                       , Pacings_1_sel8_pacingOut6 => false ) )
                                       , ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                                       , PacingOut3_sel1_boolean_1 => false )
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5 => false
                                       , Pacings_1_sel8_pacingOut6 => false ) )
                                       , ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0 => false
                                       , Pacings_1_sel3_pacingOut1 => false
                                       , Pacings_1_sel4_pacingOut2 => false
                                       , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                                       , PacingOut3_sel1_boolean_1 => false )
                                       , Pacings_1_sel6_pacingOut4 => false
                                       , Pacings_1_sel7_pacingOut5 => false
                                       , Pacings_1_sel8_pacingOut6 => false ) ) );
  -- spec.hs:(182,1)-(184,25)
  signal \c$buffer_case_alt\               : Spec_topEntity_types.array_of_Tuple2_2(0 to 3);
  -- spec.hs:(182,1)-(184,25)
  signal \c$buffer_case_alt_0\             : Spec_topEntity_types.array_of_Tuple2_2(0 to 3);
  -- spec.hs:(182,1)-(184,25)
  signal \c$buffer_case_alt_1\             : Spec_topEntity_types.array_of_Tuple2_2(0 to 3);
  -- spec.hs:(182,1)-(184,25)
  signal qData                             : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:(182,1)-(184,25)
  signal x_11                              : signed(63 downto 0);
  -- spec.hs:(182,1)-(184,25)
  signal cursor                            : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(182,1)-(184,25)
  signal \c$cursor_case_alt\               : signed(63 downto 0);
  -- spec.hs:(182,1)-(184,25)
  signal push                              : boolean;
  -- spec.hs:(182,1)-(184,25)
  signal \c$cursor_case_alt_0\             : signed(63 downto 0);
  -- spec.hs:(182,1)-(184,25)
  signal \c$cursor_case_alt_1\             : signed(63 downto 0);
  -- spec.hs:(182,1)-(184,25)
  signal \c$cursor_case_scrut\             : boolean;
  -- spec.hs:(182,1)-(184,25)
  signal \c$cursor_app_arg\                : signed(63 downto 0);
  -- spec.hs:(182,1)-(184,25)
  signal \c$cursor_case_alt_2\             : signed(63 downto 0);
  -- spec.hs:(182,1)-(184,25)
  signal pop                               : boolean;
  -- spec.hs:(182,1)-(184,25)
  signal \c$cursor_case_alt_3\             : signed(63 downto 0);
  -- spec.hs:(182,1)-(184,25)
  signal \c$cursor_case_alt_4\             : signed(63 downto 0);
  signal inputs                            : Spec_topEntity_types.Inputs_3;
  signal \c$ds2_case_alt_selection_res\    : boolean;
  signal \c$out1_case_alt_selection_res\   : boolean;
  signal result_selection_res              : boolean;
  signal \c$out3_case_alt_selection_res\   : boolean;
  signal \c$out2_case_alt_selection_res\   : boolean;
  signal \c$out2_case_alt_selection_res_0\ : boolean;
  signal \c$out1_case_alt_selection_res_0\ : boolean;
  signal result_selection_8                : boolean;
  signal \c$out0_case_alt_selection_res\   : boolean;
  signal \c$out5_case_alt_selection_res\   : boolean;
  signal \c$out4_case_alt_selection_res\   : boolean;
  signal result_selection_res_4            : boolean;
  signal result_selection_15               : boolean;
  signal result_selection_19               : boolean;
  signal result_selection_23               : boolean;
  signal result_selection_29               : boolean;
  signal result_selection_33               : boolean;
  signal result_selection_37               : boolean;
  signal result_selection_41               : boolean;
  signal result_selection_45               : boolean;
  signal \c$vec\                           : Spec_topEntity_types.array_of_Tuple2_2(0 to 4);
  signal \c$buffer_case_alt_sel_alt_3\     : Spec_topEntity_types.Tuple2_3;
  signal \c$vec_0\                         : Spec_topEntity_types.array_of_Tuple2_2(0 to 4);
  signal \c$buffer_case_alt_sel_alt_5\     : Spec_topEntity_types.Tuple2_3;
  signal result                            : Spec_topEntity_types.Outputs;
  signal result_0                          : Spec_topEntity_types.ValidInt;
  signal result_1                          : Spec_topEntity_types.ValidInt;
  signal result_2                          : Spec_topEntity_types.ValidInt;
  signal result_3                          : Spec_topEntity_types.ValidInt;
  signal result_4                          : Spec_topEntity_types.ValidInt;
  signal result_5                          : Spec_topEntity_types.ValidInt;
  signal result_6                          : Spec_topEntity_types.ValidInt;

begin
  inputs <= ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => inputs_0_0
            , ValidInt_sel1_valid => inputs_0_1 )
            , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => inputs_1_0
            , ValidInt_sel1_valid => inputs_1_1 ) );

  result_7 <= to_signed(2000,64) when timer1Over else
              (x + to_signed(2000,64));

  x <= result_8;

  -- register begin
  result_8_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_8 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_8 <= result_7;
      end if;
    end if;
  end process;
  -- register end

  result_9 <= to_signed(2000,64) when timer0Over else
              (x_0 + to_signed(2000,64));

  x_0 <= result_10;

  -- register begin
  result_10_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_10 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_10 <= result_9;
      end if;
    end if;
  end process;
  -- register end

  result_11 <= ( Tuple2_sel0_boolean => inputs.Inputs_3_sel0_input0.ValidInt_sel1_valid or (inputs.Inputs_3_sel1_input1.ValidInt_sel1_valid or (timer0Over or timer1Over))
               , Tuple2_sel1_Tuple2_2 => ( Tuple2_2_sel0_Inputs_3 => inputs
               , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => pIn0
               , Pacings_1_sel1_pacingIn1 => pIn1
               , Pacings_1_sel2_pacingOut0 => pIn0
               , Pacings_1_sel3_pacingOut1 => timer1Over
               , Pacings_1_sel4_pacingOut2 => timer0Over
               , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => pIn0
               , PacingOut3_sel1_boolean_1 => pIn1 )
               , Pacings_1_sel6_pacingOut4 => timer1Over
               , Pacings_1_sel7_pacingOut5 => timer1Over
               , Pacings_1_sel8_pacingOut6 => timer1Over ) ) );

  pIn0 <= inputs.Inputs_3_sel0_input0.ValidInt_sel1_valid;

  pIn1 <= inputs.Inputs_3_sel1_input1.ValidInt_sel1_valid;

  timer0Over <= result_10 >= (to_signed(500000,64));

  timer1Over <= result_8 >= (to_signed(1000000,64));

  result_12 <= ( Tuple2_0_sel0_boolean => result_69 and (not result_72)
               , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidInt_sel0_value => dta_1
               , ValidInt_sel1_valid => result_22 )
               , Outputs_sel1_output1 => ( ValidInt_sel0_value => dta_2
               , ValidInt_sel1_valid => result_21 )
               , Outputs_sel2_output2 => ( ValidInt_sel0_value => dta_0
               , ValidInt_sel1_valid => result_20 )
               , Outputs_sel3_output3 => ( ValidInt_sel0_value => dta
               , ValidInt_sel1_valid => result_19 )
               , Outputs_sel4_output4 => ( ValidInt_sel0_value => winData
               , ValidInt_sel1_valid => result_18 )
               , Outputs_sel5_output5 => ( ValidInt_sel0_value => result_43.Tuple2_1_sel1_signed
               , ValidInt_sel1_valid => result_17 )
               , Outputs_sel6_output6 => ( ValidInt_sel0_value => result_15.Tuple2_1_sel1_signed
               , ValidInt_sel1_valid => result_13 ) ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
    signal iterateI_ho1_4_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut6;

    r_block_62 : block
        signal \c$bb_res_res\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res\ <= iterateI_ho1_0_arg0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res <= \c$bb_res_res\;


      end block;

    r_block_63 : block
        signal \c$bb_res_res_0\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_0_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_0\ <= iterateI_ho1_0_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res <= \c$bb_res_res_0\;


      end block;

    r_block_64 : block
        signal \c$bb_res_res_1\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_1_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_1\ <= iterateI_ho1_1_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res <= \c$bb_res_res_1\;


      end block;

    r_block_65 : block
        signal \c$bb_res_res_2\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_2_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_2\ <= iterateI_ho1_2_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res <= \c$bb_res_res_2\;


      end block;

    r_block_66 : block
        signal \c$bb_res_res_3\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_3_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_3\ <= iterateI_ho1_3_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res <= \c$bb_res_res_3\;


      end block;

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut6
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res
                                                         , iterateI_ho1_3_res
                                                         , iterateI_ho1_4_res );


  end block;

  result_13 <=  \c$app_arg\(\c$app_arg\'high) ;

  result_14 <= t when b else
               result_15;

  -- register begin
  result_15_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_15 <= ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_15 <= result_14;
      end if;
    end if;
  end process;
  -- register end

  b <= result_16;

  t <= ( Tuple2_1_sel0_unsigned => result_41.Tags_sel8_output6_0
       , Tuple2_1_sel1_signed => (x_1 + to_signed(1,64)) );

  x_1 <= \c$ds2_case_alt\;

  \c$ds2_case_alt_selection_res\ <= tag = tagToMatch;

  \c$ds2_case_alt\ <= result_43.Tuple2_1_sel1_signed when \c$ds2_case_alt_selection_res\ else
                      to_signed(0,64);

  tag <= result_43.Tuple2_1_sel0_unsigned;

  tagToMatch <= result_41.Tags_sel7_output5_0;

  iterateI_0 : block
    signal iterateI_ho1_0_res_0 : boolean;
    signal iterateI_ho1_1_res_0 : boolean;
    signal iterateI_ho1_2_res_0 : boolean;
    signal iterateI_ho1_3_res_0 : boolean;
  begin
    r_block_67 : block
        signal \c$bb_res_res_4\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_4_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_4\ <= pOut6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_0 <= \c$bb_res_res_4\;


      end block;

    r_block_68 : block
        signal \c$bb_res_res_5\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_5_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_5\ <= iterateI_ho1_0_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_0 <= \c$bb_res_res_5\;


      end block;

    r_block_69 : block
        signal \c$bb_res_res_6\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_6_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_6\ <= iterateI_ho1_1_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_0 <= \c$bb_res_res_6\;


      end block;

    r_block_70 : block
        signal \c$bb_res_res_7\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= iterateI_ho1_2_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_0 <= \c$bb_res_res_7\;


      end block;

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_boolean'( pOut6
                                                           , iterateI_ho1_0_res_0
                                                           , iterateI_ho1_1_res_0
                                                           , iterateI_ho1_2_res_0
                                                           , iterateI_ho1_3_res_0 );


  end block;

  result_16 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_arg0_0 : boolean;
    signal iterateI_ho1_0_res_1  : boolean;
    signal iterateI_ho1_1_res_1  : boolean;
    signal iterateI_ho1_2_res_1  : boolean;
    signal iterateI_ho1_3_res_1  : boolean;
    signal iterateI_ho1_4_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_0 <= pOut5;

    r_block_71 : block
        signal \c$bb_res_res_8\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_8_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_8\ <= iterateI_ho1_0_arg0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_1 <= \c$bb_res_res_8\;


      end block;

    r_block_72 : block
        signal \c$bb_res_res_9\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_9_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_9\ <= iterateI_ho1_0_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_1 <= \c$bb_res_res_9\;


      end block;

    r_block_73 : block
        signal \c$bb_res_res_10\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_10_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_10\ <= iterateI_ho1_1_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_1 <= \c$bb_res_res_10\;


      end block;

    r_block_74 : block
        signal \c$bb_res_res_11\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_11_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_11\ <= iterateI_ho1_2_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_1 <= \c$bb_res_res_11\;


      end block;

    r_block_75 : block
        signal \c$bb_res_res_12\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_12_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_12\ <= iterateI_ho1_3_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_0 <= \c$bb_res_res_12\;


      end block;

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_boolean'( pOut5
                                                           , iterateI_ho1_0_res_1
                                                           , iterateI_ho1_1_res_1
                                                           , iterateI_ho1_2_res_1
                                                           , iterateI_ho1_3_res_1
                                                           , iterateI_ho1_4_res_0 );


  end block;

  result_17 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  iterateI_2 : block
    signal iterateI_ho1_0_arg0_1 : boolean;
    signal iterateI_ho1_0_res_2  : boolean;
    signal iterateI_ho1_1_res_2  : boolean;
    signal iterateI_ho1_2_res_2  : boolean;
    signal iterateI_ho1_3_res_2  : boolean;
    signal iterateI_ho1_4_res_1  : boolean;
  begin
    iterateI_ho1_0_arg0_1 <= pOut4;

    r_block_76 : block
        signal \c$bb_res_res_13\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= iterateI_ho1_0_arg0_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_2 <= \c$bb_res_res_13\;


      end block;

    r_block_77 : block
        signal \c$bb_res_res_14\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_14_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_14\ <= iterateI_ho1_0_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_2 <= \c$bb_res_res_14\;


      end block;

    r_block_78 : block
        signal \c$bb_res_res_15\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_15_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_15\ <= iterateI_ho1_1_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_2 <= \c$bb_res_res_15\;


      end block;

    r_block_79 : block
        signal \c$bb_res_res_16\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_16_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_16\ <= iterateI_ho1_2_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_2 <= \c$bb_res_res_16\;


      end block;

    r_block_80 : block
        signal \c$bb_res_res_17\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_17_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_17\ <= iterateI_ho1_3_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_1 <= \c$bb_res_res_17\;


      end block;

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_boolean'( pOut4
                                                           , iterateI_ho1_0_res_2
                                                           , iterateI_ho1_1_res_2
                                                           , iterateI_ho1_2_res_2
                                                           , iterateI_ho1_3_res_2
                                                           , iterateI_ho1_4_res_1 );


  end block;

  result_18 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  iterateI_3 : block
    signal iterateI_ho1_0_res_3 : boolean;
    signal iterateI_ho1_1_res_3 : boolean;
    signal iterateI_ho1_2_res_3 : boolean;
    signal iterateI_ho1_3_res_3 : boolean;
    signal iterateI_ho1_4_res_2 : boolean;
  begin
    r_block_81 : block
        signal \c$bb_res_res_18\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_18_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_18\ <= \c$ds7_case_alt\;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_18\;


      end block;

    r_block_82 : block
        signal \c$bb_res_res_19\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_19_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_19\ <= iterateI_ho1_0_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_3 <= \c$bb_res_res_19\;


      end block;

    r_block_83 : block
        signal \c$bb_res_res_20\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_20_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_20\ <= iterateI_ho1_1_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_3 <= \c$bb_res_res_20\;


      end block;

    r_block_84 : block
        signal \c$bb_res_res_21\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_21_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_21\ <= iterateI_ho1_2_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_3 <= \c$bb_res_res_21\;


      end block;

    r_block_85 : block
        signal \c$bb_res_res_22\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_22_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_22\ <= iterateI_ho1_3_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_2 <= \c$bb_res_res_22\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( \c$ds7_case_alt\
                                                           , iterateI_ho1_0_res_3
                                                           , iterateI_ho1_1_res_3
                                                           , iterateI_ho1_2_res_3
                                                           , iterateI_ho1_3_res_3
                                                           , iterateI_ho1_4_res_2 );


  end block;

  result_19 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  iterateI_4 : block
    signal iterateI_ho1_0_arg0_2 : boolean;
    signal iterateI_ho1_0_res_4  : boolean;
    signal iterateI_ho1_1_res_4  : boolean;
    signal iterateI_ho1_2_res_4  : boolean;
    signal iterateI_ho1_3_res_4  : boolean;
    signal iterateI_ho1_4_res_3  : boolean;
  begin
    iterateI_ho1_0_arg0_2 <= pOut2;

    r_block_86 : block
        signal \c$bb_res_res_23\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_23_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_23\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_23\;


      end block;

    r_block_87 : block
        signal \c$bb_res_res_24\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_24_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_24\ <= iterateI_ho1_0_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_4 <= \c$bb_res_res_24\;


      end block;

    r_block_88 : block
        signal \c$bb_res_res_25\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_25_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_25\ <= iterateI_ho1_1_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_4 <= \c$bb_res_res_25\;


      end block;

    r_block_89 : block
        signal \c$bb_res_res_26\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_26_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_26\ <= iterateI_ho1_2_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_4 <= \c$bb_res_res_26\;


      end block;

    r_block_90 : block
        signal \c$bb_res_res_27\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_27_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_27\ <= iterateI_ho1_3_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_3 <= \c$bb_res_res_27\;


      end block;

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                           , iterateI_ho1_0_res_4
                                                           , iterateI_ho1_1_res_4
                                                           , iterateI_ho1_2_res_4
                                                           , iterateI_ho1_3_res_4
                                                           , iterateI_ho1_4_res_3 );


  end block;

  result_20 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  iterateI_5 : block
    signal iterateI_ho1_0_arg0_3 : boolean;
    signal iterateI_ho1_0_res_5  : boolean;
    signal iterateI_ho1_1_res_5  : boolean;
    signal iterateI_ho1_2_res_5  : boolean;
    signal iterateI_ho1_3_res_5  : boolean;
    signal iterateI_ho1_4_res_4  : boolean;
  begin
    iterateI_ho1_0_arg0_3 <= pOut1;

    r_block_91 : block
        signal \c$bb_res_res_28\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_28_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_28\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_28\;


      end block;

    r_block_92 : block
        signal \c$bb_res_res_29\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_29_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_29\ <= iterateI_ho1_0_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_5 <= \c$bb_res_res_29\;


      end block;

    r_block_93 : block
        signal \c$bb_res_res_30\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_30_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_30\ <= iterateI_ho1_1_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_5 <= \c$bb_res_res_30\;


      end block;

    r_block_94 : block
        signal \c$bb_res_res_31\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_31_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_31\ <= iterateI_ho1_2_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_5 <= \c$bb_res_res_31\;


      end block;

    r_block_95 : block
        signal \c$bb_res_res_32\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_32_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_32\ <= iterateI_ho1_3_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_4 <= \c$bb_res_res_32\;


      end block;

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                           , iterateI_ho1_0_res_5
                                                           , iterateI_ho1_1_res_5
                                                           , iterateI_ho1_2_res_5
                                                           , iterateI_ho1_3_res_5
                                                           , iterateI_ho1_4_res_4 );


  end block;

  result_21 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  iterateI_6 : block
    signal iterateI_ho1_0_arg0_4 : boolean;
    signal iterateI_ho1_0_res_6  : boolean;
    signal iterateI_ho1_1_res_6  : boolean;
    signal iterateI_ho1_2_res_6  : boolean;
    signal iterateI_ho1_3_res_6  : boolean;
    signal iterateI_ho1_4_res_5  : boolean;
  begin
    iterateI_ho1_0_arg0_4 <= x_9;

    r_block_96 : block
        signal \c$bb_res_res_33\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_33_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_33\ <= iterateI_ho1_0_arg0_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_33\;


      end block;

    r_block_97 : block
        signal \c$bb_res_res_34\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_34_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_34\ <= iterateI_ho1_0_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_6 <= \c$bb_res_res_34\;


      end block;

    r_block_98 : block
        signal \c$bb_res_res_35\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_35_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_35\ <= iterateI_ho1_1_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_6 <= \c$bb_res_res_35\;


      end block;

    r_block_99 : block
        signal \c$bb_res_res_36\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_36_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_36\ <= iterateI_ho1_2_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_6 <= \c$bb_res_res_36\;


      end block;

    r_block_100 : block
        signal \c$bb_res_res_37\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_37_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_37\ <= iterateI_ho1_3_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_5 <= \c$bb_res_res_37\;


      end block;

    \c$app_arg_6\ <= Spec_topEntity_types.array_of_boolean'( x_9
                                                           , iterateI_ho1_0_res_6
                                                           , iterateI_ho1_1_res_6
                                                           , iterateI_ho1_2_res_6
                                                           , iterateI_ho1_3_res_6
                                                           , iterateI_ho1_4_res_5 );


  end block;

  result_22 <=  \c$app_arg_6\(\c$app_arg_6\'high) ;

  tag_0 <= result_24.Tuple2_1_sel0_unsigned;

  result_23 <= t_0 when b_0 else
               result_24;

  -- register begin
  result_24_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_24 <= ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_24 <= result_23;
      end if;
    end if;
  end process;
  -- register end

  b_0 <= result_33;

  t_0 <= ( Tuple2_1_sel0_unsigned => result_45.Tags_sel3_output1_0
         , Tuple2_1_sel1_signed => (x_2 + y) );

  x_2 <= \c$out1_case_alt_0\;

  y <= \c$out1_case_alt\;

  \c$out1_case_alt_selection_res\ <= tag_1 = to_unsigned(3,8);

  \c$out1_case_alt\ <= to_signed(2,64) when \c$out1_case_alt_selection_res\ else
                       dta;

  dta <= result_26.Tuple2_1_sel1_signed;

  tag_1 <= result_26.Tuple2_1_sel0_unsigned;

  result_selection_res <= x0 and x1;

  result_25 <= t_1 when result_selection_res else
               result_26;

  x0 <= result_29.PacingOut3_sel0_boolean_0;

  x1 <= result_29.PacingOut3_sel1_boolean_1;

  -- register begin
  result_26_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_26 <= ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_26 <= result_25;
      end if;
    end if;
  end process;
  -- register end

  t_1 <= ( Tuple2_1_sel0_unsigned => result_50.Tags_sel5_output3_0
         , Tuple2_1_sel1_signed => (x_3 + to_signed(1,64)) );

  x_3 <= \c$out3_case_alt\;

  \c$out3_case_alt_selection_res\ <= tag_2 = to_unsigned(3,8);

  \c$out3_case_alt\ <= to_signed(0,64) when \c$out3_case_alt_selection_res\ else
                       dta_0;

  dta_0 <= result_28.Tuple2_1_sel1_signed;

  tag_2 <= result_28.Tuple2_1_sel0_unsigned;

  result_27 <= t_2 when b_1 else
               result_28;

  -- register begin
  result_28_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_28 <= ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_28 <= result_27;
      end if;
    end if;
  end process;
  -- register end

  b_1 <= result_34;

  t_2 <= ( Tuple2_1_sel0_unsigned => result_41.Tags_sel4_output2_0
         , Tuple2_1_sel1_signed => (x_4 + y_0) );

  x_4 <= \c$out2_case_alt_0\;

  y_0 <= \c$out2_case_alt\;

  \c$out2_case_alt_selection_res\ <= tag_3 = to_unsigned(3,8);

  \c$out2_case_alt\ <= to_signed(11,64) when \c$out2_case_alt_selection_res\ else
                       dta_1;

  dta_1 <= result_36.Tuple2_1_sel1_signed;

  tag_3 <= result_36.Tuple2_1_sel0_unsigned;

  \c$out2_case_alt_selection_res_0\ <= tag_0 = to_unsigned(3,8);

  \c$out2_case_alt_0\ <= to_signed(10,64) when \c$out2_case_alt_selection_res_0\ else
                         dta_2;

  dta_2 <= result_24.Tuple2_1_sel1_signed;

  iterateI_7 : block
    signal iterateI_ho1_0_res_7 : Spec_topEntity_types.PacingOut3;
    signal iterateI_ho1_1_res_7 : Spec_topEntity_types.PacingOut3;
  begin
    r_block_101 : block
        signal \c$bb_res_res_38\ : Spec_topEntity_types.PacingOut3 := (PacingOut3_sel0_boolean_0 => false, PacingOut3_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_38_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_38\ <= pOut3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_38\;


      end block;

    r_block_102 : block
        signal \c$bb_res_res_39\ : Spec_topEntity_types.PacingOut3 := (PacingOut3_sel0_boolean_0 => false, PacingOut3_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_39_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_39\ <= iterateI_ho1_0_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_7 <= \c$bb_res_res_39\;


      end block;

    \c$app_arg_7\ <= Spec_topEntity_types.array_of_PacingOut3'( pOut3
                                                              , iterateI_ho1_0_res_7
                                                              , iterateI_ho1_1_res_7 );


  end block;

  result_29 <=  \c$app_arg_7\(\c$app_arg_7\'high) ;

  \c$out1_case_alt_selection_res_0\ <= tag_4 = to_unsigned(3,8);

  \c$out1_case_alt_0\ <= to_signed(1,64) when \c$out1_case_alt_selection_res_0\ else
                         dta_3;

  dta_3 <= result_31.Tuple2_1_sel1_signed;

  tag_4 <= result_31.Tuple2_1_sel0_unsigned;

  result_30 <= ( Tuple2_1_sel0_unsigned => t_13
               , Tuple2_1_sel1_signed => \c$ds_app_arg\ ) when b_2 else
               result_31;

  -- register begin
  result_31_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_31 <= ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_31 <= result_30;
      end if;
    end if;
  end process;
  -- register end

  b_2 <= result_32;

  -- delay begin
  cds_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$ds_app_arg\ <= inputs_2.Inputs_3_sel1_input1.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_8 : block
    signal iterateI_ho1_0_res_8 : boolean;
  begin
    r_block_103 : block
        signal \c$bb_res_res_40\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_40_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_40\ <= pIn1_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_8 <= \c$bb_res_res_40\;


      end block;

    \c$app_arg_8\ <= Spec_topEntity_types.array_of_boolean'( pIn1_0
                                                           , iterateI_ho1_0_res_8 );


  end block;

  result_32 <=  \c$app_arg_8\(\c$app_arg_8\'high) ;

  iterateI_9 : block
    signal iterateI_ho1_0_res_9 : boolean;
    signal iterateI_ho1_1_res_8 : boolean;
    signal iterateI_ho1_2_res_7 : boolean;
  begin
    r_block_104 : block
        signal \c$bb_res_res_41\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_41_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_41\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_9 <= \c$bb_res_res_41\;


      end block;

    r_block_105 : block
        signal \c$bb_res_res_42\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_42_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_42\ <= iterateI_ho1_0_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_8 <= \c$bb_res_res_42\;


      end block;

    r_block_106 : block
        signal \c$bb_res_res_43\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_43_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_43\ <= iterateI_ho1_1_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_7 <= \c$bb_res_res_43\;


      end block;

    \c$app_arg_9\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                           , iterateI_ho1_0_res_9
                                                           , iterateI_ho1_1_res_8
                                                           , iterateI_ho1_2_res_7 );


  end block;

  result_33 <=  \c$app_arg_9\(\c$app_arg_9\'high) ;

  iterateI_10 : block
    signal iterateI_ho1_0_res_10 : boolean;
    signal iterateI_ho1_1_res_9  : boolean;
    signal iterateI_ho1_2_res_8  : boolean;
    signal iterateI_ho1_3_res_7  : boolean;
  begin
    r_block_107 : block
        signal \c$bb_res_res_44\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_44_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_44\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_10 <= \c$bb_res_res_44\;


      end block;

    r_block_108 : block
        signal \c$bb_res_res_45\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_45_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_45\ <= iterateI_ho1_0_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_9 <= \c$bb_res_res_45\;


      end block;

    r_block_109 : block
        signal \c$bb_res_res_46\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_46_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_46\ <= iterateI_ho1_1_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_8 <= \c$bb_res_res_46\;


      end block;

    r_block_110 : block
        signal \c$bb_res_res_47\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_47_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_47\ <= iterateI_ho1_2_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_7 <= \c$bb_res_res_47\;


      end block;

    \c$app_arg_10\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                            , iterateI_ho1_0_res_10
                                                            , iterateI_ho1_1_res_9
                                                            , iterateI_ho1_2_res_8
                                                            , iterateI_ho1_3_res_7 );


  end block;

  result_34 <=  \c$app_arg_10\(\c$app_arg_10\'high) ;

  result_selection_8 <= x_5;

  result_35 <= t_3 when result_selection_8 else
               result_36;

  x_5 <= result_40;

  -- register begin
  result_36_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_36 <= ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_36 <= result_35;
      end if;
    end if;
  end process;
  -- register end

  t_3 <= ( Tuple2_1_sel0_unsigned => result_50.Tags_sel2_output0_0
         , Tuple2_1_sel1_signed => (x_6 + to_signed(1,64)) );

  x_6 <= \c$out0_case_alt\;

  \c$out0_case_alt_selection_res\ <= tag_5 = tagToMatch_0;

  \c$out0_case_alt\ <= dta_4 when \c$out0_case_alt_selection_res\ else
                       to_signed(0,64);

  dta_4 <= result_38.Tuple2_1_sel1_signed;

  tag_5 <= result_38.Tuple2_1_sel0_unsigned;

  tagToMatch_0 <= result_50.Tags_sel0_input0_0;

  result_37 <= ( Tuple2_1_sel0_unsigned => t_14
               , Tuple2_1_sel1_signed => \c$ds_app_arg_0\ ) when b_3 else
               result_38;

  -- register begin
  result_38_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_38 <= ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_38 <= result_37;
      end if;
    end if;
  end process;
  -- register end

  b_3 <= result_39;

  -- delay begin
  cds_app_arg_0_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$ds_app_arg_0\ <= inputs_2.Inputs_3_sel0_input0.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  inputs_2 <= result_73.Tuple3_sel2_Tuple2_2.Tuple2_2_sel0_Inputs_3;

  iterateI_11 : block
    signal iterateI_ho1_0_res_11 : boolean;
  begin
    r_block_111 : block
        signal \c$bb_res_res_48\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_48_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_48\ <= pIn0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_11 <= \c$bb_res_res_48\;


      end block;

    \c$app_arg_11\ <= Spec_topEntity_types.array_of_boolean'( pIn0_0
                                                            , iterateI_ho1_0_res_11 );


  end block;

  result_39 <=  \c$app_arg_11\(\c$app_arg_11\'high) ;

  iterateI_12 : block
    signal iterateI_ho1_0_res_12 : boolean;
    signal iterateI_ho1_1_res_10 : boolean;
  begin
    r_block_112 : block
        signal \c$bb_res_res_49\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_49_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_49\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_12 <= \c$bb_res_res_49\;


      end block;

    r_block_113 : block
        signal \c$bb_res_res_50\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_50_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_50\ <= iterateI_ho1_0_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_10 <= \c$bb_res_res_50\;


      end block;

    \c$app_arg_12\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                            , iterateI_ho1_0_res_12
                                                            , iterateI_ho1_1_res_10 );


  end block;

  result_40 <=  \c$app_arg_12\(\c$app_arg_12\'high) ;

  iterateI_13 : block
    signal iterateI_ho1_0_arg0_5 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_13 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_11 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_9  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_5 <= ( Tags_sel0_input0_0 => t_14
                             , Tags_sel1_input1_0 => t_13
                             , Tags_sel2_output0_0 => t_12
                             , Tags_sel3_output1_0 => t_11
                             , Tags_sel4_output2_0 => t_10
                             , Tags_sel5_output3_0 => t_9
                             , Tags_sel6_output4_0 => t_8
                             , Tags_sel7_output5_0 => t_7
                             , Tags_sel8_output6_0 => t_6 );

    r_block_114 : block
        signal \c$bb_res_res_51\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(3,8)
      , Tags_sel1_input1_0 => to_unsigned(3,8)
      , Tags_sel2_output0_0 => to_unsigned(3,8)
      , Tags_sel3_output1_0 => to_unsigned(3,8)
      , Tags_sel4_output2_0 => to_unsigned(3,8)
      , Tags_sel5_output3_0 => to_unsigned(3,8)
      , Tags_sel6_output4_0 => to_unsigned(3,8)
      , Tags_sel7_output5_0 => to_unsigned(3,8)
      , Tags_sel8_output6_0 => to_unsigned(3,8) );
      begin
        -- delay begin
        cbb_res_res_51_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_51\ <= iterateI_ho1_0_arg0_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_13 <= \c$bb_res_res_51\;


      end block;

    r_block_115 : block
        signal \c$bb_res_res_52\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(3,8)
      , Tags_sel1_input1_0 => to_unsigned(3,8)
      , Tags_sel2_output0_0 => to_unsigned(3,8)
      , Tags_sel3_output1_0 => to_unsigned(3,8)
      , Tags_sel4_output2_0 => to_unsigned(3,8)
      , Tags_sel5_output3_0 => to_unsigned(3,8)
      , Tags_sel6_output4_0 => to_unsigned(3,8)
      , Tags_sel7_output5_0 => to_unsigned(3,8)
      , Tags_sel8_output6_0 => to_unsigned(3,8) );
      begin
        -- delay begin
        cbb_res_res_52_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_52\ <= iterateI_ho1_0_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_11 <= \c$bb_res_res_52\;


      end block;

    r_block_116 : block
        signal \c$bb_res_res_53\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(3,8)
      , Tags_sel1_input1_0 => to_unsigned(3,8)
      , Tags_sel2_output0_0 => to_unsigned(3,8)
      , Tags_sel3_output1_0 => to_unsigned(3,8)
      , Tags_sel4_output2_0 => to_unsigned(3,8)
      , Tags_sel5_output3_0 => to_unsigned(3,8)
      , Tags_sel6_output4_0 => to_unsigned(3,8)
      , Tags_sel7_output5_0 => to_unsigned(3,8)
      , Tags_sel8_output6_0 => to_unsigned(3,8) );
      begin
        -- delay begin
        cbb_res_res_53_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_53\ <= iterateI_ho1_1_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_9 <= \c$bb_res_res_53\;


      end block;

    \c$app_arg_13\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_14
                                                         , Tags_sel1_input1_0 => t_13
                                                         , Tags_sel2_output0_0 => t_12
                                                         , Tags_sel3_output1_0 => t_11
                                                         , Tags_sel4_output2_0 => t_10
                                                         , Tags_sel5_output3_0 => t_9
                                                         , Tags_sel6_output4_0 => t_8
                                                         , Tags_sel7_output5_0 => t_7
                                                         , Tags_sel8_output6_0 => t_6 )
                                                         , iterateI_ho1_0_res_13
                                                         , iterateI_ho1_1_res_11
                                                         , iterateI_ho1_2_res_9 );


  end block;

  result_41 <=  \c$app_arg_13\(\c$app_arg_13\'high) ;

  result_42 <= t_4 when b_4 else
               result_43;

  -- register begin
  result_43_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_43 <= ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_43 <= result_42;
      end if;
    end if;
  end process;
  -- register end

  b_4 <= result_44;

  t_4 <= ( Tuple2_1_sel0_unsigned => result_45.Tags_sel7_output5_0
         , Tuple2_1_sel1_signed => (x_7 + to_signed(1,64)) );

  x_7 <= \c$out5_case_alt\;

  \c$out5_case_alt_selection_res\ <= winTag = tagToMatch_1;

  \c$out5_case_alt\ <= winData when \c$out5_case_alt_selection_res\ else
                       to_signed(0,64);

  tagToMatch_1 <= result_45.Tags_sel6_output4_0;

  iterateI_14 : block
    signal iterateI_ho1_0_res_14 : boolean;
    signal iterateI_ho1_1_res_12 : boolean;
    signal iterateI_ho1_2_res_10 : boolean;
  begin
    r_block_117 : block
        signal \c$bb_res_res_54\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_54_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_54\ <= pOut5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_14 <= \c$bb_res_res_54\;


      end block;

    r_block_118 : block
        signal \c$bb_res_res_55\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_55_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_55\ <= iterateI_ho1_0_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_12 <= \c$bb_res_res_55\;


      end block;

    r_block_119 : block
        signal \c$bb_res_res_56\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_56_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_56\ <= iterateI_ho1_1_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_10 <= \c$bb_res_res_56\;


      end block;

    \c$app_arg_14\ <= Spec_topEntity_types.array_of_boolean'( pOut5
                                                            , iterateI_ho1_0_res_14
                                                            , iterateI_ho1_1_res_12
                                                            , iterateI_ho1_2_res_10 );


  end block;

  result_44 <=  \c$app_arg_14\(\c$app_arg_14\'high) ;

  iterateI_15 : block
    signal iterateI_ho1_0_arg0_6 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_15 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_13 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_6 <= ( Tags_sel0_input0_0 => t_14
                             , Tags_sel1_input1_0 => t_13
                             , Tags_sel2_output0_0 => t_12
                             , Tags_sel3_output1_0 => t_11
                             , Tags_sel4_output2_0 => t_10
                             , Tags_sel5_output3_0 => t_9
                             , Tags_sel6_output4_0 => t_8
                             , Tags_sel7_output5_0 => t_7
                             , Tags_sel8_output6_0 => t_6 );

    r_block_120 : block
        signal \c$bb_res_res_57\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(3,8)
      , Tags_sel1_input1_0 => to_unsigned(3,8)
      , Tags_sel2_output0_0 => to_unsigned(3,8)
      , Tags_sel3_output1_0 => to_unsigned(3,8)
      , Tags_sel4_output2_0 => to_unsigned(3,8)
      , Tags_sel5_output3_0 => to_unsigned(3,8)
      , Tags_sel6_output4_0 => to_unsigned(3,8)
      , Tags_sel7_output5_0 => to_unsigned(3,8)
      , Tags_sel8_output6_0 => to_unsigned(3,8) );
      begin
        -- delay begin
        cbb_res_res_57_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_57\ <= iterateI_ho1_0_arg0_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_15 <= \c$bb_res_res_57\;


      end block;

    r_block_121 : block
        signal \c$bb_res_res_58\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(3,8)
      , Tags_sel1_input1_0 => to_unsigned(3,8)
      , Tags_sel2_output0_0 => to_unsigned(3,8)
      , Tags_sel3_output1_0 => to_unsigned(3,8)
      , Tags_sel4_output2_0 => to_unsigned(3,8)
      , Tags_sel5_output3_0 => to_unsigned(3,8)
      , Tags_sel6_output4_0 => to_unsigned(3,8)
      , Tags_sel7_output5_0 => to_unsigned(3,8)
      , Tags_sel8_output6_0 => to_unsigned(3,8) );
      begin
        -- delay begin
        cbb_res_res_58_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_58\ <= iterateI_ho1_0_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_13 <= \c$bb_res_res_58\;


      end block;

    \c$app_arg_15\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_14
                                                         , Tags_sel1_input1_0 => t_13
                                                         , Tags_sel2_output0_0 => t_12
                                                         , Tags_sel3_output1_0 => t_11
                                                         , Tags_sel4_output2_0 => t_10
                                                         , Tags_sel5_output3_0 => t_9
                                                         , Tags_sel6_output4_0 => t_8
                                                         , Tags_sel7_output5_0 => t_7
                                                         , Tags_sel8_output6_0 => t_6 )
                                                         , iterateI_ho1_0_res_15
                                                         , iterateI_ho1_1_res_13 );


  end block;

  result_45 <=  \c$app_arg_15\(\c$app_arg_15\'high) ;

  winTag <= result_47.Tuple2_1_sel0_unsigned;

  result_46 <= t_5 when b_5 else
               result_47;

  -- register begin
  result_47_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_47 <= ( Tuple2_1_sel0_unsigned => to_unsigned(3,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_47 <= result_46;
      end if;
    end if;
  end process;
  -- register end

  b_5 <= result_49;

  t_5 <= ( Tuple2_1_sel0_unsigned => result_50.Tags_sel6_output4_0
         , Tuple2_1_sel1_signed => (x_8 + to_signed(1,64)) );

  x_8 <= \c$out4_case_alt\;

  \c$out4_case_alt_selection_res\ <= result_48 = winTag;

  \c$out4_case_alt\ <= winData when \c$out4_case_alt_selection_res\ else
                       to_signed(0,64);

  winData <= result_47.Tuple2_1_sel1_signed;

  result_selection_res_4 <= result_50.Tags_sel6_output4_0 > to_unsigned(1,8);

  result_48 <= \c$app_arg_16\ when result_selection_res_4 else
               \c$app_arg_16\ + to_unsigned(2,8);

  \c$app_arg_16\ <= result_50.Tags_sel6_output4_0 - to_unsigned(1,8);

  iterateI_16 : block
    signal iterateI_ho1_0_res_16 : boolean;
    signal iterateI_ho1_1_res_14 : boolean;
  begin
    r_block_122 : block
        signal \c$bb_res_res_59\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_59_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_59\ <= pOut4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_16 <= \c$bb_res_res_59\;


      end block;

    r_block_123 : block
        signal \c$bb_res_res_60\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_60_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_60\ <= iterateI_ho1_0_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_14 <= \c$bb_res_res_60\;


      end block;

    \c$app_arg_17\ <= Spec_topEntity_types.array_of_boolean'( pOut4
                                                            , iterateI_ho1_0_res_16
                                                            , iterateI_ho1_1_res_14 );


  end block;

  result_49 <=  \c$app_arg_17\(\c$app_arg_17\'high) ;

  iterateI_17 : block
    signal iterateI_ho1_0_arg0_7 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_17 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_7 <= ( Tags_sel0_input0_0 => t_14
                             , Tags_sel1_input1_0 => t_13
                             , Tags_sel2_output0_0 => t_12
                             , Tags_sel3_output1_0 => t_11
                             , Tags_sel4_output2_0 => t_10
                             , Tags_sel5_output3_0 => t_9
                             , Tags_sel6_output4_0 => t_8
                             , Tags_sel7_output5_0 => t_7
                             , Tags_sel8_output6_0 => t_6 );

    r_block_124 : block
        signal \c$bb_res_res_61\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(3,8)
      , Tags_sel1_input1_0 => to_unsigned(3,8)
      , Tags_sel2_output0_0 => to_unsigned(3,8)
      , Tags_sel3_output1_0 => to_unsigned(3,8)
      , Tags_sel4_output2_0 => to_unsigned(3,8)
      , Tags_sel5_output3_0 => to_unsigned(3,8)
      , Tags_sel6_output4_0 => to_unsigned(3,8)
      , Tags_sel7_output5_0 => to_unsigned(3,8)
      , Tags_sel8_output6_0 => to_unsigned(3,8) );
      begin
        -- delay begin
        cbb_res_res_61_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_61\ <= iterateI_ho1_0_arg0_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_17 <= \c$bb_res_res_61\;


      end block;

    \c$app_arg_18\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_14
                                                         , Tags_sel1_input1_0 => t_13
                                                         , Tags_sel2_output0_0 => t_12
                                                         , Tags_sel3_output1_0 => t_11
                                                         , Tags_sel4_output2_0 => t_10
                                                         , Tags_sel5_output3_0 => t_9
                                                         , Tags_sel6_output4_0 => t_8
                                                         , Tags_sel7_output5_0 => t_7
                                                         , Tags_sel8_output6_0 => t_6 )
                                                         , iterateI_ho1_0_res_17 );


  end block;

  result_50 <=  \c$app_arg_18\(\c$app_arg_18\'high) ;

  -- register begin
  t_6_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_6 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_6 <= result_51;
      end if;
    end if;
  end process;
  -- register end

  result_selection_15 <= pOut6;

  result_51 <= result_52 when result_selection_15 else
               t_6;

  b_6 <= t_6 = to_unsigned(2,8);

  f1 <= t_6 + to_unsigned(1,8);

  result_52 <= to_unsigned(1,8) when b_6 else
               f1;

  pOut6 <= pacings.Pacings_1_sel8_pacingOut6;

  -- register begin
  t_7_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_7 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_7 <= result_53;
      end if;
    end if;
  end process;
  -- register end

  result_selection_19 <= pOut5;

  result_53 <= result_54 when result_selection_19 else
               t_7;

  b_7 <= t_7 = to_unsigned(2,8);

  f1_0 <= t_7 + to_unsigned(1,8);

  result_54 <= to_unsigned(1,8) when b_7 else
               f1_0;

  pOut5 <= pacings.Pacings_1_sel7_pacingOut5;

  -- register begin
  t_8_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_8 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_8 <= result_55;
      end if;
    end if;
  end process;
  -- register end

  result_selection_23 <= pOut4;

  result_55 <= result_56 when result_selection_23 else
               t_8;

  b_8 <= t_8 = to_unsigned(2,8);

  f1_1 <= t_8 + to_unsigned(1,8);

  result_56 <= to_unsigned(1,8) when b_8 else
               f1_1;

  pOut4 <= pacings.Pacings_1_sel6_pacingOut4;

  -- register begin
  t_9_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_9 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_9 <= result_57;
      end if;
    end if;
  end process;
  -- register end

  result_57 <= result_58 when \c$ds7_case_alt\ else
               t_9;

  b_9 <= t_9 = to_unsigned(2,8);

  f1_2 <= t_9 + to_unsigned(1,8);

  result_58 <= to_unsigned(1,8) when b_9 else
               f1_2;

  \c$ds7_case_alt\ <= x0_0 and x1_0;

  x1_0 <= pOut3.PacingOut3_sel1_boolean_1;

  x0_0 <= pOut3.PacingOut3_sel0_boolean_0;

  pOut3 <= pacings.Pacings_1_sel5_pacingOut3_0;

  -- register begin
  t_10_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_10 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_10 <= result_59;
      end if;
    end if;
  end process;
  -- register end

  result_selection_29 <= pOut2;

  result_59 <= result_60 when result_selection_29 else
               t_10;

  b_10 <= t_10 = to_unsigned(2,8);

  f1_3 <= t_10 + to_unsigned(1,8);

  result_60 <= to_unsigned(1,8) when b_10 else
               f1_3;

  pOut2 <= pacings.Pacings_1_sel4_pacingOut2;

  -- register begin
  t_11_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_11 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_11 <= result_61;
      end if;
    end if;
  end process;
  -- register end

  result_selection_33 <= pOut1;

  result_61 <= result_62 when result_selection_33 else
               t_11;

  b_11 <= t_11 = to_unsigned(2,8);

  f1_4 <= t_11 + to_unsigned(1,8);

  result_62 <= to_unsigned(1,8) when b_11 else
               f1_4;

  pOut1 <= pacings.Pacings_1_sel3_pacingOut1;

  -- register begin
  t_12_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_12 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_12 <= result_63;
      end if;
    end if;
  end process;
  -- register end

  result_selection_37 <= x_9;

  result_63 <= result_64 when result_selection_37 else
               t_12;

  b_12 <= t_12 = to_unsigned(2,8);

  f1_5 <= t_12 + to_unsigned(1,8);

  result_64 <= to_unsigned(1,8) when b_12 else
               f1_5;

  x_9 <= pOut0;

  pOut0 <= pacings.Pacings_1_sel2_pacingOut0;

  -- register begin
  t_13_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_13 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_13 <= result_65;
      end if;
    end if;
  end process;
  -- register end

  result_selection_41 <= pIn1_0;

  result_65 <= result_66 when result_selection_41 else
               t_13;

  b_13 <= t_13 = to_unsigned(2,8);

  f1_6 <= t_13 + to_unsigned(1,8);

  result_66 <= to_unsigned(1,8) when b_13 else
               f1_6;

  pIn1_0 <= pacings.Pacings_1_sel1_pacingIn1;

  -- register begin
  t_14_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_14 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_14 <= result_67;
      end if;
    end if;
  end process;
  -- register end

  result_selection_45 <= pIn0_0;

  result_67 <= result_68 when result_selection_45 else
               t_14;

  b_14 <= t_14 = to_unsigned(2,8);

  f1_7 <= t_14 + to_unsigned(1,8);

  result_68 <= to_unsigned(1,8) when b_14 else
               f1_7;

  pIn0_0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_73.Tuple3_sel2_Tuple2_2.Tuple2_2_sel1_Pacings_1;

  -- register begin
  toWait_register : process(clk,rst)
  begin
    if rst =  '1'  then
      toWait <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        toWait <= result_70;
      end if;
    end if;
  end process;
  -- register end

  result_69 <= toWait = (to_signed(0,64));

  result_70 <= to_signed(2,64) when result_72 else
               result_71;

  b_15 <= toWait > (to_signed(0,64));

  result_71 <= (x_10 - to_signed(1,64)) when b_15 else
               toWait;

  x_10 <= toWait;

  result_72 <= true when b_16 else
               false;

  b_16 <= result_69 and result_73.Tuple3_sel1_boolean_1;

  result_73 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_21\
               , Tuple3_sel1_boolean_1 => \c$app_arg_20\
               , Tuple3_sel2_Tuple2_2 => \c$app_arg_19\ );

  -- register begin
  capp_arg_19_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_19\ <= ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false ) )
  , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingIn1 => false
  , Pacings_1_sel2_pacingOut0 => false
  , Pacings_1_sel3_pacingOut1 => false
  , Pacings_1_sel4_pacingOut2 => false
  , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
  , PacingOut3_sel1_boolean_1 => false )
  , Pacings_1_sel6_pacingOut4 => false
  , Pacings_1_sel7_pacingOut5 => false
  , Pacings_1_sel8_pacingOut6 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_19\ <= \c$case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt\ <= \c$case_alt_0\ when push else
                  \c$case_alt_1\;

  \c$case_alt_0\ <= \c$case_alt_2\ when pop else
                    ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false ) )
                    , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingIn1 => false
                    , Pacings_1_sel2_pacingOut0 => false
                    , Pacings_1_sel3_pacingOut1 => false
                    , Pacings_1_sel4_pacingOut2 => false
                    , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                    , PacingOut3_sel1_boolean_1 => false )
                    , Pacings_1_sel6_pacingOut4 => false
                    , Pacings_1_sel7_pacingOut5 => false
                    , Pacings_1_sel8_pacingOut6 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false ) )
                    , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingIn1 => false
                    , Pacings_1_sel2_pacingOut0 => false
                    , Pacings_1_sel3_pacingOut1 => false
                    , Pacings_1_sel4_pacingOut2 => false
                    , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                    , PacingOut3_sel1_boolean_1 => false )
                    , Pacings_1_sel6_pacingOut4 => false
                    , Pacings_1_sel7_pacingOut5 => false
                    , Pacings_1_sel8_pacingOut6 => false ) );

  with (x_11) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_11) select
    \c$case_alt_3\ <= ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false ) )
                      , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingIn1 => false
                      , Pacings_1_sel2_pacingOut0 => false
                      , Pacings_1_sel3_pacingOut1 => false
                      , Pacings_1_sel4_pacingOut2 => false
                      , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                      , PacingOut3_sel1_boolean_1 => false )
                      , Pacings_1_sel6_pacingOut4 => false
                      , Pacings_1_sel7_pacingOut5 => false
                      , Pacings_1_sel8_pacingOut6 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec : block
    signal vec_index : integer range 0 to 4-1;
  begin
    vec_index <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index);
  end block;
  -- index end

  -- register begin
  capp_arg_20_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_20\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_20\ <= \c$case_alt_5\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_5\ <= \c$case_alt_6\ when push else
                    \c$case_alt_7\;

  \c$case_alt_6\ <= true when pop else
                    \c$case_alt_8\;

  \c$case_alt_7\ <= \c$case_alt_9\ when pop else
                    \c$case_alt_8\;

  \c$case_alt_8\ <= true when pop else
                    false;

  with (x_11) select
    \c$case_alt_9\ <= false when x"0000000000000000",
                      true when others;

  -- register begin
  capp_arg_21_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_21\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_21\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_22\);

  \c$app_arg_22\ <= to_signed(1,64) when x_11 /= to_signed(4,64) else to_signed(0,64);

  -- register begin
  buffer_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \buffer\ <= Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                                         , PacingOut3_sel1_boolean_1 => false )
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5 => false
                                         , Pacings_1_sel8_pacingOut6 => false ) )
                                         , ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                                         , PacingOut3_sel1_boolean_1 => false )
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5 => false
                                         , Pacings_1_sel8_pacingOut6 => false ) )
                                         , ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                                         , PacingOut3_sel1_boolean_1 => false )
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5 => false
                                         , Pacings_1_sel8_pacingOut6 => false ) )
                                         , ( Tuple2_2_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple2_2_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0 => false
                                         , Pacings_1_sel3_pacingOut1 => false
                                         , Pacings_1_sel4_pacingOut2 => false
                                         , Pacings_1_sel5_pacingOut3_0 => ( PacingOut3_sel0_boolean_0 => false
                                         , PacingOut3_sel1_boolean_1 => false )
                                         , Pacings_1_sel6_pacingOut4 => false
                                         , Pacings_1_sel7_pacingOut5 => false
                                         , Pacings_1_sel8_pacingOut6 => false ) ) );
    elsif rising_edge(clk) then
      if en then
        \buffer\ <= \c$buffer_case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$buffer_case_alt\ <= \c$buffer_case_alt_0\ when push else
                         \buffer\;

  \c$vec\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => qData)) & Spec_topEntity_types.array_of_Tuple2_2'(\buffer\)));

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec\(0 to 4-1),\c$vec\(4 to \c$vec\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_3_sel0_array_of_Tuple2_2_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_0\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => qData)) & Spec_topEntity_types.array_of_Tuple2_2'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_0\(0 to 4-1),\c$vec_0\(4 to \c$vec_0\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_3_sel0_array_of_Tuple2_2_0;

  qData <= result_11.Tuple2_sel1_Tuple2_2;

  x_11 <= cursor;

  -- register begin
  cursor_register : process(clk,rst)
  begin
    if rst =  '1'  then
      cursor <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        cursor <= \c$cursor_case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$cursor_case_alt\ <= \c$cursor_case_alt_0\ when push else
                         \c$cursor_case_alt_2\;

  push <= result_11.Tuple2_sel0_boolean;

  \c$cursor_case_alt_0\ <= cursor when pop else
                           \c$cursor_case_alt_1\;

  \c$cursor_case_alt_1\ <= cursor when \c$cursor_case_scrut\ else
                           (x_11 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_11 = to_signed(4,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_12.Tuple2_0_sel0_boolean;

  with (x_11) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_11 - to_signed(1,64));

  result <= result_12.Tuple2_0_sel1_Outputs;

  result_0 <= result.Outputs_sel0_output0;

  result_1 <= result.Outputs_sel1_output1;

  result_2 <= result.Outputs_sel2_output2;

  result_3 <= result.Outputs_sel3_output3;

  result_4 <= result.Outputs_sel4_output4;

  result_5 <= result.Outputs_sel5_output5;

  result_6 <= result.Outputs_sel6_output6;

  result_0_0 <= result_0.ValidInt_sel0_value;

  result_0_1 <= result_0.ValidInt_sel1_valid;

  result_1_0 <= result_1.ValidInt_sel0_value;

  result_1_1 <= result_1.ValidInt_sel1_valid;

  result_2_0 <= result_2.ValidInt_sel0_value;

  result_2_1 <= result_2.ValidInt_sel1_valid;

  result_3_0 <= result_3.ValidInt_sel0_value;

  result_3_1 <= result_3.ValidInt_sel1_valid;

  result_4_0 <= result_4.ValidInt_sel0_value;

  result_4_1 <= result_4.ValidInt_sel1_valid;

  result_5_0 <= result_5.ValidInt_sel0_value;

  result_5_1 <= result_5.ValidInt_sel1_valid;

  result_6_0 <= result_6.ValidInt_sel0_value;

  result_6_1 <= result_6.ValidInt_sel1_valid;


end;

