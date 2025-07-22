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
       inputs_2_0 : in signed(63 downto 0);
       inputs_2_1 : in boolean;
       result_0_0 : out boolean;
       result_0_1 : out boolean;
       result_1_0 : out boolean;
       result_1_1 : out boolean;
       result_2_0 : out boolean;
       result_2_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_3                        : signed(63 downto 0);
  signal x                               : signed(63 downto 0);
  signal result_4                        : signed(63 downto 0) := (to_signed(0,64));
  signal result_5                        : Spec_topEntity_types.Tuple2;
  -- spec.hs:(252,1)-(254,31)
  signal pIn1                            : boolean;
  -- spec.hs:(252,1)-(254,31)
  signal timer0Over                      : boolean;
  signal result_6                        : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                     : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_7                        : boolean;
  -- spec.hs:(572,1)-(576,29)
  signal \c$t_app_arg\                   : signed(63 downto 0);
  signal result_8                        : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(572,1)-(576,29)
  signal result_9                        : Spec_topEntity_types.Tuple2_1 := (Tuple2_1_sel0_unsigned => to_unsigned(8,8), Tuple2_1_sel1_boolean => false);
  -- spec.hs:(572,1)-(576,29)
  signal b                               : boolean;
  -- spec.hs:(572,1)-(576,29)
  signal t                               : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(627,1)-(632,36)
  signal x_0                             : signed(63 downto 0);
  -- spec.hs:(627,1)-(632,36)
  signal win                             : Spec_topEntity_types.array_of_signed_64(0 to 5);
  -- spec.hs:(627,1)-(632,36)
  signal window                          : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(8,8)
, Tuple2_2_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_10                       : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:(627,1)-(632,36)
  signal t_0                             : Spec_topEntity_types.Tuple2_2;
  signal result_11                       : Spec_topEntity_types.array_of_signed_64(0 to 5);
  -- spec.hs:(627,1)-(632,36)
  signal \c$t_case_alt\                  : Spec_topEntity_types.array_of_signed_64(0 to 5);
  -- spec.hs:(627,1)-(632,36)
  signal \c$t_case_alt_0\                : Spec_topEntity_types.array_of_signed_64(0 to 5);
  -- spec.hs:(627,1)-(632,36)
  signal lastBucketUpdated               : Spec_topEntity_types.array_of_signed_64(0 to 5);
  -- spec.hs:(627,1)-(632,36)
  signal b_0                             : boolean;
  -- spec.hs:102:34-42
  signal x_1                             : boolean;
  -- spec.hs:315:1-63
  signal \c$ws_app_arg\                  : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:315:1-63
  signal \c$ws_app_arg_0\                : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut\                  : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_0\                : Spec_topEntity_types.Maybe;
  signal result_12                       : boolean;
  -- spec.hs:315:1-63
  signal i                               : Spec_topEntity_types.index_3;
  -- spec.hs:315:1-63
  signal ws                              : Spec_topEntity_types.array_of_Maybe(0 to 3);
  signal \c$app_arg_0\                   : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_13                       : boolean;
  signal \c$app_arg_1\                   : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_14                       : boolean;
  signal \c$app_arg_2\                   : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_15                       : boolean;
  -- spec.hs:315:1-63
  signal \c$ws_app_arg_1\                : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:315:1-63
  signal \c$ws_app_arg_2\                : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut_1\                : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_2\                : Spec_topEntity_types.Maybe;
  signal result_16                       : boolean;
  -- spec.hs:315:1-63
  signal i_0                             : Spec_topEntity_types.index_3;
  -- spec.hs:315:1-63
  signal ws_0                            : Spec_topEntity_types.array_of_Maybe(0 to 3);
  signal \c$app_arg_3\                   : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_17                       : boolean;
  signal \c$app_arg_4\                   : Spec_topEntity_types.array_of_Tags(0 to 4);
  signal result_18                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_5\                   : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_19                       : boolean;
  -- spec.hs:(543,1)-(547,29)
  signal \c$t_app_arg_0\                 : signed(63 downto 0);
  signal result_20                       : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(543,1)-(547,29)
  signal result_21                       : Spec_topEntity_types.Tuple2_1 := (Tuple2_1_sel0_unsigned => to_unsigned(8,8), Tuple2_1_sel1_boolean => false);
  -- spec.hs:(543,1)-(547,29)
  signal b_1                             : boolean;
  -- spec.hs:(543,1)-(547,29)
  signal t_1                             : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(598,1)-(603,36)
  signal x_2                             : signed(63 downto 0);
  -- spec.hs:(598,1)-(603,36)
  signal win_0                           : Spec_topEntity_types.array_of_signed_64(0 to 3);
  -- spec.hs:(598,1)-(603,36)
  signal window_0                        : Spec_topEntity_types.Tuple2_4 := ( Tuple2_4_sel0_unsigned => to_unsigned(8,8)
, Tuple2_4_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_22                       : Spec_topEntity_types.Tuple2_4;
  -- spec.hs:(598,1)-(603,36)
  signal t_2                             : Spec_topEntity_types.Tuple2_4;
  signal result_23                       : Spec_topEntity_types.array_of_signed_64(0 to 3);
  -- spec.hs:(598,1)-(603,36)
  signal \c$t_case_alt_1\                : Spec_topEntity_types.array_of_signed_64(0 to 3);
  -- spec.hs:(598,1)-(603,36)
  signal \c$t_case_alt_2\                : Spec_topEntity_types.array_of_signed_64(0 to 3);
  -- spec.hs:(598,1)-(603,36)
  signal lastBucketUpdated_0             : Spec_topEntity_types.array_of_signed_64(0 to 3);
  -- spec.hs:(598,1)-(603,36)
  signal b_2                             : boolean;
  -- spec.hs:315:1-63
  signal \c$ws_app_arg_3\                : Spec_topEntity_types.array_of_Tuple2_5(0 to 1);
  -- spec.hs:315:1-63
  signal \c$ws_app_arg_4\                : Spec_topEntity_types.array_of_Maybe_0(0 to 1);
  signal \c$case_scrut_3\                : Spec_topEntity_types.Tuple2_6;
  signal \c$case_scrut_4\                : Spec_topEntity_types.Maybe_0;
  signal result_24                       : signed(63 downto 0);
  -- spec.hs:315:1-63
  signal i_1                             : Spec_topEntity_types.index_2;
  -- spec.hs:315:1-63
  signal ws_1                            : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal result_25                       : Spec_topEntity_types.array_of_Tuple2_6(0 to 1);
  -- spec.hs:(510,1)-(514,36)
  signal result_26                       : Spec_topEntity_types.array_of_Tuple2_6(0 to 1) := Spec_topEntity_types.array_of_Tuple2_6'( ( Tuple2_6_sel0_unsigned => to_unsigned(8,8)
                                       , Tuple2_6_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_6_sel0_unsigned => to_unsigned(8,8)
                                       , Tuple2_6_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(510,1)-(514,36)
  signal b_3                             : boolean;
  -- spec.hs:(510,1)-(514,36)
  signal t_3                             : Spec_topEntity_types.array_of_Tuple2_6(0 to 1);
  -- spec.hs:(359,1)-(361,33)
  signal \c$ds1_app_arg\                 : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_6\                   : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_27                       : boolean;
  signal \c$app_arg_7\                   : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_28                       : boolean;
  -- spec.hs:(359,1)-(361,33)
  signal slides                          : Spec_topEntity_types.Slides_1;
  signal \c$app_arg_8\                   : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_29                       : boolean;
  signal \c$app_arg_9\                   : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_30                       : boolean;
  signal result_31                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  -- spec.hs:102:34-42
  signal x_3                             : boolean;
  -- spec.hs:(558,1)-(562,37)
  signal result_32                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 2) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(8,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(8,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(8,8)
                                       , Tuple2_1_sel1_boolean => false ) );
  -- spec.hs:(558,1)-(562,37)
  signal t_4                             : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  -- spec.hs:(359,1)-(361,33)
  signal \c$out1_case_alt\               : signed(63 downto 0);
  -- spec.hs:328:1-53
  signal dta                             : signed(63 downto 0);
  -- spec.hs:328:1-53
  signal tagToMatch                      : unsigned(7 downto 0);
  -- spec.hs:328:1-53
  signal tag                             : unsigned(7 downto 0);
  signal result_33                       : Spec_topEntity_types.Tuple2_6;
  -- spec.hs:(522,1)-(526,28)
  signal result_34                       : Spec_topEntity_types.Tuple2_6 := ( Tuple2_6_sel0_unsigned => to_unsigned(8,8)
, Tuple2_6_sel1_signed => to_signed(0,64) );
  -- spec.hs:(522,1)-(526,28)
  signal b_4                             : boolean;
  -- spec.hs:328:1-53
  signal \c$ds_app_arg\                  : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(359,1)-(361,33)
  signal inputs_3                        : Spec_topEntity_types.Inputs_4;
  signal \c$app_arg_10\                  : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_35                       : boolean;
  signal \c$app_arg_11\                  : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_36                       : boolean;
  signal \c$app_arg_12\                  : Spec_topEntity_types.array_of_Tags(0 to 3);
  signal result_37                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_13\                  : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_38                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_14\                  : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_39                       : Spec_topEntity_types.Tags;
  -- spec.hs:(498,9)-(500,29)
  signal t_5                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_40                       : unsigned(7 downto 0);
  -- spec.hs:(498,9)-(500,29)
  signal b_5                             : boolean;
  -- spec.hs:(498,9)-(500,29)
  signal f1                              : unsigned(7 downto 0);
  signal result_41                       : unsigned(7 downto 0);
  -- spec.hs:(359,1)-(361,33)
  signal pOut2                           : boolean;
  -- spec.hs:(498,9)-(500,29)
  signal t_6                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_42                       : unsigned(7 downto 0);
  -- spec.hs:(498,9)-(500,29)
  signal b_6                             : boolean;
  -- spec.hs:(498,9)-(500,29)
  signal f1_0                            : unsigned(7 downto 0);
  signal result_43                       : unsigned(7 downto 0);
  -- spec.hs:102:34-42
  signal x_4                             : boolean;
  -- spec.hs:(359,1)-(361,33)
  signal pOut1                           : boolean;
  -- spec.hs:(498,9)-(500,29)
  signal t_7                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_44                       : unsigned(7 downto 0);
  -- spec.hs:(498,9)-(500,29)
  signal b_7                             : boolean;
  -- spec.hs:(498,9)-(500,29)
  signal f1_1                            : unsigned(7 downto 0);
  signal result_45                       : unsigned(7 downto 0);
  -- spec.hs:(359,1)-(361,33)
  signal pOut0                           : boolean;
  -- spec.hs:(498,9)-(500,29)
  signal t_8                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_46                       : unsigned(7 downto 0);
  -- spec.hs:(498,9)-(500,29)
  signal b_8                             : boolean;
  -- spec.hs:(498,9)-(500,29)
  signal f1_2                            : unsigned(7 downto 0);
  signal result_47                       : unsigned(7 downto 0);
  -- spec.hs:(498,9)-(500,29)
  signal t_9                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_48                       : unsigned(7 downto 0);
  -- spec.hs:(498,9)-(500,29)
  signal b_9                             : boolean;
  -- spec.hs:(498,9)-(500,29)
  signal f1_3                            : unsigned(7 downto 0);
  signal result_49                       : unsigned(7 downto 0);
  -- spec.hs:(359,1)-(361,33)
  signal pIn1_0                          : boolean;
  -- spec.hs:(498,9)-(500,29)
  signal t_10                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_50                       : unsigned(7 downto 0);
  -- spec.hs:(498,9)-(500,29)
  signal b_10                            : boolean;
  -- spec.hs:(498,9)-(500,29)
  signal f1_4                            : unsigned(7 downto 0);
  signal result_51                       : unsigned(7 downto 0);
  -- spec.hs:(359,1)-(361,33)
  signal pIn0                            : boolean;
  -- spec.hs:(359,1)-(361,33)
  signal pacings                         : Spec_topEntity_types.Pacings_1;
  signal result_52                       : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_15\                  : Spec_topEntity_types.Tuple3_0 := ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false ) )
, Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
, Slides_1_sel1_slide1 => false )
, Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingIn1 => false
, Pacings_1_sel2_pacingIn2 => false
, Pacings_1_sel3_pacingOut0 => false
, Pacings_1_sel4_pacingOut1 => false
, Pacings_1_sel5_pacingOut2 => false ) );
  signal \c$case_alt\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_0\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_1\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_2\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_3\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_4\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$app_arg_16\                  : boolean := false;
  signal \c$case_alt_5\                  : boolean;
  signal \c$case_alt_6\                  : boolean;
  signal \c$case_alt_7\                  : boolean;
  signal \c$case_alt_8\                  : boolean;
  signal \c$case_alt_9\                  : boolean;
  signal \c$app_arg_17\                  : boolean := false;
  signal \c$case_alt_10\                 : boolean;
  signal \c$case_alt_11\                 : boolean;
  signal \c$case_alt_12\                 : boolean;
  signal \c$app_arg_18\                  : signed(63 downto 0);
  -- spec.hs:(169,1)-(171,25)
  signal \buffer\                        : Spec_topEntity_types.array_of_Tuple3_0(0 to 1) := Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingIn2 => false
                                       , Pacings_1_sel3_pacingOut0 => false
                                       , Pacings_1_sel4_pacingOut1 => false
                                       , Pacings_1_sel5_pacingOut2 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingIn2 => false
                                       , Pacings_1_sel3_pacingOut0 => false
                                       , Pacings_1_sel4_pacingOut1 => false
                                       , Pacings_1_sel5_pacingOut2 => false ) ) );
  -- spec.hs:(169,1)-(171,25)
  signal \c$buffer_case_alt\             : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(169,1)-(171,25)
  signal \c$buffer_case_alt_0\           : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(169,1)-(171,25)
  signal \c$buffer_case_alt_1\           : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(169,1)-(171,25)
  signal qData                           : Spec_topEntity_types.Tuple3_0;
  -- spec.hs:(169,1)-(171,25)
  signal x_5                             : signed(63 downto 0);
  -- spec.hs:(169,1)-(171,25)
  signal cursor                          : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(169,1)-(171,25)
  signal \c$cursor_case_alt\             : signed(63 downto 0);
  -- spec.hs:(169,1)-(171,25)
  signal push                            : boolean;
  -- spec.hs:(169,1)-(171,25)
  signal \c$cursor_case_alt_0\           : signed(63 downto 0);
  -- spec.hs:(169,1)-(171,25)
  signal \c$cursor_case_alt_1\           : signed(63 downto 0);
  -- spec.hs:(169,1)-(171,25)
  signal \c$cursor_case_scrut\           : boolean;
  -- spec.hs:(169,1)-(171,25)
  signal \c$cursor_app_arg\              : signed(63 downto 0);
  -- spec.hs:(169,1)-(171,25)
  signal \c$cursor_case_alt_2\           : signed(63 downto 0);
  -- spec.hs:(169,1)-(171,25)
  signal pop                             : boolean;
  -- spec.hs:(169,1)-(171,25)
  signal \c$cursor_case_alt_3\           : signed(63 downto 0);
  -- spec.hs:(169,1)-(171,25)
  signal \c$cursor_case_alt_4\           : signed(63 downto 0);
  signal inputs                          : Spec_topEntity_types.Inputs_4;
  signal \c$vec\                         : Spec_topEntity_types.array_of_signed_64(0 to 5);
  signal x_projection_4                  : signed(63 downto 0);
  signal \c$t_case_alt_selection_1\      : boolean;
  signal \c$vec_0\                       : Spec_topEntity_types.array_of_signed_64(0 to 6);
  signal \c$t_case_alt_sel_alt_0\        : Spec_topEntity_types.Tuple2_7;
  signal \c$vec_1\                       : Spec_topEntity_types.array_of_signed_64(0 to 6);
  signal \c$t_case_alt_sel_alt_2\        : Spec_topEntity_types.Tuple2_7;
  signal \c$t_case_alt_selection_4\      : boolean;
  signal \c$vec2\                        : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$vec2_0\                      : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$vec_2\                       : Spec_topEntity_types.array_of_signed_64(0 to 3);
  signal x_projection_10                 : signed(63 downto 0);
  signal \c$t_case_alt_selection_7\      : boolean;
  signal \c$vec_3\                       : Spec_topEntity_types.array_of_signed_64(0 to 4);
  signal \c$t_case_alt_sel_alt_6\        : Spec_topEntity_types.Tuple2_8;
  signal \c$vec_4\                       : Spec_topEntity_types.array_of_signed_64(0 to 4);
  signal \c$t_case_alt_sel_alt_8\        : Spec_topEntity_types.Tuple2_8;
  signal \c$t_case_alt_selection_10\     : boolean;
  signal \c$vec2_1\                      : Spec_topEntity_types.array_of_Maybe_0(0 to 1);
  signal \c$vec_5\                       : Spec_topEntity_types.array_of_Tuple2_6(0 to 2);
  signal t_projection_2                  : Spec_topEntity_types.Tuple2_9;
  signal result_selection_12             : boolean;
  signal \c$vec_6\                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  signal t_projection_3                  : Spec_topEntity_types.Tuple2_10;
  signal \c$out1_case_alt_selection_res\ : boolean;
  signal result_selection_16             : boolean;
  signal result_selection_20             : boolean;
  signal result_selection_24             : boolean;
  signal result_selection_29             : boolean;
  signal result_selection_33             : boolean;
  signal result_selection_37             : boolean;
  signal \c$vec_7\                       : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_3\   : Spec_topEntity_types.Tuple2_11;
  signal \c$vec_8\                       : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_5\   : Spec_topEntity_types.Tuple2_11;
  signal result                          : Spec_topEntity_types.Outputs;
  signal result_0                        : Spec_topEntity_types.ValidBool;
  signal result_1                        : Spec_topEntity_types.ValidBool;
  signal result_2                        : Spec_topEntity_types.ValidBool;

begin
  inputs <= ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => inputs_0_0
            , ValidInt_sel1_valid => inputs_0_1 )
            , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => inputs_1_0
            , ValidInt_sel1_valid => inputs_1_1 )
            , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => inputs_2_0
            , ValidInt_sel1_valid => inputs_2_1 ) );

  result_3 <= to_signed(2000,64) when timer0Over else
              (x + to_signed(2000,64));

  x <= result_4;

  -- register begin
  result_4_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_4 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_4 <= result_3;
      end if;
    end if;
  end process;
  -- register end

  result_5 <= ( Tuple2_sel0_boolean => inputs.Inputs_4_sel0_input0.ValidInt_sel1_valid or (inputs.Inputs_4_sel1_input1.ValidInt_sel1_valid or (inputs.Inputs_4_sel2_input2.ValidInt_sel1_valid or timer0Over))
              , Tuple2_sel1_Tuple3_0 => ( Tuple3_0_sel0_Inputs_4 => inputs
              , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => timer0Over
              , Slides_1_sel1_slide1 => timer0Over )
              , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => inputs.Inputs_4_sel0_input0.ValidInt_sel1_valid
              , Pacings_1_sel1_pacingIn1 => pIn1
              , Pacings_1_sel2_pacingIn2 => inputs.Inputs_4_sel2_input2.ValidInt_sel1_valid
              , Pacings_1_sel3_pacingOut0 => timer0Over
              , Pacings_1_sel4_pacingOut1 => pIn1
              , Pacings_1_sel5_pacingOut2 => timer0Over ) ) );

  pIn1 <= inputs.Inputs_4_sel1_input1.ValidInt_sel1_valid;

  timer0Over <= result_4 >= (to_signed(1000000000,64));

  result_6 <= ( Tuple2_0_sel0_boolean => true
              , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidBool_sel0_value_0 => result_21.Tuple2_1_sel1_boolean
              , ValidBool_sel1_valid_0 => result_19 )
              , Outputs_sel1_output1 => ( ValidBool_sel0_value_0 => result_16
              , ValidBool_sel1_valid_0 => result_17 )
              , Outputs_sel2_output2 => ( ValidBool_sel0_value_0 => result_9.Tuple2_1_sel1_boolean
              , ValidBool_sel1_valid_0 => result_7 ) ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
    signal iterateI_ho1_4_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut2;

    r_block_48 : block
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

    r_block_49 : block
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

    r_block_50 : block
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

    r_block_51 : block
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

    r_block_52 : block
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

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res
                                                         , iterateI_ho1_3_res
                                                         , iterateI_ho1_4_res );


  end block;

  result_7 <=  \c$app_arg\(\c$app_arg\'high) ;

  \c$vec\ <= window.Tuple2_2_sel1_array_of_signed_64;

  fold : block
    signal vec     : Spec_topEntity_types.array_of_signed_64(0 to 4);
    signal acc_3_0 : signed(63 downto 0);
    signal acc_1   : signed(63 downto 0);
    signal acc_2   : signed(63 downto 0);
    signal acc_3   : signed(63 downto 0);
    signal acc_4   : signed(63 downto 0);
    signal acc_1_0 : signed(63 downto 0);
    signal acc_1_1 : signed(63 downto 0);
    signal acc_2_0 : signed(63 downto 0);
    signal acc_5   : signed(63 downto 0);
  begin
    \c$t_app_arg\ <= acc_3_0;

    vec <= (\c$vec\(1 to \c$vec\'high));

    acc_1 <= vec(0);

    acc_2 <= vec(1);

    acc_3 <= vec(2);

    acc_4 <= vec(3);

    acc_5 <= vec(4);

    fun_8 : block
        signal x_7             : signed(63 downto 0);
        signal y               : signed(63 downto 0);
        signal \c$case_alt_13\ : signed(63 downto 0);
      begin
        acc_1_0 <= \c$case_alt_13\;

        x_7 <= acc_1;

        y <= acc_2;

        \c$case_alt_13\ <= (x_7 + y);


      end block;

    fun_9 : block
        signal x_8             : signed(63 downto 0);
        signal y_1             : signed(63 downto 0);
        signal \c$case_alt_14\ : signed(63 downto 0);
      begin
        acc_1_1 <= \c$case_alt_14\;

        x_8 <= acc_3;

        y_1 <= acc_4;

        \c$case_alt_14\ <= (x_8 + y_1);


      end block;

    fun_10 : block
        signal x_9             : signed(63 downto 0);
        signal y_2             : signed(63 downto 0);
        signal \c$case_alt_15\ : signed(63 downto 0);
      begin
        acc_2_0 <= \c$case_alt_15\;

        x_9 <= acc_1_0;

        y_2 <= acc_1_1;

        \c$case_alt_15\ <= (x_9 + y_2);


      end block;

    fun_11 : block
        signal x_10            : signed(63 downto 0);
        signal y_3             : signed(63 downto 0);
        signal \c$case_alt_16\ : signed(63 downto 0);
      begin
        acc_3_0 <= \c$case_alt_16\;

        x_10 <= acc_2_0;

        y_3 <= acc_5;

        \c$case_alt_16\ <= (x_10 + y_3);


      end block;


  end block;

  result_8 <= t when b else
              result_9;

  -- register begin
  result_9_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_9 <= (Tuple2_1_sel0_unsigned => to_unsigned(8,8), Tuple2_1_sel1_boolean => false);
    elsif rising_edge(clk) then
      if en then
        result_9 <= result_8;
      end if;
    end if;
  end process;
  -- register end

  b <= result_15;

  t <= ( Tuple2_1_sel0_unsigned => result_37.Tags_sel5_output2_0
       , Tuple2_1_sel1_boolean => \c$t_app_arg\ > (to_signed(12,64)) );

  x_projection_4 <=  win(0) ;

  x_0 <= x_projection_4;

  win <= window.Tuple2_2_sel1_array_of_signed_64;

  -- register begin
  window_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window <= ( Tuple2_2_sel0_unsigned => to_unsigned(8,8)
  , Tuple2_2_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window <= result_10;
      end if;
    end if;
  end process;
  -- register end

  result_10 <= t_0 when b_0 else
               window;

  t_0 <= ( Tuple2_2_sel0_unsigned => result_38.Tags_sel7_slide1_0
         , Tuple2_2_sel1_array_of_signed_64 => result_11 );

  result_11 <= \c$t_case_alt\ when result_13 else
               \c$t_case_alt_0\;

  \c$t_case_alt_selection_1\ <= x_1;

  \c$vec_0\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win)));

  \c$t_case_alt_sel_alt_0\ <= (\c$vec_0\(0 to 6-1),\c$vec_0\(6 to \c$vec_0\'high));

  \c$vec_1\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated)));

  \c$t_case_alt_sel_alt_2\ <= (\c$vec_1\(0 to 6-1),\c$vec_1\(6 to \c$vec_1\'high));

  \c$t_case_alt\ <= \c$t_case_alt_sel_alt_2\.Tuple2_7_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_1\ else
                    \c$t_case_alt_sel_alt_0\.Tuple2_7_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_4\ <= x_1;

  \c$t_case_alt_0\ <= lastBucketUpdated when \c$t_case_alt_selection_4\ else
                      win;

  -- replace begin
  replaceVec : block
    signal vec_index : integer range 0 to 6-1;
  begin
    vec_index <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 6
    -- pragma translate_on
                 ;

    process(vec_index,win,x_0)
      variable ivec : Spec_topEntity_types.array_of_signed_64(0 to 5);
    begin
      ivec := win;
      ivec(vec_index) := ((x_0 + to_signed(1,64)));
      lastBucketUpdated <= ivec;
    end process;
  end block;
  -- replace end

  b_0 <= x_1 or result_13;

  x_1 <= result_14;

  \c$vec2\ <= (ws(1 to ws'high));

  -- zipWith begin
  zipWith : for i_2 in \c$ws_app_arg\'range generate
  begin
    \c$ws_app_arg\(i_2) <= ( Tuple2_3_sel0_Tuple2_1 => result_32(i_2)
               , Tuple2_3_sel1_Maybe => \c$vec2\(i_2) );


  end generate;
  -- zipWith end

  -- imap begin
  imap : block
    function max (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_0 : for i_3 in \c$ws_app_arg_0\'range generate
    begin
      fun_12 : block
      signal \c$case_alt_17\            : Spec_topEntity_types.Maybe;
      -- spec.hs:315:1-63
      signal t_12                       : unsigned(7 downto 0);
      -- spec.hs:315:1-63
      signal x_11                       : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res\ : boolean;
    begin
      \c$ws_app_arg_0\(i_3) <= \c$case_alt_17\;

      \c$case_alt_selection_res\ <= t_12 = result_38.Tags_sel4_output1_0;

      \c$case_alt_17\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_3,max(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res\ else
                         \c$ws_app_arg\(i_3).Tuple2_3_sel1_Maybe;

      t_12 <= x_11.Tuple2_1_sel0_unsigned;

      x_11 <= \c$ws_app_arg\(i_3).Tuple2_3_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec : block
    signal vec_index_0 : integer range 0 to 3-1;
  begin
    vec_index_0 <= to_integer((signed(std_logic_vector(resize(i,64)))))
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_scrut\ <= result_32(vec_index_0);
  end block;
  -- index end

  \c$case_scrut_0\ <=  ws(0) ;

  with (\c$case_scrut_0\(2 downto 2)) select
    result_12 <= false when "0",
                 \c$case_scrut\.Tuple2_1_sel1_boolean when others;

  i <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_0\(1 downto 0)));

  ws <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_0\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  iterateI_0 : block
    signal iterateI_ho1_0_arg0_0 : boolean;
    signal iterateI_ho1_0_res_0  : boolean;
    signal iterateI_ho1_1_res_0  : boolean;
    signal iterateI_ho1_2_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_0 <= slides.Slides_1_sel1_slide1;

    r_block_53 : block
        signal \c$bb_res_res_4\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_4_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_4\ <= iterateI_ho1_0_arg0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_0 <= \c$bb_res_res_4\;


      end block;

    r_block_54 : block
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

    r_block_55 : block
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

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel1_slide1
                                                           , iterateI_ho1_0_res_0
                                                           , iterateI_ho1_1_res_0
                                                           , iterateI_ho1_2_res_0 );


  end block;

  result_13 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_res_1 : boolean;
    signal iterateI_ho1_1_res_1 : boolean;
    signal iterateI_ho1_2_res_1 : boolean;
  begin
    r_block_56 : block
        signal \c$bb_res_res_7\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_1 <= \c$bb_res_res_7\;


      end block;

    r_block_57 : block
        signal \c$bb_res_res_8\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_8_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_8\ <= iterateI_ho1_0_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_1 <= \c$bb_res_res_8\;


      end block;

    r_block_58 : block
        signal \c$bb_res_res_9\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_9_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_9\ <= iterateI_ho1_1_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_1 <= \c$bb_res_res_9\;


      end block;

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                           , iterateI_ho1_0_res_1
                                                           , iterateI_ho1_1_res_1
                                                           , iterateI_ho1_2_res_1 );


  end block;

  result_14 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  iterateI_2 : block
    signal iterateI_ho1_0_res_2 : boolean;
    signal iterateI_ho1_1_res_2 : boolean;
    signal iterateI_ho1_2_res_2 : boolean;
    signal iterateI_ho1_3_res_0 : boolean;
  begin
    r_block_59 : block
        signal \c$bb_res_res_10\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_10_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_10\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_2 <= \c$bb_res_res_10\;


      end block;

    r_block_60 : block
        signal \c$bb_res_res_11\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_11_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_11\ <= iterateI_ho1_0_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_2 <= \c$bb_res_res_11\;


      end block;

    r_block_61 : block
        signal \c$bb_res_res_12\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_12_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_12\ <= iterateI_ho1_1_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_2 <= \c$bb_res_res_12\;


      end block;

    r_block_62 : block
        signal \c$bb_res_res_13\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= iterateI_ho1_2_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_0 <= \c$bb_res_res_13\;


      end block;

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                           , iterateI_ho1_0_res_2
                                                           , iterateI_ho1_1_res_2
                                                           , iterateI_ho1_2_res_2
                                                           , iterateI_ho1_3_res_0 );


  end block;

  result_15 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  \c$vec2_0\ <= (ws_0(1 to ws_0'high));

  -- zipWith begin
  zipWith_0 : for i_5 in \c$ws_app_arg_1\'range generate
  begin
    \c$ws_app_arg_1\(i_5) <= ( Tuple2_3_sel0_Tuple2_1 => result_32(i_5)
               , Tuple2_3_sel1_Maybe => \c$vec2_0\(i_5) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_1 : block
    function max_0 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_2 : for i_6 in \c$ws_app_arg_2\'range generate
    begin
      fun_13 : block
      signal \c$case_alt_18\              : Spec_topEntity_types.Maybe;
      -- spec.hs:315:1-63
      signal t_13                         : unsigned(7 downto 0);
      -- spec.hs:315:1-63
      signal x_12                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_2\ : boolean;
    begin
      \c$ws_app_arg_2\(i_6) <= \c$case_alt_18\;

      \c$case_alt_selection_res_2\ <= t_13 = result_18.Tags_sel4_output1_0;

      \c$case_alt_18\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_6,max_0(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_2\ else
                         \c$ws_app_arg_1\(i_6).Tuple2_3_sel1_Maybe;

      t_13 <= x_12.Tuple2_1_sel0_unsigned;

      x_12 <= \c$ws_app_arg_1\(i_6).Tuple2_3_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_0 : block
    signal vec_index_1 : integer range 0 to 3-1;
  begin
    vec_index_1 <= to_integer((signed(std_logic_vector(resize(i_0,64)))))
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_scrut_1\ <= result_32(vec_index_1);
  end block;
  -- index end

  \c$case_scrut_2\ <=  ws_0(0) ;

  with (\c$case_scrut_2\(2 downto 2)) select
    result_16 <= false when "0",
                 \c$case_scrut_1\.Tuple2_1_sel1_boolean when others;

  i_0 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_2\(1 downto 0)));

  ws_0 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_2\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  iterateI_3 : block
    signal iterateI_ho1_0_arg0_1 : boolean;
    signal iterateI_ho1_0_res_3  : boolean;
    signal iterateI_ho1_1_res_3  : boolean;
    signal iterateI_ho1_2_res_3  : boolean;
    signal iterateI_ho1_3_res_1  : boolean;
    signal iterateI_ho1_4_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_1 <= x_4;

    r_block_63 : block
        signal \c$bb_res_res_14\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_14_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_14\ <= iterateI_ho1_0_arg0_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_14\;


      end block;

    r_block_64 : block
        signal \c$bb_res_res_15\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_15_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_15\ <= iterateI_ho1_0_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_3 <= \c$bb_res_res_15\;


      end block;

    r_block_65 : block
        signal \c$bb_res_res_16\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_16_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_16\ <= iterateI_ho1_1_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_3 <= \c$bb_res_res_16\;


      end block;

    r_block_66 : block
        signal \c$bb_res_res_17\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_17_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_17\ <= iterateI_ho1_2_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_1 <= \c$bb_res_res_17\;


      end block;

    r_block_67 : block
        signal \c$bb_res_res_18\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_18_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_18\ <= iterateI_ho1_3_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_0 <= \c$bb_res_res_18\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( x_4
                                                           , iterateI_ho1_0_res_3
                                                           , iterateI_ho1_1_res_3
                                                           , iterateI_ho1_2_res_3
                                                           , iterateI_ho1_3_res_1
                                                           , iterateI_ho1_4_res_0 );


  end block;

  result_17 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  iterateI_4 : block
    signal iterateI_ho1_0_arg0_2 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_4  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_4  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_4  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_2  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_2 <= ( Tags_sel0_input0_0 => t_10
                             , Tags_sel1_input1_0 => t_9
                             , Tags_sel2_input2_0 => t_8
                             , Tags_sel3_output0_0 => t_7
                             , Tags_sel4_output1_0 => t_6
                             , Tags_sel5_output2_0 => t_5
                             , Tags_sel6_slide0_0 => t_10
                             , Tags_sel7_slide1_0 => t_6 );

    r_block_68 : block
        signal \c$bb_res_res_19\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(8,8)
      , Tags_sel1_input1_0 => to_unsigned(8,8)
      , Tags_sel2_input2_0 => to_unsigned(8,8)
      , Tags_sel3_output0_0 => to_unsigned(8,8)
      , Tags_sel4_output1_0 => to_unsigned(8,8)
      , Tags_sel5_output2_0 => to_unsigned(8,8)
      , Tags_sel6_slide0_0 => to_unsigned(8,8)
      , Tags_sel7_slide1_0 => to_unsigned(8,8) );
      begin
        -- delay begin
        cbb_res_res_19_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_19\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_19\;


      end block;

    r_block_69 : block
        signal \c$bb_res_res_20\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(8,8)
      , Tags_sel1_input1_0 => to_unsigned(8,8)
      , Tags_sel2_input2_0 => to_unsigned(8,8)
      , Tags_sel3_output0_0 => to_unsigned(8,8)
      , Tags_sel4_output1_0 => to_unsigned(8,8)
      , Tags_sel5_output2_0 => to_unsigned(8,8)
      , Tags_sel6_slide0_0 => to_unsigned(8,8)
      , Tags_sel7_slide1_0 => to_unsigned(8,8) );
      begin
        -- delay begin
        cbb_res_res_20_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_20\ <= iterateI_ho1_0_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_4 <= \c$bb_res_res_20\;


      end block;

    r_block_70 : block
        signal \c$bb_res_res_21\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(8,8)
      , Tags_sel1_input1_0 => to_unsigned(8,8)
      , Tags_sel2_input2_0 => to_unsigned(8,8)
      , Tags_sel3_output0_0 => to_unsigned(8,8)
      , Tags_sel4_output1_0 => to_unsigned(8,8)
      , Tags_sel5_output2_0 => to_unsigned(8,8)
      , Tags_sel6_slide0_0 => to_unsigned(8,8)
      , Tags_sel7_slide1_0 => to_unsigned(8,8) );
      begin
        -- delay begin
        cbb_res_res_21_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_21\ <= iterateI_ho1_1_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_4 <= \c$bb_res_res_21\;


      end block;

    r_block_71 : block
        signal \c$bb_res_res_22\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(8,8)
      , Tags_sel1_input1_0 => to_unsigned(8,8)
      , Tags_sel2_input2_0 => to_unsigned(8,8)
      , Tags_sel3_output0_0 => to_unsigned(8,8)
      , Tags_sel4_output1_0 => to_unsigned(8,8)
      , Tags_sel5_output2_0 => to_unsigned(8,8)
      , Tags_sel6_slide0_0 => to_unsigned(8,8)
      , Tags_sel7_slide1_0 => to_unsigned(8,8) );
      begin
        -- delay begin
        cbb_res_res_22_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_22\ <= iterateI_ho1_2_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_2 <= \c$bb_res_res_22\;


      end block;

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_10
                                                        , Tags_sel1_input1_0 => t_9
                                                        , Tags_sel2_input2_0 => t_8
                                                        , Tags_sel3_output0_0 => t_7
                                                        , Tags_sel4_output1_0 => t_6
                                                        , Tags_sel5_output2_0 => t_5
                                                        , Tags_sel6_slide0_0 => t_10
                                                        , Tags_sel7_slide1_0 => t_6 )
                                                        , iterateI_ho1_0_res_4
                                                        , iterateI_ho1_1_res_4
                                                        , iterateI_ho1_2_res_4
                                                        , iterateI_ho1_3_res_2 );


  end block;

  result_18 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  iterateI_5 : block
    signal iterateI_ho1_0_arg0_3 : boolean;
    signal iterateI_ho1_0_res_5  : boolean;
    signal iterateI_ho1_1_res_5  : boolean;
    signal iterateI_ho1_2_res_5  : boolean;
    signal iterateI_ho1_3_res_3  : boolean;
    signal iterateI_ho1_4_res_1  : boolean;
  begin
    iterateI_ho1_0_arg0_3 <= pOut0;

    r_block_72 : block
        signal \c$bb_res_res_23\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_23_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_23\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_23\;


      end block;

    r_block_73 : block
        signal \c$bb_res_res_24\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_24_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_24\ <= iterateI_ho1_0_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_5 <= \c$bb_res_res_24\;


      end block;

    r_block_74 : block
        signal \c$bb_res_res_25\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_25_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_25\ <= iterateI_ho1_1_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_5 <= \c$bb_res_res_25\;


      end block;

    r_block_75 : block
        signal \c$bb_res_res_26\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_26_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_26\ <= iterateI_ho1_2_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_3 <= \c$bb_res_res_26\;


      end block;

    r_block_76 : block
        signal \c$bb_res_res_27\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_27_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_27\ <= iterateI_ho1_3_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_1 <= \c$bb_res_res_27\;


      end block;

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                           , iterateI_ho1_0_res_5
                                                           , iterateI_ho1_1_res_5
                                                           , iterateI_ho1_2_res_5
                                                           , iterateI_ho1_3_res_3
                                                           , iterateI_ho1_4_res_1 );


  end block;

  result_19 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  \c$vec_2\ <= window_0.Tuple2_4_sel1_array_of_signed_64;

  fold_0 : block
    signal vec_0     : Spec_topEntity_types.array_of_signed_64(0 to 2);
    signal acc_2_0_1 : signed(63 downto 0);
    signal acc_0_3   : signed(63 downto 0);
    signal acc_0_4   : signed(63 downto 0);
    signal acc_1_0_0 : signed(63 downto 0);
    signal acc_0_5   : signed(63 downto 0);
  begin
    \c$t_app_arg_0\ <= acc_2_0_1;

    vec_0 <= (\c$vec_2\(1 to \c$vec_2\'high));

    acc_0_3 <= vec_0(0);

    acc_0_4 <= vec_0(1);

    acc_0_5 <= vec_0(2);

    fun_14 : block
        signal x_13            : signed(63 downto 0);
        signal y_4             : signed(63 downto 0);
        signal \c$case_alt_19\ : signed(63 downto 0);
      begin
        acc_1_0_0 <= \c$case_alt_19\;

        x_13 <= acc_0_3;

        y_4 <= acc_0_4;

        \c$case_alt_19\ <= (x_13 + y_4);


      end block;

    fun_15 : block
        signal x_14            : signed(63 downto 0);
        signal y_5             : signed(63 downto 0);
        signal \c$case_alt_20\ : signed(63 downto 0);
      begin
        acc_2_0_1 <= \c$case_alt_20\;

        x_14 <= acc_1_0_0;

        y_5 <= acc_0_5;

        \c$case_alt_20\ <= (x_14 + y_5);


      end block;


  end block;

  result_20 <= t_1 when b_1 else
               result_21;

  -- register begin
  result_21_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_21 <= (Tuple2_1_sel0_unsigned => to_unsigned(8,8), Tuple2_1_sel1_boolean => false);
    elsif rising_edge(clk) then
      if en then
        result_21 <= result_20;
      end if;
    end if;
  end process;
  -- register end

  b_1 <= result_30;

  t_1 <= ( Tuple2_1_sel0_unsigned => result_37.Tags_sel3_output0_0
         , Tuple2_1_sel1_boolean => \c$t_app_arg_0\ < (to_signed(10,64)) );

  x_projection_10 <=  win_0(0) ;

  x_2 <= x_projection_10;

  win_0 <= window_0.Tuple2_4_sel1_array_of_signed_64;

  -- register begin
  window_0_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window_0 <= ( Tuple2_4_sel0_unsigned => to_unsigned(8,8)
  , Tuple2_4_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window_0 <= result_22;
      end if;
    end if;
  end process;
  -- register end

  result_22 <= t_2 when b_2 else
               window_0;

  t_2 <= ( Tuple2_4_sel0_unsigned => result_38.Tags_sel6_slide0_0
         , Tuple2_4_sel1_array_of_signed_64 => result_23 );

  result_23 <= \c$t_case_alt_1\ when result_28 else
               \c$t_case_alt_2\;

  \c$t_case_alt_selection_7\ <= result_29;

  \c$vec_3\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win_0)));

  \c$t_case_alt_sel_alt_6\ <= (\c$vec_3\(0 to 4-1),\c$vec_3\(4 to \c$vec_3\'high));

  \c$vec_4\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated_0)));

  \c$t_case_alt_sel_alt_8\ <= (\c$vec_4\(0 to 4-1),\c$vec_4\(4 to \c$vec_4\'high));

  \c$t_case_alt_1\ <= \c$t_case_alt_sel_alt_8\.Tuple2_8_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_7\ else
                      \c$t_case_alt_sel_alt_6\.Tuple2_8_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_10\ <= result_29;

  \c$t_case_alt_2\ <= lastBucketUpdated_0 when \c$t_case_alt_selection_10\ else
                      win_0;

  -- replace begin
  replaceVec_0 : block
    signal vec_index_2 : integer range 0 to 4-1;
  begin
    vec_index_2 <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;

    process(vec_index_2,win_0,x_2)
      variable ivec_0 : Spec_topEntity_types.array_of_signed_64(0 to 3);
    begin
      ivec_0 := win_0;
      ivec_0(vec_index_2) := ((x_2 + to_signed(1,64)));
      lastBucketUpdated_0 <= ivec_0;
    end process;
  end block;
  -- replace end

  b_2 <= result_29 or result_28;

  \c$vec2_1\ <= (ws_1(1 to ws_1'high));

  -- zipWith begin
  zipWith_1 : for i_7 in \c$ws_app_arg_3\'range generate
  begin
    \c$ws_app_arg_3\(i_7) <= ( Tuple2_5_sel0_Tuple2_6 => result_26(i_7)
               , Tuple2_5_sel1_Maybe_0 => \c$vec2_1\(i_7) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_3 : block
    function max_1 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_4 : for i_8 in \c$ws_app_arg_4\'range generate
    begin
      fun_16 : block
      signal \c$case_alt_21\              : Spec_topEntity_types.Maybe_0;
      -- spec.hs:315:1-63
      signal t_14                         : unsigned(7 downto 0);
      -- spec.hs:315:1-63
      signal x_15                         : Spec_topEntity_types.Tuple2_6;
      signal \c$case_alt_selection_res_3\ : boolean;
    begin
      \c$ws_app_arg_4\(i_8) <= \c$case_alt_21\;

      \c$case_alt_selection_res_3\ <= t_14 = result_38.Tags_sel0_input0_0;

      \c$case_alt_21\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_8,max_1(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_3\ else
                         \c$ws_app_arg_3\(i_8).Tuple2_5_sel1_Maybe_0;

      t_14 <= x_15.Tuple2_6_sel0_unsigned;

      x_15 <= \c$ws_app_arg_3\(i_8).Tuple2_5_sel0_Tuple2_6;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_1 : block
    signal vec_index_3 : integer range 0 to 2-1;
  begin
    vec_index_3 <= to_integer((signed(std_logic_vector(resize(i_1,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_3\ <= result_26(vec_index_3);
  end block;
  -- index end

  \c$case_scrut_4\ <=  ws_1(0) ;

  with (\c$case_scrut_4\(1 downto 1)) select
    result_24 <= to_signed(0,64) when "0",
                 \c$case_scrut_3\.Tuple2_6_sel1_signed when others;

  i_1 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_4\(0 downto 0)));

  ws_1 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_4\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "-"))));

  result_25 <= t_3 when b_3 else
               result_26;

  -- register begin
  result_26_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_26 <= Spec_topEntity_types.array_of_Tuple2_6'( ( Tuple2_6_sel0_unsigned => to_unsigned(8,8)
                                         , Tuple2_6_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_6_sel0_unsigned => to_unsigned(8,8)
                                         , Tuple2_6_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_26 <= result_25;
      end if;
    end if;
  end process;
  -- register end

  b_3 <= result_27;

  \c$vec_5\ <= (Spec_topEntity_types.array_of_Tuple2_6'(Spec_topEntity_types.array_of_Tuple2_6'(result_26) & Spec_topEntity_types.array_of_Tuple2_6'(Spec_topEntity_types.array_of_Tuple2_6'(0 => ( Tuple2_6_sel0_unsigned => t_10
             , Tuple2_6_sel1_signed => \c$ds1_app_arg\ )))));

  t_projection_2 <= (\c$vec_5\(0 to 1-1),\c$vec_5\(1 to \c$vec_5\'high));

  t_3 <= t_projection_2.Tuple2_9_sel1_array_of_Tuple2_6_1;

  -- delay begin
  cds1_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$ds1_app_arg\ <= inputs_3.Inputs_4_sel0_input0.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_6 : block
    signal iterateI_ho1_0_res_6 : boolean;
  begin
    r_block_77 : block
        signal \c$bb_res_res_28\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_28_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_28\ <= pIn0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_28\;


      end block;

    \c$app_arg_6\ <= Spec_topEntity_types.array_of_boolean'( pIn0
                                                           , iterateI_ho1_0_res_6 );


  end block;

  result_27 <=  \c$app_arg_6\(\c$app_arg_6\'high) ;

  iterateI_7 : block
    signal iterateI_ho1_0_arg0_4 : boolean;
    signal iterateI_ho1_0_res_7  : boolean;
    signal iterateI_ho1_1_res_6  : boolean;
    signal iterateI_ho1_2_res_6  : boolean;
  begin
    iterateI_ho1_0_arg0_4 <= slides.Slides_1_sel0_slide0;

    r_block_78 : block
        signal \c$bb_res_res_29\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_29_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_29\ <= iterateI_ho1_0_arg0_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_29\;


      end block;

    r_block_79 : block
        signal \c$bb_res_res_30\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_30_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_30\ <= iterateI_ho1_0_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_6 <= \c$bb_res_res_30\;


      end block;

    r_block_80 : block
        signal \c$bb_res_res_31\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_31_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_31\ <= iterateI_ho1_1_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_6 <= \c$bb_res_res_31\;


      end block;

    \c$app_arg_7\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel0_slide0
                                                           , iterateI_ho1_0_res_7
                                                           , iterateI_ho1_1_res_6
                                                           , iterateI_ho1_2_res_6 );


  end block;

  result_28 <=  \c$app_arg_7\(\c$app_arg_7\'high) ;

  slides <= result_52.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_Slides_1;

  iterateI_8 : block
    signal iterateI_ho1_0_res_8 : boolean;
    signal iterateI_ho1_1_res_7 : boolean;
    signal iterateI_ho1_2_res_7 : boolean;
  begin
    r_block_81 : block
        signal \c$bb_res_res_32\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_32_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_32\ <= pIn0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_8 <= \c$bb_res_res_32\;


      end block;

    r_block_82 : block
        signal \c$bb_res_res_33\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_33_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_33\ <= iterateI_ho1_0_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_7 <= \c$bb_res_res_33\;


      end block;

    r_block_83 : block
        signal \c$bb_res_res_34\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_34_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_34\ <= iterateI_ho1_1_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_7 <= \c$bb_res_res_34\;


      end block;

    \c$app_arg_8\ <= Spec_topEntity_types.array_of_boolean'( pIn0
                                                           , iterateI_ho1_0_res_8
                                                           , iterateI_ho1_1_res_7
                                                           , iterateI_ho1_2_res_7 );


  end block;

  result_29 <=  \c$app_arg_8\(\c$app_arg_8\'high) ;

  iterateI_9 : block
    signal iterateI_ho1_0_res_9 : boolean;
    signal iterateI_ho1_1_res_8 : boolean;
    signal iterateI_ho1_2_res_8 : boolean;
    signal iterateI_ho1_3_res_4 : boolean;
  begin
    r_block_84 : block
        signal \c$bb_res_res_35\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_35_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_35\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_9 <= \c$bb_res_res_35\;


      end block;

    r_block_85 : block
        signal \c$bb_res_res_36\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_36_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_36\ <= iterateI_ho1_0_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_8 <= \c$bb_res_res_36\;


      end block;

    r_block_86 : block
        signal \c$bb_res_res_37\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_37_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_37\ <= iterateI_ho1_1_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_8 <= \c$bb_res_res_37\;


      end block;

    r_block_87 : block
        signal \c$bb_res_res_38\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_38_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_38\ <= iterateI_ho1_2_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_4 <= \c$bb_res_res_38\;


      end block;

    \c$app_arg_9\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                           , iterateI_ho1_0_res_9
                                                           , iterateI_ho1_1_res_8
                                                           , iterateI_ho1_2_res_8
                                                           , iterateI_ho1_3_res_4 );


  end block;

  result_30 <=  \c$app_arg_9\(\c$app_arg_9\'high) ;

  result_selection_12 <= x_3;

  result_31 <= t_4 when result_selection_12 else
               result_32;

  x_3 <= result_36;

  -- register begin
  result_32_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_32 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(8,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(8,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(8,8)
                                         , Tuple2_1_sel1_boolean => false ) );
    elsif rising_edge(clk) then
      if en then
        result_32 <= result_31;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_6\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_32) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => result_39.Tags_sel4_output1_0
             , Tuple2_1_sel1_boolean => \c$out1_case_alt\ < (to_signed(9,64)) )))));

  t_projection_3 <= (\c$vec_6\(0 to 1-1),\c$vec_6\(1 to \c$vec_6\'high));

  t_4 <= t_projection_3.Tuple2_10_sel1_array_of_Tuple2_1_1;

  \c$out1_case_alt_selection_res\ <= tag = tagToMatch;

  \c$out1_case_alt\ <= dta when \c$out1_case_alt_selection_res\ else
                       to_signed(0,64);

  dta <= result_34.Tuple2_6_sel1_signed;

  tagToMatch <= result_39.Tags_sel1_input1_0;

  tag <= result_34.Tuple2_6_sel0_unsigned;

  result_33 <= ( Tuple2_6_sel0_unsigned => t_9
               , Tuple2_6_sel1_signed => \c$ds_app_arg\ ) when b_4 else
               result_34;

  -- register begin
  result_34_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_34 <= ( Tuple2_6_sel0_unsigned => to_unsigned(8,8)
  , Tuple2_6_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_34 <= result_33;
      end if;
    end if;
  end process;
  -- register end

  b_4 <= result_35;

  -- delay begin
  cds_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$ds_app_arg\ <= inputs_3.Inputs_4_sel1_input1.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  inputs_3 <= result_52.Tuple3_sel2_Tuple3_0.Tuple3_0_sel0_Inputs_4;

  iterateI_10 : block
    signal iterateI_ho1_0_res_10 : boolean;
  begin
    r_block_88 : block
        signal \c$bb_res_res_39\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_39_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_39\ <= pIn1_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_10 <= \c$bb_res_res_39\;


      end block;

    \c$app_arg_10\ <= Spec_topEntity_types.array_of_boolean'( pIn1_0
                                                            , iterateI_ho1_0_res_10 );


  end block;

  result_35 <=  \c$app_arg_10\(\c$app_arg_10\'high) ;

  iterateI_11 : block
    signal iterateI_ho1_0_res_11 : boolean;
    signal iterateI_ho1_1_res_9  : boolean;
  begin
    r_block_89 : block
        signal \c$bb_res_res_40\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_40_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_40\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_11 <= \c$bb_res_res_40\;


      end block;

    r_block_90 : block
        signal \c$bb_res_res_41\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_41_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_41\ <= iterateI_ho1_0_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_9 <= \c$bb_res_res_41\;


      end block;

    \c$app_arg_11\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                            , iterateI_ho1_0_res_11
                                                            , iterateI_ho1_1_res_9 );


  end block;

  result_36 <=  \c$app_arg_11\(\c$app_arg_11\'high) ;

  iterateI_12 : block
    signal iterateI_ho1_0_arg0_5 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_12 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_10 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_9  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_5 <= ( Tags_sel0_input0_0 => t_10
                             , Tags_sel1_input1_0 => t_9
                             , Tags_sel2_input2_0 => t_8
                             , Tags_sel3_output0_0 => t_7
                             , Tags_sel4_output1_0 => t_6
                             , Tags_sel5_output2_0 => t_5
                             , Tags_sel6_slide0_0 => t_10
                             , Tags_sel7_slide1_0 => t_6 );

    r_block_91 : block
        signal \c$bb_res_res_42\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(8,8)
      , Tags_sel1_input1_0 => to_unsigned(8,8)
      , Tags_sel2_input2_0 => to_unsigned(8,8)
      , Tags_sel3_output0_0 => to_unsigned(8,8)
      , Tags_sel4_output1_0 => to_unsigned(8,8)
      , Tags_sel5_output2_0 => to_unsigned(8,8)
      , Tags_sel6_slide0_0 => to_unsigned(8,8)
      , Tags_sel7_slide1_0 => to_unsigned(8,8) );
      begin
        -- delay begin
        cbb_res_res_42_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_42\ <= iterateI_ho1_0_arg0_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_12 <= \c$bb_res_res_42\;


      end block;

    r_block_92 : block
        signal \c$bb_res_res_43\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(8,8)
      , Tags_sel1_input1_0 => to_unsigned(8,8)
      , Tags_sel2_input2_0 => to_unsigned(8,8)
      , Tags_sel3_output0_0 => to_unsigned(8,8)
      , Tags_sel4_output1_0 => to_unsigned(8,8)
      , Tags_sel5_output2_0 => to_unsigned(8,8)
      , Tags_sel6_slide0_0 => to_unsigned(8,8)
      , Tags_sel7_slide1_0 => to_unsigned(8,8) );
      begin
        -- delay begin
        cbb_res_res_43_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_43\ <= iterateI_ho1_0_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_10 <= \c$bb_res_res_43\;


      end block;

    r_block_93 : block
        signal \c$bb_res_res_44\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(8,8)
      , Tags_sel1_input1_0 => to_unsigned(8,8)
      , Tags_sel2_input2_0 => to_unsigned(8,8)
      , Tags_sel3_output0_0 => to_unsigned(8,8)
      , Tags_sel4_output1_0 => to_unsigned(8,8)
      , Tags_sel5_output2_0 => to_unsigned(8,8)
      , Tags_sel6_slide0_0 => to_unsigned(8,8)
      , Tags_sel7_slide1_0 => to_unsigned(8,8) );
      begin
        -- delay begin
        cbb_res_res_44_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_44\ <= iterateI_ho1_1_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_9 <= \c$bb_res_res_44\;


      end block;

    \c$app_arg_12\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_10
                                                         , Tags_sel1_input1_0 => t_9
                                                         , Tags_sel2_input2_0 => t_8
                                                         , Tags_sel3_output0_0 => t_7
                                                         , Tags_sel4_output1_0 => t_6
                                                         , Tags_sel5_output2_0 => t_5
                                                         , Tags_sel6_slide0_0 => t_10
                                                         , Tags_sel7_slide1_0 => t_6 )
                                                         , iterateI_ho1_0_res_12
                                                         , iterateI_ho1_1_res_10
                                                         , iterateI_ho1_2_res_9 );


  end block;

  result_37 <=  \c$app_arg_12\(\c$app_arg_12\'high) ;

  iterateI_13 : block
    signal iterateI_ho1_0_arg0_6 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_13 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_11 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_6 <= ( Tags_sel0_input0_0 => t_10
                             , Tags_sel1_input1_0 => t_9
                             , Tags_sel2_input2_0 => t_8
                             , Tags_sel3_output0_0 => t_7
                             , Tags_sel4_output1_0 => t_6
                             , Tags_sel5_output2_0 => t_5
                             , Tags_sel6_slide0_0 => t_10
                             , Tags_sel7_slide1_0 => t_6 );

    r_block_94 : block
        signal \c$bb_res_res_45\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(8,8)
      , Tags_sel1_input1_0 => to_unsigned(8,8)
      , Tags_sel2_input2_0 => to_unsigned(8,8)
      , Tags_sel3_output0_0 => to_unsigned(8,8)
      , Tags_sel4_output1_0 => to_unsigned(8,8)
      , Tags_sel5_output2_0 => to_unsigned(8,8)
      , Tags_sel6_slide0_0 => to_unsigned(8,8)
      , Tags_sel7_slide1_0 => to_unsigned(8,8) );
      begin
        -- delay begin
        cbb_res_res_45_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_45\ <= iterateI_ho1_0_arg0_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_13 <= \c$bb_res_res_45\;


      end block;

    r_block_95 : block
        signal \c$bb_res_res_46\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(8,8)
      , Tags_sel1_input1_0 => to_unsigned(8,8)
      , Tags_sel2_input2_0 => to_unsigned(8,8)
      , Tags_sel3_output0_0 => to_unsigned(8,8)
      , Tags_sel4_output1_0 => to_unsigned(8,8)
      , Tags_sel5_output2_0 => to_unsigned(8,8)
      , Tags_sel6_slide0_0 => to_unsigned(8,8)
      , Tags_sel7_slide1_0 => to_unsigned(8,8) );
      begin
        -- delay begin
        cbb_res_res_46_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_46\ <= iterateI_ho1_0_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_11 <= \c$bb_res_res_46\;


      end block;

    \c$app_arg_13\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_10
                                                         , Tags_sel1_input1_0 => t_9
                                                         , Tags_sel2_input2_0 => t_8
                                                         , Tags_sel3_output0_0 => t_7
                                                         , Tags_sel4_output1_0 => t_6
                                                         , Tags_sel5_output2_0 => t_5
                                                         , Tags_sel6_slide0_0 => t_10
                                                         , Tags_sel7_slide1_0 => t_6 )
                                                         , iterateI_ho1_0_res_13
                                                         , iterateI_ho1_1_res_11 );


  end block;

  result_38 <=  \c$app_arg_13\(\c$app_arg_13\'high) ;

  iterateI_14 : block
    signal iterateI_ho1_0_arg0_7 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_14 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_7 <= ( Tags_sel0_input0_0 => t_10
                             , Tags_sel1_input1_0 => t_9
                             , Tags_sel2_input2_0 => t_8
                             , Tags_sel3_output0_0 => t_7
                             , Tags_sel4_output1_0 => t_6
                             , Tags_sel5_output2_0 => t_5
                             , Tags_sel6_slide0_0 => t_10
                             , Tags_sel7_slide1_0 => t_6 );

    r_block_96 : block
        signal \c$bb_res_res_47\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(8,8)
      , Tags_sel1_input1_0 => to_unsigned(8,8)
      , Tags_sel2_input2_0 => to_unsigned(8,8)
      , Tags_sel3_output0_0 => to_unsigned(8,8)
      , Tags_sel4_output1_0 => to_unsigned(8,8)
      , Tags_sel5_output2_0 => to_unsigned(8,8)
      , Tags_sel6_slide0_0 => to_unsigned(8,8)
      , Tags_sel7_slide1_0 => to_unsigned(8,8) );
      begin
        -- delay begin
        cbb_res_res_47_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_47\ <= iterateI_ho1_0_arg0_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_14 <= \c$bb_res_res_47\;


      end block;

    \c$app_arg_14\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_10
                                                         , Tags_sel1_input1_0 => t_9
                                                         , Tags_sel2_input2_0 => t_8
                                                         , Tags_sel3_output0_0 => t_7
                                                         , Tags_sel4_output1_0 => t_6
                                                         , Tags_sel5_output2_0 => t_5
                                                         , Tags_sel6_slide0_0 => t_10
                                                         , Tags_sel7_slide1_0 => t_6 )
                                                         , iterateI_ho1_0_res_14 );


  end block;

  result_39 <=  \c$app_arg_14\(\c$app_arg_14\'high) ;

  -- register begin
  t_5_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_5 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_5 <= result_40;
      end if;
    end if;
  end process;
  -- register end

  result_selection_16 <= pOut2;

  result_40 <= result_41 when result_selection_16 else
               t_5;

  b_5 <= t_5 = to_unsigned(7,8);

  f1 <= t_5 + to_unsigned(1,8);

  result_41 <= to_unsigned(1,8) when b_5 else
               f1;

  pOut2 <= pacings.Pacings_1_sel5_pacingOut2;

  -- register begin
  t_6_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_6 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_6 <= result_42;
      end if;
    end if;
  end process;
  -- register end

  result_selection_20 <= x_4;

  result_42 <= result_43 when result_selection_20 else
               t_6;

  b_6 <= t_6 = to_unsigned(7,8);

  f1_0 <= t_6 + to_unsigned(1,8);

  result_43 <= to_unsigned(1,8) when b_6 else
               f1_0;

  x_4 <= pOut1;

  pOut1 <= pacings.Pacings_1_sel4_pacingOut1;

  -- register begin
  t_7_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_7 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_7 <= result_44;
      end if;
    end if;
  end process;
  -- register end

  result_selection_24 <= pOut0;

  result_44 <= result_45 when result_selection_24 else
               t_7;

  b_7 <= t_7 = to_unsigned(7,8);

  f1_1 <= t_7 + to_unsigned(1,8);

  result_45 <= to_unsigned(1,8) when b_7 else
               f1_1;

  pOut0 <= pacings.Pacings_1_sel3_pacingOut0;

  -- register begin
  t_8_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_8 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_8 <= result_46;
      end if;
    end if;
  end process;
  -- register end

  result_selection_29 <= pacings.Pacings_1_sel2_pacingIn2;

  result_46 <= result_47 when result_selection_29 else
               t_8;

  b_8 <= t_8 = to_unsigned(7,8);

  f1_2 <= t_8 + to_unsigned(1,8);

  result_47 <= to_unsigned(1,8) when b_8 else
               f1_2;

  -- register begin
  t_9_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_9 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_9 <= result_48;
      end if;
    end if;
  end process;
  -- register end

  result_selection_33 <= pIn1_0;

  result_48 <= result_49 when result_selection_33 else
               t_9;

  b_9 <= t_9 = to_unsigned(7,8);

  f1_3 <= t_9 + to_unsigned(1,8);

  result_49 <= to_unsigned(1,8) when b_9 else
               f1_3;

  pIn1_0 <= pacings.Pacings_1_sel1_pacingIn1;

  -- register begin
  t_10_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_10 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_10 <= result_50;
      end if;
    end if;
  end process;
  -- register end

  result_selection_37 <= pIn0;

  result_50 <= result_51 when result_selection_37 else
               t_10;

  b_10 <= t_10 = to_unsigned(7,8);

  f1_4 <= t_10 + to_unsigned(1,8);

  result_51 <= to_unsigned(1,8) when b_10 else
               f1_4;

  pIn0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_52.Tuple3_sel2_Tuple3_0.Tuple3_0_sel2_Pacings_1;

  result_52 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_17\
               , Tuple3_sel1_boolean_1 => \c$app_arg_16\
               , Tuple3_sel2_Tuple3_0 => \c$app_arg_15\ );

  -- register begin
  capp_arg_15_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_15\ <= ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false ) )
  , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
  , Slides_1_sel1_slide1 => false )
  , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingIn1 => false
  , Pacings_1_sel2_pacingIn2 => false
  , Pacings_1_sel3_pacingOut0 => false
  , Pacings_1_sel4_pacingOut1 => false
  , Pacings_1_sel5_pacingOut2 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_15\ <= \c$case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt\ <= \c$case_alt_0\ when push else
                  \c$case_alt_1\;

  \c$case_alt_0\ <= \c$case_alt_2\ when pop else
                    ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false ) )
                    , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                    , Slides_1_sel1_slide1 => false )
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingIn1 => false
                    , Pacings_1_sel2_pacingIn2 => false
                    , Pacings_1_sel3_pacingOut0 => false
                    , Pacings_1_sel4_pacingOut1 => false
                    , Pacings_1_sel5_pacingOut2 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false ) )
                    , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                    , Slides_1_sel1_slide1 => false )
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingIn1 => false
                    , Pacings_1_sel2_pacingIn2 => false
                    , Pacings_1_sel3_pacingOut0 => false
                    , Pacings_1_sel4_pacingOut1 => false
                    , Pacings_1_sel5_pacingOut2 => false ) );

  with (x_5) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_5) select
    \c$case_alt_3\ <= ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false ) )
                      , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                      , Slides_1_sel1_slide1 => false )
                      , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingIn1 => false
                      , Pacings_1_sel2_pacingIn2 => false
                      , Pacings_1_sel3_pacingOut0 => false
                      , Pacings_1_sel4_pacingOut1 => false
                      , Pacings_1_sel5_pacingOut2 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec_2 : block
    signal vec_index_4 : integer range 0 to 2-1;
  begin
    vec_index_4 <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index_4);
  end block;
  -- index end

  -- register begin
  capp_arg_16_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_16\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_16\ <= \c$case_alt_5\;
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

  with (x_5) select
    \c$case_alt_9\ <= false when x"0000000000000000",
                      true when others;

  -- register begin
  capp_arg_17_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_17\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_17\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_18\);

  \c$app_arg_18\ <= to_signed(1,64) when x_5 /= to_signed(2,64) else to_signed(0,64);

  -- register begin
  buffer_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \buffer\ <= Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingIn2 => false
                                         , Pacings_1_sel3_pacingOut0 => false
                                         , Pacings_1_sel4_pacingOut1 => false
                                         , Pacings_1_sel5_pacingOut2 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingIn2 => false
                                         , Pacings_1_sel3_pacingOut0 => false
                                         , Pacings_1_sel4_pacingOut1 => false
                                         , Pacings_1_sel5_pacingOut2 => false ) ) );
    elsif rising_edge(clk) then
      if en then
        \buffer\ <= \c$buffer_case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$buffer_case_alt\ <= \c$buffer_case_alt_0\ when push else
                         \buffer\;

  \c$vec_7\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec_7\(0 to 2-1),\c$vec_7\(2 to \c$vec_7\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_11_sel0_array_of_Tuple3_0_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_8\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_8\(0 to 2-1),\c$vec_8\(2 to \c$vec_8\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_11_sel0_array_of_Tuple3_0_0;

  qData <= result_5.Tuple2_sel1_Tuple3_0;

  x_5 <= cursor;

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

  push <= result_5.Tuple2_sel0_boolean;

  \c$cursor_case_alt_0\ <= cursor when pop else
                           \c$cursor_case_alt_1\;

  \c$cursor_case_alt_1\ <= cursor when \c$cursor_case_scrut\ else
                           (x_5 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_5 = to_signed(2,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_6.Tuple2_0_sel0_boolean;

  with (x_5) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_5 - to_signed(1,64));

  result <= result_6.Tuple2_0_sel1_Outputs;

  result_0 <= result.Outputs_sel0_output0;

  result_1 <= result.Outputs_sel1_output1;

  result_2 <= result.Outputs_sel2_output2;

  result_0_0 <= result_0.ValidBool_sel0_value_0;

  result_0_1 <= result_0.ValidBool_sel1_valid_0;

  result_1_0 <= result_1.ValidBool_sel0_value_0;

  result_1_1 <= result_1.ValidBool_sel1_valid_0;

  result_2_0 <= result_2.ValidBool_sel0_value_0;

  result_2_1 <= result_2.ValidBool_sel1_valid_0;


end;

