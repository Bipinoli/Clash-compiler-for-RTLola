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
       inputs_0   : in signed(63 downto 0);
       inputs_1   : in boolean;
       result_0_0 : out signed(63 downto 0);
       result_0_1 : out boolean;
       result_1_0 : out signed(63 downto 0);
       result_1_1 : out boolean;
       result_2_0 : out signed(63 downto 0);
       result_2_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_3                        : signed(63 downto 0);
  signal x                               : signed(63 downto 0);
  signal result_4                        : signed(63 downto 0) := (to_signed(0,64));
  signal result_5                        : Spec_topEntity_types.Tuple2;
  -- spec.hs:201:1-82
  signal hasInput0                       : boolean;
  -- spec.hs:201:1-82
  signal pIn0                            : boolean;
  -- spec.hs:201:1-82
  signal timer0Over                      : boolean;
  signal result_6                        : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                     : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_7                        : boolean;
  -- spec.hs:394:1-139
  signal \c$t_app_arg\                   : signed(63 downto 0);
  signal result_8                        : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:394:1-139
  signal result_9                        : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:394:1-139
  signal b                               : boolean;
  -- spec.hs:394:1-139
  signal t                               : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:409:1-159
  signal y                               : signed(63 downto 0);
  -- spec.hs:409:1-159
  signal x_0                             : signed(63 downto 0);
  -- spec.hs:409:1-159
  signal win                             : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:409:1-159
  signal window                          : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(103,8)
, Tuple2_2_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_10                       : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:409:1-159
  signal t_0                             : Spec_topEntity_types.Tuple2_2;
  signal result_11                       : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:409:1-159
  signal \c$t_case_alt\                  : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:409:1-159
  signal \c$t_case_alt_0\                : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:409:1-159
  signal lastBucketUpdated               : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:409:1-159
  signal b_0                             : boolean;
  -- spec.hs:77:34-42
  signal x_1                             : boolean;
  -- spec.hs:247:1-63
  signal \c$ws_app_arg\                  : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:247:1-63
  signal \c$ws_app_arg_0\                : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut\                  : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_0\                : Spec_topEntity_types.Maybe;
  signal result_12                       : signed(63 downto 0);
  -- spec.hs:247:1-63
  signal i                               : Spec_topEntity_types.index_3;
  -- spec.hs:247:1-63
  signal ws                              : Spec_topEntity_types.array_of_Maybe(0 to 3);
  signal \c$app_arg_0\                   : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_13                       : boolean;
  signal \c$app_arg_1\                   : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_14                       : boolean;
  signal \c$app_arg_2\                   : Spec_topEntity_types.array_of_Tags(0 to 3);
  signal result_15                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_3\                   : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_16                       : boolean;
  -- spec.hs:247:1-63
  signal \c$ws_app_arg_1\                : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:247:1-63
  signal \c$ws_app_arg_2\                : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut_1\                : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_2\                : Spec_topEntity_types.Maybe;
  signal result_17                       : signed(63 downto 0);
  -- spec.hs:247:1-63
  signal i_0                             : Spec_topEntity_types.index_3;
  -- spec.hs:247:1-63
  signal ws_0                            : Spec_topEntity_types.array_of_Maybe(0 to 3);
  signal \c$app_arg_4\                   : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_18                       : boolean;
  -- spec.hs:247:1-63
  signal \c$ws_app_arg_3\                : Spec_topEntity_types.array_of_Tuple2_4(0 to 3);
  -- spec.hs:247:1-63
  signal \c$ws_app_arg_4\                : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal \c$case_scrut_3\                : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_4\                : Spec_topEntity_types.Maybe_0;
  signal result_19                       : signed(63 downto 0);
  -- spec.hs:247:1-63
  signal i_1                             : Spec_topEntity_types.index_4;
  -- spec.hs:247:1-63
  signal ws_1                            : Spec_topEntity_types.array_of_Maybe_0(0 to 4);
  signal \c$app_arg_5\                   : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_20                       : boolean;
  signal result_21                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  -- spec.hs:77:34-42
  signal x_2                             : boolean;
  -- spec.hs:385:1-155
  signal result_22                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 2) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:385:1-155
  signal t_1                             : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  signal x_3                             : signed(63 downto 0);
  signal y_0                             : signed(63 downto 0);
  -- spec.hs:247:1-63
  signal \c$ws_app_arg_5\                : Spec_topEntity_types.array_of_Tuple2_4(0 to 3);
  -- spec.hs:247:1-63
  signal \c$ws_app_arg_6\                : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal \c$case_scrut_5\                : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_6\                : Spec_topEntity_types.Maybe_0;
  signal result_23                       : signed(63 downto 0);
  -- spec.hs:247:1-63
  signal i_2                             : Spec_topEntity_types.index_4;
  -- spec.hs:247:1-63
  signal ws_2                            : Spec_topEntity_types.array_of_Maybe_0(0 to 4);
  -- spec.hs:281:1-91
  signal \c$out1_case_alt\               : signed(63 downto 0);
  -- spec.hs:260:1-53
  signal tagToMatch                      : unsigned(7 downto 0);
  signal \c$app_arg_6\                   : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_24                       : boolean;
  signal result_25                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  -- spec.hs:76:34-42
  signal x_4                             : boolean;
  -- spec.hs:376:1-137
  signal result_26                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 3) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:376:1-137
  signal t_2                             : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  signal x_5                             : signed(63 downto 0);
  -- spec.hs:281:1-91
  signal \c$out0_case_alt\               : signed(63 downto 0);
  -- spec.hs:254:1-55
  signal winData                         : signed(63 downto 0);
  -- spec.hs:254:1-55
  signal winTag                          : unsigned(7 downto 0);
  signal result_27                       : unsigned(7 downto 0);
  signal \c$app_arg_7\                   : unsigned(7 downto 0);
  signal \c$app_arg_8\                   : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_28                       : boolean;
  signal result_29                       : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:370:1-127
  signal result_30                       : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:370:1-127
  signal b_1                             : boolean;
  -- spec.hs:281:1-91
  signal \c$input0Win_app_arg\           : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_9\                   : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_31                       : boolean;
  signal \c$app_arg_10\                  : Spec_topEntity_types.array_of_Tags(0 to 4);
  signal result_32                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_11\                  : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_33                       : Spec_topEntity_types.Tags;
  signal \c$app_arg_12\                  : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_34                       : Spec_topEntity_types.Tags;
  -- spec.hs:360:9-81
  signal t_3                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_35                       : unsigned(7 downto 0);
  -- spec.hs:360:9-81
  signal b_2                             : boolean;
  -- spec.hs:360:9-81
  signal f1                              : unsigned(7 downto 0);
  signal result_36                       : unsigned(7 downto 0);
  -- spec.hs:281:1-91
  signal pOut2                           : boolean;
  -- spec.hs:360:9-81
  signal t_4                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_37                       : unsigned(7 downto 0);
  -- spec.hs:360:9-81
  signal b_3                             : boolean;
  -- spec.hs:360:9-81
  signal f1_0                            : unsigned(7 downto 0);
  signal result_38                       : unsigned(7 downto 0);
  -- spec.hs:77:34-42
  signal x_6                             : boolean;
  -- spec.hs:281:1-91
  signal pOut1                           : boolean;
  -- spec.hs:360:9-81
  signal t_5                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_39                       : unsigned(7 downto 0);
  -- spec.hs:360:9-81
  signal b_4                             : boolean;
  -- spec.hs:360:9-81
  signal f1_1                            : unsigned(7 downto 0);
  signal result_40                       : unsigned(7 downto 0);
  -- spec.hs:76:34-42
  signal x_7                             : boolean;
  -- spec.hs:281:1-91
  signal pOut0                           : boolean;
  -- spec.hs:360:9-81
  signal t_6                             : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_41                       : unsigned(7 downto 0);
  -- spec.hs:360:9-81
  signal b_5                             : boolean;
  -- spec.hs:360:9-81
  signal f1_2                            : unsigned(7 downto 0);
  signal result_42                       : unsigned(7 downto 0);
  -- spec.hs:281:1-91
  signal pIn0_0                          : boolean;
  -- spec.hs:281:1-91
  signal pacings                         : Spec_topEntity_types.Pacings_1;
  signal result_43                       : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_13\                  : Spec_topEntity_types.Tuple3_0 := ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Tuple3_0_sel1_boolean => false
, Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingOut0 => false
, Pacings_1_sel2_pacingOut1 => false
, Pacings_1_sel3_pacingOut2 => false ) );
  signal \c$case_alt\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_0\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_1\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_2\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_3\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_4\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$app_arg_14\                  : boolean := false;
  signal \c$case_alt_5\                  : boolean;
  signal \c$case_alt_6\                  : boolean;
  signal \c$case_alt_7\                  : boolean;
  signal \c$case_alt_8\                  : boolean;
  signal \c$case_alt_9\                  : boolean;
  signal \c$app_arg_15\                  : boolean := false;
  signal \c$case_alt_10\                 : boolean;
  signal \c$case_alt_11\                 : boolean;
  signal \c$case_alt_12\                 : boolean;
  signal \c$app_arg_16\                  : signed(63 downto 0);
  -- spec.hs:132:1-78
  signal \buffer\                        : Spec_topEntity_types.array_of_Tuple3_0(0 to 1) := Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple3_0_sel1_boolean => false
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false ) )
                                       , ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple3_0_sel1_boolean => false
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false ) ) );
  -- spec.hs:132:1-78
  signal \c$buffer_case_alt\             : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:132:1-78
  signal \c$buffer_case_alt_0\           : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:132:1-78
  signal \c$buffer_case_alt_1\           : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:132:1-78
  signal qData                           : Spec_topEntity_types.Tuple3_0;
  -- spec.hs:132:1-78
  signal x_8                             : signed(63 downto 0);
  -- spec.hs:132:1-78
  signal cursor                          : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:132:1-78
  signal \c$cursor_case_alt\             : signed(63 downto 0);
  -- spec.hs:132:1-78
  signal push                            : boolean;
  -- spec.hs:132:1-78
  signal \c$cursor_case_alt_0\           : signed(63 downto 0);
  -- spec.hs:132:1-78
  signal \c$cursor_case_alt_1\           : signed(63 downto 0);
  -- spec.hs:132:1-78
  signal \c$cursor_case_scrut\           : boolean;
  -- spec.hs:132:1-78
  signal \c$cursor_app_arg\              : signed(63 downto 0);
  -- spec.hs:132:1-78
  signal \c$cursor_case_alt_2\           : signed(63 downto 0);
  -- spec.hs:132:1-78
  signal pop                             : boolean;
  -- spec.hs:132:1-78
  signal \c$cursor_case_alt_3\           : signed(63 downto 0);
  -- spec.hs:132:1-78
  signal \c$cursor_case_alt_4\           : signed(63 downto 0);
  signal inputs                          : Spec_topEntity_types.ValidInt;
  signal \c$vec\                         : Spec_topEntity_types.array_of_signed_64(0 to 100);
  signal x_projection_99                 : signed(63 downto 0);
  signal \c$t_case_alt_selection_1\      : boolean;
  signal \c$vec_0\                       : Spec_topEntity_types.array_of_signed_64(0 to 101);
  signal \c$t_case_alt_sel_alt_0\        : Spec_topEntity_types.Tuple2_5;
  signal \c$vec_1\                       : Spec_topEntity_types.array_of_signed_64(0 to 101);
  signal \c$t_case_alt_sel_alt_2\        : Spec_topEntity_types.Tuple2_5;
  signal \c$t_case_alt_selection_4\      : boolean;
  signal \c$vec2\                        : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$vec2_0\                      : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$vec2_1\                      : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal result_selection_8              : boolean;
  signal \c$vec_2\                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  signal t_projection_2                  : Spec_topEntity_types.Tuple2_6;
  signal \c$vec2_2\                      : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal \c$out1_case_alt_selection_res\ : boolean;
  signal result_selection_12             : boolean;
  signal \c$vec_3\                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 4);
  signal t_projection_4                  : Spec_topEntity_types.Tuple2_7;
  signal \c$out0_case_alt_selection_res\ : boolean;
  signal result_selection_res            : boolean;
  signal result_selection_17             : boolean;
  signal result_selection_21             : boolean;
  signal result_selection_25             : boolean;
  signal result_selection_29             : boolean;
  signal \c$vec_4\                       : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_3\   : Spec_topEntity_types.Tuple2_8;
  signal \c$vec_5\                       : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_5\   : Spec_topEntity_types.Tuple2_8;
  signal result                          : Spec_topEntity_types.Outputs;
  signal result_0                        : Spec_topEntity_types.ValidInt;
  signal result_1                        : Spec_topEntity_types.ValidInt;
  signal result_2                        : Spec_topEntity_types.ValidInt;

begin
  inputs <= ( ValidInt_sel0_value => inputs_0
            , ValidInt_sel1_valid => inputs_1 );

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

  result_5 <= ( Tuple2_sel0_boolean => hasInput0 or timer0Over
              , Tuple2_sel1_Tuple3_0 => ( Tuple3_0_sel0_ValidInt => inputs
              , Tuple3_0_sel1_boolean => timer0Over
              , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => pIn0
              , Pacings_1_sel1_pacingOut0 => pIn0
              , Pacings_1_sel2_pacingOut1 => pIn0
              , Pacings_1_sel3_pacingOut2 => timer0Over ) ) );

  hasInput0 <= inputs.ValidInt_sel1_valid;

  pIn0 <= hasInput0;

  timer0Over <= result_4 >= (to_signed(1000000,64));

  result_6 <= ( Tuple2_0_sel0_boolean => true
              , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidInt_sel0_value => result_19
              , ValidInt_sel1_valid => result_20 )
              , Outputs_sel1_output1 => ( ValidInt_sel0_value => result_17
              , ValidInt_sel1_valid => result_18 )
              , Outputs_sel2_output2 => ( ValidInt_sel0_value => result_9.Tuple2_1_sel1_signed
              , ValidInt_sel1_valid => result_7 ) ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
    signal iterateI_ho1_4_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut2;

    r_block_38 : block
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

    r_block_39 : block
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

    r_block_40 : block
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

    r_block_41 : block
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

    r_block_42 : block
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
    signal vec      : Spec_topEntity_types.array_of_signed_64(0 to 99);
    signal acc_7_0  : signed(63 downto 0);
    signal acc_1    : signed(63 downto 0);
    signal acc_2    : signed(63 downto 0);
    signal acc_3    : signed(63 downto 0);
    signal acc_4    : signed(63 downto 0);
    signal acc_5    : signed(63 downto 0);
    signal acc_6    : signed(63 downto 0);
    signal acc_7    : signed(63 downto 0);
    signal acc_8    : signed(63 downto 0);
    signal acc_9    : signed(63 downto 0);
    signal acc_10   : signed(63 downto 0);
    signal acc_11   : signed(63 downto 0);
    signal acc_12   : signed(63 downto 0);
    signal acc_13   : signed(63 downto 0);
    signal acc_14   : signed(63 downto 0);
    signal acc_15   : signed(63 downto 0);
    signal acc_16   : signed(63 downto 0);
    signal acc_17   : signed(63 downto 0);
    signal acc_18   : signed(63 downto 0);
    signal acc_19   : signed(63 downto 0);
    signal acc_20   : signed(63 downto 0);
    signal acc_21   : signed(63 downto 0);
    signal acc_22   : signed(63 downto 0);
    signal acc_23   : signed(63 downto 0);
    signal acc_24   : signed(63 downto 0);
    signal acc_25   : signed(63 downto 0);
    signal acc_26   : signed(63 downto 0);
    signal acc_27   : signed(63 downto 0);
    signal acc_28   : signed(63 downto 0);
    signal acc_29   : signed(63 downto 0);
    signal acc_30   : signed(63 downto 0);
    signal acc_31   : signed(63 downto 0);
    signal acc_32   : signed(63 downto 0);
    signal acc_33   : signed(63 downto 0);
    signal acc_34   : signed(63 downto 0);
    signal acc_35   : signed(63 downto 0);
    signal acc_36   : signed(63 downto 0);
    signal acc_37   : signed(63 downto 0);
    signal acc_38   : signed(63 downto 0);
    signal acc_39   : signed(63 downto 0);
    signal acc_40   : signed(63 downto 0);
    signal acc_41   : signed(63 downto 0);
    signal acc_42   : signed(63 downto 0);
    signal acc_43   : signed(63 downto 0);
    signal acc_44   : signed(63 downto 0);
    signal acc_45   : signed(63 downto 0);
    signal acc_46   : signed(63 downto 0);
    signal acc_47   : signed(63 downto 0);
    signal acc_48   : signed(63 downto 0);
    signal acc_49   : signed(63 downto 0);
    signal acc_50   : signed(63 downto 0);
    signal acc_51   : signed(63 downto 0);
    signal acc_52   : signed(63 downto 0);
    signal acc_53   : signed(63 downto 0);
    signal acc_54   : signed(63 downto 0);
    signal acc_55   : signed(63 downto 0);
    signal acc_56   : signed(63 downto 0);
    signal acc_57   : signed(63 downto 0);
    signal acc_58   : signed(63 downto 0);
    signal acc_59   : signed(63 downto 0);
    signal acc_60   : signed(63 downto 0);
    signal acc_61   : signed(63 downto 0);
    signal acc_62   : signed(63 downto 0);
    signal acc_63   : signed(63 downto 0);
    signal acc_64   : signed(63 downto 0);
    signal acc_65   : signed(63 downto 0);
    signal acc_66   : signed(63 downto 0);
    signal acc_67   : signed(63 downto 0);
    signal acc_68   : signed(63 downto 0);
    signal acc_69   : signed(63 downto 0);
    signal acc_70   : signed(63 downto 0);
    signal acc_71   : signed(63 downto 0);
    signal acc_72   : signed(63 downto 0);
    signal acc_73   : signed(63 downto 0);
    signal acc_74   : signed(63 downto 0);
    signal acc_75   : signed(63 downto 0);
    signal acc_76   : signed(63 downto 0);
    signal acc_77   : signed(63 downto 0);
    signal acc_78   : signed(63 downto 0);
    signal acc_79   : signed(63 downto 0);
    signal acc_80   : signed(63 downto 0);
    signal acc_81   : signed(63 downto 0);
    signal acc_82   : signed(63 downto 0);
    signal acc_83   : signed(63 downto 0);
    signal acc_84   : signed(63 downto 0);
    signal acc_85   : signed(63 downto 0);
    signal acc_86   : signed(63 downto 0);
    signal acc_87   : signed(63 downto 0);
    signal acc_88   : signed(63 downto 0);
    signal acc_89   : signed(63 downto 0);
    signal acc_90   : signed(63 downto 0);
    signal acc_91   : signed(63 downto 0);
    signal acc_92   : signed(63 downto 0);
    signal acc_93   : signed(63 downto 0);
    signal acc_94   : signed(63 downto 0);
    signal acc_95   : signed(63 downto 0);
    signal acc_96   : signed(63 downto 0);
    signal acc_97   : signed(63 downto 0);
    signal acc_98   : signed(63 downto 0);
    signal acc_99   : signed(63 downto 0);
    signal acc_100  : signed(63 downto 0);
    signal acc_1_0  : signed(63 downto 0);
    signal acc_1_1  : signed(63 downto 0);
    signal acc_1_2  : signed(63 downto 0);
    signal acc_1_3  : signed(63 downto 0);
    signal acc_1_4  : signed(63 downto 0);
    signal acc_1_5  : signed(63 downto 0);
    signal acc_1_6  : signed(63 downto 0);
    signal acc_1_7  : signed(63 downto 0);
    signal acc_1_8  : signed(63 downto 0);
    signal acc_1_9  : signed(63 downto 0);
    signal acc_1_10 : signed(63 downto 0);
    signal acc_1_11 : signed(63 downto 0);
    signal acc_1_12 : signed(63 downto 0);
    signal acc_1_13 : signed(63 downto 0);
    signal acc_1_14 : signed(63 downto 0);
    signal acc_1_15 : signed(63 downto 0);
    signal acc_1_16 : signed(63 downto 0);
    signal acc_1_17 : signed(63 downto 0);
    signal acc_1_18 : signed(63 downto 0);
    signal acc_1_19 : signed(63 downto 0);
    signal acc_1_20 : signed(63 downto 0);
    signal acc_1_21 : signed(63 downto 0);
    signal acc_1_22 : signed(63 downto 0);
    signal acc_1_23 : signed(63 downto 0);
    signal acc_1_24 : signed(63 downto 0);
    signal acc_1_25 : signed(63 downto 0);
    signal acc_1_26 : signed(63 downto 0);
    signal acc_1_27 : signed(63 downto 0);
    signal acc_1_28 : signed(63 downto 0);
    signal acc_1_29 : signed(63 downto 0);
    signal acc_1_30 : signed(63 downto 0);
    signal acc_1_31 : signed(63 downto 0);
    signal acc_1_32 : signed(63 downto 0);
    signal acc_1_33 : signed(63 downto 0);
    signal acc_1_34 : signed(63 downto 0);
    signal acc_1_35 : signed(63 downto 0);
    signal acc_1_36 : signed(63 downto 0);
    signal acc_1_37 : signed(63 downto 0);
    signal acc_1_38 : signed(63 downto 0);
    signal acc_1_39 : signed(63 downto 0);
    signal acc_1_40 : signed(63 downto 0);
    signal acc_1_41 : signed(63 downto 0);
    signal acc_1_42 : signed(63 downto 0);
    signal acc_1_43 : signed(63 downto 0);
    signal acc_1_44 : signed(63 downto 0);
    signal acc_1_45 : signed(63 downto 0);
    signal acc_1_46 : signed(63 downto 0);
    signal acc_1_47 : signed(63 downto 0);
    signal acc_1_48 : signed(63 downto 0);
    signal acc_1_49 : signed(63 downto 0);
    signal acc_2_0  : signed(63 downto 0);
    signal acc_2_1  : signed(63 downto 0);
    signal acc_2_2  : signed(63 downto 0);
    signal acc_2_3  : signed(63 downto 0);
    signal acc_2_4  : signed(63 downto 0);
    signal acc_2_5  : signed(63 downto 0);
    signal acc_2_6  : signed(63 downto 0);
    signal acc_2_7  : signed(63 downto 0);
    signal acc_2_8  : signed(63 downto 0);
    signal acc_2_9  : signed(63 downto 0);
    signal acc_2_10 : signed(63 downto 0);
    signal acc_2_11 : signed(63 downto 0);
    signal acc_2_12 : signed(63 downto 0);
    signal acc_2_13 : signed(63 downto 0);
    signal acc_2_14 : signed(63 downto 0);
    signal acc_2_15 : signed(63 downto 0);
    signal acc_2_16 : signed(63 downto 0);
    signal acc_2_17 : signed(63 downto 0);
    signal acc_2_18 : signed(63 downto 0);
    signal acc_2_19 : signed(63 downto 0);
    signal acc_2_20 : signed(63 downto 0);
    signal acc_2_21 : signed(63 downto 0);
    signal acc_2_22 : signed(63 downto 0);
    signal acc_2_23 : signed(63 downto 0);
    signal acc_3_0  : signed(63 downto 0);
    signal acc_3_1  : signed(63 downto 0);
    signal acc_3_2  : signed(63 downto 0);
    signal acc_3_3  : signed(63 downto 0);
    signal acc_3_4  : signed(63 downto 0);
    signal acc_3_5  : signed(63 downto 0);
    signal acc_3_6  : signed(63 downto 0);
    signal acc_3_7  : signed(63 downto 0);
    signal acc_3_8  : signed(63 downto 0);
    signal acc_3_9  : signed(63 downto 0);
    signal acc_3_10 : signed(63 downto 0);
    signal acc_3_11 : signed(63 downto 0);
    signal acc_4_0  : signed(63 downto 0);
    signal acc_4_1  : signed(63 downto 0);
    signal acc_4_2  : signed(63 downto 0);
    signal acc_4_3  : signed(63 downto 0);
    signal acc_4_4  : signed(63 downto 0);
    signal acc_4_5  : signed(63 downto 0);
    signal acc_5_0  : signed(63 downto 0);
    signal acc_5_1  : signed(63 downto 0);
    signal acc_5_2  : signed(63 downto 0);
    signal acc_2_24 : signed(63 downto 0);
    signal acc_6_0  : signed(63 downto 0);
    signal acc_6_1  : signed(63 downto 0);
  begin
    \c$t_app_arg\ <= acc_7_0;

    vec <= (\c$vec\(1 to \c$vec\'high));

    acc_1 <= vec(0);

    acc_2 <= vec(1);

    acc_3 <= vec(2);

    acc_4 <= vec(3);

    acc_5 <= vec(4);

    acc_6 <= vec(5);

    acc_7 <= vec(6);

    acc_8 <= vec(7);

    acc_9 <= vec(8);

    acc_10 <= vec(9);

    acc_11 <= vec(10);

    acc_12 <= vec(11);

    acc_13 <= vec(12);

    acc_14 <= vec(13);

    acc_15 <= vec(14);

    acc_16 <= vec(15);

    acc_17 <= vec(16);

    acc_18 <= vec(17);

    acc_19 <= vec(18);

    acc_20 <= vec(19);

    acc_21 <= vec(20);

    acc_22 <= vec(21);

    acc_23 <= vec(22);

    acc_24 <= vec(23);

    acc_25 <= vec(24);

    acc_26 <= vec(25);

    acc_27 <= vec(26);

    acc_28 <= vec(27);

    acc_29 <= vec(28);

    acc_30 <= vec(29);

    acc_31 <= vec(30);

    acc_32 <= vec(31);

    acc_33 <= vec(32);

    acc_34 <= vec(33);

    acc_35 <= vec(34);

    acc_36 <= vec(35);

    acc_37 <= vec(36);

    acc_38 <= vec(37);

    acc_39 <= vec(38);

    acc_40 <= vec(39);

    acc_41 <= vec(40);

    acc_42 <= vec(41);

    acc_43 <= vec(42);

    acc_44 <= vec(43);

    acc_45 <= vec(44);

    acc_46 <= vec(45);

    acc_47 <= vec(46);

    acc_48 <= vec(47);

    acc_49 <= vec(48);

    acc_50 <= vec(49);

    acc_51 <= vec(50);

    acc_52 <= vec(51);

    acc_53 <= vec(52);

    acc_54 <= vec(53);

    acc_55 <= vec(54);

    acc_56 <= vec(55);

    acc_57 <= vec(56);

    acc_58 <= vec(57);

    acc_59 <= vec(58);

    acc_60 <= vec(59);

    acc_61 <= vec(60);

    acc_62 <= vec(61);

    acc_63 <= vec(62);

    acc_64 <= vec(63);

    acc_65 <= vec(64);

    acc_66 <= vec(65);

    acc_67 <= vec(66);

    acc_68 <= vec(67);

    acc_69 <= vec(68);

    acc_70 <= vec(69);

    acc_71 <= vec(70);

    acc_72 <= vec(71);

    acc_73 <= vec(72);

    acc_74 <= vec(73);

    acc_75 <= vec(74);

    acc_76 <= vec(75);

    acc_77 <= vec(76);

    acc_78 <= vec(77);

    acc_79 <= vec(78);

    acc_80 <= vec(79);

    acc_81 <= vec(80);

    acc_82 <= vec(81);

    acc_83 <= vec(82);

    acc_84 <= vec(83);

    acc_85 <= vec(84);

    acc_86 <= vec(85);

    acc_87 <= vec(86);

    acc_88 <= vec(87);

    acc_89 <= vec(88);

    acc_90 <= vec(89);

    acc_91 <= vec(90);

    acc_92 <= vec(91);

    acc_93 <= vec(92);

    acc_94 <= vec(93);

    acc_95 <= vec(94);

    acc_96 <= vec(95);

    acc_97 <= vec(96);

    acc_98 <= vec(97);

    acc_99 <= vec(98);

    acc_100 <= vec(99);

    fun_102 : block
        signal x_10            : signed(63 downto 0);
        signal y_1             : signed(63 downto 0);
        signal \c$case_alt_13\ : signed(63 downto 0);
      begin
        acc_1_0 <= \c$case_alt_13\;

        x_10 <= acc_1;

        y_1 <= acc_2;

        \c$case_alt_13\ <= (x_10 + y_1);


      end block;

    fun_103 : block
        signal x_11            : signed(63 downto 0);
        signal y_2             : signed(63 downto 0);
        signal \c$case_alt_14\ : signed(63 downto 0);
      begin
        acc_1_1 <= \c$case_alt_14\;

        x_11 <= acc_3;

        y_2 <= acc_4;

        \c$case_alt_14\ <= (x_11 + y_2);


      end block;

    fun_104 : block
        signal x_12            : signed(63 downto 0);
        signal y_3             : signed(63 downto 0);
        signal \c$case_alt_15\ : signed(63 downto 0);
      begin
        acc_1_2 <= \c$case_alt_15\;

        x_12 <= acc_5;

        y_3 <= acc_6;

        \c$case_alt_15\ <= (x_12 + y_3);


      end block;

    fun_105 : block
        signal x_13            : signed(63 downto 0);
        signal y_4             : signed(63 downto 0);
        signal \c$case_alt_16\ : signed(63 downto 0);
      begin
        acc_1_3 <= \c$case_alt_16\;

        x_13 <= acc_7;

        y_4 <= acc_8;

        \c$case_alt_16\ <= (x_13 + y_4);


      end block;

    fun_106 : block
        signal x_14            : signed(63 downto 0);
        signal y_5             : signed(63 downto 0);
        signal \c$case_alt_17\ : signed(63 downto 0);
      begin
        acc_1_4 <= \c$case_alt_17\;

        x_14 <= acc_9;

        y_5 <= acc_10;

        \c$case_alt_17\ <= (x_14 + y_5);


      end block;

    fun_107 : block
        signal x_15            : signed(63 downto 0);
        signal y_6             : signed(63 downto 0);
        signal \c$case_alt_18\ : signed(63 downto 0);
      begin
        acc_1_5 <= \c$case_alt_18\;

        x_15 <= acc_11;

        y_6 <= acc_12;

        \c$case_alt_18\ <= (x_15 + y_6);


      end block;

    fun_108 : block
        signal x_16            : signed(63 downto 0);
        signal y_7             : signed(63 downto 0);
        signal \c$case_alt_19\ : signed(63 downto 0);
      begin
        acc_1_6 <= \c$case_alt_19\;

        x_16 <= acc_13;

        y_7 <= acc_14;

        \c$case_alt_19\ <= (x_16 + y_7);


      end block;

    fun_109 : block
        signal x_17            : signed(63 downto 0);
        signal y_8             : signed(63 downto 0);
        signal \c$case_alt_20\ : signed(63 downto 0);
      begin
        acc_1_7 <= \c$case_alt_20\;

        x_17 <= acc_15;

        y_8 <= acc_16;

        \c$case_alt_20\ <= (x_17 + y_8);


      end block;

    fun_110 : block
        signal x_18            : signed(63 downto 0);
        signal y_9             : signed(63 downto 0);
        signal \c$case_alt_21\ : signed(63 downto 0);
      begin
        acc_1_8 <= \c$case_alt_21\;

        x_18 <= acc_17;

        y_9 <= acc_18;

        \c$case_alt_21\ <= (x_18 + y_9);


      end block;

    fun_111 : block
        signal x_19            : signed(63 downto 0);
        signal y_10            : signed(63 downto 0);
        signal \c$case_alt_22\ : signed(63 downto 0);
      begin
        acc_1_9 <= \c$case_alt_22\;

        x_19 <= acc_19;

        y_10 <= acc_20;

        \c$case_alt_22\ <= (x_19 + y_10);


      end block;

    fun_112 : block
        signal x_20            : signed(63 downto 0);
        signal y_11            : signed(63 downto 0);
        signal \c$case_alt_23\ : signed(63 downto 0);
      begin
        acc_1_10 <= \c$case_alt_23\;

        x_20 <= acc_21;

        y_11 <= acc_22;

        \c$case_alt_23\ <= (x_20 + y_11);


      end block;

    fun_113 : block
        signal x_21            : signed(63 downto 0);
        signal y_12            : signed(63 downto 0);
        signal \c$case_alt_24\ : signed(63 downto 0);
      begin
        acc_1_11 <= \c$case_alt_24\;

        x_21 <= acc_23;

        y_12 <= acc_24;

        \c$case_alt_24\ <= (x_21 + y_12);


      end block;

    fun_114 : block
        signal x_22            : signed(63 downto 0);
        signal y_13            : signed(63 downto 0);
        signal \c$case_alt_25\ : signed(63 downto 0);
      begin
        acc_1_12 <= \c$case_alt_25\;

        x_22 <= acc_25;

        y_13 <= acc_26;

        \c$case_alt_25\ <= (x_22 + y_13);


      end block;

    fun_115 : block
        signal x_23            : signed(63 downto 0);
        signal y_14            : signed(63 downto 0);
        signal \c$case_alt_26\ : signed(63 downto 0);
      begin
        acc_1_13 <= \c$case_alt_26\;

        x_23 <= acc_27;

        y_14 <= acc_28;

        \c$case_alt_26\ <= (x_23 + y_14);


      end block;

    fun_116 : block
        signal x_24            : signed(63 downto 0);
        signal y_15            : signed(63 downto 0);
        signal \c$case_alt_27\ : signed(63 downto 0);
      begin
        acc_1_14 <= \c$case_alt_27\;

        x_24 <= acc_29;

        y_15 <= acc_30;

        \c$case_alt_27\ <= (x_24 + y_15);


      end block;

    fun_117 : block
        signal x_25            : signed(63 downto 0);
        signal y_16            : signed(63 downto 0);
        signal \c$case_alt_28\ : signed(63 downto 0);
      begin
        acc_1_15 <= \c$case_alt_28\;

        x_25 <= acc_31;

        y_16 <= acc_32;

        \c$case_alt_28\ <= (x_25 + y_16);


      end block;

    fun_118 : block
        signal x_26            : signed(63 downto 0);
        signal y_17            : signed(63 downto 0);
        signal \c$case_alt_29\ : signed(63 downto 0);
      begin
        acc_1_16 <= \c$case_alt_29\;

        x_26 <= acc_33;

        y_17 <= acc_34;

        \c$case_alt_29\ <= (x_26 + y_17);


      end block;

    fun_119 : block
        signal x_27            : signed(63 downto 0);
        signal y_18            : signed(63 downto 0);
        signal \c$case_alt_30\ : signed(63 downto 0);
      begin
        acc_1_17 <= \c$case_alt_30\;

        x_27 <= acc_35;

        y_18 <= acc_36;

        \c$case_alt_30\ <= (x_27 + y_18);


      end block;

    fun_120 : block
        signal x_28            : signed(63 downto 0);
        signal y_19            : signed(63 downto 0);
        signal \c$case_alt_31\ : signed(63 downto 0);
      begin
        acc_1_18 <= \c$case_alt_31\;

        x_28 <= acc_37;

        y_19 <= acc_38;

        \c$case_alt_31\ <= (x_28 + y_19);


      end block;

    fun_121 : block
        signal x_29            : signed(63 downto 0);
        signal y_20            : signed(63 downto 0);
        signal \c$case_alt_32\ : signed(63 downto 0);
      begin
        acc_1_19 <= \c$case_alt_32\;

        x_29 <= acc_39;

        y_20 <= acc_40;

        \c$case_alt_32\ <= (x_29 + y_20);


      end block;

    fun_122 : block
        signal x_30            : signed(63 downto 0);
        signal y_21            : signed(63 downto 0);
        signal \c$case_alt_33\ : signed(63 downto 0);
      begin
        acc_1_20 <= \c$case_alt_33\;

        x_30 <= acc_41;

        y_21 <= acc_42;

        \c$case_alt_33\ <= (x_30 + y_21);


      end block;

    fun_123 : block
        signal x_31            : signed(63 downto 0);
        signal y_22            : signed(63 downto 0);
        signal \c$case_alt_34\ : signed(63 downto 0);
      begin
        acc_1_21 <= \c$case_alt_34\;

        x_31 <= acc_43;

        y_22 <= acc_44;

        \c$case_alt_34\ <= (x_31 + y_22);


      end block;

    fun_124 : block
        signal x_32            : signed(63 downto 0);
        signal y_23            : signed(63 downto 0);
        signal \c$case_alt_35\ : signed(63 downto 0);
      begin
        acc_1_22 <= \c$case_alt_35\;

        x_32 <= acc_45;

        y_23 <= acc_46;

        \c$case_alt_35\ <= (x_32 + y_23);


      end block;

    fun_125 : block
        signal x_33            : signed(63 downto 0);
        signal y_24            : signed(63 downto 0);
        signal \c$case_alt_36\ : signed(63 downto 0);
      begin
        acc_1_23 <= \c$case_alt_36\;

        x_33 <= acc_47;

        y_24 <= acc_48;

        \c$case_alt_36\ <= (x_33 + y_24);


      end block;

    fun_126 : block
        signal x_34            : signed(63 downto 0);
        signal y_25            : signed(63 downto 0);
        signal \c$case_alt_37\ : signed(63 downto 0);
      begin
        acc_1_24 <= \c$case_alt_37\;

        x_34 <= acc_49;

        y_25 <= acc_50;

        \c$case_alt_37\ <= (x_34 + y_25);


      end block;

    fun_127 : block
        signal x_35            : signed(63 downto 0);
        signal y_26            : signed(63 downto 0);
        signal \c$case_alt_38\ : signed(63 downto 0);
      begin
        acc_1_25 <= \c$case_alt_38\;

        x_35 <= acc_51;

        y_26 <= acc_52;

        \c$case_alt_38\ <= (x_35 + y_26);


      end block;

    fun_128 : block
        signal x_36            : signed(63 downto 0);
        signal y_27            : signed(63 downto 0);
        signal \c$case_alt_39\ : signed(63 downto 0);
      begin
        acc_1_26 <= \c$case_alt_39\;

        x_36 <= acc_53;

        y_27 <= acc_54;

        \c$case_alt_39\ <= (x_36 + y_27);


      end block;

    fun_129 : block
        signal x_37            : signed(63 downto 0);
        signal y_28            : signed(63 downto 0);
        signal \c$case_alt_40\ : signed(63 downto 0);
      begin
        acc_1_27 <= \c$case_alt_40\;

        x_37 <= acc_55;

        y_28 <= acc_56;

        \c$case_alt_40\ <= (x_37 + y_28);


      end block;

    fun_130 : block
        signal x_38            : signed(63 downto 0);
        signal y_29            : signed(63 downto 0);
        signal \c$case_alt_41\ : signed(63 downto 0);
      begin
        acc_1_28 <= \c$case_alt_41\;

        x_38 <= acc_57;

        y_29 <= acc_58;

        \c$case_alt_41\ <= (x_38 + y_29);


      end block;

    fun_131 : block
        signal x_39            : signed(63 downto 0);
        signal y_30            : signed(63 downto 0);
        signal \c$case_alt_42\ : signed(63 downto 0);
      begin
        acc_1_29 <= \c$case_alt_42\;

        x_39 <= acc_59;

        y_30 <= acc_60;

        \c$case_alt_42\ <= (x_39 + y_30);


      end block;

    fun_132 : block
        signal x_40            : signed(63 downto 0);
        signal y_31            : signed(63 downto 0);
        signal \c$case_alt_43\ : signed(63 downto 0);
      begin
        acc_1_30 <= \c$case_alt_43\;

        x_40 <= acc_61;

        y_31 <= acc_62;

        \c$case_alt_43\ <= (x_40 + y_31);


      end block;

    fun_133 : block
        signal x_41            : signed(63 downto 0);
        signal y_32            : signed(63 downto 0);
        signal \c$case_alt_44\ : signed(63 downto 0);
      begin
        acc_1_31 <= \c$case_alt_44\;

        x_41 <= acc_63;

        y_32 <= acc_64;

        \c$case_alt_44\ <= (x_41 + y_32);


      end block;

    fun_134 : block
        signal x_42            : signed(63 downto 0);
        signal y_33            : signed(63 downto 0);
        signal \c$case_alt_45\ : signed(63 downto 0);
      begin
        acc_1_32 <= \c$case_alt_45\;

        x_42 <= acc_65;

        y_33 <= acc_66;

        \c$case_alt_45\ <= (x_42 + y_33);


      end block;

    fun_135 : block
        signal x_43            : signed(63 downto 0);
        signal y_34            : signed(63 downto 0);
        signal \c$case_alt_46\ : signed(63 downto 0);
      begin
        acc_1_33 <= \c$case_alt_46\;

        x_43 <= acc_67;

        y_34 <= acc_68;

        \c$case_alt_46\ <= (x_43 + y_34);


      end block;

    fun_136 : block
        signal x_44            : signed(63 downto 0);
        signal y_35            : signed(63 downto 0);
        signal \c$case_alt_47\ : signed(63 downto 0);
      begin
        acc_1_34 <= \c$case_alt_47\;

        x_44 <= acc_69;

        y_35 <= acc_70;

        \c$case_alt_47\ <= (x_44 + y_35);


      end block;

    fun_137 : block
        signal x_45            : signed(63 downto 0);
        signal y_36            : signed(63 downto 0);
        signal \c$case_alt_48\ : signed(63 downto 0);
      begin
        acc_1_35 <= \c$case_alt_48\;

        x_45 <= acc_71;

        y_36 <= acc_72;

        \c$case_alt_48\ <= (x_45 + y_36);


      end block;

    fun_138 : block
        signal x_46            : signed(63 downto 0);
        signal y_37            : signed(63 downto 0);
        signal \c$case_alt_49\ : signed(63 downto 0);
      begin
        acc_1_36 <= \c$case_alt_49\;

        x_46 <= acc_73;

        y_37 <= acc_74;

        \c$case_alt_49\ <= (x_46 + y_37);


      end block;

    fun_139 : block
        signal x_47            : signed(63 downto 0);
        signal y_38            : signed(63 downto 0);
        signal \c$case_alt_50\ : signed(63 downto 0);
      begin
        acc_1_37 <= \c$case_alt_50\;

        x_47 <= acc_75;

        y_38 <= acc_76;

        \c$case_alt_50\ <= (x_47 + y_38);


      end block;

    fun_140 : block
        signal x_48            : signed(63 downto 0);
        signal y_39            : signed(63 downto 0);
        signal \c$case_alt_51\ : signed(63 downto 0);
      begin
        acc_1_38 <= \c$case_alt_51\;

        x_48 <= acc_77;

        y_39 <= acc_78;

        \c$case_alt_51\ <= (x_48 + y_39);


      end block;

    fun_141 : block
        signal x_49            : signed(63 downto 0);
        signal y_40            : signed(63 downto 0);
        signal \c$case_alt_52\ : signed(63 downto 0);
      begin
        acc_1_39 <= \c$case_alt_52\;

        x_49 <= acc_79;

        y_40 <= acc_80;

        \c$case_alt_52\ <= (x_49 + y_40);


      end block;

    fun_142 : block
        signal x_50            : signed(63 downto 0);
        signal y_41            : signed(63 downto 0);
        signal \c$case_alt_53\ : signed(63 downto 0);
      begin
        acc_1_40 <= \c$case_alt_53\;

        x_50 <= acc_81;

        y_41 <= acc_82;

        \c$case_alt_53\ <= (x_50 + y_41);


      end block;

    fun_143 : block
        signal x_51            : signed(63 downto 0);
        signal y_42            : signed(63 downto 0);
        signal \c$case_alt_54\ : signed(63 downto 0);
      begin
        acc_1_41 <= \c$case_alt_54\;

        x_51 <= acc_83;

        y_42 <= acc_84;

        \c$case_alt_54\ <= (x_51 + y_42);


      end block;

    fun_144 : block
        signal x_52            : signed(63 downto 0);
        signal y_43            : signed(63 downto 0);
        signal \c$case_alt_55\ : signed(63 downto 0);
      begin
        acc_1_42 <= \c$case_alt_55\;

        x_52 <= acc_85;

        y_43 <= acc_86;

        \c$case_alt_55\ <= (x_52 + y_43);


      end block;

    fun_145 : block
        signal x_53            : signed(63 downto 0);
        signal y_44            : signed(63 downto 0);
        signal \c$case_alt_56\ : signed(63 downto 0);
      begin
        acc_1_43 <= \c$case_alt_56\;

        x_53 <= acc_87;

        y_44 <= acc_88;

        \c$case_alt_56\ <= (x_53 + y_44);


      end block;

    fun_146 : block
        signal x_54            : signed(63 downto 0);
        signal y_45            : signed(63 downto 0);
        signal \c$case_alt_57\ : signed(63 downto 0);
      begin
        acc_1_44 <= \c$case_alt_57\;

        x_54 <= acc_89;

        y_45 <= acc_90;

        \c$case_alt_57\ <= (x_54 + y_45);


      end block;

    fun_147 : block
        signal x_55            : signed(63 downto 0);
        signal y_46            : signed(63 downto 0);
        signal \c$case_alt_58\ : signed(63 downto 0);
      begin
        acc_1_45 <= \c$case_alt_58\;

        x_55 <= acc_91;

        y_46 <= acc_92;

        \c$case_alt_58\ <= (x_55 + y_46);


      end block;

    fun_148 : block
        signal x_56            : signed(63 downto 0);
        signal y_47            : signed(63 downto 0);
        signal \c$case_alt_59\ : signed(63 downto 0);
      begin
        acc_1_46 <= \c$case_alt_59\;

        x_56 <= acc_93;

        y_47 <= acc_94;

        \c$case_alt_59\ <= (x_56 + y_47);


      end block;

    fun_149 : block
        signal x_57            : signed(63 downto 0);
        signal y_48            : signed(63 downto 0);
        signal \c$case_alt_60\ : signed(63 downto 0);
      begin
        acc_1_47 <= \c$case_alt_60\;

        x_57 <= acc_95;

        y_48 <= acc_96;

        \c$case_alt_60\ <= (x_57 + y_48);


      end block;

    fun_150 : block
        signal x_58            : signed(63 downto 0);
        signal y_49            : signed(63 downto 0);
        signal \c$case_alt_61\ : signed(63 downto 0);
      begin
        acc_1_48 <= \c$case_alt_61\;

        x_58 <= acc_97;

        y_49 <= acc_98;

        \c$case_alt_61\ <= (x_58 + y_49);


      end block;

    fun_151 : block
        signal x_59            : signed(63 downto 0);
        signal y_50            : signed(63 downto 0);
        signal \c$case_alt_62\ : signed(63 downto 0);
      begin
        acc_1_49 <= \c$case_alt_62\;

        x_59 <= acc_99;

        y_50 <= acc_100;

        \c$case_alt_62\ <= (x_59 + y_50);


      end block;

    fun_152 : block
        signal x_60            : signed(63 downto 0);
        signal y_51            : signed(63 downto 0);
        signal \c$case_alt_63\ : signed(63 downto 0);
      begin
        acc_2_0 <= \c$case_alt_63\;

        x_60 <= acc_1_0;

        y_51 <= acc_1_1;

        \c$case_alt_63\ <= (x_60 + y_51);


      end block;

    fun_153 : block
        signal x_61            : signed(63 downto 0);
        signal y_52            : signed(63 downto 0);
        signal \c$case_alt_64\ : signed(63 downto 0);
      begin
        acc_2_1 <= \c$case_alt_64\;

        x_61 <= acc_1_2;

        y_52 <= acc_1_3;

        \c$case_alt_64\ <= (x_61 + y_52);


      end block;

    fun_154 : block
        signal x_62            : signed(63 downto 0);
        signal y_53            : signed(63 downto 0);
        signal \c$case_alt_65\ : signed(63 downto 0);
      begin
        acc_2_2 <= \c$case_alt_65\;

        x_62 <= acc_1_4;

        y_53 <= acc_1_5;

        \c$case_alt_65\ <= (x_62 + y_53);


      end block;

    fun_155 : block
        signal x_63            : signed(63 downto 0);
        signal y_54            : signed(63 downto 0);
        signal \c$case_alt_66\ : signed(63 downto 0);
      begin
        acc_2_3 <= \c$case_alt_66\;

        x_63 <= acc_1_6;

        y_54 <= acc_1_7;

        \c$case_alt_66\ <= (x_63 + y_54);


      end block;

    fun_156 : block
        signal x_64            : signed(63 downto 0);
        signal y_55            : signed(63 downto 0);
        signal \c$case_alt_67\ : signed(63 downto 0);
      begin
        acc_2_4 <= \c$case_alt_67\;

        x_64 <= acc_1_8;

        y_55 <= acc_1_9;

        \c$case_alt_67\ <= (x_64 + y_55);


      end block;

    fun_157 : block
        signal x_65            : signed(63 downto 0);
        signal y_56            : signed(63 downto 0);
        signal \c$case_alt_68\ : signed(63 downto 0);
      begin
        acc_2_5 <= \c$case_alt_68\;

        x_65 <= acc_1_10;

        y_56 <= acc_1_11;

        \c$case_alt_68\ <= (x_65 + y_56);


      end block;

    fun_158 : block
        signal x_66            : signed(63 downto 0);
        signal y_57            : signed(63 downto 0);
        signal \c$case_alt_69\ : signed(63 downto 0);
      begin
        acc_2_6 <= \c$case_alt_69\;

        x_66 <= acc_1_12;

        y_57 <= acc_1_13;

        \c$case_alt_69\ <= (x_66 + y_57);


      end block;

    fun_159 : block
        signal x_67            : signed(63 downto 0);
        signal y_58            : signed(63 downto 0);
        signal \c$case_alt_70\ : signed(63 downto 0);
      begin
        acc_2_7 <= \c$case_alt_70\;

        x_67 <= acc_1_14;

        y_58 <= acc_1_15;

        \c$case_alt_70\ <= (x_67 + y_58);


      end block;

    fun_160 : block
        signal x_68            : signed(63 downto 0);
        signal y_59            : signed(63 downto 0);
        signal \c$case_alt_71\ : signed(63 downto 0);
      begin
        acc_2_8 <= \c$case_alt_71\;

        x_68 <= acc_1_16;

        y_59 <= acc_1_17;

        \c$case_alt_71\ <= (x_68 + y_59);


      end block;

    fun_161 : block
        signal x_69            : signed(63 downto 0);
        signal y_60            : signed(63 downto 0);
        signal \c$case_alt_72\ : signed(63 downto 0);
      begin
        acc_2_9 <= \c$case_alt_72\;

        x_69 <= acc_1_18;

        y_60 <= acc_1_19;

        \c$case_alt_72\ <= (x_69 + y_60);


      end block;

    fun_162 : block
        signal x_70            : signed(63 downto 0);
        signal y_61            : signed(63 downto 0);
        signal \c$case_alt_73\ : signed(63 downto 0);
      begin
        acc_2_10 <= \c$case_alt_73\;

        x_70 <= acc_1_20;

        y_61 <= acc_1_21;

        \c$case_alt_73\ <= (x_70 + y_61);


      end block;

    fun_163 : block
        signal x_71            : signed(63 downto 0);
        signal y_62            : signed(63 downto 0);
        signal \c$case_alt_74\ : signed(63 downto 0);
      begin
        acc_2_11 <= \c$case_alt_74\;

        x_71 <= acc_1_22;

        y_62 <= acc_1_23;

        \c$case_alt_74\ <= (x_71 + y_62);


      end block;

    fun_164 : block
        signal x_72            : signed(63 downto 0);
        signal y_63            : signed(63 downto 0);
        signal \c$case_alt_75\ : signed(63 downto 0);
      begin
        acc_2_12 <= \c$case_alt_75\;

        x_72 <= acc_1_24;

        y_63 <= acc_1_25;

        \c$case_alt_75\ <= (x_72 + y_63);


      end block;

    fun_165 : block
        signal x_73            : signed(63 downto 0);
        signal y_64            : signed(63 downto 0);
        signal \c$case_alt_76\ : signed(63 downto 0);
      begin
        acc_2_13 <= \c$case_alt_76\;

        x_73 <= acc_1_26;

        y_64 <= acc_1_27;

        \c$case_alt_76\ <= (x_73 + y_64);


      end block;

    fun_166 : block
        signal x_74            : signed(63 downto 0);
        signal y_65            : signed(63 downto 0);
        signal \c$case_alt_77\ : signed(63 downto 0);
      begin
        acc_2_14 <= \c$case_alt_77\;

        x_74 <= acc_1_28;

        y_65 <= acc_1_29;

        \c$case_alt_77\ <= (x_74 + y_65);


      end block;

    fun_167 : block
        signal x_75            : signed(63 downto 0);
        signal y_66            : signed(63 downto 0);
        signal \c$case_alt_78\ : signed(63 downto 0);
      begin
        acc_2_15 <= \c$case_alt_78\;

        x_75 <= acc_1_30;

        y_66 <= acc_1_31;

        \c$case_alt_78\ <= (x_75 + y_66);


      end block;

    fun_168 : block
        signal x_76            : signed(63 downto 0);
        signal y_67            : signed(63 downto 0);
        signal \c$case_alt_79\ : signed(63 downto 0);
      begin
        acc_2_16 <= \c$case_alt_79\;

        x_76 <= acc_1_32;

        y_67 <= acc_1_33;

        \c$case_alt_79\ <= (x_76 + y_67);


      end block;

    fun_169 : block
        signal x_77            : signed(63 downto 0);
        signal y_68            : signed(63 downto 0);
        signal \c$case_alt_80\ : signed(63 downto 0);
      begin
        acc_2_17 <= \c$case_alt_80\;

        x_77 <= acc_1_34;

        y_68 <= acc_1_35;

        \c$case_alt_80\ <= (x_77 + y_68);


      end block;

    fun_170 : block
        signal x_78            : signed(63 downto 0);
        signal y_69            : signed(63 downto 0);
        signal \c$case_alt_81\ : signed(63 downto 0);
      begin
        acc_2_18 <= \c$case_alt_81\;

        x_78 <= acc_1_36;

        y_69 <= acc_1_37;

        \c$case_alt_81\ <= (x_78 + y_69);


      end block;

    fun_171 : block
        signal x_79            : signed(63 downto 0);
        signal y_70            : signed(63 downto 0);
        signal \c$case_alt_82\ : signed(63 downto 0);
      begin
        acc_2_19 <= \c$case_alt_82\;

        x_79 <= acc_1_38;

        y_70 <= acc_1_39;

        \c$case_alt_82\ <= (x_79 + y_70);


      end block;

    fun_172 : block
        signal x_80            : signed(63 downto 0);
        signal y_71            : signed(63 downto 0);
        signal \c$case_alt_83\ : signed(63 downto 0);
      begin
        acc_2_20 <= \c$case_alt_83\;

        x_80 <= acc_1_40;

        y_71 <= acc_1_41;

        \c$case_alt_83\ <= (x_80 + y_71);


      end block;

    fun_173 : block
        signal x_81            : signed(63 downto 0);
        signal y_72            : signed(63 downto 0);
        signal \c$case_alt_84\ : signed(63 downto 0);
      begin
        acc_2_21 <= \c$case_alt_84\;

        x_81 <= acc_1_42;

        y_72 <= acc_1_43;

        \c$case_alt_84\ <= (x_81 + y_72);


      end block;

    fun_174 : block
        signal x_82            : signed(63 downto 0);
        signal y_73            : signed(63 downto 0);
        signal \c$case_alt_85\ : signed(63 downto 0);
      begin
        acc_2_22 <= \c$case_alt_85\;

        x_82 <= acc_1_44;

        y_73 <= acc_1_45;

        \c$case_alt_85\ <= (x_82 + y_73);


      end block;

    fun_175 : block
        signal x_83            : signed(63 downto 0);
        signal y_74            : signed(63 downto 0);
        signal \c$case_alt_86\ : signed(63 downto 0);
      begin
        acc_2_23 <= \c$case_alt_86\;

        x_83 <= acc_1_46;

        y_74 <= acc_1_47;

        \c$case_alt_86\ <= (x_83 + y_74);


      end block;

    fun_176 : block
        signal x_84            : signed(63 downto 0);
        signal y_75            : signed(63 downto 0);
        signal \c$case_alt_87\ : signed(63 downto 0);
      begin
        acc_2_24 <= \c$case_alt_87\;

        x_84 <= acc_1_48;

        y_75 <= acc_1_49;

        \c$case_alt_87\ <= (x_84 + y_75);


      end block;

    fun_177 : block
        signal x_85            : signed(63 downto 0);
        signal y_76            : signed(63 downto 0);
        signal \c$case_alt_88\ : signed(63 downto 0);
      begin
        acc_3_0 <= \c$case_alt_88\;

        x_85 <= acc_2_0;

        y_76 <= acc_2_1;

        \c$case_alt_88\ <= (x_85 + y_76);


      end block;

    fun_178 : block
        signal x_86            : signed(63 downto 0);
        signal y_77            : signed(63 downto 0);
        signal \c$case_alt_89\ : signed(63 downto 0);
      begin
        acc_3_1 <= \c$case_alt_89\;

        x_86 <= acc_2_2;

        y_77 <= acc_2_3;

        \c$case_alt_89\ <= (x_86 + y_77);


      end block;

    fun_179 : block
        signal x_87            : signed(63 downto 0);
        signal y_78            : signed(63 downto 0);
        signal \c$case_alt_90\ : signed(63 downto 0);
      begin
        acc_3_2 <= \c$case_alt_90\;

        x_87 <= acc_2_4;

        y_78 <= acc_2_5;

        \c$case_alt_90\ <= (x_87 + y_78);


      end block;

    fun_180 : block
        signal x_88            : signed(63 downto 0);
        signal y_79            : signed(63 downto 0);
        signal \c$case_alt_91\ : signed(63 downto 0);
      begin
        acc_3_3 <= \c$case_alt_91\;

        x_88 <= acc_2_6;

        y_79 <= acc_2_7;

        \c$case_alt_91\ <= (x_88 + y_79);


      end block;

    fun_181 : block
        signal x_89            : signed(63 downto 0);
        signal y_80            : signed(63 downto 0);
        signal \c$case_alt_92\ : signed(63 downto 0);
      begin
        acc_3_4 <= \c$case_alt_92\;

        x_89 <= acc_2_8;

        y_80 <= acc_2_9;

        \c$case_alt_92\ <= (x_89 + y_80);


      end block;

    fun_182 : block
        signal x_90            : signed(63 downto 0);
        signal y_81            : signed(63 downto 0);
        signal \c$case_alt_93\ : signed(63 downto 0);
      begin
        acc_3_5 <= \c$case_alt_93\;

        x_90 <= acc_2_10;

        y_81 <= acc_2_11;

        \c$case_alt_93\ <= (x_90 + y_81);


      end block;

    fun_183 : block
        signal x_91            : signed(63 downto 0);
        signal y_82            : signed(63 downto 0);
        signal \c$case_alt_94\ : signed(63 downto 0);
      begin
        acc_3_6 <= \c$case_alt_94\;

        x_91 <= acc_2_12;

        y_82 <= acc_2_13;

        \c$case_alt_94\ <= (x_91 + y_82);


      end block;

    fun_184 : block
        signal x_92            : signed(63 downto 0);
        signal y_83            : signed(63 downto 0);
        signal \c$case_alt_95\ : signed(63 downto 0);
      begin
        acc_3_7 <= \c$case_alt_95\;

        x_92 <= acc_2_14;

        y_83 <= acc_2_15;

        \c$case_alt_95\ <= (x_92 + y_83);


      end block;

    fun_185 : block
        signal x_93            : signed(63 downto 0);
        signal y_84            : signed(63 downto 0);
        signal \c$case_alt_96\ : signed(63 downto 0);
      begin
        acc_3_8 <= \c$case_alt_96\;

        x_93 <= acc_2_16;

        y_84 <= acc_2_17;

        \c$case_alt_96\ <= (x_93 + y_84);


      end block;

    fun_186 : block
        signal x_94            : signed(63 downto 0);
        signal y_85            : signed(63 downto 0);
        signal \c$case_alt_97\ : signed(63 downto 0);
      begin
        acc_3_9 <= \c$case_alt_97\;

        x_94 <= acc_2_18;

        y_85 <= acc_2_19;

        \c$case_alt_97\ <= (x_94 + y_85);


      end block;

    fun_187 : block
        signal x_95            : signed(63 downto 0);
        signal y_86            : signed(63 downto 0);
        signal \c$case_alt_98\ : signed(63 downto 0);
      begin
        acc_3_10 <= \c$case_alt_98\;

        x_95 <= acc_2_20;

        y_86 <= acc_2_21;

        \c$case_alt_98\ <= (x_95 + y_86);


      end block;

    fun_188 : block
        signal x_96            : signed(63 downto 0);
        signal y_87            : signed(63 downto 0);
        signal \c$case_alt_99\ : signed(63 downto 0);
      begin
        acc_3_11 <= \c$case_alt_99\;

        x_96 <= acc_2_22;

        y_87 <= acc_2_23;

        \c$case_alt_99\ <= (x_96 + y_87);


      end block;

    fun_189 : block
        signal x_97             : signed(63 downto 0);
        signal y_88             : signed(63 downto 0);
        signal \c$case_alt_100\ : signed(63 downto 0);
      begin
        acc_4_0 <= \c$case_alt_100\;

        x_97 <= acc_3_0;

        y_88 <= acc_3_1;

        \c$case_alt_100\ <= (x_97 + y_88);


      end block;

    fun_190 : block
        signal x_98             : signed(63 downto 0);
        signal y_89             : signed(63 downto 0);
        signal \c$case_alt_101\ : signed(63 downto 0);
      begin
        acc_4_1 <= \c$case_alt_101\;

        x_98 <= acc_3_2;

        y_89 <= acc_3_3;

        \c$case_alt_101\ <= (x_98 + y_89);


      end block;

    fun_191 : block
        signal x_99             : signed(63 downto 0);
        signal y_90             : signed(63 downto 0);
        signal \c$case_alt_102\ : signed(63 downto 0);
      begin
        acc_4_2 <= \c$case_alt_102\;

        x_99 <= acc_3_4;

        y_90 <= acc_3_5;

        \c$case_alt_102\ <= (x_99 + y_90);


      end block;

    fun_192 : block
        signal x_100            : signed(63 downto 0);
        signal y_91             : signed(63 downto 0);
        signal \c$case_alt_103\ : signed(63 downto 0);
      begin
        acc_4_3 <= \c$case_alt_103\;

        x_100 <= acc_3_6;

        y_91 <= acc_3_7;

        \c$case_alt_103\ <= (x_100 + y_91);


      end block;

    fun_193 : block
        signal x_101            : signed(63 downto 0);
        signal y_92             : signed(63 downto 0);
        signal \c$case_alt_104\ : signed(63 downto 0);
      begin
        acc_4_4 <= \c$case_alt_104\;

        x_101 <= acc_3_8;

        y_92 <= acc_3_9;

        \c$case_alt_104\ <= (x_101 + y_92);


      end block;

    fun_194 : block
        signal x_102            : signed(63 downto 0);
        signal y_93             : signed(63 downto 0);
        signal \c$case_alt_105\ : signed(63 downto 0);
      begin
        acc_4_5 <= \c$case_alt_105\;

        x_102 <= acc_3_10;

        y_93 <= acc_3_11;

        \c$case_alt_105\ <= (x_102 + y_93);


      end block;

    fun_195 : block
        signal x_103            : signed(63 downto 0);
        signal y_94             : signed(63 downto 0);
        signal \c$case_alt_106\ : signed(63 downto 0);
      begin
        acc_5_0 <= \c$case_alt_106\;

        x_103 <= acc_4_0;

        y_94 <= acc_4_1;

        \c$case_alt_106\ <= (x_103 + y_94);


      end block;

    fun_196 : block
        signal x_104            : signed(63 downto 0);
        signal y_95             : signed(63 downto 0);
        signal \c$case_alt_107\ : signed(63 downto 0);
      begin
        acc_5_1 <= \c$case_alt_107\;

        x_104 <= acc_4_2;

        y_95 <= acc_4_3;

        \c$case_alt_107\ <= (x_104 + y_95);


      end block;

    fun_197 : block
        signal x_105            : signed(63 downto 0);
        signal y_96             : signed(63 downto 0);
        signal \c$case_alt_108\ : signed(63 downto 0);
      begin
        acc_5_2 <= \c$case_alt_108\;

        x_105 <= acc_4_4;

        y_96 <= acc_4_5;

        \c$case_alt_108\ <= (x_105 + y_96);


      end block;

    fun_198 : block
        signal x_106            : signed(63 downto 0);
        signal y_97             : signed(63 downto 0);
        signal \c$case_alt_109\ : signed(63 downto 0);
      begin
        acc_6_0 <= \c$case_alt_109\;

        x_106 <= acc_5_0;

        y_97 <= acc_5_1;

        \c$case_alt_109\ <= (x_106 + y_97);


      end block;

    fun_199 : block
        signal x_107            : signed(63 downto 0);
        signal y_98             : signed(63 downto 0);
        signal \c$case_alt_110\ : signed(63 downto 0);
      begin
        acc_6_1 <= \c$case_alt_110\;

        x_107 <= acc_5_2;

        y_98 <= acc_2_24;

        \c$case_alt_110\ <= (x_107 + y_98);


      end block;

    fun_200 : block
        signal x_108            : signed(63 downto 0);
        signal y_99             : signed(63 downto 0);
        signal \c$case_alt_111\ : signed(63 downto 0);
      begin
        acc_7_0 <= \c$case_alt_111\;

        x_108 <= acc_6_0;

        y_99 <= acc_6_1;

        \c$case_alt_111\ <= (x_108 + y_99);


      end block;


  end block;

  result_8 <= t when b else
              result_9;

  -- register begin
  result_9_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_9 <= ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_9 <= result_8;
      end if;
    end if;
  end process;
  -- register end

  b <= result_16;

  t <= ( Tuple2_1_sel0_unsigned => result_15.Tags_sel3_output2_0
       , Tuple2_1_sel1_signed => \c$t_app_arg\ );

  y <= result_12;

  x_projection_99 <=  win(0) ;

  x_0 <= x_projection_99;

  win <= window.Tuple2_2_sel1_array_of_signed_64;

  -- register begin
  window_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window <= ( Tuple2_2_sel0_unsigned => to_unsigned(103,8)
  , Tuple2_2_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
                                                                                , to_signed(0,64)
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

  t_0 <= ( Tuple2_2_sel0_unsigned => result_33.Tags_sel4_slide0
         , Tuple2_2_sel1_array_of_signed_64 => result_11 );

  result_11 <= \c$t_case_alt\ when result_13 else
               \c$t_case_alt_0\;

  \c$t_case_alt_selection_1\ <= x_1;

  \c$vec_0\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win)));

  \c$t_case_alt_sel_alt_0\ <= (\c$vec_0\(0 to 101-1),\c$vec_0\(101 to \c$vec_0\'high));

  \c$vec_1\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated)));

  \c$t_case_alt_sel_alt_2\ <= (\c$vec_1\(0 to 101-1),\c$vec_1\(101 to \c$vec_1\'high));

  \c$t_case_alt\ <= \c$t_case_alt_sel_alt_2\.Tuple2_5_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_1\ else
                    \c$t_case_alt_sel_alt_0\.Tuple2_5_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_4\ <= x_1;

  \c$t_case_alt_0\ <= lastBucketUpdated when \c$t_case_alt_selection_4\ else
                      win;

  -- replace begin
  replaceVec : block
    signal vec_index : integer range 0 to 101-1;
  begin
    vec_index <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 101
    -- pragma translate_on
                 ;

    process(vec_index,win,x_0,y)
      variable ivec : Spec_topEntity_types.array_of_signed_64(0 to 100);
    begin
      ivec := win;
      ivec(vec_index) := ((x_0 + y));
      lastBucketUpdated <= ivec;
    end process;
  end block;
  -- replace end

  b_0 <= x_1 or result_13;

  x_1 <= result_14;

  \c$vec2\ <= (ws(1 to ws'high));

  -- zipWith begin
  zipWith : for i_3 in \c$ws_app_arg\'range generate
  begin
    \c$ws_app_arg\(i_3) <= ( Tuple2_3_sel0_Tuple2_1 => result_22(i_3)
               , Tuple2_3_sel1_Maybe => \c$vec2\(i_3) );


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
    imap_0 : for i_4 in \c$ws_app_arg_0\'range generate
    begin
      fun_201 : block
      signal \c$case_alt_112\           : Spec_topEntity_types.Maybe;
      -- spec.hs:247:1-63
      signal t_8                        : unsigned(7 downto 0);
      -- spec.hs:247:1-63
      signal x_109                      : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res\ : boolean;
    begin
      \c$ws_app_arg_0\(i_4) <= \c$case_alt_112\;

      \c$case_alt_selection_res\ <= t_8 = result_33.Tags_sel2_output1_0;

      \c$case_alt_112\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_4,max(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res\ else
                          \c$ws_app_arg\(i_4).Tuple2_3_sel1_Maybe;

      t_8 <= x_109.Tuple2_1_sel0_unsigned;

      x_109 <= \c$ws_app_arg\(i_4).Tuple2_3_sel0_Tuple2_1;


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
    \c$case_scrut\ <= result_22(vec_index_0);
  end block;
  -- index end

  \c$case_scrut_0\ <=  ws(0) ;

  with (\c$case_scrut_0\(2 downto 2)) select
    result_12 <= to_signed(0,64) when "0",
                 \c$case_scrut\.Tuple2_1_sel1_signed when others;

  i <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_0\(1 downto 0)));

  ws <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_0\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  iterateI_0 : block
    signal iterateI_ho1_0_arg0_0 : boolean;
    signal iterateI_ho1_0_res_0  : boolean;
    signal iterateI_ho1_1_res_0  : boolean;
    signal iterateI_ho1_2_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_0 <= result_43.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_boolean;

    r_block_43 : block
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

    r_block_44 : block
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

    r_block_45 : block
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

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_boolean'( result_43.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_boolean
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
    r_block_46 : block
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

    r_block_47 : block
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

    r_block_48 : block
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
    signal iterateI_ho1_0_arg0_1 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_2  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_2  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_2  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_1 <= ( Tags_sel0_input0 => t_6
                             , Tags_sel1_output0_0 => t_5
                             , Tags_sel2_output1_0 => t_4
                             , Tags_sel3_output2_0 => t_3
                             , Tags_sel4_slide0 => t_4 );

    r_block_49 : block
        signal \c$bb_res_res_10\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_slide0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_10_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_10\ <= iterateI_ho1_0_arg0_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_2 <= \c$bb_res_res_10\;


      end block;

    r_block_50 : block
        signal \c$bb_res_res_11\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_slide0 => to_unsigned(103,8) );
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

    r_block_51 : block
        signal \c$bb_res_res_12\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_slide0 => to_unsigned(103,8) );
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

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_6
                                                        , Tags_sel1_output0_0 => t_5
                                                        , Tags_sel2_output1_0 => t_4
                                                        , Tags_sel3_output2_0 => t_3
                                                        , Tags_sel4_slide0 => t_4 )
                                                        , iterateI_ho1_0_res_2
                                                        , iterateI_ho1_1_res_2
                                                        , iterateI_ho1_2_res_2 );


  end block;

  result_15 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  iterateI_3 : block
    signal iterateI_ho1_0_res_3 : boolean;
    signal iterateI_ho1_1_res_3 : boolean;
    signal iterateI_ho1_2_res_3 : boolean;
    signal iterateI_ho1_3_res_0 : boolean;
  begin
    r_block_52 : block
        signal \c$bb_res_res_13\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_13\;


      end block;

    r_block_53 : block
        signal \c$bb_res_res_14\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_14_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_14\ <= iterateI_ho1_0_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_3 <= \c$bb_res_res_14\;


      end block;

    r_block_54 : block
        signal \c$bb_res_res_15\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_15_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_15\ <= iterateI_ho1_1_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_3 <= \c$bb_res_res_15\;


      end block;

    r_block_55 : block
        signal \c$bb_res_res_16\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_16_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_16\ <= iterateI_ho1_2_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_0 <= \c$bb_res_res_16\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                           , iterateI_ho1_0_res_3
                                                           , iterateI_ho1_1_res_3
                                                           , iterateI_ho1_2_res_3
                                                           , iterateI_ho1_3_res_0 );


  end block;

  result_16 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  \c$vec2_0\ <= (ws_0(1 to ws_0'high));

  -- zipWith begin
  zipWith_0 : for i_6 in \c$ws_app_arg_1\'range generate
  begin
    \c$ws_app_arg_1\(i_6) <= ( Tuple2_3_sel0_Tuple2_1 => result_22(i_6)
               , Tuple2_3_sel1_Maybe => \c$vec2_0\(i_6) );


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
    imap_2 : for i_7 in \c$ws_app_arg_2\'range generate
    begin
      fun_202 : block
      signal \c$case_alt_113\             : Spec_topEntity_types.Maybe;
      -- spec.hs:247:1-63
      signal t_9                          : unsigned(7 downto 0);
      -- spec.hs:247:1-63
      signal x_110                        : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_2\ : boolean;
    begin
      \c$ws_app_arg_2\(i_7) <= \c$case_alt_113\;

      \c$case_alt_selection_res_2\ <= t_9 = result_32.Tags_sel2_output1_0;

      \c$case_alt_113\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_7,max_0(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_2\ else
                          \c$ws_app_arg_1\(i_7).Tuple2_3_sel1_Maybe;

      t_9 <= x_110.Tuple2_1_sel0_unsigned;

      x_110 <= \c$ws_app_arg_1\(i_7).Tuple2_3_sel0_Tuple2_1;


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
    \c$case_scrut_1\ <= result_22(vec_index_1);
  end block;
  -- index end

  \c$case_scrut_2\ <=  ws_0(0) ;

  with (\c$case_scrut_2\(2 downto 2)) select
    result_17 <= to_signed(0,64) when "0",
                 \c$case_scrut_1\.Tuple2_1_sel1_signed when others;

  i_0 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_2\(1 downto 0)));

  ws_0 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_2\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  iterateI_4 : block
    signal iterateI_ho1_0_arg0_2 : boolean;
    signal iterateI_ho1_0_res_4  : boolean;
    signal iterateI_ho1_1_res_4  : boolean;
    signal iterateI_ho1_2_res_4  : boolean;
    signal iterateI_ho1_3_res_1  : boolean;
    signal iterateI_ho1_4_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_2 <= x_6;

    r_block_56 : block
        signal \c$bb_res_res_17\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_17_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_17\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_17\;


      end block;

    r_block_57 : block
        signal \c$bb_res_res_18\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_18_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_18\ <= iterateI_ho1_0_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_4 <= \c$bb_res_res_18\;


      end block;

    r_block_58 : block
        signal \c$bb_res_res_19\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_19_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_19\ <= iterateI_ho1_1_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_4 <= \c$bb_res_res_19\;


      end block;

    r_block_59 : block
        signal \c$bb_res_res_20\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_20_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_20\ <= iterateI_ho1_2_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_1 <= \c$bb_res_res_20\;


      end block;

    r_block_60 : block
        signal \c$bb_res_res_21\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_21_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_21\ <= iterateI_ho1_3_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_0 <= \c$bb_res_res_21\;


      end block;

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_boolean'( x_6
                                                           , iterateI_ho1_0_res_4
                                                           , iterateI_ho1_1_res_4
                                                           , iterateI_ho1_2_res_4
                                                           , iterateI_ho1_3_res_1
                                                           , iterateI_ho1_4_res_0 );


  end block;

  result_18 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  \c$vec2_1\ <= (ws_1(1 to ws_1'high));

  -- zipWith begin
  zipWith_1 : for i_8 in \c$ws_app_arg_3\'range generate
  begin
    \c$ws_app_arg_3\(i_8) <= ( Tuple2_4_sel0_Tuple2_1 => result_26(i_8)
               , Tuple2_4_sel1_Maybe_0 => \c$vec2_1\(i_8) );


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
    imap_4 : for i_9 in \c$ws_app_arg_4\'range generate
    begin
      fun_203 : block
      signal \c$case_alt_114\             : Spec_topEntity_types.Maybe_0;
      -- spec.hs:247:1-63
      signal t_10                         : unsigned(7 downto 0);
      -- spec.hs:247:1-63
      signal x_111                        : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_3\ : boolean;
    begin
      \c$ws_app_arg_4\(i_9) <= \c$case_alt_114\;

      \c$case_alt_selection_res_3\ <= t_10 = result_32.Tags_sel1_output0_0;

      \c$case_alt_114\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_9,max_1(1,integer(ceil(log2(real(4))))))))) when \c$case_alt_selection_res_3\ else
                          \c$ws_app_arg_3\(i_9).Tuple2_4_sel1_Maybe_0;

      t_10 <= x_111.Tuple2_1_sel0_unsigned;

      x_111 <= \c$ws_app_arg_3\(i_9).Tuple2_4_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_1 : block
    signal vec_index_2 : integer range 0 to 4-1;
  begin
    vec_index_2 <= to_integer((signed(std_logic_vector(resize(i_1,64)))))
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
    \c$case_scrut_3\ <= result_26(vec_index_2);
  end block;
  -- index end

  \c$case_scrut_4\ <=  ws_1(0) ;

  with (\c$case_scrut_4\(2 downto 2)) select
    result_19 <= to_signed(0,64) when "0",
                 \c$case_scrut_3\.Tuple2_1_sel1_signed when others;

  i_1 <= Spec_topEntity_types.index_4'(spec_topentity_types.fromSLV(\c$case_scrut_4\(1 downto 0)));

  ws_1 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_4\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "--"))));

  iterateI_5 : block
    signal iterateI_ho1_0_arg0_3 : boolean;
    signal iterateI_ho1_0_res_5  : boolean;
    signal iterateI_ho1_1_res_5  : boolean;
    signal iterateI_ho1_2_res_5  : boolean;
    signal iterateI_ho1_3_res_2  : boolean;
    signal iterateI_ho1_4_res_1  : boolean;
  begin
    iterateI_ho1_0_arg0_3 <= x_7;

    r_block_61 : block
        signal \c$bb_res_res_22\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_22_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_22\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_22\;


      end block;

    r_block_62 : block
        signal \c$bb_res_res_23\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_23_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_23\ <= iterateI_ho1_0_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_5 <= \c$bb_res_res_23\;


      end block;

    r_block_63 : block
        signal \c$bb_res_res_24\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_24_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_24\ <= iterateI_ho1_1_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_5 <= \c$bb_res_res_24\;


      end block;

    r_block_64 : block
        signal \c$bb_res_res_25\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_25_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_25\ <= iterateI_ho1_2_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_2 <= \c$bb_res_res_25\;


      end block;

    r_block_65 : block
        signal \c$bb_res_res_26\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_26_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_26\ <= iterateI_ho1_3_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_1 <= \c$bb_res_res_26\;


      end block;

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_boolean'( x_7
                                                           , iterateI_ho1_0_res_5
                                                           , iterateI_ho1_1_res_5
                                                           , iterateI_ho1_2_res_5
                                                           , iterateI_ho1_3_res_2
                                                           , iterateI_ho1_4_res_1 );


  end block;

  result_20 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  result_selection_8 <= x_2;

  result_21 <= t_1 when result_selection_8 else
               result_22;

  x_2 <= result_24;

  -- register begin
  result_22_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_22 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_22 <= result_21;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_2\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_22) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => result_34.Tags_sel2_output1_0
             , Tuple2_1_sel1_signed => (x_3 + y_0) )))));

  t_projection_2 <= (\c$vec_2\(0 to 1-1),\c$vec_2\(1 to \c$vec_2\'high));

  t_1 <= t_projection_2.Tuple2_6_sel1_array_of_Tuple2_1_1;

  x_3 <= \c$out1_case_alt\;

  y_0 <= result_23;

  \c$vec2_2\ <= (ws_2(1 to ws_2'high));

  -- zipWith begin
  zipWith_2 : for i_10 in \c$ws_app_arg_5\'range generate
  begin
    \c$ws_app_arg_5\(i_10) <= ( Tuple2_4_sel0_Tuple2_1 => result_26(i_10)
               , Tuple2_4_sel1_Maybe_0 => \c$vec2_2\(i_10) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_5 : block
    function max_2 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_6 : for i_11 in \c$ws_app_arg_6\'range generate
    begin
      fun_204 : block
      signal \c$case_alt_115\             : Spec_topEntity_types.Maybe_0;
      -- spec.hs:247:1-63
      signal t_11                         : unsigned(7 downto 0);
      -- spec.hs:247:1-63
      signal x_112                        : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_4\ : boolean;
    begin
      \c$ws_app_arg_6\(i_11) <= \c$case_alt_115\;

      \c$case_alt_selection_res_4\ <= t_11 = result_34.Tags_sel1_output0_0;

      \c$case_alt_115\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_11,max_2(1,integer(ceil(log2(real(4))))))))) when \c$case_alt_selection_res_4\ else
                          \c$ws_app_arg_5\(i_11).Tuple2_4_sel1_Maybe_0;

      t_11 <= x_112.Tuple2_1_sel0_unsigned;

      x_112 <= \c$ws_app_arg_5\(i_11).Tuple2_4_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_2 : block
    signal vec_index_3 : integer range 0 to 4-1;
  begin
    vec_index_3 <= to_integer((signed(std_logic_vector(resize(i_2,64)))))
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
    \c$case_scrut_5\ <= result_26(vec_index_3);
  end block;
  -- index end

  \c$case_scrut_6\ <=  ws_2(0) ;

  with (\c$case_scrut_6\(2 downto 2)) select
    result_23 <= to_signed(0,64) when "0",
                 \c$case_scrut_5\.Tuple2_1_sel1_signed when others;

  i_2 <= Spec_topEntity_types.index_4'(spec_topentity_types.fromSLV(\c$case_scrut_6\(1 downto 0)));

  ws_2 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_6\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "--"))));

  \c$out1_case_alt_selection_res\ <= winTag = tagToMatch;

  \c$out1_case_alt\ <= winData when \c$out1_case_alt_selection_res\ else
                       to_signed(0,64);

  tagToMatch <= result_34.Tags_sel0_input0;

  iterateI_6 : block
    signal iterateI_ho1_0_res_6 : boolean;
    signal iterateI_ho1_1_res_6 : boolean;
  begin
    r_block_66 : block
        signal \c$bb_res_res_27\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_27_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_27\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_27\;


      end block;

    r_block_67 : block
        signal \c$bb_res_res_28\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_28_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_28\ <= iterateI_ho1_0_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_6 <= \c$bb_res_res_28\;


      end block;

    \c$app_arg_6\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                           , iterateI_ho1_0_res_6
                                                           , iterateI_ho1_1_res_6 );


  end block;

  result_24 <=  \c$app_arg_6\(\c$app_arg_6\'high) ;

  result_selection_12 <= x_4;

  result_25 <= t_2 when result_selection_12 else
               result_26;

  x_4 <= result_28;

  -- register begin
  result_26_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_26 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_26 <= result_25;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_3\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_26) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => t_5
             , Tuple2_1_sel1_signed => (x_5 + to_signed(1,64)) )))));

  t_projection_4 <= (\c$vec_3\(0 to 1-1),\c$vec_3\(1 to \c$vec_3\'high));

  t_2 <= t_projection_4.Tuple2_7_sel1_array_of_Tuple2_1_1;

  x_5 <= \c$out0_case_alt\;

  \c$out0_case_alt_selection_res\ <= result_27 = winTag;

  \c$out0_case_alt\ <= winData when \c$out0_case_alt_selection_res\ else
                       to_signed(0,64);

  winData <= result_30.Tuple2_1_sel1_signed;

  winTag <= result_30.Tuple2_1_sel0_unsigned;

  result_selection_res <= t_6 > to_unsigned(1,8);

  result_27 <= \c$app_arg_7\ when result_selection_res else
               \c$app_arg_7\ + to_unsigned(102,8);

  \c$app_arg_7\ <= t_6 - to_unsigned(1,8);

  iterateI_7 : block
    signal iterateI_ho1_0_res_7 : boolean;
  begin
    r_block_68 : block
        signal \c$bb_res_res_29\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_29_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_29\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_29\;


      end block;

    \c$app_arg_8\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                           , iterateI_ho1_0_res_7 );


  end block;

  result_28 <=  \c$app_arg_8\(\c$app_arg_8\'high) ;

  result_29 <= ( Tuple2_1_sel0_unsigned => t_6
               , Tuple2_1_sel1_signed => \c$input0Win_app_arg\ ) when b_1 else
               result_30;

  -- register begin
  result_30_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_30 <= ( Tuple2_1_sel0_unsigned => to_unsigned(103,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_30 <= result_29;
      end if;
    end if;
  end process;
  -- register end

  b_1 <= result_31;

  -- delay begin
  cinput0Win_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$input0Win_app_arg\ <= result_43.Tuple3_sel2_Tuple3_0.Tuple3_0_sel0_ValidInt.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_8 : block
    signal iterateI_ho1_0_res_8 : boolean;
  begin
    r_block_69 : block
        signal \c$bb_res_res_30\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_30_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_30\ <= pIn0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_8 <= \c$bb_res_res_30\;


      end block;

    \c$app_arg_9\ <= Spec_topEntity_types.array_of_boolean'( pIn0_0
                                                           , iterateI_ho1_0_res_8 );


  end block;

  result_31 <=  \c$app_arg_9\(\c$app_arg_9\'high) ;

  iterateI_9 : block
    signal iterateI_ho1_0_arg0_4 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_9  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_7  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_6  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_3  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_4 <= ( Tags_sel0_input0 => t_6
                             , Tags_sel1_output0_0 => t_5
                             , Tags_sel2_output1_0 => t_4
                             , Tags_sel3_output2_0 => t_3
                             , Tags_sel4_slide0 => t_4 );

    r_block_70 : block
        signal \c$bb_res_res_31\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_slide0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_31_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_31\ <= iterateI_ho1_0_arg0_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_9 <= \c$bb_res_res_31\;


      end block;

    r_block_71 : block
        signal \c$bb_res_res_32\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_slide0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_32_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_32\ <= iterateI_ho1_0_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_7 <= \c$bb_res_res_32\;


      end block;

    r_block_72 : block
        signal \c$bb_res_res_33\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_slide0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_33_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_33\ <= iterateI_ho1_1_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_6 <= \c$bb_res_res_33\;


      end block;

    r_block_73 : block
        signal \c$bb_res_res_34\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_slide0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_34_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_34\ <= iterateI_ho1_2_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_3 <= \c$bb_res_res_34\;


      end block;

    \c$app_arg_10\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_6
                                                         , Tags_sel1_output0_0 => t_5
                                                         , Tags_sel2_output1_0 => t_4
                                                         , Tags_sel3_output2_0 => t_3
                                                         , Tags_sel4_slide0 => t_4 )
                                                         , iterateI_ho1_0_res_9
                                                         , iterateI_ho1_1_res_7
                                                         , iterateI_ho1_2_res_6
                                                         , iterateI_ho1_3_res_3 );


  end block;

  result_32 <=  \c$app_arg_10\(\c$app_arg_10\'high) ;

  iterateI_10 : block
    signal iterateI_ho1_0_arg0_5 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_10 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_8  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_5 <= ( Tags_sel0_input0 => t_6
                             , Tags_sel1_output0_0 => t_5
                             , Tags_sel2_output1_0 => t_4
                             , Tags_sel3_output2_0 => t_3
                             , Tags_sel4_slide0 => t_4 );

    r_block_74 : block
        signal \c$bb_res_res_35\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_slide0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_35_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_35\ <= iterateI_ho1_0_arg0_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_10 <= \c$bb_res_res_35\;


      end block;

    r_block_75 : block
        signal \c$bb_res_res_36\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_slide0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_36_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_36\ <= iterateI_ho1_0_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_8 <= \c$bb_res_res_36\;


      end block;

    \c$app_arg_11\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_6
                                                         , Tags_sel1_output0_0 => t_5
                                                         , Tags_sel2_output1_0 => t_4
                                                         , Tags_sel3_output2_0 => t_3
                                                         , Tags_sel4_slide0 => t_4 )
                                                         , iterateI_ho1_0_res_10
                                                         , iterateI_ho1_1_res_8 );


  end block;

  result_33 <=  \c$app_arg_11\(\c$app_arg_11\'high) ;

  iterateI_11 : block
    signal iterateI_ho1_0_arg0_6 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_11 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_6 <= ( Tags_sel0_input0 => t_6
                             , Tags_sel1_output0_0 => t_5
                             , Tags_sel2_output1_0 => t_4
                             , Tags_sel3_output2_0 => t_3
                             , Tags_sel4_slide0 => t_4 );

    r_block_76 : block
        signal \c$bb_res_res_37\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(103,8)
      , Tags_sel1_output0_0 => to_unsigned(103,8)
      , Tags_sel2_output1_0 => to_unsigned(103,8)
      , Tags_sel3_output2_0 => to_unsigned(103,8)
      , Tags_sel4_slide0 => to_unsigned(103,8) );
      begin
        -- delay begin
        cbb_res_res_37_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_37\ <= iterateI_ho1_0_arg0_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_11 <= \c$bb_res_res_37\;


      end block;

    \c$app_arg_12\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_6
                                                         , Tags_sel1_output0_0 => t_5
                                                         , Tags_sel2_output1_0 => t_4
                                                         , Tags_sel3_output2_0 => t_3
                                                         , Tags_sel4_slide0 => t_4 )
                                                         , iterateI_ho1_0_res_11 );


  end block;

  result_34 <=  \c$app_arg_12\(\c$app_arg_12\'high) ;

  -- register begin
  t_3_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_3 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_3 <= result_35;
      end if;
    end if;
  end process;
  -- register end

  result_selection_17 <= pOut2;

  result_35 <= result_36 when result_selection_17 else
               t_3;

  b_2 <= t_3 = to_unsigned(102,8);

  f1 <= t_3 + to_unsigned(1,8);

  result_36 <= to_unsigned(1,8) when b_2 else
               f1;

  pOut2 <= pacings.Pacings_1_sel3_pacingOut2;

  -- register begin
  t_4_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_4 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_4 <= result_37;
      end if;
    end if;
  end process;
  -- register end

  result_selection_21 <= x_6;

  result_37 <= result_38 when result_selection_21 else
               t_4;

  b_3 <= t_4 = to_unsigned(102,8);

  f1_0 <= t_4 + to_unsigned(1,8);

  result_38 <= to_unsigned(1,8) when b_3 else
               f1_0;

  x_6 <= pOut1;

  pOut1 <= pacings.Pacings_1_sel2_pacingOut1;

  -- register begin
  t_5_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_5 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_5 <= result_39;
      end if;
    end if;
  end process;
  -- register end

  result_selection_25 <= x_7;

  result_39 <= result_40 when result_selection_25 else
               t_5;

  b_4 <= t_5 = to_unsigned(102,8);

  f1_1 <= t_5 + to_unsigned(1,8);

  result_40 <= to_unsigned(1,8) when b_4 else
               f1_1;

  x_7 <= pOut0;

  pOut0 <= pacings.Pacings_1_sel1_pacingOut0;

  -- register begin
  t_6_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_6 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_6 <= result_41;
      end if;
    end if;
  end process;
  -- register end

  result_selection_29 <= pIn0_0;

  result_41 <= result_42 when result_selection_29 else
               t_6;

  b_5 <= t_6 = to_unsigned(102,8);

  f1_2 <= t_6 + to_unsigned(1,8);

  result_42 <= to_unsigned(1,8) when b_5 else
               f1_2;

  pIn0_0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_43.Tuple3_sel2_Tuple3_0.Tuple3_0_sel2_Pacings_1;

  result_43 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_15\
               , Tuple3_sel1_boolean_1 => \c$app_arg_14\
               , Tuple3_sel2_Tuple3_0 => \c$app_arg_13\ );

  -- register begin
  capp_arg_13_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_13\ <= ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Tuple3_0_sel1_boolean => false
  , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingOut0 => false
  , Pacings_1_sel2_pacingOut1 => false
  , Pacings_1_sel3_pacingOut2 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_13\ <= \c$case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt\ <= \c$case_alt_0\ when push else
                  \c$case_alt_1\;

  \c$case_alt_0\ <= \c$case_alt_2\ when pop else
                    ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple3_0_sel1_boolean => false
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false
                    , Pacings_1_sel2_pacingOut1 => false
                    , Pacings_1_sel3_pacingOut2 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple3_0_sel1_boolean => false
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false
                    , Pacings_1_sel2_pacingOut1 => false
                    , Pacings_1_sel3_pacingOut2 => false ) );

  with (x_8) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_8) select
    \c$case_alt_3\ <= ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Tuple3_0_sel1_boolean => false
                      , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingOut0 => false
                      , Pacings_1_sel2_pacingOut1 => false
                      , Pacings_1_sel3_pacingOut2 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec_3 : block
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
  capp_arg_14_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_14\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_14\ <= \c$case_alt_5\;
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

  with (x_8) select
    \c$case_alt_9\ <= false when x"0000000000000000",
                      true when others;

  -- register begin
  capp_arg_15_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_15\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_15\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_16\);

  \c$app_arg_16\ <= to_signed(1,64) when x_8 /= to_signed(2,64) else to_signed(0,64);

  -- register begin
  buffer_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \buffer\ <= Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple3_0_sel1_boolean => false
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false ) )
                                         , ( Tuple3_0_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple3_0_sel1_boolean => false
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false ) ) );
    elsif rising_edge(clk) then
      if en then
        \buffer\ <= \c$buffer_case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$buffer_case_alt\ <= \c$buffer_case_alt_0\ when push else
                         \buffer\;

  \c$vec_4\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec_4\(0 to 2-1),\c$vec_4\(2 to \c$vec_4\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_8_sel0_array_of_Tuple3_0_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_5\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_5\(0 to 2-1),\c$vec_5\(2 to \c$vec_5\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_8_sel0_array_of_Tuple3_0_0;

  qData <= result_5.Tuple2_sel1_Tuple3_0;

  x_8 <= cursor;

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
                           (x_8 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_8 = to_signed(2,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_6.Tuple2_0_sel0_boolean;

  with (x_8) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_8 - to_signed(1,64));

  result <= result_6.Tuple2_0_sel1_Outputs;

  result_0 <= result.Outputs_sel0_output0;

  result_1 <= result.Outputs_sel1_output1;

  result_2 <= result.Outputs_sel2_output2;

  result_0_0 <= result_0.ValidInt_sel0_value;

  result_0_1 <= result_0.ValidInt_sel1_valid;

  result_1_0 <= result_1.ValidInt_sel0_value;

  result_1_1 <= result_1.ValidInt_sel1_valid;

  result_2_0 <= result_2.ValidInt_sel0_value;

  result_2_1 <= result_2.ValidInt_sel1_valid;


end;

