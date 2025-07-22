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
       result_1_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_2                      : signed(63 downto 0);
  signal x                             : signed(63 downto 0);
  signal result_3                      : signed(63 downto 0) := (to_signed(0,64));
  signal result_4                      : Spec_topEntity_types.Tuple2;
  -- spec.hs:(226,1)-(228,31)
  signal pIn0                          : boolean;
  -- spec.hs:(226,1)-(228,31)
  signal pIn1                          : boolean;
  -- spec.hs:(226,1)-(228,31)
  signal timer0Over                    : boolean;
  signal result_5                      : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                   : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_6                      : boolean;
  -- spec.hs:(490,1)-(494,28)
  signal \c$t_app_arg\                 : signed(63 downto 0);
  signal result_7                      : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(490,1)-(494,28)
  signal result_8                      : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:(490,1)-(494,28)
  signal b                             : boolean;
  -- spec.hs:(490,1)-(494,28)
  signal t                             : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(513,1)-(518,37)
  signal x_0                           : signed(63 downto 0);
  -- spec.hs:(513,1)-(518,37)
  signal win                           : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:(513,1)-(518,37)
  signal window                        : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(13,8)
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
                                                                              , to_signed(0,64) ) );
  signal result_9                      : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:(513,1)-(518,37)
  signal t_0                           : Spec_topEntity_types.Tuple2_2;
  signal result_10                     : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:(513,1)-(518,37)
  signal \c$t_case_alt\                : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:(513,1)-(518,37)
  signal \c$t_case_alt_0\              : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:(513,1)-(518,37)
  signal lastBucketUpdated             : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:(513,1)-(518,37)
  signal b_0                           : boolean;
  -- spec.hs:(513,1)-(518,37)
  signal \c$b_case_alt\                : boolean;
  -- spec.hs:84:34-42
  signal x1                            : boolean;
  -- spec.hs:84:34-42
  signal x0                            : boolean;
  -- spec.hs:282:1-63
  signal \c$ws_app_arg\                : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:282:1-63
  signal \c$ws_app_arg_0\              : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut\                : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_0\              : Spec_topEntity_types.Maybe;
  signal result_11                     : signed(63 downto 0);
  -- spec.hs:282:1-63
  signal i                             : Spec_topEntity_types.index_3;
  -- spec.hs:282:1-63
  signal ws                            : Spec_topEntity_types.array_of_Maybe(0 to 3);
  signal \c$app_arg_0\                 : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_12                     : boolean;
  signal \c$app_arg_1\                 : Spec_topEntity_types.array_of_PacingOut0(0 to 3);
  signal result_13                     : Spec_topEntity_types.PacingOut0;
  signal \c$app_arg_2\                 : Spec_topEntity_types.array_of_Tags(0 to 3);
  signal result_14                     : Spec_topEntity_types.Tags;
  signal \c$app_arg_3\                 : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_15                     : boolean;
  -- spec.hs:282:1-63
  signal \c$ws_app_arg_1\              : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:282:1-63
  signal \c$ws_app_arg_2\              : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut_1\              : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_2\              : Spec_topEntity_types.Maybe;
  signal result_16                     : signed(63 downto 0);
  -- spec.hs:282:1-63
  signal i_0                           : Spec_topEntity_types.index_3;
  -- spec.hs:282:1-63
  signal ws_0                          : Spec_topEntity_types.array_of_Maybe(0 to 3);
  signal \c$app_arg_4\                 : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_17                     : boolean;
  signal \c$app_arg_5\                 : Spec_topEntity_types.array_of_Tags(0 to 4);
  signal result_18                     : Spec_topEntity_types.Tags;
  signal y                             : signed(63 downto 0);
  -- spec.hs:274:1-65
  signal \c$ws_app_arg_3\              : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:274:1-65
  signal \c$ws_app_arg_4\              : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut_3\              : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_4\              : Spec_topEntity_types.Maybe;
  signal result_19                     : signed(63 downto 0);
  signal result_20                     : unsigned(7 downto 0);
  signal \c$app_arg_6\                 : unsigned(7 downto 0);
  -- spec.hs:274:1-65
  signal i_1                           : Spec_topEntity_types.index_3;
  -- spec.hs:274:1-65
  signal ws_1                          : Spec_topEntity_types.array_of_Maybe(0 to 3);
  signal result_21                     : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  -- spec.hs:84:34-42
  signal x0_0                          : boolean;
  -- spec.hs:84:34-42
  signal x1_0                          : boolean;
  -- spec.hs:(475,1)-(480,36)
  signal result_22                     : Spec_topEntity_types.array_of_Tuple2_1(0 to 2) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(475,1)-(480,36)
  signal t_1                           : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  signal x_1                           : signed(63 downto 0);
  signal y_0                           : signed(63 downto 0);
  -- spec.hs:274:1-65
  signal eta3                          : signed(63 downto 0);
  -- spec.hs:274:1-65
  signal \c$ws_app_arg_5\              : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:274:1-65
  signal \c$ws_app_arg_6\              : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut_5\              : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_6\              : Spec_topEntity_types.Maybe;
  signal result_23                     : signed(63 downto 0);
  signal result_24                     : unsigned(7 downto 0);
  signal \c$app_arg_7\                 : unsigned(7 downto 0);
  -- spec.hs:274:1-65
  signal i_2                           : Spec_topEntity_types.index_3;
  -- spec.hs:274:1-65
  signal ws_2                          : Spec_topEntity_types.array_of_Maybe(0 to 3);
  signal x_2                           : signed(63 downto 0);
  -- spec.hs:274:1-65
  signal \c$ws_app_arg_7\              : Spec_topEntity_types.array_of_Tuple2_4(0 to 1);
  -- spec.hs:274:1-65
  signal \c$ws_app_arg_8\              : Spec_topEntity_types.array_of_Maybe_0(0 to 1);
  signal \c$case_scrut_7\              : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_8\              : Spec_topEntity_types.Maybe_0;
  signal result_25                     : signed(63 downto 0);
  signal result_26                     : unsigned(7 downto 0);
  signal \c$app_arg_8\                 : unsigned(7 downto 0);
  -- spec.hs:274:1-65
  signal i_3                           : Spec_topEntity_types.index_2;
  -- spec.hs:274:1-65
  signal ws_3                          : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  -- spec.hs:274:1-65
  signal \c$ws_app_arg_9\              : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:274:1-65
  signal \c$ws_app_arg_10\             : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut_9\              : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_10\             : Spec_topEntity_types.Maybe;
  signal result_27                     : signed(63 downto 0);
  signal result_28                     : unsigned(7 downto 0);
  signal \c$app_arg_9\                 : unsigned(7 downto 0);
  -- spec.hs:274:1-65
  signal i_4                           : Spec_topEntity_types.index_3;
  -- spec.hs:274:1-65
  signal ws_4                          : Spec_topEntity_types.array_of_Maybe(0 to 3);
  -- spec.hs:282:1-63
  signal \c$ws_app_arg_11\             : Spec_topEntity_types.array_of_Tuple2_4(0 to 1);
  -- spec.hs:282:1-63
  signal \c$ws_app_arg_12\             : Spec_topEntity_types.array_of_Maybe_0(0 to 1);
  signal \c$case_scrut_11\             : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_12\             : Spec_topEntity_types.Maybe_0;
  signal result_29                     : signed(63 downto 0);
  -- spec.hs:282:1-63
  signal i_5                           : Spec_topEntity_types.index_2;
  -- spec.hs:282:1-63
  signal ws_5                          : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal \c$app_arg_10\                : Spec_topEntity_types.array_of_PacingOut0(0 to 2);
  signal result_30                     : Spec_topEntity_types.PacingOut0;
  signal result_31                     : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:(462,1)-(466,36)
  signal result_32                     : Spec_topEntity_types.array_of_Tuple2_1(0 to 1) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(462,1)-(466,36)
  signal b_1                           : boolean;
  -- spec.hs:(462,1)-(466,36)
  signal t_2                           : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:(326,1)-(328,33)
  signal \c$input1Win_app_arg\         : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_11\                : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_33                     : boolean;
  signal result_34                     : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  -- spec.hs:(450,1)-(454,36)
  signal result_35                     : Spec_topEntity_types.array_of_Tuple2_1(0 to 2) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(450,1)-(454,36)
  signal b_2                           : boolean;
  -- spec.hs:(450,1)-(454,36)
  signal t_3                           : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  -- spec.hs:(326,1)-(328,33)
  signal \c$input0Win_app_arg\         : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(326,1)-(328,33)
  signal inputs_2                      : Spec_topEntity_types.Inputs_3;
  signal \c$app_arg_12\                : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_36                     : boolean;
  signal \c$app_arg_13\                : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_37                     : Spec_topEntity_types.Tags;
  signal \c$app_arg_14\                : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_38                     : Spec_topEntity_types.Tags;
  -- spec.hs:(438,9)-(440,29)
  signal t_4                           : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_39                     : unsigned(7 downto 0);
  -- spec.hs:(438,9)-(440,29)
  signal b_3                           : boolean;
  -- spec.hs:(438,9)-(440,29)
  signal f1                            : unsigned(7 downto 0);
  signal result_40                     : unsigned(7 downto 0);
  -- spec.hs:(326,1)-(328,33)
  signal pOut1                         : boolean;
  -- spec.hs:(438,9)-(440,29)
  signal t_5                           : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_41                     : unsigned(7 downto 0);
  -- spec.hs:(438,9)-(440,29)
  signal b_4                           : boolean;
  -- spec.hs:(438,9)-(440,29)
  signal f1_0                          : unsigned(7 downto 0);
  signal result_42                     : unsigned(7 downto 0);
  -- spec.hs:(326,1)-(328,33)
  signal \c$ds3_case_alt\              : boolean;
  -- spec.hs:84:34-42
  signal x1_1                          : boolean;
  -- spec.hs:84:34-42
  signal x0_1                          : boolean;
  -- spec.hs:(326,1)-(328,33)
  signal pOut0                         : Spec_topEntity_types.PacingOut0;
  -- spec.hs:(438,9)-(440,29)
  signal t_6                           : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_43                     : unsigned(7 downto 0);
  -- spec.hs:(438,9)-(440,29)
  signal b_5                           : boolean;
  -- spec.hs:(438,9)-(440,29)
  signal f1_1                          : unsigned(7 downto 0);
  signal result_44                     : unsigned(7 downto 0);
  -- spec.hs:(326,1)-(328,33)
  signal pIn1_0                        : boolean;
  -- spec.hs:(438,9)-(440,29)
  signal t_7                           : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_45                     : unsigned(7 downto 0);
  -- spec.hs:(438,9)-(440,29)
  signal b_6                           : boolean;
  -- spec.hs:(438,9)-(440,29)
  signal f1_2                          : unsigned(7 downto 0);
  signal result_46                     : unsigned(7 downto 0);
  -- spec.hs:(326,1)-(328,33)
  signal pIn0_0                        : boolean;
  -- spec.hs:(326,1)-(328,33)
  signal pacings                       : Spec_topEntity_types.Pacings_1;
  signal result_47                     : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_15\                : Spec_topEntity_types.Tuple3_0 := ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false ) )
, Tuple3_0_sel1_boolean => false
, Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingIn1 => false
, Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
, PacingOut0_sel1_boolean_1 => false )
, Pacings_1_sel3_pacingOut1 => false ) );
  signal \c$case_alt\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_0\                : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_1\                : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_2\                : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_3\                : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_4\                : Spec_topEntity_types.Tuple3_0;
  signal \c$app_arg_16\                : boolean := false;
  signal \c$case_alt_5\                : boolean;
  signal \c$case_alt_6\                : boolean;
  signal \c$case_alt_7\                : boolean;
  signal \c$case_alt_8\                : boolean;
  signal \c$case_alt_9\                : boolean;
  signal \c$app_arg_17\                : boolean := false;
  signal \c$case_alt_10\               : boolean;
  signal \c$case_alt_11\               : boolean;
  signal \c$case_alt_12\               : boolean;
  signal \c$app_arg_18\                : signed(63 downto 0);
  -- spec.hs:(143,1)-(145,25)
  signal \buffer\                      : Spec_topEntity_types.array_of_Tuple3_0(0 to 1) := Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_boolean => false
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                                       , PacingOut0_sel1_boolean_1 => false )
                                       , Pacings_1_sel3_pacingOut1 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_boolean => false
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                                       , PacingOut0_sel1_boolean_1 => false )
                                       , Pacings_1_sel3_pacingOut1 => false ) ) );
  -- spec.hs:(143,1)-(145,25)
  signal \c$buffer_case_alt\           : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(143,1)-(145,25)
  signal \c$buffer_case_alt_0\         : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(143,1)-(145,25)
  signal \c$buffer_case_alt_1\         : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(143,1)-(145,25)
  signal qData                         : Spec_topEntity_types.Tuple3_0;
  -- spec.hs:(143,1)-(145,25)
  signal x_3                           : signed(63 downto 0);
  -- spec.hs:(143,1)-(145,25)
  signal cursor                        : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(143,1)-(145,25)
  signal \c$cursor_case_alt\           : signed(63 downto 0);
  -- spec.hs:(143,1)-(145,25)
  signal push                          : boolean;
  -- spec.hs:(143,1)-(145,25)
  signal \c$cursor_case_alt_0\         : signed(63 downto 0);
  -- spec.hs:(143,1)-(145,25)
  signal \c$cursor_case_alt_1\         : signed(63 downto 0);
  -- spec.hs:(143,1)-(145,25)
  signal \c$cursor_case_scrut\         : boolean;
  -- spec.hs:(143,1)-(145,25)
  signal \c$cursor_app_arg\            : signed(63 downto 0);
  -- spec.hs:(143,1)-(145,25)
  signal \c$cursor_case_alt_2\         : signed(63 downto 0);
  -- spec.hs:(143,1)-(145,25)
  signal pop                           : boolean;
  -- spec.hs:(143,1)-(145,25)
  signal \c$cursor_case_alt_3\         : signed(63 downto 0);
  -- spec.hs:(143,1)-(145,25)
  signal \c$cursor_case_alt_4\         : signed(63 downto 0);
  signal inputs                        : Spec_topEntity_types.Inputs_3;
  signal \c$vec\                       : Spec_topEntity_types.array_of_signed_64(0 to 10);
  signal x_projection_9                : signed(63 downto 0);
  signal \c$vec_0\                     : Spec_topEntity_types.array_of_signed_64(0 to 11);
  signal \c$t_case_alt_sel_alt_0\      : Spec_topEntity_types.Tuple2_5;
  signal \c$vec_1\                     : Spec_topEntity_types.array_of_signed_64(0 to 11);
  signal \c$t_case_alt_sel_alt_2\      : Spec_topEntity_types.Tuple2_5;
  signal \c$vec2\                      : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$vec2_0\                    : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$vec2_1\                    : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal result_selection_res          : boolean;
  signal result_selection_res_2        : boolean;
  signal \c$vec_2\                     : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  signal t_projection_2                : Spec_topEntity_types.Tuple2_6;
  signal \c$vec2_2\                    : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal result_selection_res_3        : boolean;
  signal \c$vec2_3\                    : Spec_topEntity_types.array_of_Maybe_0(0 to 1);
  signal result_selection_res_4        : boolean;
  signal \c$vec2_4\                    : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal result_selection_res_5        : boolean;
  signal \c$vec2_5\                    : Spec_topEntity_types.array_of_Maybe_0(0 to 1);
  signal \c$vec_3\                     : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  signal t_projection_7                : Spec_topEntity_types.Tuple2_7;
  signal \c$vec_4\                     : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  signal t_projection_8                : Spec_topEntity_types.Tuple2_6;
  signal result_selection_19           : boolean;
  signal result_selection_25           : boolean;
  signal result_selection_29           : boolean;
  signal \c$vec_5\                     : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_3\ : Spec_topEntity_types.Tuple2_8;
  signal \c$vec_6\                     : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_5\ : Spec_topEntity_types.Tuple2_8;
  signal result                        : Spec_topEntity_types.Outputs;
  signal result_0                      : Spec_topEntity_types.ValidInt;
  signal result_1                      : Spec_topEntity_types.ValidInt;

begin
  inputs <= ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => inputs_0_0
            , ValidInt_sel1_valid => inputs_0_1 )
            , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => inputs_1_0
            , ValidInt_sel1_valid => inputs_1_1 ) );

  result_2 <= to_signed(2000,64) when timer0Over else
              (x + to_signed(2000,64));

  x <= result_3;

  -- register begin
  result_3_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_3 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_3 <= result_2;
      end if;
    end if;
  end process;
  -- register end

  result_4 <= ( Tuple2_sel0_boolean => inputs.Inputs_3_sel0_input0.ValidInt_sel1_valid or (inputs.Inputs_3_sel1_input1.ValidInt_sel1_valid or timer0Over)
              , Tuple2_sel1_Tuple3_0 => ( Tuple3_0_sel0_Inputs_3 => inputs
              , Tuple3_0_sel1_boolean => timer0Over
              , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => pIn0
              , Pacings_1_sel1_pacingIn1 => pIn1
              , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => pIn0
              , PacingOut0_sel1_boolean_1 => pIn1 )
              , Pacings_1_sel3_pacingOut1 => timer0Over ) ) );

  pIn0 <= inputs.Inputs_3_sel0_input0.ValidInt_sel1_valid;

  pIn1 <= inputs.Inputs_3_sel1_input1.ValidInt_sel1_valid;

  timer0Over <= result_3 >= (to_signed(1000000,64));

  result_5 <= ( Tuple2_0_sel0_boolean => true
              , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidInt_sel0_value => result_16
              , ValidInt_sel1_valid => result_17 )
              , Outputs_sel1_output1 => ( ValidInt_sel0_value => result_8.Tuple2_1_sel1_signed
              , ValidInt_sel1_valid => result_6 ) ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
    signal iterateI_ho1_4_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut1;

    r_block_33 : block
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

    r_block_34 : block
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

    r_block_35 : block
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

    r_block_36 : block
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

    r_block_37 : block
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

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res
                                                         , iterateI_ho1_3_res
                                                         , iterateI_ho1_4_res );


  end block;

  result_6 <=  \c$app_arg\(\c$app_arg\'high) ;

  \c$vec\ <= window.Tuple2_2_sel1_array_of_signed_64;

  fold : block
    signal vec     : Spec_topEntity_types.array_of_signed_64(0 to 9);
    signal acc_4_0 : signed(63 downto 0);
    signal acc_1   : signed(63 downto 0);
    signal acc_2   : signed(63 downto 0);
    signal acc_3   : signed(63 downto 0);
    signal acc_4   : signed(63 downto 0);
    signal acc_5   : signed(63 downto 0);
    signal acc_6   : signed(63 downto 0);
    signal acc_7   : signed(63 downto 0);
    signal acc_8   : signed(63 downto 0);
    signal acc_9   : signed(63 downto 0);
    signal acc_10  : signed(63 downto 0);
    signal acc_1_0 : signed(63 downto 0);
    signal acc_1_1 : signed(63 downto 0);
    signal acc_1_2 : signed(63 downto 0);
    signal acc_1_3 : signed(63 downto 0);
    signal acc_2_0 : signed(63 downto 0);
    signal acc_2_1 : signed(63 downto 0);
    signal acc_3_0 : signed(63 downto 0);
    signal acc_1_4 : signed(63 downto 0);
  begin
    \c$t_app_arg\ <= acc_4_0;

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

    fun_15 : block
        signal x_5             : signed(63 downto 0);
        signal y_1             : signed(63 downto 0);
        signal \c$case_alt_13\ : signed(63 downto 0);
      begin
        acc_1_0 <= \c$case_alt_13\;

        x_5 <= acc_1;

        y_1 <= acc_2;

        \c$case_alt_13\ <= (x_5 + y_1);


      end block;

    fun_16 : block
        signal x_6             : signed(63 downto 0);
        signal y_2             : signed(63 downto 0);
        signal \c$case_alt_14\ : signed(63 downto 0);
      begin
        acc_1_1 <= \c$case_alt_14\;

        x_6 <= acc_3;

        y_2 <= acc_4;

        \c$case_alt_14\ <= (x_6 + y_2);


      end block;

    fun_17 : block
        signal x_7             : signed(63 downto 0);
        signal y_3             : signed(63 downto 0);
        signal \c$case_alt_15\ : signed(63 downto 0);
      begin
        acc_1_2 <= \c$case_alt_15\;

        x_7 <= acc_5;

        y_3 <= acc_6;

        \c$case_alt_15\ <= (x_7 + y_3);


      end block;

    fun_18 : block
        signal x_8             : signed(63 downto 0);
        signal y_4             : signed(63 downto 0);
        signal \c$case_alt_16\ : signed(63 downto 0);
      begin
        acc_1_3 <= \c$case_alt_16\;

        x_8 <= acc_7;

        y_4 <= acc_8;

        \c$case_alt_16\ <= (x_8 + y_4);


      end block;

    fun_19 : block
        signal x_9             : signed(63 downto 0);
        signal y_5             : signed(63 downto 0);
        signal \c$case_alt_17\ : signed(63 downto 0);
      begin
        acc_1_4 <= \c$case_alt_17\;

        x_9 <= acc_9;

        y_5 <= acc_10;

        \c$case_alt_17\ <= (x_9 + y_5);


      end block;

    fun_20 : block
        signal x_10            : signed(63 downto 0);
        signal y_6             : signed(63 downto 0);
        signal \c$case_alt_18\ : signed(63 downto 0);
      begin
        acc_2_0 <= \c$case_alt_18\;

        x_10 <= acc_1_0;

        y_6 <= acc_1_1;

        \c$case_alt_18\ <= (x_10 + y_6);


      end block;

    fun_21 : block
        signal x_11            : signed(63 downto 0);
        signal y_7             : signed(63 downto 0);
        signal \c$case_alt_19\ : signed(63 downto 0);
      begin
        acc_2_1 <= \c$case_alt_19\;

        x_11 <= acc_1_2;

        y_7 <= acc_1_3;

        \c$case_alt_19\ <= (x_11 + y_7);


      end block;

    fun_22 : block
        signal x_12            : signed(63 downto 0);
        signal y_8             : signed(63 downto 0);
        signal \c$case_alt_20\ : signed(63 downto 0);
      begin
        acc_3_0 <= \c$case_alt_20\;

        x_12 <= acc_2_0;

        y_8 <= acc_2_1;

        \c$case_alt_20\ <= (x_12 + y_8);


      end block;

    fun_23 : block
        signal x_13            : signed(63 downto 0);
        signal y_9             : signed(63 downto 0);
        signal \c$case_alt_21\ : signed(63 downto 0);
      begin
        acc_4_0 <= \c$case_alt_21\;

        x_13 <= acc_3_0;

        y_9 <= acc_1_4;

        \c$case_alt_21\ <= (x_13 + y_9);


      end block;


  end block;

  result_7 <= t when b else
              result_8;

  -- register begin
  result_8_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_8 <= ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_8 <= result_7;
      end if;
    end if;
  end process;
  -- register end

  b <= result_15;

  t <= ( Tuple2_1_sel0_unsigned => result_14.Tags_sel3_output1_0
       , Tuple2_1_sel1_signed => \c$t_app_arg\ );

  x_projection_9 <=  win(0) ;

  x_0 <= x_projection_9;

  win <= window.Tuple2_2_sel1_array_of_signed_64;

  -- register begin
  window_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window <= ( Tuple2_2_sel0_unsigned => to_unsigned(13,8)
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
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window <= result_9;
      end if;
    end if;
  end process;
  -- register end

  result_9 <= t_0 when b_0 else
              window;

  t_0 <= ( Tuple2_2_sel0_unsigned => result_37.Tags_sel4_slide0
         , Tuple2_2_sel1_array_of_signed_64 => result_10 );

  result_10 <= \c$t_case_alt\ when result_12 else
               \c$t_case_alt_0\;

  \c$vec_0\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win)));

  \c$t_case_alt_sel_alt_0\ <= (\c$vec_0\(0 to 11-1),\c$vec_0\(11 to \c$vec_0\'high));

  \c$vec_1\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated)));

  \c$t_case_alt_sel_alt_2\ <= (\c$vec_1\(0 to 11-1),\c$vec_1\(11 to \c$vec_1\'high));

  \c$t_case_alt\ <= \c$t_case_alt_sel_alt_2\.Tuple2_5_sel0_array_of_signed_64_0 when \c$b_case_alt\ else
                    \c$t_case_alt_sel_alt_0\.Tuple2_5_sel0_array_of_signed_64_0;

  \c$t_case_alt_0\ <= lastBucketUpdated when \c$b_case_alt\ else
                      win;

  -- replace begin
  replaceVec : block
    signal vec_index : integer range 0 to 11-1;
  begin
    vec_index <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 11
    -- pragma translate_on
                 ;

    process(vec_index,win,x_0)
      variable ivec : Spec_topEntity_types.array_of_signed_64(0 to 10);
    begin
      ivec := win;
      ivec(vec_index) := ((x_0 + to_signed(1,64)));
      lastBucketUpdated <= ivec;
    end process;
  end block;
  -- replace end

  b_0 <= \c$b_case_alt\ or result_12;

  \c$b_case_alt\ <= x0 and x1;

  x1 <= result_13.PacingOut0_sel1_boolean_1;

  x0 <= result_13.PacingOut0_sel0_boolean_0;

  \c$vec2\ <= (ws(1 to ws'high));

  -- zipWith begin
  zipWith : for i_6 in \c$ws_app_arg\'range generate
  begin
    \c$ws_app_arg\(i_6) <= ( Tuple2_3_sel0_Tuple2_1 => result_22(i_6)
               , Tuple2_3_sel1_Maybe => \c$vec2\(i_6) );


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
    imap_0 : for i_7 in \c$ws_app_arg_0\'range generate
    begin
      fun_24 : block
      signal \c$case_alt_22\            : Spec_topEntity_types.Maybe;
      -- spec.hs:282:1-63
      signal t_9                        : unsigned(7 downto 0);
      -- spec.hs:282:1-63
      signal x_14                       : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res\ : boolean;
    begin
      \c$ws_app_arg_0\(i_7) <= \c$case_alt_22\;

      \c$case_alt_selection_res\ <= t_9 = result_37.Tags_sel2_output0_0;

      \c$case_alt_22\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_7,max(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res\ else
                         \c$ws_app_arg\(i_7).Tuple2_3_sel1_Maybe;

      t_9 <= x_14.Tuple2_1_sel0_unsigned;

      x_14 <= \c$ws_app_arg\(i_7).Tuple2_3_sel0_Tuple2_1;


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
    result_11 <= to_signed(0,64) when "0",
                 \c$case_scrut\.Tuple2_1_sel1_signed when others;

  i <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_0\(1 downto 0)));

  ws <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_0\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  iterateI_0 : block
    signal iterateI_ho1_0_arg0_0 : boolean;
    signal iterateI_ho1_0_res_0  : boolean;
    signal iterateI_ho1_1_res_0  : boolean;
    signal iterateI_ho1_2_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_0 <= result_47.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_boolean;

    r_block_38 : block
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

    r_block_39 : block
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

    r_block_40 : block
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

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_boolean'( result_47.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_boolean
                                                           , iterateI_ho1_0_res_0
                                                           , iterateI_ho1_1_res_0
                                                           , iterateI_ho1_2_res_0 );


  end block;

  result_12 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_res_1 : Spec_topEntity_types.PacingOut0;
    signal iterateI_ho1_1_res_1 : Spec_topEntity_types.PacingOut0;
    signal iterateI_ho1_2_res_1 : Spec_topEntity_types.PacingOut0;
  begin
    r_block_41 : block
        signal \c$bb_res_res_7\ : Spec_topEntity_types.PacingOut0 := (PacingOut0_sel0_boolean_0 => false, PacingOut0_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_1 <= \c$bb_res_res_7\;


      end block;

    r_block_42 : block
        signal \c$bb_res_res_8\ : Spec_topEntity_types.PacingOut0 := (PacingOut0_sel0_boolean_0 => false, PacingOut0_sel1_boolean_1 => false);
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

    r_block_43 : block
        signal \c$bb_res_res_9\ : Spec_topEntity_types.PacingOut0 := (PacingOut0_sel0_boolean_0 => false, PacingOut0_sel1_boolean_1 => false);
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

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_PacingOut0'( pOut0
                                                              , iterateI_ho1_0_res_1
                                                              , iterateI_ho1_1_res_1
                                                              , iterateI_ho1_2_res_1 );


  end block;

  result_13 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  iterateI_2 : block
    signal iterateI_ho1_0_arg0_1 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_2  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_2  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_2  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_1 <= ( Tags_sel0_input0_0 => t_7
                             , Tags_sel1_input1_0 => t_6
                             , Tags_sel2_output0_0 => t_5
                             , Tags_sel3_output1_0 => t_4
                             , Tags_sel4_slide0 => t_5 );

    r_block_44 : block
        signal \c$bb_res_res_10\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_slide0 => to_unsigned(13,8) );
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

    r_block_45 : block
        signal \c$bb_res_res_11\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_slide0 => to_unsigned(13,8) );
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

    r_block_46 : block
        signal \c$bb_res_res_12\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_slide0 => to_unsigned(13,8) );
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

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_7
                                                        , Tags_sel1_input1_0 => t_6
                                                        , Tags_sel2_output0_0 => t_5
                                                        , Tags_sel3_output1_0 => t_4
                                                        , Tags_sel4_slide0 => t_5 )
                                                        , iterateI_ho1_0_res_2
                                                        , iterateI_ho1_1_res_2
                                                        , iterateI_ho1_2_res_2 );


  end block;

  result_14 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  iterateI_3 : block
    signal iterateI_ho1_0_res_3 : boolean;
    signal iterateI_ho1_1_res_3 : boolean;
    signal iterateI_ho1_2_res_3 : boolean;
    signal iterateI_ho1_3_res_0 : boolean;
  begin
    r_block_47 : block
        signal \c$bb_res_res_13\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_13\;


      end block;

    r_block_48 : block
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

    r_block_49 : block
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

    r_block_50 : block
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

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                           , iterateI_ho1_0_res_3
                                                           , iterateI_ho1_1_res_3
                                                           , iterateI_ho1_2_res_3
                                                           , iterateI_ho1_3_res_0 );


  end block;

  result_15 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  \c$vec2_0\ <= (ws_0(1 to ws_0'high));

  -- zipWith begin
  zipWith_0 : for i_9 in \c$ws_app_arg_1\'range generate
  begin
    \c$ws_app_arg_1\(i_9) <= ( Tuple2_3_sel0_Tuple2_1 => result_22(i_9)
               , Tuple2_3_sel1_Maybe => \c$vec2_0\(i_9) );


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
    imap_2 : for i_10 in \c$ws_app_arg_2\'range generate
    begin
      fun_25 : block
      signal \c$case_alt_23\              : Spec_topEntity_types.Maybe;
      -- spec.hs:282:1-63
      signal t_10                         : unsigned(7 downto 0);
      -- spec.hs:282:1-63
      signal x_15                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_2\ : boolean;
    begin
      \c$ws_app_arg_2\(i_10) <= \c$case_alt_23\;

      \c$case_alt_selection_res_2\ <= t_10 = result_18.Tags_sel2_output0_0;

      \c$case_alt_23\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_10,max_0(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_2\ else
                         \c$ws_app_arg_1\(i_10).Tuple2_3_sel1_Maybe;

      t_10 <= x_15.Tuple2_1_sel0_unsigned;

      x_15 <= \c$ws_app_arg_1\(i_10).Tuple2_3_sel0_Tuple2_1;


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
    result_16 <= to_signed(0,64) when "0",
                 \c$case_scrut_1\.Tuple2_1_sel1_signed when others;

  i_0 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_2\(1 downto 0)));

  ws_0 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_2\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  iterateI_4 : block
    signal iterateI_ho1_0_res_4 : boolean;
    signal iterateI_ho1_1_res_4 : boolean;
    signal iterateI_ho1_2_res_4 : boolean;
    signal iterateI_ho1_3_res_1 : boolean;
    signal iterateI_ho1_4_res_0 : boolean;
  begin
    r_block_51 : block
        signal \c$bb_res_res_17\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_17_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_17\ <= \c$ds3_case_alt\;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_17\;


      end block;

    r_block_52 : block
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

    r_block_53 : block
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

    r_block_54 : block
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

    r_block_55 : block
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

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_boolean'( \c$ds3_case_alt\
                                                           , iterateI_ho1_0_res_4
                                                           , iterateI_ho1_1_res_4
                                                           , iterateI_ho1_2_res_4
                                                           , iterateI_ho1_3_res_1
                                                           , iterateI_ho1_4_res_0 );


  end block;

  result_17 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  iterateI_5 : block
    signal iterateI_ho1_0_arg0_2 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_5  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_5  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_5  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_2  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_2 <= ( Tags_sel0_input0_0 => t_7
                             , Tags_sel1_input1_0 => t_6
                             , Tags_sel2_output0_0 => t_5
                             , Tags_sel3_output1_0 => t_4
                             , Tags_sel4_slide0 => t_5 );

    r_block_56 : block
        signal \c$bb_res_res_22\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_22_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_22\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_22\;


      end block;

    r_block_57 : block
        signal \c$bb_res_res_23\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_slide0 => to_unsigned(13,8) );
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

    r_block_58 : block
        signal \c$bb_res_res_24\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_slide0 => to_unsigned(13,8) );
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

    r_block_59 : block
        signal \c$bb_res_res_25\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_slide0 => to_unsigned(13,8) );
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

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_7
                                                        , Tags_sel1_input1_0 => t_6
                                                        , Tags_sel2_output0_0 => t_5
                                                        , Tags_sel3_output1_0 => t_4
                                                        , Tags_sel4_slide0 => t_5 )
                                                        , iterateI_ho1_0_res_5
                                                        , iterateI_ho1_1_res_5
                                                        , iterateI_ho1_2_res_5
                                                        , iterateI_ho1_3_res_2 );


  end block;

  result_18 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  y <= result_19;

  \c$vec2_1\ <= (ws_1(1 to ws_1'high));

  -- zipWith begin
  zipWith_1 : for i_11 in \c$ws_app_arg_3\'range generate
  begin
    \c$ws_app_arg_3\(i_11) <= ( Tuple2_3_sel0_Tuple2_1 => result_22(i_11)
               , Tuple2_3_sel1_Maybe => \c$vec2_1\(i_11) );


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
    imap_4 : for i_12 in \c$ws_app_arg_4\'range generate
    begin
      fun_26 : block
      signal \c$case_alt_24\              : Spec_topEntity_types.Maybe;
      -- spec.hs:282:1-63
      signal t_11                         : unsigned(7 downto 0);
      -- spec.hs:282:1-63
      signal x_16                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_3\ : boolean;
    begin
      \c$ws_app_arg_4\(i_12) <= \c$case_alt_24\;

      \c$case_alt_selection_res_3\ <= t_11 = result_20;

      \c$case_alt_24\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_12,max_1(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_3\ else
                         \c$ws_app_arg_3\(i_12).Tuple2_3_sel1_Maybe;

      t_11 <= x_16.Tuple2_1_sel0_unsigned;

      x_16 <= \c$ws_app_arg_3\(i_12).Tuple2_3_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_1 : block
    signal vec_index_2 : integer range 0 to 3-1;
  begin
    vec_index_2 <= to_integer((signed(std_logic_vector(resize(i_1,64)))))
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_scrut_3\ <= result_22(vec_index_2);
  end block;
  -- index end

  \c$case_scrut_4\ <=  ws_1(0) ;

  with (\c$case_scrut_4\(2 downto 2)) select
    result_19 <= result_27 when "0",
                 \c$case_scrut_3\.Tuple2_1_sel1_signed when others;

  result_selection_res <= result_38.Tags_sel2_output0_0 > to_unsigned(1,8);

  result_20 <= \c$app_arg_6\ when result_selection_res else
               \c$app_arg_6\ + to_unsigned(12,8);

  \c$app_arg_6\ <= result_38.Tags_sel2_output0_0 - to_unsigned(1,8);

  i_1 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_4\(1 downto 0)));

  ws_1 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_4\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  result_selection_res_2 <= x0_0 and x1_0;

  result_21 <= t_1 when result_selection_res_2 else
               result_22;

  x0_0 <= result_30.PacingOut0_sel0_boolean_0;

  x1_0 <= result_30.PacingOut0_sel1_boolean_1;

  -- register begin
  result_22_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_22 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_22 <= result_21;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_2\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_22) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => result_38.Tags_sel2_output0_0
             , Tuple2_1_sel1_signed => (x_1 + y_0) )))));

  t_projection_2 <= (\c$vec_2\(0 to 1-1),\c$vec_2\(1 to \c$vec_2\'high));

  t_1 <= t_projection_2.Tuple2_6_sel1_array_of_Tuple2_1_1;

  x_1 <= result_29;

  y_0 <= result_23;

  eta3 <= (x_2 + y);

  \c$vec2_2\ <= (ws_2(1 to ws_2'high));

  -- zipWith begin
  zipWith_2 : for i_13 in \c$ws_app_arg_5\'range generate
  begin
    \c$ws_app_arg_5\(i_13) <= ( Tuple2_3_sel0_Tuple2_1 => result_35(i_13)
               , Tuple2_3_sel1_Maybe => \c$vec2_2\(i_13) );


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
    imap_6 : for i_14 in \c$ws_app_arg_6\'range generate
    begin
      fun_27 : block
      signal \c$case_alt_25\              : Spec_topEntity_types.Maybe;
      -- spec.hs:282:1-63
      signal t_12                         : unsigned(7 downto 0);
      -- spec.hs:282:1-63
      signal x_17                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_4\ : boolean;
    begin
      \c$ws_app_arg_6\(i_14) <= \c$case_alt_25\;

      \c$case_alt_selection_res_4\ <= t_12 = result_24;

      \c$case_alt_25\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_14,max_2(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_4\ else
                         \c$ws_app_arg_5\(i_14).Tuple2_3_sel1_Maybe;

      t_12 <= x_17.Tuple2_1_sel0_unsigned;

      x_17 <= \c$ws_app_arg_5\(i_14).Tuple2_3_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_2 : block
    signal vec_index_3 : integer range 0 to 3-1;
  begin
    vec_index_3 <= to_integer((signed(std_logic_vector(resize(i_2,64)))))
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_scrut_5\ <= result_35(vec_index_3);
  end block;
  -- index end

  \c$case_scrut_6\ <=  ws_2(0) ;

  with (\c$case_scrut_6\(2 downto 2)) select
    result_23 <= eta3 when "0",
                 \c$case_scrut_5\.Tuple2_1_sel1_signed when others;

  result_selection_res_3 <= result_38.Tags_sel0_input0_0 > to_unsigned(2,8);

  result_24 <= \c$app_arg_7\ when result_selection_res_3 else
               \c$app_arg_7\ + to_unsigned(12,8);

  \c$app_arg_7\ <= result_38.Tags_sel0_input0_0 - to_unsigned(2,8);

  i_2 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_6\(1 downto 0)));

  ws_2 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_6\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  x_2 <= result_25;

  \c$vec2_3\ <= (ws_3(1 to ws_3'high));

  -- zipWith begin
  zipWith_3 : for i_15 in \c$ws_app_arg_7\'range generate
  begin
    \c$ws_app_arg_7\(i_15) <= ( Tuple2_4_sel0_Tuple2_1 => result_32(i_15)
               , Tuple2_4_sel1_Maybe_0 => \c$vec2_3\(i_15) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_7 : block
    function max_3 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_8 : for i_16 in \c$ws_app_arg_8\'range generate
    begin
      fun_28 : block
      signal \c$case_alt_26\              : Spec_topEntity_types.Maybe_0;
      -- spec.hs:274:1-65
      signal t_13                         : unsigned(7 downto 0);
      -- spec.hs:274:1-65
      signal x_18                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_5\ : boolean;
    begin
      \c$ws_app_arg_8\(i_16) <= \c$case_alt_26\;

      \c$case_alt_selection_res_5\ <= t_13 = result_26;

      \c$case_alt_26\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_16,max_3(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_5\ else
                         \c$ws_app_arg_7\(i_16).Tuple2_4_sel1_Maybe_0;

      t_13 <= x_18.Tuple2_1_sel0_unsigned;

      x_18 <= \c$ws_app_arg_7\(i_16).Tuple2_4_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_3 : block
    signal vec_index_4 : integer range 0 to 2-1;
  begin
    vec_index_4 <= to_integer((signed(std_logic_vector(resize(i_3,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_7\ <= result_32(vec_index_4);
  end block;
  -- index end

  \c$case_scrut_8\ <=  ws_3(0) ;

  with (\c$case_scrut_8\(1 downto 1)) select
    result_25 <= to_signed(10,64) when "0",
                 \c$case_scrut_7\.Tuple2_1_sel1_signed when others;

  result_selection_res_4 <= result_38.Tags_sel1_input1_0 > to_unsigned(1,8);

  result_26 <= \c$app_arg_8\ when result_selection_res_4 else
               \c$app_arg_8\ + to_unsigned(12,8);

  \c$app_arg_8\ <= result_38.Tags_sel1_input1_0 - to_unsigned(1,8);

  i_3 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_8\(0 downto 0)));

  ws_3 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_8\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "-"))));

  \c$vec2_4\ <= (ws_4(1 to ws_4'high));

  -- zipWith begin
  zipWith_4 : for i_17 in \c$ws_app_arg_9\'range generate
  begin
    \c$ws_app_arg_9\(i_17) <= ( Tuple2_3_sel0_Tuple2_1 => result_35(i_17)
               , Tuple2_3_sel1_Maybe => \c$vec2_4\(i_17) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_9 : block
    function max_4 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_10 : for i_18 in \c$ws_app_arg_10\'range generate
    begin
      fun_29 : block
      signal \c$case_alt_27\              : Spec_topEntity_types.Maybe;
      -- spec.hs:282:1-63
      signal t_14                         : unsigned(7 downto 0);
      -- spec.hs:282:1-63
      signal x_19                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_6\ : boolean;
    begin
      \c$ws_app_arg_10\(i_18) <= \c$case_alt_27\;

      \c$case_alt_selection_res_6\ <= t_14 = result_28;

      \c$case_alt_27\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_18,max_4(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_6\ else
                         \c$ws_app_arg_9\(i_18).Tuple2_3_sel1_Maybe;

      t_14 <= x_19.Tuple2_1_sel0_unsigned;

      x_19 <= \c$ws_app_arg_9\(i_18).Tuple2_3_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_4 : block
    signal vec_index_5 : integer range 0 to 3-1;
  begin
    vec_index_5 <= to_integer((signed(std_logic_vector(resize(i_4,64)))))
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_scrut_9\ <= result_35(vec_index_5);
  end block;
  -- index end

  \c$case_scrut_10\ <=  ws_4(0) ;

  with (\c$case_scrut_10\(2 downto 2)) select
    result_27 <= to_signed(20,64) when "0",
                 \c$case_scrut_9\.Tuple2_1_sel1_signed when others;

  result_selection_res_5 <= result_38.Tags_sel0_input0_0 > to_unsigned(1,8);

  result_28 <= \c$app_arg_9\ when result_selection_res_5 else
               \c$app_arg_9\ + to_unsigned(12,8);

  \c$app_arg_9\ <= result_38.Tags_sel0_input0_0 - to_unsigned(1,8);

  i_4 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_10\(1 downto 0)));

  ws_4 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_10\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  \c$vec2_5\ <= (ws_5(1 to ws_5'high));

  -- zipWith begin
  zipWith_5 : for i_19 in \c$ws_app_arg_11\'range generate
  begin
    \c$ws_app_arg_11\(i_19) <= ( Tuple2_4_sel0_Tuple2_1 => result_32(i_19)
               , Tuple2_4_sel1_Maybe_0 => \c$vec2_5\(i_19) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_11 : block
    function max_5 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_12 : for i_20 in \c$ws_app_arg_12\'range generate
    begin
      fun_30 : block
      signal \c$case_alt_28\              : Spec_topEntity_types.Maybe_0;
      -- spec.hs:274:1-65
      signal t_15                         : unsigned(7 downto 0);
      -- spec.hs:274:1-65
      signal x_20                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_7\ : boolean;
    begin
      \c$ws_app_arg_12\(i_20) <= \c$case_alt_28\;

      \c$case_alt_selection_res_7\ <= t_15 = result_38.Tags_sel1_input1_0;

      \c$case_alt_28\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_20,max_5(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_7\ else
                         \c$ws_app_arg_11\(i_20).Tuple2_4_sel1_Maybe_0;

      t_15 <= x_20.Tuple2_1_sel0_unsigned;

      x_20 <= \c$ws_app_arg_11\(i_20).Tuple2_4_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_5 : block
    signal vec_index_6 : integer range 0 to 2-1;
  begin
    vec_index_6 <= to_integer((signed(std_logic_vector(resize(i_5,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_11\ <= result_32(vec_index_6);
  end block;
  -- index end

  \c$case_scrut_12\ <=  ws_5(0) ;

  with (\c$case_scrut_12\(1 downto 1)) select
    result_29 <= to_signed(0,64) when "0",
                 \c$case_scrut_11\.Tuple2_1_sel1_signed when others;

  i_5 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_12\(0 downto 0)));

  ws_5 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_12\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "-"))));

  iterateI_6 : block
    signal iterateI_ho1_0_res_6 : Spec_topEntity_types.PacingOut0;
    signal iterateI_ho1_1_res_6 : Spec_topEntity_types.PacingOut0;
  begin
    r_block_60 : block
        signal \c$bb_res_res_26\ : Spec_topEntity_types.PacingOut0 := (PacingOut0_sel0_boolean_0 => false, PacingOut0_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_26_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_26\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_26\;


      end block;

    r_block_61 : block
        signal \c$bb_res_res_27\ : Spec_topEntity_types.PacingOut0 := (PacingOut0_sel0_boolean_0 => false, PacingOut0_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_27_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_27\ <= iterateI_ho1_0_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_6 <= \c$bb_res_res_27\;


      end block;

    \c$app_arg_10\ <= Spec_topEntity_types.array_of_PacingOut0'( pOut0
                                                               , iterateI_ho1_0_res_6
                                                               , iterateI_ho1_1_res_6 );


  end block;

  result_30 <=  \c$app_arg_10\(\c$app_arg_10\'high) ;

  result_31 <= t_2 when b_1 else
               result_32;

  -- register begin
  result_32_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_32 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_32 <= result_31;
      end if;
    end if;
  end process;
  -- register end

  b_1 <= result_33;

  \c$vec_3\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_32) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => t_6
             , Tuple2_1_sel1_signed => \c$input1Win_app_arg\ )))));

  t_projection_7 <= (\c$vec_3\(0 to 1-1),\c$vec_3\(1 to \c$vec_3\'high));

  t_2 <= t_projection_7.Tuple2_7_sel1_array_of_Tuple2_1_1;

  -- delay begin
  cinput1Win_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$input1Win_app_arg\ <= inputs_2.Inputs_3_sel1_input1.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_7 : block
    signal iterateI_ho1_0_res_7 : boolean;
  begin
    r_block_62 : block
        signal \c$bb_res_res_28\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_28_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_28\ <= pIn1_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_28\;


      end block;

    \c$app_arg_11\ <= Spec_topEntity_types.array_of_boolean'( pIn1_0
                                                            , iterateI_ho1_0_res_7 );


  end block;

  result_33 <=  \c$app_arg_11\(\c$app_arg_11\'high) ;

  result_34 <= t_3 when b_2 else
               result_35;

  -- register begin
  result_35_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_35 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_35 <= result_34;
      end if;
    end if;
  end process;
  -- register end

  b_2 <= result_36;

  \c$vec_4\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_35) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => t_7
             , Tuple2_1_sel1_signed => \c$input0Win_app_arg\ )))));

  t_projection_8 <= (\c$vec_4\(0 to 1-1),\c$vec_4\(1 to \c$vec_4\'high));

  t_3 <= t_projection_8.Tuple2_6_sel1_array_of_Tuple2_1_1;

  -- delay begin
  cinput0Win_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$input0Win_app_arg\ <= inputs_2.Inputs_3_sel0_input0.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  inputs_2 <= result_47.Tuple3_sel2_Tuple3_0.Tuple3_0_sel0_Inputs_3;

  iterateI_8 : block
    signal iterateI_ho1_0_res_8 : boolean;
  begin
    r_block_63 : block
        signal \c$bb_res_res_29\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_29_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_29\ <= pIn0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_8 <= \c$bb_res_res_29\;


      end block;

    \c$app_arg_12\ <= Spec_topEntity_types.array_of_boolean'( pIn0_0
                                                            , iterateI_ho1_0_res_8 );


  end block;

  result_36 <=  \c$app_arg_12\(\c$app_arg_12\'high) ;

  iterateI_9 : block
    signal iterateI_ho1_0_arg0_3 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_9  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_7  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_3 <= ( Tags_sel0_input0_0 => t_7
                             , Tags_sel1_input1_0 => t_6
                             , Tags_sel2_output0_0 => t_5
                             , Tags_sel3_output1_0 => t_4
                             , Tags_sel4_slide0 => t_5 );

    r_block_64 : block
        signal \c$bb_res_res_30\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_30_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_30\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_9 <= \c$bb_res_res_30\;


      end block;

    r_block_65 : block
        signal \c$bb_res_res_31\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_31_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_31\ <= iterateI_ho1_0_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_7 <= \c$bb_res_res_31\;


      end block;

    \c$app_arg_13\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_7
                                                         , Tags_sel1_input1_0 => t_6
                                                         , Tags_sel2_output0_0 => t_5
                                                         , Tags_sel3_output1_0 => t_4
                                                         , Tags_sel4_slide0 => t_5 )
                                                         , iterateI_ho1_0_res_9
                                                         , iterateI_ho1_1_res_7 );


  end block;

  result_37 <=  \c$app_arg_13\(\c$app_arg_13\'high) ;

  iterateI_10 : block
    signal iterateI_ho1_0_arg0_4 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_10 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_4 <= ( Tags_sel0_input0_0 => t_7
                             , Tags_sel1_input1_0 => t_6
                             , Tags_sel2_output0_0 => t_5
                             , Tags_sel3_output1_0 => t_4
                             , Tags_sel4_slide0 => t_5 );

    r_block_66 : block
        signal \c$bb_res_res_32\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_32_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_32\ <= iterateI_ho1_0_arg0_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_10 <= \c$bb_res_res_32\;


      end block;

    \c$app_arg_14\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_7
                                                         , Tags_sel1_input1_0 => t_6
                                                         , Tags_sel2_output0_0 => t_5
                                                         , Tags_sel3_output1_0 => t_4
                                                         , Tags_sel4_slide0 => t_5 )
                                                         , iterateI_ho1_0_res_10 );


  end block;

  result_38 <=  \c$app_arg_14\(\c$app_arg_14\'high) ;

  -- register begin
  t_4_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_4 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_4 <= result_39;
      end if;
    end if;
  end process;
  -- register end

  result_selection_19 <= pOut1;

  result_39 <= result_40 when result_selection_19 else
               t_4;

  b_3 <= t_4 = to_unsigned(12,8);

  f1 <= t_4 + to_unsigned(1,8);

  result_40 <= to_unsigned(1,8) when b_3 else
               f1;

  pOut1 <= pacings.Pacings_1_sel3_pacingOut1;

  -- register begin
  t_5_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_5 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_5 <= result_41;
      end if;
    end if;
  end process;
  -- register end

  result_41 <= result_42 when \c$ds3_case_alt\ else
               t_5;

  b_4 <= t_5 = to_unsigned(12,8);

  f1_0 <= t_5 + to_unsigned(1,8);

  result_42 <= to_unsigned(1,8) when b_4 else
               f1_0;

  \c$ds3_case_alt\ <= x0_1 and x1_1;

  x1_1 <= pOut0.PacingOut0_sel1_boolean_1;

  x0_1 <= pOut0.PacingOut0_sel0_boolean_0;

  pOut0 <= pacings.Pacings_1_sel2_pacingOut0_0;

  -- register begin
  t_6_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_6 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_6 <= result_43;
      end if;
    end if;
  end process;
  -- register end

  result_selection_25 <= pIn1_0;

  result_43 <= result_44 when result_selection_25 else
               t_6;

  b_5 <= t_6 = to_unsigned(12,8);

  f1_1 <= t_6 + to_unsigned(1,8);

  result_44 <= to_unsigned(1,8) when b_5 else
               f1_1;

  pIn1_0 <= pacings.Pacings_1_sel1_pacingIn1;

  -- register begin
  t_7_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_7 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_7 <= result_45;
      end if;
    end if;
  end process;
  -- register end

  result_selection_29 <= pIn0_0;

  result_45 <= result_46 when result_selection_29 else
               t_7;

  b_6 <= t_7 = to_unsigned(12,8);

  f1_2 <= t_7 + to_unsigned(1,8);

  result_46 <= to_unsigned(1,8) when b_6 else
               f1_2;

  pIn0_0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_47.Tuple3_sel2_Tuple3_0.Tuple3_0_sel2_Pacings_1;

  result_47 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_17\
               , Tuple3_sel1_boolean_1 => \c$app_arg_16\
               , Tuple3_sel2_Tuple3_0 => \c$app_arg_15\ );

  -- register begin
  capp_arg_15_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_15\ <= ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false ) )
  , Tuple3_0_sel1_boolean => false
  , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingIn1 => false
  , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
  , PacingOut0_sel1_boolean_1 => false )
  , Pacings_1_sel3_pacingOut1 => false ) );
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
                    ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false ) )
                    , Tuple3_0_sel1_boolean => false
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingIn1 => false
                    , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                    , PacingOut0_sel1_boolean_1 => false )
                    , Pacings_1_sel3_pacingOut1 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false ) )
                    , Tuple3_0_sel1_boolean => false
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingIn1 => false
                    , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                    , PacingOut0_sel1_boolean_1 => false )
                    , Pacings_1_sel3_pacingOut1 => false ) );

  with (x_3) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_3) select
    \c$case_alt_3\ <= ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false ) )
                      , Tuple3_0_sel1_boolean => false
                      , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingIn1 => false
                      , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                      , PacingOut0_sel1_boolean_1 => false )
                      , Pacings_1_sel3_pacingOut1 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec_6 : block
    signal vec_index_7 : integer range 0 to 2-1;
  begin
    vec_index_7 <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index_7);
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

  with (x_3) select
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

  \c$app_arg_18\ <= to_signed(1,64) when x_3 /= to_signed(2,64) else to_signed(0,64);

  -- register begin
  buffer_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \buffer\ <= Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_boolean => false
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                                         , PacingOut0_sel1_boolean_1 => false )
                                         , Pacings_1_sel3_pacingOut1 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_boolean => false
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                                         , PacingOut0_sel1_boolean_1 => false )
                                         , Pacings_1_sel3_pacingOut1 => false ) ) );
    elsif rising_edge(clk) then
      if en then
        \buffer\ <= \c$buffer_case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$buffer_case_alt\ <= \c$buffer_case_alt_0\ when push else
                         \buffer\;

  \c$vec_5\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec_5\(0 to 2-1),\c$vec_5\(2 to \c$vec_5\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_8_sel0_array_of_Tuple3_0_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_6\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_6\(0 to 2-1),\c$vec_6\(2 to \c$vec_6\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_8_sel0_array_of_Tuple3_0_0;

  qData <= result_4.Tuple2_sel1_Tuple3_0;

  x_3 <= cursor;

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

  push <= result_4.Tuple2_sel0_boolean;

  \c$cursor_case_alt_0\ <= cursor when pop else
                           \c$cursor_case_alt_1\;

  \c$cursor_case_alt_1\ <= cursor when \c$cursor_case_scrut\ else
                           (x_3 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_3 = to_signed(2,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_5.Tuple2_0_sel0_boolean;

  with (x_3) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_3 - to_signed(1,64));

  result <= result_5.Tuple2_0_sel1_Outputs;

  result_0 <= result.Outputs_sel0_output0;

  result_1 <= result.Outputs_sel1_output1;

  result_0_0 <= result_0.ValidInt_sel0_value;

  result_0_1 <= result_0.ValidInt_sel1_valid;

  result_1_0 <= result_1.ValidInt_sel0_value;

  result_1_1 <= result_1.ValidInt_sel1_valid;


end;

