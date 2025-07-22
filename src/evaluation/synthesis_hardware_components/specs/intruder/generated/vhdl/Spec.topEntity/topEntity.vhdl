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
       result_1_0 : out boolean;
       result_1_1 : out boolean;
       result_2_0 : out boolean;
       result_2_1 : out boolean;
       result_3_0 : out boolean;
       result_3_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_4                          : signed(63 downto 0);
  signal x                                 : signed(63 downto 0);
  signal result_5                          : signed(63 downto 0) := (to_signed(0,64));
  signal result_6                          : Spec_topEntity_types.Tuple2;
  -- spec.hs:(258,1)-(260,31)
  signal pIn0                              : boolean;
  -- spec.hs:(258,1)-(260,31)
  signal pIn1                              : boolean;
  -- spec.hs:(258,1)-(260,31)
  signal timer0Over                        : boolean;
  signal result_7                          : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                       : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_8                          : boolean;
  -- spec.hs:(605,1)-(609,29)
  signal \c$t_app_arg\                     : signed(63 downto 0);
  signal result_9                          : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(605,1)-(609,29)
  signal result_10                         : Spec_topEntity_types.Tuple2_1 := (Tuple2_1_sel0_unsigned => to_unsigned(13,8), Tuple2_1_sel1_boolean => false);
  -- spec.hs:(605,1)-(609,29)
  signal b                                 : boolean;
  -- spec.hs:(605,1)-(609,29)
  signal t                                 : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(628,1)-(633,37)
  signal x_0                               : signed(63 downto 0);
  -- spec.hs:(628,1)-(633,37)
  signal win                               : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:(628,1)-(633,37)
  signal window                            : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(13,8)
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
  signal result_11                         : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:(628,1)-(633,37)
  signal t_0                               : Spec_topEntity_types.Tuple2_2;
  signal result_12                         : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:(628,1)-(633,37)
  signal \c$t_case_alt\                    : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:(628,1)-(633,37)
  signal \c$t_case_alt_0\                  : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:(628,1)-(633,37)
  signal lastBucketUpdated                 : Spec_topEntity_types.array_of_signed_64(0 to 10);
  -- spec.hs:(628,1)-(633,37)
  signal b_0                               : boolean;
  -- spec.hs:(628,1)-(633,37)
  signal \c$b_case_alt\                    : boolean;
  -- spec.hs:108:34-42
  signal x1                                : boolean;
  -- spec.hs:108:34-42
  signal x0                                : boolean;
  -- spec.hs:318:1-63
  signal \c$ws_app_arg\                    : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_0\                  : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut\                    : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_0\                  : Spec_topEntity_types.Maybe;
  signal result_13                         : boolean;
  -- spec.hs:318:1-63
  signal i                                 : Spec_topEntity_types.index_3;
  -- spec.hs:318:1-63
  signal ws                                : Spec_topEntity_types.array_of_Maybe(0 to 3);
  signal \c$app_arg_0\                     : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_14                         : boolean;
  signal \c$app_arg_1\                     : Spec_topEntity_types.array_of_PacingOut2(0 to 5);
  signal result_15                         : Spec_topEntity_types.PacingOut2;
  signal \c$app_arg_2\                     : Spec_topEntity_types.array_of_Tags(0 to 5);
  signal result_16                         : Spec_topEntity_types.Tags;
  signal \c$app_arg_3\                     : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_17                         : boolean;
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_1\                  : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_2\                  : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$case_scrut_1\                  : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_2\                  : Spec_topEntity_types.Maybe;
  signal result_18                         : boolean;
  -- spec.hs:318:1-63
  signal i_0                               : Spec_topEntity_types.index_3;
  -- spec.hs:318:1-63
  signal ws_0                              : Spec_topEntity_types.array_of_Maybe(0 to 3);
  signal \c$app_arg_4\                     : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_19                         : boolean;
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_3\                  : Spec_topEntity_types.array_of_Tuple2_4(0 to 3);
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_4\                  : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal \c$case_scrut_3\                  : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_4\                  : Spec_topEntity_types.Maybe_0;
  signal result_20                         : boolean;
  -- spec.hs:318:1-63
  signal i_1                               : Spec_topEntity_types.index_4;
  -- spec.hs:318:1-63
  signal ws_1                              : Spec_topEntity_types.array_of_Maybe_0(0 to 4);
  signal \c$app_arg_5\                     : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_21                         : boolean;
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_5\                  : Spec_topEntity_types.array_of_Tuple2_5(0 to 4);
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_6\                  : Spec_topEntity_types.array_of_Maybe_1(0 to 4);
  signal \c$case_scrut_5\                  : Spec_topEntity_types.Tuple2_6;
  signal \c$case_scrut_6\                  : Spec_topEntity_types.Maybe_1;
  signal result_22                         : signed(63 downto 0);
  -- spec.hs:318:1-63
  signal i_2                               : Spec_topEntity_types.index_5;
  -- spec.hs:318:1-63
  signal ws_2                              : Spec_topEntity_types.array_of_Maybe_1(0 to 5);
  signal \c$app_arg_6\                     : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_23                         : boolean;
  signal result_24                         : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  -- spec.hs:108:34-42
  signal x0_0                              : boolean;
  -- spec.hs:108:34-42
  signal x1_0                              : boolean;
  -- spec.hs:(590,1)-(595,37)
  signal result_25                         : Spec_topEntity_types.array_of_Tuple2_1(0 to 2) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_boolean => false ) );
  -- spec.hs:(590,1)-(595,37)
  signal t_1                               : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_7\                  : Spec_topEntity_types.array_of_Tuple2_5(0 to 4);
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_8\                  : Spec_topEntity_types.array_of_Maybe_1(0 to 4);
  signal \c$case_scrut_7\                  : Spec_topEntity_types.Tuple2_6;
  signal \c$case_scrut_8\                  : Spec_topEntity_types.Maybe_1;
  signal result_26                         : signed(63 downto 0);
  -- spec.hs:318:1-63
  signal i_3                               : Spec_topEntity_types.index_5;
  -- spec.hs:318:1-63
  signal ws_3                              : Spec_topEntity_types.array_of_Maybe_1(0 to 5);
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_9\                  : Spec_topEntity_types.array_of_Tuple2_4(0 to 3);
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_10\                 : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal \c$case_scrut_9\                  : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_10\                 : Spec_topEntity_types.Maybe_0;
  signal result_27                         : boolean;
  -- spec.hs:318:1-63
  signal i_4                               : Spec_topEntity_types.index_4;
  -- spec.hs:318:1-63
  signal ws_4                              : Spec_topEntity_types.array_of_Maybe_0(0 to 4);
  signal \c$app_arg_7\                     : Spec_topEntity_types.array_of_PacingOut2(0 to 4);
  signal result_28                         : Spec_topEntity_types.PacingOut2;
  signal result_29                         : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  -- spec.hs:107:34-42
  signal x0_1                              : boolean;
  -- spec.hs:107:34-42
  signal x1_1                              : boolean;
  -- spec.hs:(575,1)-(580,37)
  signal result_30                         : Spec_topEntity_types.array_of_Tuple2_1(0 to 3) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_1_sel1_boolean => false ) );
  -- spec.hs:(575,1)-(580,37)
  signal t_2                               : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  -- spec.hs:310:1-65
  signal \c$ws_app_arg_11\                 : Spec_topEntity_types.array_of_Tuple2_5(0 to 4);
  -- spec.hs:310:1-65
  signal \c$ws_app_arg_12\                 : Spec_topEntity_types.array_of_Maybe_1(0 to 4);
  signal \c$case_scrut_11\                 : Spec_topEntity_types.Tuple2_6;
  signal \c$case_scrut_12\                 : Spec_topEntity_types.Maybe_1;
  signal result_31                         : signed(63 downto 0);
  signal result_32                         : unsigned(7 downto 0);
  signal \c$app_arg_8\                     : unsigned(7 downto 0);
  -- spec.hs:310:1-65
  signal i_5                               : Spec_topEntity_types.index_5;
  -- spec.hs:310:1-65
  signal ws_5                              : Spec_topEntity_types.array_of_Maybe_1(0 to 5);
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_13\                 : Spec_topEntity_types.array_of_Tuple2_5(0 to 4);
  -- spec.hs:318:1-63
  signal \c$ws_app_arg_14\                 : Spec_topEntity_types.array_of_Maybe_1(0 to 4);
  signal \c$case_scrut_13\                 : Spec_topEntity_types.Tuple2_6;
  signal \c$case_scrut_14\                 : Spec_topEntity_types.Maybe_1;
  signal result_33                         : signed(63 downto 0);
  -- spec.hs:318:1-63
  signal i_6                               : Spec_topEntity_types.index_5;
  -- spec.hs:318:1-63
  signal ws_6                              : Spec_topEntity_types.array_of_Maybe_1(0 to 5);
  signal \c$app_arg_9\                     : Spec_topEntity_types.array_of_PacingOut1(0 to 3);
  signal result_34                         : Spec_topEntity_types.PacingOut1;
  signal result_35                         : Spec_topEntity_types.array_of_Tuple2_6(0 to 4);
  -- spec.hs:106:34-42
  signal x0_2                              : boolean;
  -- spec.hs:106:34-42
  signal x1_2                              : boolean;
  -- spec.hs:(560,1)-(565,36)
  signal result_36                         : Spec_topEntity_types.array_of_Tuple2_6(0 to 4) := Spec_topEntity_types.array_of_Tuple2_6'( ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_6_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_6_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_6_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_6_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
                                       , Tuple2_6_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(560,1)-(565,36)
  signal t_3                               : Spec_topEntity_types.array_of_Tuple2_6(0 to 4);
  signal x_1                               : signed(63 downto 0);
  signal y                                 : signed(63 downto 0);
  signal y_0                               : signed(63 downto 0);
  signal y_1                               : signed(63 downto 0);
  -- spec.hs:(362,1)-(364,33)
  signal \c$out0_case_alt\                 : signed(63 downto 0);
  -- spec.hs:331:1-53
  signal dta                               : signed(63 downto 0);
  -- spec.hs:331:1-53
  signal tag                               : unsigned(7 downto 0);
  -- spec.hs:331:1-53
  signal tagToMatch                        : unsigned(7 downto 0);
  signal result_37                         : Spec_topEntity_types.Tuple2_6;
  -- spec.hs:(549,1)-(553,28)
  signal result_38                         : Spec_topEntity_types.Tuple2_6 := ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
, Tuple2_6_sel1_signed => to_signed(0,64) );
  -- spec.hs:(549,1)-(553,28)
  signal b_1                               : boolean;
  -- spec.hs:331:1-53
  signal \c$ds_app_arg\                    : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_10\                    : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_39                         : boolean;
  -- spec.hs:(362,1)-(364,33)
  signal \c$out0_case_alt_0\               : signed(63 downto 0);
  -- spec.hs:331:1-53
  signal dta_0                             : signed(63 downto 0);
  -- spec.hs:331:1-53
  signal tagToMatch_0                      : unsigned(7 downto 0);
  -- spec.hs:331:1-53
  signal tag_0                             : unsigned(7 downto 0);
  signal result_40                         : Spec_topEntity_types.Tuple2_6;
  -- spec.hs:(539,1)-(543,28)
  signal result_41                         : Spec_topEntity_types.Tuple2_6 := ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
, Tuple2_6_sel1_signed => to_signed(0,64) );
  -- spec.hs:(539,1)-(543,28)
  signal b_2                               : boolean;
  -- spec.hs:331:1-53
  signal \c$ds_app_arg_0\                  : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(362,1)-(364,33)
  signal inputs_2                          : Spec_topEntity_types.Inputs_3;
  signal \c$app_arg_11\                    : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_42                         : boolean;
  signal \c$app_arg_12\                    : Spec_topEntity_types.array_of_PacingOut0(0 to 2);
  signal result_43                         : Spec_topEntity_types.PacingOut0;
  signal \c$app_arg_13\                    : Spec_topEntity_types.array_of_Tags(0 to 6);
  signal result_44                         : Spec_topEntity_types.Tags;
  signal \c$app_arg_14\                    : Spec_topEntity_types.array_of_Tags(0 to 4);
  signal result_45                         : Spec_topEntity_types.Tags;
  signal \c$app_arg_15\                    : Spec_topEntity_types.array_of_Tags(0 to 3);
  signal result_46                         : Spec_topEntity_types.Tags;
  signal \c$app_arg_16\                    : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_47                         : Spec_topEntity_types.Tags;
  signal \c$app_arg_17\                    : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_48                         : Spec_topEntity_types.Tags;
  -- spec.hs:(527,9)-(529,29)
  signal t_4                               : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_49                         : unsigned(7 downto 0);
  -- spec.hs:(527,9)-(529,29)
  signal b_3                               : boolean;
  -- spec.hs:(527,9)-(529,29)
  signal f1                                : unsigned(7 downto 0);
  signal result_50                         : unsigned(7 downto 0);
  -- spec.hs:(362,1)-(364,33)
  signal pOut3                             : boolean;
  -- spec.hs:(527,9)-(529,29)
  signal t_5                               : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_51                         : unsigned(7 downto 0);
  -- spec.hs:(527,9)-(529,29)
  signal b_4                               : boolean;
  -- spec.hs:(527,9)-(529,29)
  signal f1_0                              : unsigned(7 downto 0);
  signal result_52                         : unsigned(7 downto 0);
  -- spec.hs:(362,1)-(364,33)
  signal \c$ds5_case_alt\                  : boolean;
  -- spec.hs:108:34-42
  signal x1_3                              : boolean;
  -- spec.hs:108:34-42
  signal x0_3                              : boolean;
  -- spec.hs:(362,1)-(364,33)
  signal pOut2                             : Spec_topEntity_types.PacingOut2;
  -- spec.hs:(527,9)-(529,29)
  signal t_6                               : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_53                         : unsigned(7 downto 0);
  -- spec.hs:(527,9)-(529,29)
  signal b_5                               : boolean;
  -- spec.hs:(527,9)-(529,29)
  signal f1_1                              : unsigned(7 downto 0);
  signal result_54                         : unsigned(7 downto 0);
  -- spec.hs:(362,1)-(364,33)
  signal \c$ds4_case_alt\                  : boolean;
  -- spec.hs:107:34-42
  signal x1_4                              : boolean;
  -- spec.hs:107:34-42
  signal x0_4                              : boolean;
  -- spec.hs:(362,1)-(364,33)
  signal pOut1                             : Spec_topEntity_types.PacingOut1;
  -- spec.hs:(527,9)-(529,29)
  signal t_7                               : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_55                         : unsigned(7 downto 0);
  -- spec.hs:(527,9)-(529,29)
  signal b_6                               : boolean;
  -- spec.hs:(527,9)-(529,29)
  signal f1_2                              : unsigned(7 downto 0);
  signal result_56                         : unsigned(7 downto 0);
  -- spec.hs:(362,1)-(364,33)
  signal \c$ds3_case_alt\                  : boolean;
  -- spec.hs:106:34-42
  signal x1_5                              : boolean;
  -- spec.hs:106:34-42
  signal x0_5                              : boolean;
  -- spec.hs:(362,1)-(364,33)
  signal pOut0                             : Spec_topEntity_types.PacingOut0;
  -- spec.hs:(527,9)-(529,29)
  signal t_8                               : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_57                         : unsigned(7 downto 0);
  -- spec.hs:(527,9)-(529,29)
  signal b_7                               : boolean;
  -- spec.hs:(527,9)-(529,29)
  signal f1_3                              : unsigned(7 downto 0);
  signal result_58                         : unsigned(7 downto 0);
  -- spec.hs:(362,1)-(364,33)
  signal pIn1_0                            : boolean;
  -- spec.hs:(527,9)-(529,29)
  signal t_9                               : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_59                         : unsigned(7 downto 0);
  -- spec.hs:(527,9)-(529,29)
  signal b_8                               : boolean;
  -- spec.hs:(527,9)-(529,29)
  signal f1_4                              : unsigned(7 downto 0);
  signal result_60                         : unsigned(7 downto 0);
  -- spec.hs:(362,1)-(364,33)
  signal pIn0_0                            : boolean;
  -- spec.hs:(362,1)-(364,33)
  signal pacings                           : Spec_topEntity_types.Pacings_1;
  signal result_61                         : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_18\                    : Spec_topEntity_types.Tuple3_0 := ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false ) )
, Tuple3_0_sel1_boolean => false
, Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingIn1 => false
, Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
, PacingOut0_sel1_boolean_1 => false )
, Pacings_1_sel3_pacingOut1_0 => ( PacingOut1_sel0_boolean_0 => false
, PacingOut1_sel1_boolean_1 => false )
, Pacings_1_sel4_pacingOut2_0 => ( PacingOut2_sel0_boolean_0 => false
, PacingOut2_sel1_boolean_1 => false )
, Pacings_1_sel5_pacingOut3 => false ) );
  signal \c$case_alt\                      : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_0\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_1\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_2\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_3\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_4\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$app_arg_19\                    : boolean := false;
  signal \c$case_alt_5\                    : boolean;
  signal \c$case_alt_6\                    : boolean;
  signal \c$case_alt_7\                    : boolean;
  signal \c$case_alt_8\                    : boolean;
  signal \c$case_alt_9\                    : boolean;
  signal \c$app_arg_20\                    : boolean := false;
  signal \c$case_alt_10\                   : boolean;
  signal \c$case_alt_11\                   : boolean;
  signal \c$case_alt_12\                   : boolean;
  signal \c$app_arg_21\                    : signed(63 downto 0);
  -- spec.hs:(175,1)-(177,25)
  signal \buffer\                          : Spec_topEntity_types.array_of_Tuple3_0(0 to 1) := Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_boolean => false
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                                       , PacingOut0_sel1_boolean_1 => false )
                                       , Pacings_1_sel3_pacingOut1_0 => ( PacingOut1_sel0_boolean_0 => false
                                       , PacingOut1_sel1_boolean_1 => false )
                                       , Pacings_1_sel4_pacingOut2_0 => ( PacingOut2_sel0_boolean_0 => false
                                       , PacingOut2_sel1_boolean_1 => false )
                                       , Pacings_1_sel5_pacingOut3 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_boolean => false
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                                       , PacingOut0_sel1_boolean_1 => false )
                                       , Pacings_1_sel3_pacingOut1_0 => ( PacingOut1_sel0_boolean_0 => false
                                       , PacingOut1_sel1_boolean_1 => false )
                                       , Pacings_1_sel4_pacingOut2_0 => ( PacingOut2_sel0_boolean_0 => false
                                       , PacingOut2_sel1_boolean_1 => false )
                                       , Pacings_1_sel5_pacingOut3 => false ) ) );
  -- spec.hs:(175,1)-(177,25)
  signal \c$buffer_case_alt\               : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(175,1)-(177,25)
  signal \c$buffer_case_alt_0\             : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(175,1)-(177,25)
  signal \c$buffer_case_alt_1\             : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(175,1)-(177,25)
  signal qData                             : Spec_topEntity_types.Tuple3_0;
  -- spec.hs:(175,1)-(177,25)
  signal x_2                               : signed(63 downto 0);
  -- spec.hs:(175,1)-(177,25)
  signal cursor                            : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(175,1)-(177,25)
  signal \c$cursor_case_alt\               : signed(63 downto 0);
  -- spec.hs:(175,1)-(177,25)
  signal push                              : boolean;
  -- spec.hs:(175,1)-(177,25)
  signal \c$cursor_case_alt_0\             : signed(63 downto 0);
  -- spec.hs:(175,1)-(177,25)
  signal \c$cursor_case_alt_1\             : signed(63 downto 0);
  -- spec.hs:(175,1)-(177,25)
  signal \c$cursor_case_scrut\             : boolean;
  -- spec.hs:(175,1)-(177,25)
  signal \c$cursor_app_arg\                : signed(63 downto 0);
  -- spec.hs:(175,1)-(177,25)
  signal \c$cursor_case_alt_2\             : signed(63 downto 0);
  -- spec.hs:(175,1)-(177,25)
  signal pop                               : boolean;
  -- spec.hs:(175,1)-(177,25)
  signal \c$cursor_case_alt_3\             : signed(63 downto 0);
  -- spec.hs:(175,1)-(177,25)
  signal \c$cursor_case_alt_4\             : signed(63 downto 0);
  signal inputs                            : Spec_topEntity_types.Inputs_3;
  signal \c$vec\                           : Spec_topEntity_types.array_of_signed_64(0 to 10);
  signal x_projection_9                    : signed(63 downto 0);
  signal \c$vec_0\                         : Spec_topEntity_types.array_of_signed_64(0 to 11);
  signal \c$t_case_alt_sel_alt_0\          : Spec_topEntity_types.Tuple2_7;
  signal \c$vec_1\                         : Spec_topEntity_types.array_of_signed_64(0 to 11);
  signal \c$t_case_alt_sel_alt_2\          : Spec_topEntity_types.Tuple2_7;
  signal \c$vec2\                          : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$vec2_0\                        : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$vec2_1\                        : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal \c$vec2_2\                        : Spec_topEntity_types.array_of_Maybe_1(0 to 4);
  signal result_selection_res              : boolean;
  signal \c$vec_2\                         : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  signal t_projection_3                    : Spec_topEntity_types.Tuple2_8;
  signal \c$vec2_3\                        : Spec_topEntity_types.array_of_Maybe_1(0 to 4);
  signal \c$vec2_4\                        : Spec_topEntity_types.array_of_Maybe_0(0 to 3);
  signal result_selection_res_4            : boolean;
  signal \c$vec_3\                         : Spec_topEntity_types.array_of_Tuple2_1(0 to 4);
  signal t_projection_6                    : Spec_topEntity_types.Tuple2_9;
  signal \c$vec2_5\                        : Spec_topEntity_types.array_of_Maybe_1(0 to 4);
  signal result_selection_res_5            : boolean;
  signal \c$vec2_6\                        : Spec_topEntity_types.array_of_Maybe_1(0 to 4);
  signal result_selection_res_6            : boolean;
  signal \c$vec_4\                         : Spec_topEntity_types.array_of_Tuple2_6(0 to 5);
  signal t_projection_9                    : Spec_topEntity_types.Tuple2_10;
  signal x_projection_18                   : signed(63 downto 0);
  signal y_projection_8                    : signed(63 downto 0);
  signal \c$out0_case_alt_selection_res\   : boolean;
  signal \c$out0_case_alt_selection_res_0\ : boolean;
  signal result_selection_19               : boolean;
  signal result_selection_29               : boolean;
  signal result_selection_33               : boolean;
  signal \c$vec_5\                         : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_3\     : Spec_topEntity_types.Tuple2_11;
  signal \c$vec_6\                         : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_5\     : Spec_topEntity_types.Tuple2_11;
  signal result                            : Spec_topEntity_types.Outputs;
  signal result_0                          : Spec_topEntity_types.ValidInt;
  signal result_1                          : Spec_topEntity_types.ValidBool;
  signal result_2                          : Spec_topEntity_types.ValidBool;
  signal result_3                          : Spec_topEntity_types.ValidBool;

begin
  inputs <= ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => inputs_0_0
            , ValidInt_sel1_valid => inputs_0_1 )
            , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => inputs_1_0
            , ValidInt_sel1_valid => inputs_1_1 ) );

  result_4 <= to_signed(2000,64) when timer0Over else
              (x + to_signed(2000,64));

  x <= result_5;

  -- register begin
  result_5_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_5 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_5 <= result_4;
      end if;
    end if;
  end process;
  -- register end

  result_6 <= ( Tuple2_sel0_boolean => inputs.Inputs_3_sel0_input0.ValidInt_sel1_valid or (inputs.Inputs_3_sel1_input1.ValidInt_sel1_valid or timer0Over)
              , Tuple2_sel1_Tuple3_0 => ( Tuple3_0_sel0_Inputs_3 => inputs
              , Tuple3_0_sel1_boolean => timer0Over
              , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => pIn0
              , Pacings_1_sel1_pacingIn1 => pIn1
              , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => pIn0
              , PacingOut0_sel1_boolean_1 => pIn1 )
              , Pacings_1_sel3_pacingOut1_0 => ( PacingOut1_sel0_boolean_0 => pIn0
              , PacingOut1_sel1_boolean_1 => pIn1 )
              , Pacings_1_sel4_pacingOut2_0 => ( PacingOut2_sel0_boolean_0 => pIn0
              , PacingOut2_sel1_boolean_1 => pIn1 )
              , Pacings_1_sel5_pacingOut3 => timer0Over ) ) );

  pIn0 <= inputs.Inputs_3_sel0_input0.ValidInt_sel1_valid;

  pIn1 <= inputs.Inputs_3_sel1_input1.ValidInt_sel1_valid;

  timer0Over <= result_5 >= (to_signed(1000000,64));

  result_7 <= ( Tuple2_0_sel0_boolean => true
              , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidInt_sel0_value => result_22
              , ValidInt_sel1_valid => result_23 )
              , Outputs_sel1_output1 => ( ValidBool_sel0_value_0 => result_20
              , ValidBool_sel1_valid_0 => result_21 )
              , Outputs_sel2_output2 => ( ValidBool_sel0_value_0 => result_18
              , ValidBool_sel1_valid_0 => result_19 )
              , Outputs_sel3_output3 => ( ValidBool_sel0_value_0 => result_10.Tuple2_1_sel1_boolean
              , ValidBool_sel1_valid_0 => result_8 ) ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
    signal iterateI_ho1_4_res  : boolean;
    signal iterateI_ho1_5_res  : boolean;
    signal iterateI_ho1_6_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut3;

    r_block_75 : block
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

    r_block_76 : block
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

    r_block_77 : block
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

    r_block_78 : block
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

    r_block_79 : block
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

    r_block_80 : block
        signal \c$bb_res_res_4\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_4_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_4\ <= iterateI_ho1_4_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res <= \c$bb_res_res_4\;


      end block;

    r_block_81 : block
        signal \c$bb_res_res_5\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_5_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_5\ <= iterateI_ho1_5_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res <= \c$bb_res_res_5\;


      end block;

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res
                                                         , iterateI_ho1_3_res
                                                         , iterateI_ho1_4_res
                                                         , iterateI_ho1_5_res
                                                         , iterateI_ho1_6_res );


  end block;

  result_8 <=  \c$app_arg\(\c$app_arg\'high) ;

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

    fun_16 : block
        signal x_4             : signed(63 downto 0);
        signal y_2             : signed(63 downto 0);
        signal \c$case_alt_13\ : signed(63 downto 0);
      begin
        acc_1_0 <= \c$case_alt_13\;

        x_4 <= acc_1;

        y_2 <= acc_2;

        \c$case_alt_13\ <= (x_4 + y_2);


      end block;

    fun_17 : block
        signal x_5             : signed(63 downto 0);
        signal y_3             : signed(63 downto 0);
        signal \c$case_alt_14\ : signed(63 downto 0);
      begin
        acc_1_1 <= \c$case_alt_14\;

        x_5 <= acc_3;

        y_3 <= acc_4;

        \c$case_alt_14\ <= (x_5 + y_3);


      end block;

    fun_18 : block
        signal x_6             : signed(63 downto 0);
        signal y_4             : signed(63 downto 0);
        signal \c$case_alt_15\ : signed(63 downto 0);
      begin
        acc_1_2 <= \c$case_alt_15\;

        x_6 <= acc_5;

        y_4 <= acc_6;

        \c$case_alt_15\ <= (x_6 + y_4);


      end block;

    fun_19 : block
        signal x_7             : signed(63 downto 0);
        signal y_5             : signed(63 downto 0);
        signal \c$case_alt_16\ : signed(63 downto 0);
      begin
        acc_1_3 <= \c$case_alt_16\;

        x_7 <= acc_7;

        y_5 <= acc_8;

        \c$case_alt_16\ <= (x_7 + y_5);


      end block;

    fun_20 : block
        signal x_8             : signed(63 downto 0);
        signal y_6             : signed(63 downto 0);
        signal \c$case_alt_17\ : signed(63 downto 0);
      begin
        acc_1_4 <= \c$case_alt_17\;

        x_8 <= acc_9;

        y_6 <= acc_10;

        \c$case_alt_17\ <= (x_8 + y_6);


      end block;

    fun_21 : block
        signal x_9             : signed(63 downto 0);
        signal y_7             : signed(63 downto 0);
        signal \c$case_alt_18\ : signed(63 downto 0);
      begin
        acc_2_0 <= \c$case_alt_18\;

        x_9 <= acc_1_0;

        y_7 <= acc_1_1;

        \c$case_alt_18\ <= (x_9 + y_7);


      end block;

    fun_22 : block
        signal x_10            : signed(63 downto 0);
        signal y_8             : signed(63 downto 0);
        signal \c$case_alt_19\ : signed(63 downto 0);
      begin
        acc_2_1 <= \c$case_alt_19\;

        x_10 <= acc_1_2;

        y_8 <= acc_1_3;

        \c$case_alt_19\ <= (x_10 + y_8);


      end block;

    fun_23 : block
        signal x_11            : signed(63 downto 0);
        signal y_9             : signed(63 downto 0);
        signal \c$case_alt_20\ : signed(63 downto 0);
      begin
        acc_3_0 <= \c$case_alt_20\;

        x_11 <= acc_2_0;

        y_9 <= acc_2_1;

        \c$case_alt_20\ <= (x_11 + y_9);


      end block;

    fun_24 : block
        signal x_12            : signed(63 downto 0);
        signal y_10            : signed(63 downto 0);
        signal \c$case_alt_21\ : signed(63 downto 0);
      begin
        acc_4_0 <= \c$case_alt_21\;

        x_12 <= acc_3_0;

        y_10 <= acc_1_4;

        \c$case_alt_21\ <= (x_12 + y_10);


      end block;


  end block;

  result_9 <= t when b else
              result_10;

  -- register begin
  result_10_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_10 <= (Tuple2_1_sel0_unsigned => to_unsigned(13,8), Tuple2_1_sel1_boolean => false);
    elsif rising_edge(clk) then
      if en then
        result_10 <= result_9;
      end if;
    end if;
  end process;
  -- register end

  b <= result_17;

  t <= ( Tuple2_1_sel0_unsigned => result_16.Tags_sel5_output3_0
       , Tuple2_1_sel1_boolean => \c$t_app_arg\ < (to_signed(5,64)) );

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
        window <= result_11;
      end if;
    end if;
  end process;
  -- register end

  result_11 <= t_0 when b_0 else
               window;

  t_0 <= ( Tuple2_2_sel0_unsigned => result_45.Tags_sel6_slide0
         , Tuple2_2_sel1_array_of_signed_64 => result_12 );

  result_12 <= \c$t_case_alt\ when result_14 else
               \c$t_case_alt_0\;

  \c$vec_0\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win)));

  \c$t_case_alt_sel_alt_0\ <= (\c$vec_0\(0 to 11-1),\c$vec_0\(11 to \c$vec_0\'high));

  \c$vec_1\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated)));

  \c$t_case_alt_sel_alt_2\ <= (\c$vec_1\(0 to 11-1),\c$vec_1\(11 to \c$vec_1\'high));

  \c$t_case_alt\ <= \c$t_case_alt_sel_alt_2\.Tuple2_7_sel0_array_of_signed_64_0 when \c$b_case_alt\ else
                    \c$t_case_alt_sel_alt_0\.Tuple2_7_sel0_array_of_signed_64_0;

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

  b_0 <= \c$b_case_alt\ or result_14;

  \c$b_case_alt\ <= x0 and x1;

  x1 <= result_15.PacingOut2_sel1_boolean_1;

  x0 <= result_15.PacingOut2_sel0_boolean_0;

  \c$vec2\ <= (ws(1 to ws'high));

  -- zipWith begin
  zipWith : for i_7 in \c$ws_app_arg\'range generate
  begin
    \c$ws_app_arg\(i_7) <= ( Tuple2_3_sel0_Tuple2_1 => result_25(i_7)
               , Tuple2_3_sel1_Maybe => \c$vec2\(i_7) );


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
    imap_0 : for i_8 in \c$ws_app_arg_0\'range generate
    begin
      fun_25 : block
      signal \c$case_alt_22\            : Spec_topEntity_types.Maybe;
      -- spec.hs:318:1-63
      signal t_11                       : unsigned(7 downto 0);
      -- spec.hs:318:1-63
      signal x_13                       : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res\ : boolean;
    begin
      \c$ws_app_arg_0\(i_8) <= \c$case_alt_22\;

      \c$case_alt_selection_res\ <= t_11 = result_45.Tags_sel4_output2_0;

      \c$case_alt_22\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_8,max(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res\ else
                         \c$ws_app_arg\(i_8).Tuple2_3_sel1_Maybe;

      t_11 <= x_13.Tuple2_1_sel0_unsigned;

      x_13 <= \c$ws_app_arg\(i_8).Tuple2_3_sel0_Tuple2_1;


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
    \c$case_scrut\ <= result_25(vec_index_0);
  end block;
  -- index end

  \c$case_scrut_0\ <=  ws(0) ;

  with (\c$case_scrut_0\(2 downto 2)) select
    result_13 <= false when "0",
                 \c$case_scrut\.Tuple2_1_sel1_boolean when others;

  i <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_0\(1 downto 0)));

  ws <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_0\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  iterateI_0 : block
    signal iterateI_ho1_0_arg0_0 : boolean;
    signal iterateI_ho1_0_res_0  : boolean;
    signal iterateI_ho1_1_res_0  : boolean;
    signal iterateI_ho1_2_res_0  : boolean;
    signal iterateI_ho1_3_res_0  : boolean;
    signal iterateI_ho1_4_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_0 <= result_61.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_boolean;

    r_block_82 : block
        signal \c$bb_res_res_6\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_6_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_6\ <= iterateI_ho1_0_arg0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_0 <= \c$bb_res_res_6\;


      end block;

    r_block_83 : block
        signal \c$bb_res_res_7\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= iterateI_ho1_0_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_0 <= \c$bb_res_res_7\;


      end block;

    r_block_84 : block
        signal \c$bb_res_res_8\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_8_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_8\ <= iterateI_ho1_1_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_0 <= \c$bb_res_res_8\;


      end block;

    r_block_85 : block
        signal \c$bb_res_res_9\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_9_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_9\ <= iterateI_ho1_2_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_0 <= \c$bb_res_res_9\;


      end block;

    r_block_86 : block
        signal \c$bb_res_res_10\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_10_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_10\ <= iterateI_ho1_3_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_0 <= \c$bb_res_res_10\;


      end block;

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_boolean'( result_61.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_boolean
                                                           , iterateI_ho1_0_res_0
                                                           , iterateI_ho1_1_res_0
                                                           , iterateI_ho1_2_res_0
                                                           , iterateI_ho1_3_res_0
                                                           , iterateI_ho1_4_res_0 );


  end block;

  result_14 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_res_1 : Spec_topEntity_types.PacingOut2;
    signal iterateI_ho1_1_res_1 : Spec_topEntity_types.PacingOut2;
    signal iterateI_ho1_2_res_1 : Spec_topEntity_types.PacingOut2;
    signal iterateI_ho1_3_res_1 : Spec_topEntity_types.PacingOut2;
    signal iterateI_ho1_4_res_1 : Spec_topEntity_types.PacingOut2;
  begin
    r_block_87 : block
        signal \c$bb_res_res_11\ : Spec_topEntity_types.PacingOut2 := (PacingOut2_sel0_boolean_0 => false, PacingOut2_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_11_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_11\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_1 <= \c$bb_res_res_11\;


      end block;

    r_block_88 : block
        signal \c$bb_res_res_12\ : Spec_topEntity_types.PacingOut2 := (PacingOut2_sel0_boolean_0 => false, PacingOut2_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_12_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_12\ <= iterateI_ho1_0_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_1 <= \c$bb_res_res_12\;


      end block;

    r_block_89 : block
        signal \c$bb_res_res_13\ : Spec_topEntity_types.PacingOut2 := (PacingOut2_sel0_boolean_0 => false, PacingOut2_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= iterateI_ho1_1_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_1 <= \c$bb_res_res_13\;


      end block;

    r_block_90 : block
        signal \c$bb_res_res_14\ : Spec_topEntity_types.PacingOut2 := (PacingOut2_sel0_boolean_0 => false, PacingOut2_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_14_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_14\ <= iterateI_ho1_2_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_1 <= \c$bb_res_res_14\;


      end block;

    r_block_91 : block
        signal \c$bb_res_res_15\ : Spec_topEntity_types.PacingOut2 := (PacingOut2_sel0_boolean_0 => false, PacingOut2_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_15_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_15\ <= iterateI_ho1_3_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_1 <= \c$bb_res_res_15\;


      end block;

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_PacingOut2'( pOut2
                                                              , iterateI_ho1_0_res_1
                                                              , iterateI_ho1_1_res_1
                                                              , iterateI_ho1_2_res_1
                                                              , iterateI_ho1_3_res_1
                                                              , iterateI_ho1_4_res_1 );


  end block;

  result_15 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  iterateI_2 : block
    signal iterateI_ho1_0_arg0_1 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_2  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_2  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_2  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_2  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_4_res_2  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_1 <= ( Tags_sel0_input0_0 => t_9
                             , Tags_sel1_input1_0 => t_8
                             , Tags_sel2_output0_0 => t_7
                             , Tags_sel3_output1_0 => t_6
                             , Tags_sel4_output2_0 => t_5
                             , Tags_sel5_output3_0 => t_4
                             , Tags_sel6_slide0 => t_5 );

    r_block_92 : block
        signal \c$bb_res_res_16\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_16_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_16\ <= iterateI_ho1_0_arg0_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_2 <= \c$bb_res_res_16\;


      end block;

    r_block_93 : block
        signal \c$bb_res_res_17\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_17_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_17\ <= iterateI_ho1_0_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_2 <= \c$bb_res_res_17\;


      end block;

    r_block_94 : block
        signal \c$bb_res_res_18\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_18_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_18\ <= iterateI_ho1_1_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_2 <= \c$bb_res_res_18\;


      end block;

    r_block_95 : block
        signal \c$bb_res_res_19\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_19_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_19\ <= iterateI_ho1_2_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_2 <= \c$bb_res_res_19\;


      end block;

    r_block_96 : block
        signal \c$bb_res_res_20\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_20_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_20\ <= iterateI_ho1_3_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_2 <= \c$bb_res_res_20\;


      end block;

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_9
                                                        , Tags_sel1_input1_0 => t_8
                                                        , Tags_sel2_output0_0 => t_7
                                                        , Tags_sel3_output1_0 => t_6
                                                        , Tags_sel4_output2_0 => t_5
                                                        , Tags_sel5_output3_0 => t_4
                                                        , Tags_sel6_slide0 => t_5 )
                                                        , iterateI_ho1_0_res_2
                                                        , iterateI_ho1_1_res_2
                                                        , iterateI_ho1_2_res_2
                                                        , iterateI_ho1_3_res_2
                                                        , iterateI_ho1_4_res_2 );


  end block;

  result_16 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  iterateI_3 : block
    signal iterateI_ho1_0_res_3 : boolean;
    signal iterateI_ho1_1_res_3 : boolean;
    signal iterateI_ho1_2_res_3 : boolean;
    signal iterateI_ho1_3_res_3 : boolean;
    signal iterateI_ho1_4_res_3 : boolean;
    signal iterateI_ho1_5_res_0 : boolean;
  begin
    r_block_97 : block
        signal \c$bb_res_res_21\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_21_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_21\ <= pOut3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_21\;


      end block;

    r_block_98 : block
        signal \c$bb_res_res_22\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_22_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_22\ <= iterateI_ho1_0_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_3 <= \c$bb_res_res_22\;


      end block;

    r_block_99 : block
        signal \c$bb_res_res_23\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_23_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_23\ <= iterateI_ho1_1_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_3 <= \c$bb_res_res_23\;


      end block;

    r_block_100 : block
        signal \c$bb_res_res_24\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_24_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_24\ <= iterateI_ho1_2_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_3 <= \c$bb_res_res_24\;


      end block;

    r_block_101 : block
        signal \c$bb_res_res_25\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_25_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_25\ <= iterateI_ho1_3_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_3 <= \c$bb_res_res_25\;


      end block;

    r_block_102 : block
        signal \c$bb_res_res_26\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_26_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_26\ <= iterateI_ho1_4_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_0 <= \c$bb_res_res_26\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                           , iterateI_ho1_0_res_3
                                                           , iterateI_ho1_1_res_3
                                                           , iterateI_ho1_2_res_3
                                                           , iterateI_ho1_3_res_3
                                                           , iterateI_ho1_4_res_3
                                                           , iterateI_ho1_5_res_0 );


  end block;

  result_17 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  \c$vec2_0\ <= (ws_0(1 to ws_0'high));

  -- zipWith begin
  zipWith_0 : for i_10 in \c$ws_app_arg_1\'range generate
  begin
    \c$ws_app_arg_1\(i_10) <= ( Tuple2_3_sel0_Tuple2_1 => result_25(i_10)
               , Tuple2_3_sel1_Maybe => \c$vec2_0\(i_10) );


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
    imap_2 : for i_11 in \c$ws_app_arg_2\'range generate
    begin
      fun_26 : block
      signal \c$case_alt_23\              : Spec_topEntity_types.Maybe;
      -- spec.hs:318:1-63
      signal t_12                         : unsigned(7 downto 0);
      -- spec.hs:318:1-63
      signal x_14                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_2\ : boolean;
    begin
      \c$ws_app_arg_2\(i_11) <= \c$case_alt_23\;

      \c$case_alt_selection_res_2\ <= t_12 = result_44.Tags_sel4_output2_0;

      \c$case_alt_23\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_11,max_0(1,integer(ceil(log2(real(3))))))))) when \c$case_alt_selection_res_2\ else
                         \c$ws_app_arg_1\(i_11).Tuple2_3_sel1_Maybe;

      t_12 <= x_14.Tuple2_1_sel0_unsigned;

      x_14 <= \c$ws_app_arg_1\(i_11).Tuple2_3_sel0_Tuple2_1;


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
    \c$case_scrut_1\ <= result_25(vec_index_1);
  end block;
  -- index end

  \c$case_scrut_2\ <=  ws_0(0) ;

  with (\c$case_scrut_2\(2 downto 2)) select
    result_18 <= false when "0",
                 \c$case_scrut_1\.Tuple2_1_sel1_boolean when others;

  i_0 <= Spec_topEntity_types.index_3'(spec_topentity_types.fromSLV(\c$case_scrut_2\(1 downto 0)));

  ws_0 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_2\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "--"))));

  iterateI_4 : block
    signal iterateI_ho1_0_res_4 : boolean;
    signal iterateI_ho1_1_res_4 : boolean;
    signal iterateI_ho1_2_res_4 : boolean;
    signal iterateI_ho1_3_res_4 : boolean;
    signal iterateI_ho1_4_res_4 : boolean;
    signal iterateI_ho1_5_res_1 : boolean;
    signal iterateI_ho1_6_res_0 : boolean;
  begin
    r_block_103 : block
        signal \c$bb_res_res_27\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_27_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_27\ <= \c$ds5_case_alt\;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_27\;


      end block;

    r_block_104 : block
        signal \c$bb_res_res_28\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_28_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_28\ <= iterateI_ho1_0_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_4 <= \c$bb_res_res_28\;


      end block;

    r_block_105 : block
        signal \c$bb_res_res_29\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_29_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_29\ <= iterateI_ho1_1_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_4 <= \c$bb_res_res_29\;


      end block;

    r_block_106 : block
        signal \c$bb_res_res_30\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_30_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_30\ <= iterateI_ho1_2_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_4 <= \c$bb_res_res_30\;


      end block;

    r_block_107 : block
        signal \c$bb_res_res_31\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_31_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_31\ <= iterateI_ho1_3_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_4 <= \c$bb_res_res_31\;


      end block;

    r_block_108 : block
        signal \c$bb_res_res_32\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_32_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_32\ <= iterateI_ho1_4_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_1 <= \c$bb_res_res_32\;


      end block;

    r_block_109 : block
        signal \c$bb_res_res_33\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_33_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_33\ <= iterateI_ho1_5_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_0 <= \c$bb_res_res_33\;


      end block;

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_boolean'( \c$ds5_case_alt\
                                                           , iterateI_ho1_0_res_4
                                                           , iterateI_ho1_1_res_4
                                                           , iterateI_ho1_2_res_4
                                                           , iterateI_ho1_3_res_4
                                                           , iterateI_ho1_4_res_4
                                                           , iterateI_ho1_5_res_1
                                                           , iterateI_ho1_6_res_0 );


  end block;

  result_19 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  \c$vec2_1\ <= (ws_1(1 to ws_1'high));

  -- zipWith begin
  zipWith_1 : for i_12 in \c$ws_app_arg_3\'range generate
  begin
    \c$ws_app_arg_3\(i_12) <= ( Tuple2_4_sel0_Tuple2_1 => result_30(i_12)
               , Tuple2_4_sel1_Maybe_0 => \c$vec2_1\(i_12) );


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
    imap_4 : for i_13 in \c$ws_app_arg_4\'range generate
    begin
      fun_27 : block
      signal \c$case_alt_24\              : Spec_topEntity_types.Maybe_0;
      -- spec.hs:318:1-63
      signal t_13                         : unsigned(7 downto 0);
      -- spec.hs:318:1-63
      signal x_15                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_3\ : boolean;
    begin
      \c$ws_app_arg_4\(i_13) <= \c$case_alt_24\;

      \c$case_alt_selection_res_3\ <= t_13 = result_44.Tags_sel3_output1_0;

      \c$case_alt_24\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_13,max_1(1,integer(ceil(log2(real(4))))))))) when \c$case_alt_selection_res_3\ else
                         \c$ws_app_arg_3\(i_13).Tuple2_4_sel1_Maybe_0;

      t_13 <= x_15.Tuple2_1_sel0_unsigned;

      x_15 <= \c$ws_app_arg_3\(i_13).Tuple2_4_sel0_Tuple2_1;


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
    \c$case_scrut_3\ <= result_30(vec_index_2);
  end block;
  -- index end

  \c$case_scrut_4\ <=  ws_1(0) ;

  with (\c$case_scrut_4\(2 downto 2)) select
    result_20 <= false when "0",
                 \c$case_scrut_3\.Tuple2_1_sel1_boolean when others;

  i_1 <= Spec_topEntity_types.index_4'(spec_topentity_types.fromSLV(\c$case_scrut_4\(1 downto 0)));

  ws_1 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_4\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "--"))));

  iterateI_5 : block
    signal iterateI_ho1_0_res_5 : boolean;
    signal iterateI_ho1_1_res_5 : boolean;
    signal iterateI_ho1_2_res_5 : boolean;
    signal iterateI_ho1_3_res_5 : boolean;
    signal iterateI_ho1_4_res_5 : boolean;
    signal iterateI_ho1_5_res_2 : boolean;
    signal iterateI_ho1_6_res_1 : boolean;
  begin
    r_block_110 : block
        signal \c$bb_res_res_34\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_34_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_34\ <= \c$ds4_case_alt\;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_34\;


      end block;

    r_block_111 : block
        signal \c$bb_res_res_35\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_35_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_35\ <= iterateI_ho1_0_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_5 <= \c$bb_res_res_35\;


      end block;

    r_block_112 : block
        signal \c$bb_res_res_36\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_36_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_36\ <= iterateI_ho1_1_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_5 <= \c$bb_res_res_36\;


      end block;

    r_block_113 : block
        signal \c$bb_res_res_37\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_37_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_37\ <= iterateI_ho1_2_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_5 <= \c$bb_res_res_37\;


      end block;

    r_block_114 : block
        signal \c$bb_res_res_38\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_38_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_38\ <= iterateI_ho1_3_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_5 <= \c$bb_res_res_38\;


      end block;

    r_block_115 : block
        signal \c$bb_res_res_39\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_39_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_39\ <= iterateI_ho1_4_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_2 <= \c$bb_res_res_39\;


      end block;

    r_block_116 : block
        signal \c$bb_res_res_40\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_40_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_40\ <= iterateI_ho1_5_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_1 <= \c$bb_res_res_40\;


      end block;

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_boolean'( \c$ds4_case_alt\
                                                           , iterateI_ho1_0_res_5
                                                           , iterateI_ho1_1_res_5
                                                           , iterateI_ho1_2_res_5
                                                           , iterateI_ho1_3_res_5
                                                           , iterateI_ho1_4_res_5
                                                           , iterateI_ho1_5_res_2
                                                           , iterateI_ho1_6_res_1 );


  end block;

  result_21 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  \c$vec2_2\ <= (ws_2(1 to ws_2'high));

  -- zipWith begin
  zipWith_2 : for i_14 in \c$ws_app_arg_5\'range generate
  begin
    \c$ws_app_arg_5\(i_14) <= ( Tuple2_5_sel0_Tuple2_6 => result_36(i_14)
               , Tuple2_5_sel1_Maybe_1 => \c$vec2_2\(i_14) );


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
    imap_6 : for i_15 in \c$ws_app_arg_6\'range generate
    begin
      fun_28 : block
      signal \c$case_alt_25\              : Spec_topEntity_types.Maybe_1;
      -- spec.hs:318:1-63
      signal t_14                         : unsigned(7 downto 0);
      -- spec.hs:318:1-63
      signal x_16                         : Spec_topEntity_types.Tuple2_6;
      signal \c$case_alt_selection_res_4\ : boolean;
    begin
      \c$ws_app_arg_6\(i_15) <= \c$case_alt_25\;

      \c$case_alt_selection_res_4\ <= t_14 = result_44.Tags_sel2_output0_0;

      \c$case_alt_25\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_15,max_2(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res_4\ else
                         \c$ws_app_arg_5\(i_15).Tuple2_5_sel1_Maybe_1;

      t_14 <= x_16.Tuple2_6_sel0_unsigned;

      x_16 <= \c$ws_app_arg_5\(i_15).Tuple2_5_sel0_Tuple2_6;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_2 : block
    signal vec_index_3 : integer range 0 to 5-1;
  begin
    vec_index_3 <= to_integer((signed(std_logic_vector(resize(i_2,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut_5\ <= result_36(vec_index_3);
  end block;
  -- index end

  \c$case_scrut_6\ <=  ws_2(0) ;

  with (\c$case_scrut_6\(3 downto 3)) select
    result_22 <= to_signed(0,64) when "0",
                 \c$case_scrut_5\.Tuple2_6_sel1_signed when others;

  i_2 <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_6\(2 downto 0)));

  ws_2 <= Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(\c$ws_app_arg_6\) & Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(0 => std_logic_vector'("0" & "---"))));

  iterateI_6 : block
    signal iterateI_ho1_0_res_6 : boolean;
    signal iterateI_ho1_1_res_6 : boolean;
    signal iterateI_ho1_2_res_6 : boolean;
    signal iterateI_ho1_3_res_6 : boolean;
    signal iterateI_ho1_4_res_6 : boolean;
    signal iterateI_ho1_5_res_3 : boolean;
    signal iterateI_ho1_6_res_2 : boolean;
  begin
    r_block_117 : block
        signal \c$bb_res_res_41\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_41_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_41\ <= \c$ds3_case_alt\;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_41\;


      end block;

    r_block_118 : block
        signal \c$bb_res_res_42\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_42_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_42\ <= iterateI_ho1_0_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_6 <= \c$bb_res_res_42\;


      end block;

    r_block_119 : block
        signal \c$bb_res_res_43\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_43_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_43\ <= iterateI_ho1_1_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_6 <= \c$bb_res_res_43\;


      end block;

    r_block_120 : block
        signal \c$bb_res_res_44\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_44_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_44\ <= iterateI_ho1_2_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_6 <= \c$bb_res_res_44\;


      end block;

    r_block_121 : block
        signal \c$bb_res_res_45\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_45_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_45\ <= iterateI_ho1_3_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_6 <= \c$bb_res_res_45\;


      end block;

    r_block_122 : block
        signal \c$bb_res_res_46\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_46_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_46\ <= iterateI_ho1_4_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_3 <= \c$bb_res_res_46\;


      end block;

    r_block_123 : block
        signal \c$bb_res_res_47\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_47_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_47\ <= iterateI_ho1_5_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_2 <= \c$bb_res_res_47\;


      end block;

    \c$app_arg_6\ <= Spec_topEntity_types.array_of_boolean'( \c$ds3_case_alt\
                                                           , iterateI_ho1_0_res_6
                                                           , iterateI_ho1_1_res_6
                                                           , iterateI_ho1_2_res_6
                                                           , iterateI_ho1_3_res_6
                                                           , iterateI_ho1_4_res_6
                                                           , iterateI_ho1_5_res_3
                                                           , iterateI_ho1_6_res_2 );


  end block;

  result_23 <=  \c$app_arg_6\(\c$app_arg_6\'high) ;

  result_selection_res <= x0_0 and x1_0;

  result_24 <= t_1 when result_selection_res else
               result_25;

  x0_0 <= result_28.PacingOut2_sel0_boolean_0;

  x1_0 <= result_28.PacingOut2_sel1_boolean_1;

  -- register begin
  result_25_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_25 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_boolean => false ) );
    elsif rising_edge(clk) then
      if en then
        result_25 <= result_24;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_2\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_25) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => result_46.Tags_sel4_output2_0
             , Tuple2_1_sel1_boolean => result_27 and (result_26 < (to_signed(1,64))) )))));

  t_projection_3 <= (\c$vec_2\(0 to 1-1),\c$vec_2\(1 to \c$vec_2\'high));

  t_1 <= t_projection_3.Tuple2_8_sel1_array_of_Tuple2_1_1;

  \c$vec2_3\ <= (ws_3(1 to ws_3'high));

  -- zipWith begin
  zipWith_3 : for i_16 in \c$ws_app_arg_7\'range generate
  begin
    \c$ws_app_arg_7\(i_16) <= ( Tuple2_5_sel0_Tuple2_6 => result_36(i_16)
               , Tuple2_5_sel1_Maybe_1 => \c$vec2_3\(i_16) );


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
    imap_8 : for i_17 in \c$ws_app_arg_8\'range generate
    begin
      fun_29 : block
      signal \c$case_alt_26\              : Spec_topEntity_types.Maybe_1;
      -- spec.hs:318:1-63
      signal t_15                         : unsigned(7 downto 0);
      -- spec.hs:318:1-63
      signal x_17                         : Spec_topEntity_types.Tuple2_6;
      signal \c$case_alt_selection_res_5\ : boolean;
    begin
      \c$ws_app_arg_8\(i_17) <= \c$case_alt_26\;

      \c$case_alt_selection_res_5\ <= t_15 = result_46.Tags_sel2_output0_0;

      \c$case_alt_26\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_17,max_3(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res_5\ else
                         \c$ws_app_arg_7\(i_17).Tuple2_5_sel1_Maybe_1;

      t_15 <= x_17.Tuple2_6_sel0_unsigned;

      x_17 <= \c$ws_app_arg_7\(i_17).Tuple2_5_sel0_Tuple2_6;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_3 : block
    signal vec_index_4 : integer range 0 to 5-1;
  begin
    vec_index_4 <= to_integer((signed(std_logic_vector(resize(i_3,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut_7\ <= result_36(vec_index_4);
  end block;
  -- index end

  \c$case_scrut_8\ <=  ws_3(0) ;

  with (\c$case_scrut_8\(3 downto 3)) select
    result_26 <= to_signed(0,64) when "0",
                 \c$case_scrut_7\.Tuple2_6_sel1_signed when others;

  i_3 <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_8\(2 downto 0)));

  ws_3 <= Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(\c$ws_app_arg_8\) & Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(0 => std_logic_vector'("0" & "---"))));

  \c$vec2_4\ <= (ws_4(1 to ws_4'high));

  -- zipWith begin
  zipWith_4 : for i_18 in \c$ws_app_arg_9\'range generate
  begin
    \c$ws_app_arg_9\(i_18) <= ( Tuple2_4_sel0_Tuple2_1 => result_30(i_18)
               , Tuple2_4_sel1_Maybe_0 => \c$vec2_4\(i_18) );


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
    imap_10 : for i_19 in \c$ws_app_arg_10\'range generate
    begin
      fun_30 : block
      signal \c$case_alt_27\              : Spec_topEntity_types.Maybe_0;
      -- spec.hs:318:1-63
      signal t_16                         : unsigned(7 downto 0);
      -- spec.hs:318:1-63
      signal x_18                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_6\ : boolean;
    begin
      \c$ws_app_arg_10\(i_19) <= \c$case_alt_27\;

      \c$case_alt_selection_res_6\ <= t_16 = result_46.Tags_sel3_output1_0;

      \c$case_alt_27\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_19,max_4(1,integer(ceil(log2(real(4))))))))) when \c$case_alt_selection_res_6\ else
                         \c$ws_app_arg_9\(i_19).Tuple2_4_sel1_Maybe_0;

      t_16 <= x_18.Tuple2_1_sel0_unsigned;

      x_18 <= \c$ws_app_arg_9\(i_19).Tuple2_4_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_4 : block
    signal vec_index_5 : integer range 0 to 4-1;
  begin
    vec_index_5 <= to_integer((signed(std_logic_vector(resize(i_4,64)))))
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
    \c$case_scrut_9\ <= result_30(vec_index_5);
  end block;
  -- index end

  \c$case_scrut_10\ <=  ws_4(0) ;

  with (\c$case_scrut_10\(2 downto 2)) select
    result_27 <= false when "0",
                 \c$case_scrut_9\.Tuple2_1_sel1_boolean when others;

  i_4 <= Spec_topEntity_types.index_4'(spec_topentity_types.fromSLV(\c$case_scrut_10\(1 downto 0)));

  ws_4 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_10\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "--"))));

  iterateI_7 : block
    signal iterateI_ho1_0_res_7 : Spec_topEntity_types.PacingOut2;
    signal iterateI_ho1_1_res_7 : Spec_topEntity_types.PacingOut2;
    signal iterateI_ho1_2_res_7 : Spec_topEntity_types.PacingOut2;
    signal iterateI_ho1_3_res_7 : Spec_topEntity_types.PacingOut2;
  begin
    r_block_124 : block
        signal \c$bb_res_res_48\ : Spec_topEntity_types.PacingOut2 := (PacingOut2_sel0_boolean_0 => false, PacingOut2_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_48_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_48\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_48\;


      end block;

    r_block_125 : block
        signal \c$bb_res_res_49\ : Spec_topEntity_types.PacingOut2 := (PacingOut2_sel0_boolean_0 => false, PacingOut2_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_49_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_49\ <= iterateI_ho1_0_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_7 <= \c$bb_res_res_49\;


      end block;

    r_block_126 : block
        signal \c$bb_res_res_50\ : Spec_topEntity_types.PacingOut2 := (PacingOut2_sel0_boolean_0 => false, PacingOut2_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_50_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_50\ <= iterateI_ho1_1_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_7 <= \c$bb_res_res_50\;


      end block;

    r_block_127 : block
        signal \c$bb_res_res_51\ : Spec_topEntity_types.PacingOut2 := (PacingOut2_sel0_boolean_0 => false, PacingOut2_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_51_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_51\ <= iterateI_ho1_2_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_7 <= \c$bb_res_res_51\;


      end block;

    \c$app_arg_7\ <= Spec_topEntity_types.array_of_PacingOut2'( pOut2
                                                              , iterateI_ho1_0_res_7
                                                              , iterateI_ho1_1_res_7
                                                              , iterateI_ho1_2_res_7
                                                              , iterateI_ho1_3_res_7 );


  end block;

  result_28 <=  \c$app_arg_7\(\c$app_arg_7\'high) ;

  result_selection_res_4 <= x0_1 and x1_1;

  result_29 <= t_2 when result_selection_res_4 else
               result_30;

  x0_1 <= result_34.PacingOut1_sel0_boolean_0;

  x1_1 <= result_34.PacingOut1_sel1_boolean_1;

  -- register begin
  result_30_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_30 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_1_sel1_boolean => false ) );
    elsif rising_edge(clk) then
      if en then
        result_30 <= result_29;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_3\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_30) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => result_47.Tags_sel3_output1_0
             , Tuple2_1_sel1_boolean => result_31 >= result_33 )))));

  t_projection_6 <= (\c$vec_3\(0 to 1-1),\c$vec_3\(1 to \c$vec_3\'high));

  t_2 <= t_projection_6.Tuple2_9_sel1_array_of_Tuple2_1_1;

  \c$vec2_5\ <= (ws_5(1 to ws_5'high));

  -- zipWith begin
  zipWith_5 : for i_20 in \c$ws_app_arg_11\'range generate
  begin
    \c$ws_app_arg_11\(i_20) <= ( Tuple2_5_sel0_Tuple2_6 => result_36(i_20)
               , Tuple2_5_sel1_Maybe_1 => \c$vec2_5\(i_20) );


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
    imap_12 : for i_21 in \c$ws_app_arg_12\'range generate
    begin
      fun_31 : block
      signal \c$case_alt_28\              : Spec_topEntity_types.Maybe_1;
      -- spec.hs:318:1-63
      signal t_17                         : unsigned(7 downto 0);
      -- spec.hs:318:1-63
      signal x_19                         : Spec_topEntity_types.Tuple2_6;
      signal \c$case_alt_selection_res_7\ : boolean;
    begin
      \c$ws_app_arg_12\(i_21) <= \c$case_alt_28\;

      \c$case_alt_selection_res_7\ <= t_17 = result_32;

      \c$case_alt_28\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_21,max_5(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res_7\ else
                         \c$ws_app_arg_11\(i_21).Tuple2_5_sel1_Maybe_1;

      t_17 <= x_19.Tuple2_6_sel0_unsigned;

      x_19 <= \c$ws_app_arg_11\(i_21).Tuple2_5_sel0_Tuple2_6;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_5 : block
    signal vec_index_6 : integer range 0 to 5-1;
  begin
    vec_index_6 <= to_integer((signed(std_logic_vector(resize(i_5,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut_11\ <= result_36(vec_index_6);
  end block;
  -- index end

  \c$case_scrut_12\ <=  ws_5(0) ;

  with (\c$case_scrut_12\(3 downto 3)) select
    result_31 <= result_33 when "0",
                 \c$case_scrut_11\.Tuple2_6_sel1_signed when others;

  result_selection_res_5 <= result_47.Tags_sel2_output0_0 > to_unsigned(1,8);

  result_32 <= \c$app_arg_8\ when result_selection_res_5 else
               \c$app_arg_8\ + to_unsigned(12,8);

  \c$app_arg_8\ <= result_47.Tags_sel2_output0_0 - to_unsigned(1,8);

  i_5 <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_12\(2 downto 0)));

  ws_5 <= Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(\c$ws_app_arg_12\) & Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(0 => std_logic_vector'("0" & "---"))));

  \c$vec2_6\ <= (ws_6(1 to ws_6'high));

  -- zipWith begin
  zipWith_6 : for i_22 in \c$ws_app_arg_13\'range generate
  begin
    \c$ws_app_arg_13\(i_22) <= ( Tuple2_5_sel0_Tuple2_6 => result_36(i_22)
               , Tuple2_5_sel1_Maybe_1 => \c$vec2_6\(i_22) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_13 : block
    function max_6 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_14 : for i_23 in \c$ws_app_arg_14\'range generate
    begin
      fun_32 : block
      signal \c$case_alt_29\              : Spec_topEntity_types.Maybe_1;
      -- spec.hs:318:1-63
      signal t_18                         : unsigned(7 downto 0);
      -- spec.hs:318:1-63
      signal x_20                         : Spec_topEntity_types.Tuple2_6;
      signal \c$case_alt_selection_res_8\ : boolean;
    begin
      \c$ws_app_arg_14\(i_23) <= \c$case_alt_29\;

      \c$case_alt_selection_res_8\ <= t_18 = result_47.Tags_sel2_output0_0;

      \c$case_alt_29\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_23,max_6(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res_8\ else
                         \c$ws_app_arg_13\(i_23).Tuple2_5_sel1_Maybe_1;

      t_18 <= x_20.Tuple2_6_sel0_unsigned;

      x_20 <= \c$ws_app_arg_13\(i_23).Tuple2_5_sel0_Tuple2_6;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_6 : block
    signal vec_index_7 : integer range 0 to 5-1;
  begin
    vec_index_7 <= to_integer((signed(std_logic_vector(resize(i_6,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut_13\ <= result_36(vec_index_7);
  end block;
  -- index end

  \c$case_scrut_14\ <=  ws_6(0) ;

  with (\c$case_scrut_14\(3 downto 3)) select
    result_33 <= to_signed(0,64) when "0",
                 \c$case_scrut_13\.Tuple2_6_sel1_signed when others;

  i_6 <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_14\(2 downto 0)));

  ws_6 <= Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(\c$ws_app_arg_14\) & Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(0 => std_logic_vector'("0" & "---"))));

  iterateI_8 : block
    signal iterateI_ho1_0_res_8 : Spec_topEntity_types.PacingOut1;
    signal iterateI_ho1_1_res_8 : Spec_topEntity_types.PacingOut1;
    signal iterateI_ho1_2_res_8 : Spec_topEntity_types.PacingOut1;
  begin
    r_block_128 : block
        signal \c$bb_res_res_52\ : Spec_topEntity_types.PacingOut1 := (PacingOut1_sel0_boolean_0 => false, PacingOut1_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_52_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_52\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_8 <= \c$bb_res_res_52\;


      end block;

    r_block_129 : block
        signal \c$bb_res_res_53\ : Spec_topEntity_types.PacingOut1 := (PacingOut1_sel0_boolean_0 => false, PacingOut1_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_53_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_53\ <= iterateI_ho1_0_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_8 <= \c$bb_res_res_53\;


      end block;

    r_block_130 : block
        signal \c$bb_res_res_54\ : Spec_topEntity_types.PacingOut1 := (PacingOut1_sel0_boolean_0 => false, PacingOut1_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_54_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_54\ <= iterateI_ho1_1_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_8 <= \c$bb_res_res_54\;


      end block;

    \c$app_arg_9\ <= Spec_topEntity_types.array_of_PacingOut1'( pOut1
                                                              , iterateI_ho1_0_res_8
                                                              , iterateI_ho1_1_res_8
                                                              , iterateI_ho1_2_res_8 );


  end block;

  result_34 <=  \c$app_arg_9\(\c$app_arg_9\'high) ;

  result_selection_res_6 <= x0_2 and x1_2;

  result_35 <= t_3 when result_selection_res_6 else
               result_36;

  x0_2 <= result_43.PacingOut0_sel0_boolean_0;

  x1_2 <= result_43.PacingOut0_sel1_boolean_1;

  -- register begin
  result_36_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_36 <= Spec_topEntity_types.array_of_Tuple2_6'( ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_6_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_6_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_6_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_6_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
                                         , Tuple2_6_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_36 <= result_35;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_4\ <= (Spec_topEntity_types.array_of_Tuple2_6'(Spec_topEntity_types.array_of_Tuple2_6'(result_36) & Spec_topEntity_types.array_of_Tuple2_6'(Spec_topEntity_types.array_of_Tuple2_6'(0 => ( Tuple2_6_sel0_unsigned => result_48.Tags_sel2_output0_0
             , Tuple2_6_sel1_signed => (x_1 + y) )))));

  t_projection_9 <= (\c$vec_4\(0 to 1-1),\c$vec_4\(1 to \c$vec_4\'high));

  t_3 <= t_projection_9.Tuple2_10_sel1_array_of_Tuple2_6_1;

  x_projection_18 <= (to_signed(249,64) - y_0);

  x_1 <= x_projection_18;

  y_projection_8 <= (to_signed(23,64) - y_1);

  y <= y_projection_8;

  y_0 <= \c$out0_case_alt_0\;

  y_1 <= \c$out0_case_alt\;

  \c$out0_case_alt_selection_res\ <= tag = tagToMatch;

  \c$out0_case_alt\ <= dta when \c$out0_case_alt_selection_res\ else
                       to_signed(0,64);

  dta <= result_38.Tuple2_6_sel1_signed;

  tag <= result_38.Tuple2_6_sel0_unsigned;

  tagToMatch <= result_48.Tags_sel1_input1_0;

  result_37 <= ( Tuple2_6_sel0_unsigned => t_8
               , Tuple2_6_sel1_signed => \c$ds_app_arg\ ) when b_1 else
               result_38;

  -- register begin
  result_38_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_38 <= ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
  , Tuple2_6_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_38 <= result_37;
      end if;
    end if;
  end process;
  -- register end

  b_1 <= result_39;

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

  iterateI_9 : block
    signal iterateI_ho1_0_res_9 : boolean;
  begin
    r_block_131 : block
        signal \c$bb_res_res_55\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_55_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_55\ <= pIn1_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_9 <= \c$bb_res_res_55\;


      end block;

    \c$app_arg_10\ <= Spec_topEntity_types.array_of_boolean'( pIn1_0
                                                            , iterateI_ho1_0_res_9 );


  end block;

  result_39 <=  \c$app_arg_10\(\c$app_arg_10\'high) ;

  \c$out0_case_alt_selection_res_0\ <= tag_0 = tagToMatch_0;

  \c$out0_case_alt_0\ <= dta_0 when \c$out0_case_alt_selection_res_0\ else
                         to_signed(0,64);

  dta_0 <= result_41.Tuple2_6_sel1_signed;

  tagToMatch_0 <= result_48.Tags_sel0_input0_0;

  tag_0 <= result_41.Tuple2_6_sel0_unsigned;

  result_40 <= ( Tuple2_6_sel0_unsigned => t_9
               , Tuple2_6_sel1_signed => \c$ds_app_arg_0\ ) when b_2 else
               result_41;

  -- register begin
  result_41_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_41 <= ( Tuple2_6_sel0_unsigned => to_unsigned(13,8)
  , Tuple2_6_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_41 <= result_40;
      end if;
    end if;
  end process;
  -- register end

  b_2 <= result_42;

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

  inputs_2 <= result_61.Tuple3_sel2_Tuple3_0.Tuple3_0_sel0_Inputs_3;

  iterateI_10 : block
    signal iterateI_ho1_0_res_10 : boolean;
  begin
    r_block_132 : block
        signal \c$bb_res_res_56\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_56_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_56\ <= pIn0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_10 <= \c$bb_res_res_56\;


      end block;

    \c$app_arg_11\ <= Spec_topEntity_types.array_of_boolean'( pIn0_0
                                                            , iterateI_ho1_0_res_10 );


  end block;

  result_42 <=  \c$app_arg_11\(\c$app_arg_11\'high) ;

  iterateI_11 : block
    signal iterateI_ho1_0_res_11 : Spec_topEntity_types.PacingOut0;
    signal iterateI_ho1_1_res_9  : Spec_topEntity_types.PacingOut0;
  begin
    r_block_133 : block
        signal \c$bb_res_res_57\ : Spec_topEntity_types.PacingOut0 := (PacingOut0_sel0_boolean_0 => false, PacingOut0_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_57_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_57\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_11 <= \c$bb_res_res_57\;


      end block;

    r_block_134 : block
        signal \c$bb_res_res_58\ : Spec_topEntity_types.PacingOut0 := (PacingOut0_sel0_boolean_0 => false, PacingOut0_sel1_boolean_1 => false);
      begin
        -- delay begin
        cbb_res_res_58_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_58\ <= iterateI_ho1_0_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_9 <= \c$bb_res_res_58\;


      end block;

    \c$app_arg_12\ <= Spec_topEntity_types.array_of_PacingOut0'( pOut0
                                                               , iterateI_ho1_0_res_11
                                                               , iterateI_ho1_1_res_9 );


  end block;

  result_43 <=  \c$app_arg_12\(\c$app_arg_12\'high) ;

  iterateI_12 : block
    signal iterateI_ho1_0_arg0_2 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_12 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_10 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_9  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_8  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_4_res_7  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_5_res_4  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_2 <= ( Tags_sel0_input0_0 => t_9
                             , Tags_sel1_input1_0 => t_8
                             , Tags_sel2_output0_0 => t_7
                             , Tags_sel3_output1_0 => t_6
                             , Tags_sel4_output2_0 => t_5
                             , Tags_sel5_output3_0 => t_4
                             , Tags_sel6_slide0 => t_5 );

    r_block_135 : block
        signal \c$bb_res_res_59\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_59_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_59\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_12 <= \c$bb_res_res_59\;


      end block;

    r_block_136 : block
        signal \c$bb_res_res_60\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_60_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_60\ <= iterateI_ho1_0_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_10 <= \c$bb_res_res_60\;


      end block;

    r_block_137 : block
        signal \c$bb_res_res_61\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_61_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_61\ <= iterateI_ho1_1_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_9 <= \c$bb_res_res_61\;


      end block;

    r_block_138 : block
        signal \c$bb_res_res_62\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_62_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_62\ <= iterateI_ho1_2_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_8 <= \c$bb_res_res_62\;


      end block;

    r_block_139 : block
        signal \c$bb_res_res_63\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_63_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_63\ <= iterateI_ho1_3_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_7 <= \c$bb_res_res_63\;


      end block;

    r_block_140 : block
        signal \c$bb_res_res_64\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_64_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_64\ <= iterateI_ho1_4_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_4 <= \c$bb_res_res_64\;


      end block;

    \c$app_arg_13\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_9
                                                         , Tags_sel1_input1_0 => t_8
                                                         , Tags_sel2_output0_0 => t_7
                                                         , Tags_sel3_output1_0 => t_6
                                                         , Tags_sel4_output2_0 => t_5
                                                         , Tags_sel5_output3_0 => t_4
                                                         , Tags_sel6_slide0 => t_5 )
                                                         , iterateI_ho1_0_res_12
                                                         , iterateI_ho1_1_res_10
                                                         , iterateI_ho1_2_res_9
                                                         , iterateI_ho1_3_res_8
                                                         , iterateI_ho1_4_res_7
                                                         , iterateI_ho1_5_res_4 );


  end block;

  result_44 <=  \c$app_arg_13\(\c$app_arg_13\'high) ;

  iterateI_13 : block
    signal iterateI_ho1_0_arg0_3 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_13 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_11 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_10 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_9  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_3 <= ( Tags_sel0_input0_0 => t_9
                             , Tags_sel1_input1_0 => t_8
                             , Tags_sel2_output0_0 => t_7
                             , Tags_sel3_output1_0 => t_6
                             , Tags_sel4_output2_0 => t_5
                             , Tags_sel5_output3_0 => t_4
                             , Tags_sel6_slide0 => t_5 );

    r_block_141 : block
        signal \c$bb_res_res_65\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_65_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_65\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_13 <= \c$bb_res_res_65\;


      end block;

    r_block_142 : block
        signal \c$bb_res_res_66\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_66_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_66\ <= iterateI_ho1_0_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_11 <= \c$bb_res_res_66\;


      end block;

    r_block_143 : block
        signal \c$bb_res_res_67\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_67_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_67\ <= iterateI_ho1_1_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_10 <= \c$bb_res_res_67\;


      end block;

    r_block_144 : block
        signal \c$bb_res_res_68\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_68_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_68\ <= iterateI_ho1_2_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_9 <= \c$bb_res_res_68\;


      end block;

    \c$app_arg_14\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_9
                                                         , Tags_sel1_input1_0 => t_8
                                                         , Tags_sel2_output0_0 => t_7
                                                         , Tags_sel3_output1_0 => t_6
                                                         , Tags_sel4_output2_0 => t_5
                                                         , Tags_sel5_output3_0 => t_4
                                                         , Tags_sel6_slide0 => t_5 )
                                                         , iterateI_ho1_0_res_13
                                                         , iterateI_ho1_1_res_11
                                                         , iterateI_ho1_2_res_10
                                                         , iterateI_ho1_3_res_9 );


  end block;

  result_45 <=  \c$app_arg_14\(\c$app_arg_14\'high) ;

  iterateI_14 : block
    signal iterateI_ho1_0_arg0_4 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_14 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_12 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_11 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_4 <= ( Tags_sel0_input0_0 => t_9
                             , Tags_sel1_input1_0 => t_8
                             , Tags_sel2_output0_0 => t_7
                             , Tags_sel3_output1_0 => t_6
                             , Tags_sel4_output2_0 => t_5
                             , Tags_sel5_output3_0 => t_4
                             , Tags_sel6_slide0 => t_5 );

    r_block_145 : block
        signal \c$bb_res_res_69\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_69_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_69\ <= iterateI_ho1_0_arg0_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_14 <= \c$bb_res_res_69\;


      end block;

    r_block_146 : block
        signal \c$bb_res_res_70\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_70_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_70\ <= iterateI_ho1_0_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_12 <= \c$bb_res_res_70\;


      end block;

    r_block_147 : block
        signal \c$bb_res_res_71\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_71_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_71\ <= iterateI_ho1_1_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_11 <= \c$bb_res_res_71\;


      end block;

    \c$app_arg_15\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_9
                                                         , Tags_sel1_input1_0 => t_8
                                                         , Tags_sel2_output0_0 => t_7
                                                         , Tags_sel3_output1_0 => t_6
                                                         , Tags_sel4_output2_0 => t_5
                                                         , Tags_sel5_output3_0 => t_4
                                                         , Tags_sel6_slide0 => t_5 )
                                                         , iterateI_ho1_0_res_14
                                                         , iterateI_ho1_1_res_12
                                                         , iterateI_ho1_2_res_11 );


  end block;

  result_46 <=  \c$app_arg_15\(\c$app_arg_15\'high) ;

  iterateI_15 : block
    signal iterateI_ho1_0_arg0_5 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_15 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_13 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_5 <= ( Tags_sel0_input0_0 => t_9
                             , Tags_sel1_input1_0 => t_8
                             , Tags_sel2_output0_0 => t_7
                             , Tags_sel3_output1_0 => t_6
                             , Tags_sel4_output2_0 => t_5
                             , Tags_sel5_output3_0 => t_4
                             , Tags_sel6_slide0 => t_5 );

    r_block_148 : block
        signal \c$bb_res_res_72\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_72_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_72\ <= iterateI_ho1_0_arg0_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_15 <= \c$bb_res_res_72\;


      end block;

    r_block_149 : block
        signal \c$bb_res_res_73\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_73_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_73\ <= iterateI_ho1_0_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_13 <= \c$bb_res_res_73\;


      end block;

    \c$app_arg_16\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_9
                                                         , Tags_sel1_input1_0 => t_8
                                                         , Tags_sel2_output0_0 => t_7
                                                         , Tags_sel3_output1_0 => t_6
                                                         , Tags_sel4_output2_0 => t_5
                                                         , Tags_sel5_output3_0 => t_4
                                                         , Tags_sel6_slide0 => t_5 )
                                                         , iterateI_ho1_0_res_15
                                                         , iterateI_ho1_1_res_13 );


  end block;

  result_47 <=  \c$app_arg_16\(\c$app_arg_16\'high) ;

  iterateI_16 : block
    signal iterateI_ho1_0_arg0_6 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_16 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_6 <= ( Tags_sel0_input0_0 => t_9
                             , Tags_sel1_input1_0 => t_8
                             , Tags_sel2_output0_0 => t_7
                             , Tags_sel3_output1_0 => t_6
                             , Tags_sel4_output2_0 => t_5
                             , Tags_sel5_output3_0 => t_4
                             , Tags_sel6_slide0 => t_5 );

    r_block_150 : block
        signal \c$bb_res_res_74\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(13,8)
      , Tags_sel1_input1_0 => to_unsigned(13,8)
      , Tags_sel2_output0_0 => to_unsigned(13,8)
      , Tags_sel3_output1_0 => to_unsigned(13,8)
      , Tags_sel4_output2_0 => to_unsigned(13,8)
      , Tags_sel5_output3_0 => to_unsigned(13,8)
      , Tags_sel6_slide0 => to_unsigned(13,8) );
      begin
        -- delay begin
        cbb_res_res_74_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_74\ <= iterateI_ho1_0_arg0_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_16 <= \c$bb_res_res_74\;


      end block;

    \c$app_arg_17\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_9
                                                         , Tags_sel1_input1_0 => t_8
                                                         , Tags_sel2_output0_0 => t_7
                                                         , Tags_sel3_output1_0 => t_6
                                                         , Tags_sel4_output2_0 => t_5
                                                         , Tags_sel5_output3_0 => t_4
                                                         , Tags_sel6_slide0 => t_5 )
                                                         , iterateI_ho1_0_res_16 );


  end block;

  result_48 <=  \c$app_arg_17\(\c$app_arg_17\'high) ;

  -- register begin
  t_4_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_4 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_4 <= result_49;
      end if;
    end if;
  end process;
  -- register end

  result_selection_19 <= pOut3;

  result_49 <= result_50 when result_selection_19 else
               t_4;

  b_3 <= t_4 = to_unsigned(12,8);

  f1 <= t_4 + to_unsigned(1,8);

  result_50 <= to_unsigned(1,8) when b_3 else
               f1;

  pOut3 <= pacings.Pacings_1_sel5_pacingOut3;

  -- register begin
  t_5_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_5 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_5 <= result_51;
      end if;
    end if;
  end process;
  -- register end

  result_51 <= result_52 when \c$ds5_case_alt\ else
               t_5;

  b_4 <= t_5 = to_unsigned(12,8);

  f1_0 <= t_5 + to_unsigned(1,8);

  result_52 <= to_unsigned(1,8) when b_4 else
               f1_0;

  \c$ds5_case_alt\ <= x0_3 and x1_3;

  x1_3 <= pOut2.PacingOut2_sel1_boolean_1;

  x0_3 <= pOut2.PacingOut2_sel0_boolean_0;

  pOut2 <= pacings.Pacings_1_sel4_pacingOut2_0;

  -- register begin
  t_6_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_6 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_6 <= result_53;
      end if;
    end if;
  end process;
  -- register end

  result_53 <= result_54 when \c$ds4_case_alt\ else
               t_6;

  b_5 <= t_6 = to_unsigned(12,8);

  f1_1 <= t_6 + to_unsigned(1,8);

  result_54 <= to_unsigned(1,8) when b_5 else
               f1_1;

  \c$ds4_case_alt\ <= x0_4 and x1_4;

  x1_4 <= pOut1.PacingOut1_sel1_boolean_1;

  x0_4 <= pOut1.PacingOut1_sel0_boolean_0;

  pOut1 <= pacings.Pacings_1_sel3_pacingOut1_0;

  -- register begin
  t_7_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_7 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_7 <= result_55;
      end if;
    end if;
  end process;
  -- register end

  result_55 <= result_56 when \c$ds3_case_alt\ else
               t_7;

  b_6 <= t_7 = to_unsigned(12,8);

  f1_2 <= t_7 + to_unsigned(1,8);

  result_56 <= to_unsigned(1,8) when b_6 else
               f1_2;

  \c$ds3_case_alt\ <= x0_5 and x1_5;

  x1_5 <= pOut0.PacingOut0_sel1_boolean_1;

  x0_5 <= pOut0.PacingOut0_sel0_boolean_0;

  pOut0 <= pacings.Pacings_1_sel2_pacingOut0_0;

  -- register begin
  t_8_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_8 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_8 <= result_57;
      end if;
    end if;
  end process;
  -- register end

  result_selection_29 <= pIn1_0;

  result_57 <= result_58 when result_selection_29 else
               t_8;

  b_7 <= t_8 = to_unsigned(12,8);

  f1_3 <= t_8 + to_unsigned(1,8);

  result_58 <= to_unsigned(1,8) when b_7 else
               f1_3;

  pIn1_0 <= pacings.Pacings_1_sel1_pacingIn1;

  -- register begin
  t_9_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_9 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_9 <= result_59;
      end if;
    end if;
  end process;
  -- register end

  result_selection_33 <= pIn0_0;

  result_59 <= result_60 when result_selection_33 else
               t_9;

  b_8 <= t_9 = to_unsigned(12,8);

  f1_4 <= t_9 + to_unsigned(1,8);

  result_60 <= to_unsigned(1,8) when b_8 else
               f1_4;

  pIn0_0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_61.Tuple3_sel2_Tuple3_0.Tuple3_0_sel2_Pacings_1;

  result_61 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_20\
               , Tuple3_sel1_boolean_1 => \c$app_arg_19\
               , Tuple3_sel2_Tuple3_0 => \c$app_arg_18\ );

  -- register begin
  capp_arg_18_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_18\ <= ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false ) )
  , Tuple3_0_sel1_boolean => false
  , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingIn1 => false
  , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
  , PacingOut0_sel1_boolean_1 => false )
  , Pacings_1_sel3_pacingOut1_0 => ( PacingOut1_sel0_boolean_0 => false
  , PacingOut1_sel1_boolean_1 => false )
  , Pacings_1_sel4_pacingOut2_0 => ( PacingOut2_sel0_boolean_0 => false
  , PacingOut2_sel1_boolean_1 => false )
  , Pacings_1_sel5_pacingOut3 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_18\ <= \c$case_alt\;
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
                    , Pacings_1_sel3_pacingOut1_0 => ( PacingOut1_sel0_boolean_0 => false
                    , PacingOut1_sel1_boolean_1 => false )
                    , Pacings_1_sel4_pacingOut2_0 => ( PacingOut2_sel0_boolean_0 => false
                    , PacingOut2_sel1_boolean_1 => false )
                    , Pacings_1_sel5_pacingOut3 => false ) );

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
                    , Pacings_1_sel3_pacingOut1_0 => ( PacingOut1_sel0_boolean_0 => false
                    , PacingOut1_sel1_boolean_1 => false )
                    , Pacings_1_sel4_pacingOut2_0 => ( PacingOut2_sel0_boolean_0 => false
                    , PacingOut2_sel1_boolean_1 => false )
                    , Pacings_1_sel5_pacingOut3 => false ) );

  with (x_2) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_2) select
    \c$case_alt_3\ <= ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false ) )
                      , Tuple3_0_sel1_boolean => false
                      , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingIn1 => false
                      , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                      , PacingOut0_sel1_boolean_1 => false )
                      , Pacings_1_sel3_pacingOut1_0 => ( PacingOut1_sel0_boolean_0 => false
                      , PacingOut1_sel1_boolean_1 => false )
                      , Pacings_1_sel4_pacingOut2_0 => ( PacingOut2_sel0_boolean_0 => false
                      , PacingOut2_sel1_boolean_1 => false )
                      , Pacings_1_sel5_pacingOut3 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec_7 : block
    signal vec_index_8 : integer range 0 to 2-1;
  begin
    vec_index_8 <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index_8);
  end block;
  -- index end

  -- register begin
  capp_arg_19_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_19\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_19\ <= \c$case_alt_5\;
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

  with (x_2) select
    \c$case_alt_9\ <= false when x"0000000000000000",
                      true when others;

  -- register begin
  capp_arg_20_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_20\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_20\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_21\);

  \c$app_arg_21\ <= to_signed(1,64) when x_2 /= to_signed(2,64) else to_signed(0,64);

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
                                         , Pacings_1_sel3_pacingOut1_0 => ( PacingOut1_sel0_boolean_0 => false
                                         , PacingOut1_sel1_boolean_1 => false )
                                         , Pacings_1_sel4_pacingOut2_0 => ( PacingOut2_sel0_boolean_0 => false
                                         , PacingOut2_sel1_boolean_1 => false )
                                         , Pacings_1_sel5_pacingOut3 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_3 => ( Inputs_3_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_3_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_boolean => false
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingOut0_0 => ( PacingOut0_sel0_boolean_0 => false
                                         , PacingOut0_sel1_boolean_1 => false )
                                         , Pacings_1_sel3_pacingOut1_0 => ( PacingOut1_sel0_boolean_0 => false
                                         , PacingOut1_sel1_boolean_1 => false )
                                         , Pacings_1_sel4_pacingOut2_0 => ( PacingOut2_sel0_boolean_0 => false
                                         , PacingOut2_sel1_boolean_1 => false )
                                         , Pacings_1_sel5_pacingOut3 => false ) ) );
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

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_11_sel0_array_of_Tuple3_0_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_6\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_6\(0 to 2-1),\c$vec_6\(2 to \c$vec_6\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_11_sel0_array_of_Tuple3_0_0;

  qData <= result_6.Tuple2_sel1_Tuple3_0;

  x_2 <= cursor;

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

  push <= result_6.Tuple2_sel0_boolean;

  \c$cursor_case_alt_0\ <= cursor when pop else
                           \c$cursor_case_alt_1\;

  \c$cursor_case_alt_1\ <= cursor when \c$cursor_case_scrut\ else
                           (x_2 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_2 = to_signed(2,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_7.Tuple2_0_sel0_boolean;

  with (x_2) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_2 - to_signed(1,64));

  result <= result_7.Tuple2_0_sel1_Outputs;

  result_0 <= result.Outputs_sel0_output0;

  result_1 <= result.Outputs_sel1_output1;

  result_2 <= result.Outputs_sel2_output2;

  result_3 <= result.Outputs_sel3_output3;

  result_0_0 <= result_0.ValidInt_sel0_value;

  result_0_1 <= result_0.ValidInt_sel1_valid;

  result_1_0 <= result_1.ValidBool_sel0_value_0;

  result_1_1 <= result_1.ValidBool_sel1_valid_0;

  result_2_0 <= result_2.ValidBool_sel0_value_0;

  result_2_1 <= result_2.ValidBool_sel1_valid_0;

  result_3_0 <= result_3.ValidBool_sel0_value_0;

  result_3_1 <= result_3.ValidBool_sel1_valid_0;


end;

