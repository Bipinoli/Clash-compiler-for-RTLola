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
       clk         : in Spec_topEntity_types.clk_TestDomain;
       -- reset
       rst         : in Spec_topEntity_types.rst_TestDomain;
       -- enable
       en          : in Spec_topEntity_types.en_TestDomain;
       inputs_0_0  : in signed(63 downto 0);
       inputs_0_1  : in boolean;
       inputs_1_0  : in signed(63 downto 0);
       inputs_1_1  : in boolean;
       inputs_2_0  : in signed(63 downto 0);
       inputs_2_1  : in boolean;
       result_0_0  : out signed(63 downto 0);
       result_0_1  : out boolean;
       result_1_0  : out boolean;
       result_1_1  : out boolean;
       result_2_0  : out boolean;
       result_2_1  : out boolean;
       result_3_0  : out boolean;
       result_3_1  : out boolean;
       result_4_0  : out signed(63 downto 0);
       result_4_1  : out boolean;
       result_5_0  : out boolean;
       result_5_1  : out boolean;
       result_6_0  : out boolean;
       result_6_1  : out boolean;
       result_7_0  : out boolean;
       result_7_1  : out boolean;
       result_8_0  : out boolean;
       result_8_1  : out boolean;
       result_9_0  : out boolean;
       result_9_1  : out boolean;
       result_10_0 : out boolean;
       result_10_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_11                       : signed(63 downto 0);
  signal x                               : signed(63 downto 0);
  signal result_12                       : signed(63 downto 0) := (to_signed(0,64));
  signal result_13                       : signed(63 downto 0);
  signal x_0                             : signed(63 downto 0);
  signal result_14                       : signed(63 downto 0) := (to_signed(0,64));
  signal result_15                       : Spec_topEntity_types.Tuple2;
  -- spec.hs:(363,1)-(365,31)
  signal timer0Over                      : boolean;
  -- spec.hs:(363,1)-(365,31)
  signal timer1Over                      : boolean;
  signal result_16                       : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                     : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_17                       : boolean;
  signal result_18                       : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(1102,1)-(1106,29)
  signal result_19                       : Spec_topEntity_types.Tuple2_1 := (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
  -- spec.hs:(1102,1)-(1106,29)
  signal b                               : boolean;
  -- spec.hs:(1102,1)-(1106,29)
  signal t                               : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(499,1)-(501,33)
  signal \c$ds1_case_alt\                : signed(63 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal dta                             : signed(63 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal tag                             : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal \c$ds1_case_scrut\              : Spec_topEntity_types.Tuple2_2;
  signal result_20                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 5);
  -- spec.hs:(921,1)-(925,36)
  signal result_21                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 5) := Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(921,1)-(925,36)
  signal b_0                             : boolean;
  -- spec.hs:(921,1)-(925,36)
  signal t_0                             : Spec_topEntity_types.array_of_Tuple2_2(0 to 5);
  -- spec.hs:(499,1)-(501,33)
  signal \c$win_app_arg\                 : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_0\                   : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_22                       : boolean;
  signal \c$app_arg_1\                   : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_23                       : boolean;
  signal \c$app_arg_2\                   : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_24                       : boolean;
  signal result_25                       : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(1087,1)-(1091,29)
  signal result_26                       : Spec_topEntity_types.Tuple2_1 := (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
  -- spec.hs:(1087,1)-(1091,29)
  signal b_1                             : boolean;
  -- spec.hs:(1087,1)-(1091,29)
  signal t_1                             : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(1300,1)-(1305,36)
  signal x_1                             : signed(63 downto 0);
  -- spec.hs:(1300,1)-(1305,36)
  signal win                             : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1300,1)-(1305,36)
  signal window                          : Spec_topEntity_types.Tuple2_3 := ( Tuple2_3_sel0_unsigned => to_unsigned(113,8)
, Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_27                       : Spec_topEntity_types.Tuple2_3;
  -- spec.hs:(1300,1)-(1305,36)
  signal t_2                             : Spec_topEntity_types.Tuple2_3;
  signal result_28                       : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1300,1)-(1305,36)
  signal \c$t_case_alt\                  : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1300,1)-(1305,36)
  signal \c$t_case_alt_0\                : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1300,1)-(1305,36)
  signal lastBucketUpdated               : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1300,1)-(1305,36)
  signal b_2                             : boolean;
  signal \c$app_arg_3\                   : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_29                       : boolean;
  signal \c$app_arg_4\                   : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_30                       : boolean;
  signal \c$app_arg_5\                   : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_31                       : boolean;
  signal result_32                       : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(1069,1)-(1074,29)
  signal result_33                       : Spec_topEntity_types.Tuple2_1 := (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
  -- spec.hs:(1069,1)-(1074,29)
  signal b_3                             : boolean;
  -- spec.hs:(1069,1)-(1074,29)
  signal t_3                             : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(1271,1)-(1276,36)
  signal x_2                             : signed(63 downto 0);
  -- spec.hs:(1271,1)-(1276,36)
  signal win_0                           : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1271,1)-(1276,36)
  signal window_0                        : Spec_topEntity_types.Tuple2_3 := ( Tuple2_3_sel0_unsigned => to_unsigned(113,8)
, Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_34                       : Spec_topEntity_types.Tuple2_3;
  -- spec.hs:(1271,1)-(1276,36)
  signal t_4                             : Spec_topEntity_types.Tuple2_3;
  signal result_35                       : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1271,1)-(1276,36)
  signal \c$t_case_alt_1\                : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1271,1)-(1276,36)
  signal \c$t_case_alt_2\                : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1271,1)-(1276,36)
  signal lastBucketUpdated_0             : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1271,1)-(1276,36)
  signal b_4                             : boolean;
  signal \c$app_arg_6\                   : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_36                       : boolean;
  -- spec.hs:(1242,1)-(1247,36)
  signal x_3                             : signed(63 downto 0);
  -- spec.hs:(1242,1)-(1247,36)
  signal win_1                           : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1242,1)-(1247,36)
  signal window_1                        : Spec_topEntity_types.Tuple2_3 := ( Tuple2_3_sel0_unsigned => to_unsigned(113,8)
, Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_37                       : Spec_topEntity_types.Tuple2_3;
  -- spec.hs:(1242,1)-(1247,36)
  signal t_5                             : Spec_topEntity_types.Tuple2_3;
  signal result_38                       : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1242,1)-(1247,36)
  signal \c$t_case_alt_3\                : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1242,1)-(1247,36)
  signal \c$t_case_alt_4\                : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1242,1)-(1247,36)
  signal lastBucketUpdated_1             : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1242,1)-(1247,36)
  signal b_5                             : boolean;
  signal \c$app_arg_7\                   : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_39                       : boolean;
  signal \c$app_arg_8\                   : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_40                       : boolean;
  signal \c$app_arg_9\                   : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_41                       : boolean;
  signal result_42                       : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(1051,1)-(1056,29)
  signal result_43                       : Spec_topEntity_types.Tuple2_1 := (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
  -- spec.hs:(1051,1)-(1056,29)
  signal b_6                             : boolean;
  -- spec.hs:(1051,1)-(1056,29)
  signal t_6                             : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(1213,1)-(1218,36)
  signal x_4                             : signed(63 downto 0);
  -- spec.hs:(1213,1)-(1218,36)
  signal win_2                           : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1213,1)-(1218,36)
  signal window_2                        : Spec_topEntity_types.Tuple2_3 := ( Tuple2_3_sel0_unsigned => to_unsigned(113,8)
, Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_44                       : Spec_topEntity_types.Tuple2_3;
  -- spec.hs:(1213,1)-(1218,36)
  signal t_7                             : Spec_topEntity_types.Tuple2_3;
  signal result_45                       : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1213,1)-(1218,36)
  signal \c$t_case_alt_5\                : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1213,1)-(1218,36)
  signal \c$t_case_alt_6\                : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1213,1)-(1218,36)
  signal lastBucketUpdated_2             : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1213,1)-(1218,36)
  signal b_7                             : boolean;
  signal \c$app_arg_10\                  : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_46                       : boolean;
  -- spec.hs:(1184,1)-(1189,36)
  signal x_5                             : signed(63 downto 0);
  -- spec.hs:(1184,1)-(1189,36)
  signal win_3                           : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1184,1)-(1189,36)
  signal window_3                        : Spec_topEntity_types.Tuple2_3 := ( Tuple2_3_sel0_unsigned => to_unsigned(113,8)
, Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                              , to_signed(0,64) ) );
  signal result_47                       : Spec_topEntity_types.Tuple2_3;
  -- spec.hs:(1184,1)-(1189,36)
  signal t_8                             : Spec_topEntity_types.Tuple2_3;
  signal result_48                       : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1184,1)-(1189,36)
  signal \c$t_case_alt_7\                : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1184,1)-(1189,36)
  signal \c$t_case_alt_8\                : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1184,1)-(1189,36)
  signal lastBucketUpdated_3             : Spec_topEntity_types.array_of_signed_64(0 to 1);
  -- spec.hs:(1184,1)-(1189,36)
  signal b_8                             : boolean;
  signal \c$app_arg_11\                  : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_49                       : boolean;
  signal \c$app_arg_12\                  : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_50                       : boolean;
  signal \c$app_arg_13\                  : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_51                       : boolean;
  -- spec.hs:(1036,1)-(1040,29)
  signal \c$t_app_arg\                   : signed(63 downto 0);
  signal result_52                       : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(1036,1)-(1040,29)
  signal result_53                       : Spec_topEntity_types.Tuple2_1 := (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
  -- spec.hs:(1036,1)-(1040,29)
  signal b_9                             : boolean;
  -- spec.hs:(1036,1)-(1040,29)
  signal t_9                             : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(1155,1)-(1160,38)
  signal x_6                             : signed(63 downto 0);
  -- spec.hs:(1155,1)-(1160,38)
  signal win_4                           : Spec_topEntity_types.array_of_signed_64(0 to 110);
  -- spec.hs:(1155,1)-(1160,38)
  signal window_4                        : Spec_topEntity_types.Tuple2_4 := ( Tuple2_4_sel0_unsigned => to_unsigned(113,8)
, Tuple2_4_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
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
  signal result_54                       : Spec_topEntity_types.Tuple2_4;
  -- spec.hs:(1155,1)-(1160,38)
  signal t_10                            : Spec_topEntity_types.Tuple2_4;
  signal result_55                       : Spec_topEntity_types.array_of_signed_64(0 to 110);
  -- spec.hs:(1155,1)-(1160,38)
  signal \c$t_case_alt_9\                : Spec_topEntity_types.array_of_signed_64(0 to 110);
  -- spec.hs:(1155,1)-(1160,38)
  signal \c$t_case_alt_10\               : Spec_topEntity_types.array_of_signed_64(0 to 110);
  -- spec.hs:(1155,1)-(1160,38)
  signal lastBucketUpdated_4             : Spec_topEntity_types.array_of_signed_64(0 to 110);
  -- spec.hs:(1155,1)-(1160,38)
  signal b_10                            : boolean;
  -- spec.hs:455:1-63
  signal \c$ws_app_arg\                  : Spec_topEntity_types.array_of_Tuple2_5(0 to 4);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_0\                : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$case_scrut\                  : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_0\                : Spec_topEntity_types.Maybe;
  signal result_56                       : signed(63 downto 0);
  -- spec.hs:455:1-63
  signal i                               : Spec_topEntity_types.index_5;
  -- spec.hs:455:1-63
  signal ws                              : Spec_topEntity_types.array_of_Maybe(0 to 5);
  signal \c$app_arg_14\                  : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_57                       : boolean;
  signal \c$app_arg_15\                  : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_58                       : boolean;
  signal \c$app_arg_16\                  : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_59                       : boolean;
  signal \c$app_arg_17\                  : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_60                       : boolean;
  signal result_61                       : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:(1023,1)-(1027,29)
  signal result_62                       : Spec_topEntity_types.Tuple2_1 := (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
  -- spec.hs:(1023,1)-(1027,29)
  signal b_11                            : boolean;
  -- spec.hs:(1023,1)-(1027,29)
  signal t_11                            : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_1\                : Spec_topEntity_types.array_of_Tuple2_6(0 to 1);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_2\                : Spec_topEntity_types.array_of_Maybe_0(0 to 1);
  signal \c$case_scrut_1\                : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_2\                : Spec_topEntity_types.Maybe_0;
  signal result_63                       : signed(63 downto 0);
  -- spec.hs:455:1-63
  signal i_0                             : Spec_topEntity_types.index_2;
  -- spec.hs:455:1-63
  signal ws_0                            : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal \c$app_arg_18\                  : Spec_topEntity_types.array_of_boolean(0 to 7);
  signal result_64                       : boolean;
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_3\                : Spec_topEntity_types.array_of_Tuple2_6(0 to 1);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_4\                : Spec_topEntity_types.array_of_Maybe_0(0 to 1);
  signal \c$case_scrut_3\                : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_4\                : Spec_topEntity_types.Maybe_0;
  signal result_65                       : signed(63 downto 0);
  -- spec.hs:455:1-63
  signal i_1                             : Spec_topEntity_types.index_2;
  -- spec.hs:455:1-63
  signal ws_1                            : Spec_topEntity_types.array_of_Maybe_0(0 to 2);
  signal \c$app_arg_19\                  : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_66                       : boolean;
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_5\                : Spec_topEntity_types.array_of_Tuple2_7(0 to 3);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_6\                : Spec_topEntity_types.array_of_Maybe_1(0 to 3);
  signal \c$case_scrut_5\                : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_6\                : Spec_topEntity_types.Maybe_1;
  signal result_67                       : boolean;
  -- spec.hs:455:1-63
  signal i_2                             : Spec_topEntity_types.index_4;
  -- spec.hs:455:1-63
  signal ws_2                            : Spec_topEntity_types.array_of_Maybe_1(0 to 4);
  signal \c$app_arg_20\                  : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_68                       : boolean;
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_7\                : Spec_topEntity_types.array_of_Tuple2_8(0 to 4);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_8\                : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$case_scrut_7\                : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_8\                : Spec_topEntity_types.Maybe;
  signal result_69                       : boolean;
  -- spec.hs:455:1-63
  signal i_3                             : Spec_topEntity_types.index_5;
  -- spec.hs:455:1-63
  signal ws_3                            : Spec_topEntity_types.array_of_Maybe(0 to 5);
  signal \c$app_arg_21\                  : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_70                       : boolean;
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_9\                : Spec_topEntity_types.array_of_Tuple2_8(0 to 4);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_10\               : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$case_scrut_9\                : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_10\               : Spec_topEntity_types.Maybe;
  signal result_71                       : boolean;
  -- spec.hs:455:1-63
  signal i_4                             : Spec_topEntity_types.index_5;
  -- spec.hs:455:1-63
  signal ws_4                            : Spec_topEntity_types.array_of_Maybe(0 to 5);
  signal \c$app_arg_22\                  : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_72                       : boolean;
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_11\               : Spec_topEntity_types.array_of_Tuple2_9(0 to 5);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_12\               : Spec_topEntity_types.array_of_Maybe_2(0 to 5);
  signal \c$case_scrut_11\               : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_12\               : Spec_topEntity_types.Maybe_2;
  signal result_73                       : signed(63 downto 0);
  -- spec.hs:455:1-63
  signal i_5                             : Spec_topEntity_types.index_6;
  -- spec.hs:455:1-63
  signal ws_5                            : Spec_topEntity_types.array_of_Maybe_2(0 to 6);
  signal \c$app_arg_23\                  : Spec_topEntity_types.array_of_boolean(0 to 8);
  signal result_74                       : boolean;
  signal result_75                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 4);
  -- spec.hs:(933,1)-(937,36)
  signal result_76                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 4) := Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(933,1)-(937,36)
  signal b_12                            : boolean;
  -- spec.hs:(933,1)-(937,36)
  signal t_12                            : Spec_topEntity_types.array_of_Tuple2_2(0 to 4);
  -- spec.hs:(499,1)-(501,33)
  signal \c$input2Win_app_arg\           : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_24\                  : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_77                       : boolean;
  -- spec.hs:(1007,1)-(1011,36)
  signal \c$t_app_arg_0\                 : signed(63 downto 0);
  signal result_78                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:(1007,1)-(1011,36)
  signal result_79                       : Spec_topEntity_types.array_of_Tuple2_2(0 to 1) := Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(1007,1)-(1011,36)
  signal b_13                            : boolean;
  -- spec.hs:(1007,1)-(1011,36)
  signal t_13                            : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:(1126,1)-(1131,38)
  signal x_7                             : signed(63 downto 0);
  -- spec.hs:(1126,1)-(1131,38)
  signal win_5                           : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(1126,1)-(1131,38)
  signal window_5                        : Spec_topEntity_types.Tuple2_10 := ( Tuple2_10_sel0_unsigned => to_unsigned(113,8)
, Tuple2_10_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
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
  signal result_80                       : Spec_topEntity_types.Tuple2_10;
  -- spec.hs:(1126,1)-(1131,38)
  signal t_14                            : Spec_topEntity_types.Tuple2_10;
  signal result_81                       : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(1126,1)-(1131,38)
  signal \c$t_case_alt_11\               : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(1126,1)-(1131,38)
  signal \c$t_case_alt_12\               : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(1126,1)-(1131,38)
  signal lastBucketUpdated_5             : Spec_topEntity_types.array_of_signed_64(0 to 100);
  -- spec.hs:(1126,1)-(1131,38)
  signal b_14                            : boolean;
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_13\               : Spec_topEntity_types.array_of_Tuple2_7(0 to 3);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_14\               : Spec_topEntity_types.array_of_Maybe_1(0 to 3);
  signal \c$case_scrut_13\               : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_14\               : Spec_topEntity_types.Maybe_1;
  signal result_82                       : boolean;
  -- spec.hs:455:1-63
  signal i_6                             : Spec_topEntity_types.index_4;
  -- spec.hs:455:1-63
  signal ws_6                            : Spec_topEntity_types.array_of_Maybe_1(0 to 4);
  signal \c$app_arg_25\                  : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_83                       : boolean;
  -- spec.hs:(499,1)-(501,33)
  signal slides                          : Spec_topEntity_types.Slides_1;
  signal \c$app_arg_26\                  : Spec_topEntity_types.array_of_boolean(0 to 5);
  signal result_84                       : boolean;
  signal \c$app_arg_27\                  : Spec_topEntity_types.array_of_boolean(0 to 6);
  signal result_85                       : boolean;
  signal result_86                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  -- spec.hs:(990,1)-(997,37)
  signal result_87                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 3) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false ) );
  -- spec.hs:(990,1)-(997,37)
  signal b_15                            : boolean;
  -- spec.hs:(990,1)-(997,37)
  signal t_15                            : Spec_topEntity_types.array_of_Tuple2_1(0 to 3);
  -- spec.hs:447:1-65
  signal \c$ws_app_arg_15\               : Spec_topEntity_types.array_of_Tuple2_8(0 to 4);
  -- spec.hs:447:1-65
  signal \c$ws_app_arg_16\               : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$case_scrut_15\               : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_16\               : Spec_topEntity_types.Maybe;
  signal result_88                       : boolean;
  signal result_89                       : unsigned(7 downto 0);
  signal \c$app_arg_28\                  : unsigned(7 downto 0);
  -- spec.hs:447:1-65
  signal i_7                             : Spec_topEntity_types.index_5;
  -- spec.hs:447:1-65
  signal ws_7                            : Spec_topEntity_types.array_of_Maybe(0 to 5);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_17\               : Spec_topEntity_types.array_of_Tuple2_8(0 to 4);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_18\               : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$case_scrut_17\               : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_18\               : Spec_topEntity_types.Maybe;
  signal result_90                       : boolean;
  -- spec.hs:455:1-63
  signal i_8                             : Spec_topEntity_types.index_5;
  -- spec.hs:455:1-63
  signal ws_8                            : Spec_topEntity_types.array_of_Maybe(0 to 5);
  -- spec.hs:447:1-65
  signal \c$ws_app_arg_19\               : Spec_topEntity_types.array_of_Tuple2_8(0 to 4);
  -- spec.hs:447:1-65
  signal \c$ws_app_arg_20\               : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$case_scrut_19\               : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_20\               : Spec_topEntity_types.Maybe;
  signal result_91                       : boolean;
  signal result_92                       : unsigned(7 downto 0);
  signal \c$app_arg_29\                  : unsigned(7 downto 0);
  -- spec.hs:447:1-65
  signal i_9                             : Spec_topEntity_types.index_5;
  -- spec.hs:447:1-65
  signal ws_9                            : Spec_topEntity_types.array_of_Maybe(0 to 5);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_21\               : Spec_topEntity_types.array_of_Tuple2_8(0 to 4);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_22\               : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$case_scrut_21\               : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_22\               : Spec_topEntity_types.Maybe;
  signal result_93                       : boolean;
  -- spec.hs:455:1-63
  signal i_10                            : Spec_topEntity_types.index_5;
  -- spec.hs:455:1-63
  signal ws_10                           : Spec_topEntity_types.array_of_Maybe(0 to 5);
  signal \c$app_arg_30\                  : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_94                       : boolean;
  signal result_95                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 4);
  -- spec.hs:(975,1)-(980,37)
  signal result_96                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 4) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false ) );
  -- spec.hs:(975,1)-(980,37)
  signal b_16                            : boolean;
  -- spec.hs:(975,1)-(980,37)
  signal t_16                            : Spec_topEntity_types.array_of_Tuple2_1(0 to 4);
  signal x_8                             : signed(63 downto 0);
  signal y                               : signed(63 downto 0);
  signal \c$app_arg_31\                  : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_97                       : boolean;
  signal result_98                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 4);
  -- spec.hs:(960,1)-(965,37)
  signal result_99                       : Spec_topEntity_types.array_of_Tuple2_1(0 to 4) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_1_sel1_boolean => false ) );
  -- spec.hs:(960,1)-(965,37)
  signal b_17                            : boolean;
  -- spec.hs:(960,1)-(965,37)
  signal t_17                            : Spec_topEntity_types.array_of_Tuple2_1(0 to 4);
  signal x_9                             : signed(63 downto 0);
  signal y_0                             : signed(63 downto 0);
  -- spec.hs:447:1-65
  signal \c$ws_app_arg_23\               : Spec_topEntity_types.array_of_Tuple2_9(0 to 5);
  -- spec.hs:447:1-65
  signal \c$ws_app_arg_24\               : Spec_topEntity_types.array_of_Maybe_2(0 to 5);
  signal \c$case_scrut_23\               : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_24\               : Spec_topEntity_types.Maybe_2;
  signal result_100                      : signed(63 downto 0);
  signal result_101                      : unsigned(7 downto 0);
  signal \c$app_arg_32\                  : unsigned(7 downto 0);
  -- spec.hs:447:1-65
  signal i_11                            : Spec_topEntity_types.index_6;
  -- spec.hs:447:1-65
  signal ws_11                           : Spec_topEntity_types.array_of_Maybe_2(0 to 6);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_25\               : Spec_topEntity_types.array_of_Tuple2_9(0 to 5);
  -- spec.hs:455:1-63
  signal \c$ws_app_arg_26\               : Spec_topEntity_types.array_of_Maybe_2(0 to 5);
  signal \c$case_scrut_25\               : Spec_topEntity_types.Tuple2_2;
  signal \c$case_scrut_26\               : Spec_topEntity_types.Maybe_2;
  signal result_102                      : signed(63 downto 0);
  -- spec.hs:455:1-63
  signal i_12                            : Spec_topEntity_types.index_6;
  -- spec.hs:455:1-63
  signal ws_12                           : Spec_topEntity_types.array_of_Maybe_2(0 to 6);
  signal \c$app_arg_33\                  : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_103                      : boolean;
  signal result_104                      : Spec_topEntity_types.array_of_Tuple2_2(0 to 5);
  -- spec.hs:(946,1)-(950,36)
  signal result_105                      : Spec_topEntity_types.array_of_Tuple2_2(0 to 5) := Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                       , Tuple2_2_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:(946,1)-(950,36)
  signal b_18                            : boolean;
  -- spec.hs:(946,1)-(950,36)
  signal t_18                            : Spec_topEntity_types.array_of_Tuple2_2(0 to 5);
  -- spec.hs:(499,1)-(501,33)
  signal \c$out0_case_alt\               : signed(63 downto 0);
  -- spec.hs:477:1-46
  signal dta_0                           : signed(63 downto 0);
  -- spec.hs:477:1-46
  signal tag_0                           : unsigned(7 downto 0);
  signal result_106                      : Spec_topEntity_types.Tuple2_2;
  -- spec.hs:(911,1)-(915,28)
  signal result_107                      : Spec_topEntity_types.Tuple2_2 := ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
, Tuple2_2_sel1_signed => to_signed(0,64) );
  -- spec.hs:(911,1)-(915,28)
  signal b_19                            : boolean;
  -- spec.hs:477:1-46
  signal \c$ds_app_arg\                  : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(499,1)-(501,33)
  signal inputs_3                        : Spec_topEntity_types.Inputs_4;
  signal \c$app_arg_34\                  : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_108                      : boolean;
  signal \c$app_arg_35\                  : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_109                      : Spec_topEntity_types.Tags;
  signal \c$app_arg_36\                  : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_110                      : boolean;
  signal \c$app_arg_37\                  : Spec_topEntity_types.array_of_Tags(0 to 7);
  signal result_111                      : Spec_topEntity_types.Tags;
  signal \c$app_arg_38\                  : Spec_topEntity_types.array_of_Tags(0 to 6);
  signal result_112                      : Spec_topEntity_types.Tags;
  signal \c$app_arg_39\                  : Spec_topEntity_types.array_of_Tags(0 to 5);
  signal result_113                      : Spec_topEntity_types.Tags;
  signal \c$app_arg_40\                  : Spec_topEntity_types.array_of_Tags(0 to 4);
  signal result_114                      : Spec_topEntity_types.Tags;
  signal \c$app_arg_41\                  : Spec_topEntity_types.array_of_Tags(0 to 3);
  signal result_115                      : Spec_topEntity_types.Tags;
  signal \c$app_arg_42\                  : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_116                      : Spec_topEntity_types.Tags;
  -- spec.hs:(899,9)-(901,29)
  signal t_19                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_117                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_20                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1                              : unsigned(7 downto 0);
  signal result_118                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut10                          : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_20                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_119                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_21                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_0                            : unsigned(7 downto 0);
  signal result_120                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut9                           : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_21                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_121                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_22                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_1                            : unsigned(7 downto 0);
  signal result_122                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut8                           : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_22                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_123                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_23                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_2                            : unsigned(7 downto 0);
  signal result_124                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut7                           : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_23                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_125                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_24                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_3                            : unsigned(7 downto 0);
  signal result_126                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut6                           : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_24                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_127                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_25                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_4                            : unsigned(7 downto 0);
  signal result_128                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut5                           : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_25                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_129                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_26                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_5                            : unsigned(7 downto 0);
  signal result_130                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut4                           : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_26                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_131                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_27                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_6                            : unsigned(7 downto 0);
  signal result_132                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut3                           : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_27                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_133                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_28                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_7                            : unsigned(7 downto 0);
  signal result_134                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut2                           : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_28                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_135                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_29                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_8                            : unsigned(7 downto 0);
  signal result_136                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut1                           : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_29                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_137                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_30                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_9                            : unsigned(7 downto 0);
  signal result_138                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pOut0                           : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_30                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_139                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_31                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_10                           : unsigned(7 downto 0);
  signal result_140                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pIn2                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_31                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_141                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_32                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_11                           : unsigned(7 downto 0);
  signal result_142                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pIn1                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal t_32                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_143                      : unsigned(7 downto 0);
  -- spec.hs:(899,9)-(901,29)
  signal b_33                            : boolean;
  -- spec.hs:(899,9)-(901,29)
  signal f1_12                           : unsigned(7 downto 0);
  signal result_144                      : unsigned(7 downto 0);
  -- spec.hs:(499,1)-(501,33)
  signal pIn0                            : boolean;
  -- spec.hs:(499,1)-(501,33)
  signal pacings                         : Spec_topEntity_types.Pacings_1;
  signal result_145                      : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_43\                  : Spec_topEntity_types.Tuple3_0 := ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false ) )
, Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
, Slides_1_sel1_slide1 => false
, Slides_1_sel2_slide2 => false
, Slides_1_sel3_slide3 => false
, Slides_1_sel4_slide4 => false
, Slides_1_sel5_slide5 => false
, Slides_1_sel6_slide6 => false )
, Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingIn1 => false
, Pacings_1_sel2_pacingIn2 => false
, Pacings_1_sel3_pacingOut0 => false
, Pacings_1_sel4_pacingOut1 => false
, Pacings_1_sel5_pacingOut2 => false
, Pacings_1_sel6_pacingOut3 => false
, Pacings_1_sel7_pacingOut4 => false
, Pacings_1_sel8_pacingOut5 => false
, Pacings_1_sel9_pacingOut6 => false
, Pacings_1_sel10_pacingOut7 => false
, Pacings_1_sel11_pacingOut8 => false
, Pacings_1_sel12_pacingOut9 => false
, Pacings_1_sel13_pacingOut10 => false ) );
  signal \c$case_alt\                    : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_0\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_1\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_2\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_3\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$case_alt_4\                  : Spec_topEntity_types.Tuple3_0;
  signal \c$app_arg_44\                  : boolean := false;
  signal \c$case_alt_5\                  : boolean;
  signal \c$case_alt_6\                  : boolean;
  signal \c$case_alt_7\                  : boolean;
  signal \c$case_alt_8\                  : boolean;
  signal \c$case_alt_9\                  : boolean;
  signal \c$app_arg_45\                  : boolean := false;
  signal \c$case_alt_10\                 : boolean;
  signal \c$case_alt_11\                 : boolean;
  signal \c$case_alt_12\                 : boolean;
  signal \c$app_arg_46\                  : signed(63 downto 0);
  -- spec.hs:(280,1)-(282,25)
  signal \buffer\                        : Spec_topEntity_types.array_of_Tuple3_0(0 to 1) := Spec_topEntity_types.array_of_Tuple3_0'( ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false
                                       , Slides_1_sel2_slide2 => false
                                       , Slides_1_sel3_slide3 => false
                                       , Slides_1_sel4_slide4 => false
                                       , Slides_1_sel5_slide5 => false
                                       , Slides_1_sel6_slide6 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingIn2 => false
                                       , Pacings_1_sel3_pacingOut0 => false
                                       , Pacings_1_sel4_pacingOut1 => false
                                       , Pacings_1_sel5_pacingOut2 => false
                                       , Pacings_1_sel6_pacingOut3 => false
                                       , Pacings_1_sel7_pacingOut4 => false
                                       , Pacings_1_sel8_pacingOut5 => false
                                       , Pacings_1_sel9_pacingOut6 => false
                                       , Pacings_1_sel10_pacingOut7 => false
                                       , Pacings_1_sel11_pacingOut8 => false
                                       , Pacings_1_sel12_pacingOut9 => false
                                       , Pacings_1_sel13_pacingOut10 => false ) )
                                       , ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false ) )
                                       , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                       , Slides_1_sel1_slide1 => false
                                       , Slides_1_sel2_slide2 => false
                                       , Slides_1_sel3_slide3 => false
                                       , Slides_1_sel4_slide4 => false
                                       , Slides_1_sel5_slide5 => false
                                       , Slides_1_sel6_slide6 => false )
                                       , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingIn1 => false
                                       , Pacings_1_sel2_pacingIn2 => false
                                       , Pacings_1_sel3_pacingOut0 => false
                                       , Pacings_1_sel4_pacingOut1 => false
                                       , Pacings_1_sel5_pacingOut2 => false
                                       , Pacings_1_sel6_pacingOut3 => false
                                       , Pacings_1_sel7_pacingOut4 => false
                                       , Pacings_1_sel8_pacingOut5 => false
                                       , Pacings_1_sel9_pacingOut6 => false
                                       , Pacings_1_sel10_pacingOut7 => false
                                       , Pacings_1_sel11_pacingOut8 => false
                                       , Pacings_1_sel12_pacingOut9 => false
                                       , Pacings_1_sel13_pacingOut10 => false ) ) );
  -- spec.hs:(280,1)-(282,25)
  signal \c$buffer_case_alt\             : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(280,1)-(282,25)
  signal \c$buffer_case_alt_0\           : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(280,1)-(282,25)
  signal \c$buffer_case_alt_1\           : Spec_topEntity_types.array_of_Tuple3_0(0 to 1);
  -- spec.hs:(280,1)-(282,25)
  signal qData                           : Spec_topEntity_types.Tuple3_0;
  -- spec.hs:(280,1)-(282,25)
  signal x_10                            : signed(63 downto 0);
  -- spec.hs:(280,1)-(282,25)
  signal cursor                          : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:(280,1)-(282,25)
  signal \c$cursor_case_alt\             : signed(63 downto 0);
  -- spec.hs:(280,1)-(282,25)
  signal push                            : boolean;
  -- spec.hs:(280,1)-(282,25)
  signal \c$cursor_case_alt_0\           : signed(63 downto 0);
  -- spec.hs:(280,1)-(282,25)
  signal \c$cursor_case_alt_1\           : signed(63 downto 0);
  -- spec.hs:(280,1)-(282,25)
  signal \c$cursor_case_scrut\           : boolean;
  -- spec.hs:(280,1)-(282,25)
  signal \c$cursor_app_arg\              : signed(63 downto 0);
  -- spec.hs:(280,1)-(282,25)
  signal \c$cursor_case_alt_2\           : signed(63 downto 0);
  -- spec.hs:(280,1)-(282,25)
  signal pop                             : boolean;
  -- spec.hs:(280,1)-(282,25)
  signal \c$cursor_case_alt_3\           : signed(63 downto 0);
  -- spec.hs:(280,1)-(282,25)
  signal \c$cursor_case_alt_4\           : signed(63 downto 0);
  signal inputs                          : Spec_topEntity_types.Inputs_4;
  signal \c$ds1_case_alt_selection_res\  : boolean;
  signal \c$vec\                         : Spec_topEntity_types.array_of_Tuple2_2(0 to 6);
  signal t_projection                    : Spec_topEntity_types.Tuple2_11;
  signal \c$vec_0\                       : Spec_topEntity_types.array_of_signed_64(0 to 1);
  signal \t__dc_arg_1_1\                 : Spec_topEntity_types.array_of_signed_64(0 to 0);
  signal x_projection_1                  : signed(63 downto 0);
  signal \c$t_case_alt_selection_1\      : boolean;
  signal \c$vec_1\                       : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_0\        : Spec_topEntity_types.Tuple2_12;
  signal \c$vec_2\                       : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_2\        : Spec_topEntity_types.Tuple2_12;
  signal \c$t_case_alt_selection_4\      : boolean;
  signal \c$vec_3\                       : Spec_topEntity_types.array_of_signed_64(0 to 1);
  signal \t__dc_arg_5_3\                 : Spec_topEntity_types.array_of_signed_64(0 to 0);
  signal \c$vec_4\                       : Spec_topEntity_types.array_of_signed_64(0 to 1);
  signal \t__dc_arg_5_5\                 : Spec_topEntity_types.array_of_signed_64(0 to 0);
  signal x_projection_2                  : signed(63 downto 0);
  signal \c$t_case_alt_selection_7\      : boolean;
  signal \c$vec_5\                       : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_6\        : Spec_topEntity_types.Tuple2_12;
  signal \c$vec_6\                       : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_8\        : Spec_topEntity_types.Tuple2_12;
  signal \c$t_case_alt_selection_10\     : boolean;
  signal x_projection_3                  : signed(63 downto 0);
  signal \c$t_case_alt_selection_13\     : boolean;
  signal \c$vec_7\                       : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_12\       : Spec_topEntity_types.Tuple2_12;
  signal \c$vec_8\                       : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_14\       : Spec_topEntity_types.Tuple2_12;
  signal \c$t_case_alt_selection_16\     : boolean;
  signal \c$vec_9\                       : Spec_topEntity_types.array_of_signed_64(0 to 1);
  signal \t__dc_arg_11_7\                : Spec_topEntity_types.array_of_signed_64(0 to 0);
  signal \c$vec_10\                      : Spec_topEntity_types.array_of_signed_64(0 to 1);
  signal \t__dc_arg_11_9\                : Spec_topEntity_types.array_of_signed_64(0 to 0);
  signal x_projection_4                  : signed(63 downto 0);
  signal \c$t_case_alt_selection_19\     : boolean;
  signal \c$vec_11\                      : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_18\       : Spec_topEntity_types.Tuple2_12;
  signal \c$vec_12\                      : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_20\       : Spec_topEntity_types.Tuple2_12;
  signal \c$t_case_alt_selection_22\     : boolean;
  signal x_projection_5                  : signed(63 downto 0);
  signal \c$t_case_alt_selection_25\     : boolean;
  signal \c$vec_13\                      : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_24\       : Spec_topEntity_types.Tuple2_12;
  signal \c$vec_14\                      : Spec_topEntity_types.array_of_signed_64(0 to 2);
  signal \c$t_case_alt_sel_alt_26\       : Spec_topEntity_types.Tuple2_12;
  signal \c$t_case_alt_selection_28\     : boolean;
  signal \c$vec_15\                      : Spec_topEntity_types.array_of_signed_64(0 to 110);
  signal x_projection_115                : signed(63 downto 0);
  signal \c$t_case_alt_selection_31\     : boolean;
  signal \c$vec_16\                      : Spec_topEntity_types.array_of_signed_64(0 to 111);
  signal \c$t_case_alt_sel_alt_30\       : Spec_topEntity_types.Tuple2_13;
  signal \c$vec_17\                      : Spec_topEntity_types.array_of_signed_64(0 to 111);
  signal \c$t_case_alt_sel_alt_32\       : Spec_topEntity_types.Tuple2_13;
  signal \c$t_case_alt_selection_34\     : boolean;
  signal \c$vec2\                        : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$vec2_0\                      : Spec_topEntity_types.array_of_Maybe_0(0 to 1);
  signal \c$vec2_1\                      : Spec_topEntity_types.array_of_Maybe_0(0 to 1);
  signal \c$vec2_2\                      : Spec_topEntity_types.array_of_Maybe_1(0 to 3);
  signal \c$vec2_3\                      : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$vec2_4\                      : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$vec2_5\                      : Spec_topEntity_types.array_of_Maybe_2(0 to 5);
  signal \c$vec_18\                      : Spec_topEntity_types.array_of_Tuple2_2(0 to 5);
  signal t_projection_7                  : Spec_topEntity_types.Tuple2_14;
  signal \c$vec_19\                      : Spec_topEntity_types.array_of_signed_64(0 to 100);
  signal \c$vec_20\                      : Spec_topEntity_types.array_of_Tuple2_2(0 to 2);
  signal t_projection_8                  : Spec_topEntity_types.Tuple2_15;
  signal x_projection_222                : signed(63 downto 0);
  signal \c$t_case_alt_selection_37\     : boolean;
  signal \c$vec_21\                      : Spec_topEntity_types.array_of_signed_64(0 to 101);
  signal \c$t_case_alt_sel_alt_36\       : Spec_topEntity_types.Tuple2_16;
  signal \c$vec_22\                      : Spec_topEntity_types.array_of_signed_64(0 to 101);
  signal \c$t_case_alt_sel_alt_38\       : Spec_topEntity_types.Tuple2_16;
  signal \c$t_case_alt_selection_40\     : boolean;
  signal \c$vec2_6\                      : Spec_topEntity_types.array_of_Maybe_1(0 to 3);
  signal \c$vec_23\                      : Spec_topEntity_types.array_of_Tuple2_1(0 to 4);
  signal t_projection_10                 : Spec_topEntity_types.Tuple2_17;
  signal \c$vec2_7\                      : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal result_selection_res            : boolean;
  signal \c$vec2_8\                      : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$vec2_9\                      : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal result_selection_res_2          : boolean;
  signal \c$vec2_10\                     : Spec_topEntity_types.array_of_Maybe(0 to 4);
  signal \c$vec_24\                      : Spec_topEntity_types.array_of_Tuple2_1(0 to 5);
  signal t_projection_15                 : Spec_topEntity_types.Tuple2_18;
  signal \c$vec_25\                      : Spec_topEntity_types.array_of_Tuple2_1(0 to 5);
  signal t_projection_16                 : Spec_topEntity_types.Tuple2_18;
  signal \c$vec2_11\                     : Spec_topEntity_types.array_of_Maybe_2(0 to 5);
  signal result_selection_res_3          : boolean;
  signal \c$vec2_12\                     : Spec_topEntity_types.array_of_Maybe_2(0 to 5);
  signal \c$vec_26\                      : Spec_topEntity_types.array_of_Tuple2_2(0 to 6);
  signal t_projection_19                 : Spec_topEntity_types.Tuple2_11;
  signal \c$out0_case_alt_selection_res\ : boolean;
  signal result_selection_48             : boolean;
  signal result_selection_52             : boolean;
  signal result_selection_56             : boolean;
  signal result_selection_60             : boolean;
  signal result_selection_64             : boolean;
  signal result_selection_68             : boolean;
  signal result_selection_72             : boolean;
  signal result_selection_76             : boolean;
  signal result_selection_80             : boolean;
  signal result_selection_84             : boolean;
  signal result_selection_88             : boolean;
  signal result_selection_92             : boolean;
  signal result_selection_96             : boolean;
  signal result_selection_100            : boolean;
  signal \c$vec_27\                      : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_3\   : Spec_topEntity_types.Tuple2_19;
  signal \c$vec_28\                      : Spec_topEntity_types.array_of_Tuple3_0(0 to 2);
  signal \c$buffer_case_alt_sel_alt_5\   : Spec_topEntity_types.Tuple2_19;
  signal result                          : Spec_topEntity_types.Outputs;
  signal result_0                        : Spec_topEntity_types.ValidInt;
  signal result_1                        : Spec_topEntity_types.ValidBool;
  signal result_2                        : Spec_topEntity_types.ValidBool;
  signal result_3                        : Spec_topEntity_types.ValidBool;
  signal result_4                        : Spec_topEntity_types.ValidInt;
  signal result_5                        : Spec_topEntity_types.ValidBool;
  signal result_6                        : Spec_topEntity_types.ValidBool;
  signal result_7                        : Spec_topEntity_types.ValidBool;
  signal result_8                        : Spec_topEntity_types.ValidBool;
  signal result_9                        : Spec_topEntity_types.ValidBool;
  signal result_10                       : Spec_topEntity_types.ValidBool;

begin
  inputs <= ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => inputs_0_0
            , ValidInt_sel1_valid => inputs_0_1 )
            , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => inputs_1_0
            , ValidInt_sel1_valid => inputs_1_1 )
            , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => inputs_2_0
            , ValidInt_sel1_valid => inputs_2_1 ) );

  result_11 <= to_signed(2000,64) when timer1Over else
               (x + to_signed(2000,64));

  x <= result_12;

  -- register begin
  result_12_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_12 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_12 <= result_11;
      end if;
    end if;
  end process;
  -- register end

  result_13 <= to_signed(2000,64) when timer0Over else
               (x_0 + to_signed(2000,64));

  x_0 <= result_14;

  -- register begin
  result_14_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_14 <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        result_14 <= result_13;
      end if;
    end if;
  end process;
  -- register end

  result_15 <= ( Tuple2_sel0_boolean => inputs.Inputs_4_sel0_input0.ValidInt_sel1_valid or (inputs.Inputs_4_sel1_input1.ValidInt_sel1_valid or (inputs.Inputs_4_sel2_input2.ValidInt_sel1_valid or (timer0Over or timer1Over)))
               , Tuple2_sel1_Tuple3_0 => ( Tuple3_0_sel0_Inputs_4 => inputs
               , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => timer0Over
               , Slides_1_sel1_slide1 => timer0Over
               , Slides_1_sel2_slide2 => timer1Over
               , Slides_1_sel3_slide3 => timer1Over
               , Slides_1_sel4_slide4 => timer1Over
               , Slides_1_sel5_slide5 => timer1Over
               , Slides_1_sel6_slide6 => timer1Over )
               , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => inputs.Inputs_4_sel0_input0.ValidInt_sel1_valid
               , Pacings_1_sel1_pacingIn1 => inputs.Inputs_4_sel1_input1.ValidInt_sel1_valid
               , Pacings_1_sel2_pacingIn2 => inputs.Inputs_4_sel2_input2.ValidInt_sel1_valid
               , Pacings_1_sel3_pacingOut0 => timer0Over
               , Pacings_1_sel4_pacingOut1 => timer0Over
               , Pacings_1_sel5_pacingOut2 => timer0Over
               , Pacings_1_sel6_pacingOut3 => timer0Over
               , Pacings_1_sel7_pacingOut4 => timer0Over
               , Pacings_1_sel8_pacingOut5 => timer0Over
               , Pacings_1_sel9_pacingOut6 => timer0Over
               , Pacings_1_sel10_pacingOut7 => timer1Over
               , Pacings_1_sel11_pacingOut8 => timer1Over
               , Pacings_1_sel12_pacingOut9 => timer1Over
               , Pacings_1_sel13_pacingOut10 => timer1Over ) ) );

  timer0Over <= result_14 >= (to_signed(500000,64));

  timer1Over <= result_12 >= (to_signed(10000000,64));

  result_16 <= ( Tuple2_0_sel0_boolean => true
               , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidInt_sel0_value => result_73
               , ValidInt_sel1_valid => result_74 )
               , Outputs_sel1_output1 => ( ValidBool_sel0_value_0 => result_71
               , ValidBool_sel1_valid_0 => result_72 )
               , Outputs_sel2_output2 => ( ValidBool_sel0_value_0 => result_69
               , ValidBool_sel1_valid_0 => result_70 )
               , Outputs_sel3_output3 => ( ValidBool_sel0_value_0 => result_67
               , ValidBool_sel1_valid_0 => result_68 )
               , Outputs_sel4_output4 => ( ValidInt_sel0_value => result_65
               , ValidInt_sel1_valid => result_66 )
               , Outputs_sel5_output5 => ( ValidBool_sel0_value_0 => result_62.Tuple2_1_sel1_boolean
               , ValidBool_sel1_valid_0 => result_60 )
               , Outputs_sel6_output6 => ( ValidBool_sel0_value_0 => result_53.Tuple2_1_sel1_boolean
               , ValidBool_sel1_valid_0 => result_51 )
               , Outputs_sel7_output7 => ( ValidBool_sel0_value_0 => result_43.Tuple2_1_sel1_boolean
               , ValidBool_sel1_valid_0 => result_41 )
               , Outputs_sel8_output8 => ( ValidBool_sel0_value_0 => result_33.Tuple2_1_sel1_boolean
               , ValidBool_sel1_valid_0 => result_31 )
               , Outputs_sel9_output9 => ( ValidBool_sel0_value_0 => result_26.Tuple2_1_sel1_boolean
               , ValidBool_sel1_valid_0 => result_24 )
               , Outputs_sel10_output10 => ( ValidBool_sel0_value_0 => result_19.Tuple2_1_sel1_boolean
               , ValidBool_sel1_valid_0 => result_17 ) ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
    signal iterateI_ho1_4_res  : boolean;
    signal iterateI_ho1_5_res  : boolean;
    signal iterateI_ho1_6_res  : boolean;
    signal iterateI_ho1_7_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut10;

    r_block_230 : block
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

    r_block_231 : block
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

    r_block_232 : block
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

    r_block_233 : block
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

    r_block_234 : block
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

    r_block_235 : block
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

    r_block_236 : block
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

    r_block_237 : block
        signal \c$bb_res_res_6\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_6_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_6\ <= iterateI_ho1_6_res;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res <= \c$bb_res_res_6\;


      end block;

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut10
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res
                                                         , iterateI_ho1_3_res
                                                         , iterateI_ho1_4_res
                                                         , iterateI_ho1_5_res
                                                         , iterateI_ho1_6_res
                                                         , iterateI_ho1_7_res );


  end block;

  result_17 <=  \c$app_arg\(\c$app_arg\'high) ;

  result_18 <= t when b else
               result_19;

  -- register begin
  result_19_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_19 <= (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
    elsif rising_edge(clk) then
      if en then
        result_19 <= result_18;
      end if;
    end if;
  end process;
  -- register end

  b <= result_23;

  t <= ( Tuple2_1_sel0_unsigned => result_112.Tags_sel13_output10_0
       , Tuple2_1_sel1_boolean => \c$ds1_case_alt\ < (to_signed(6,64)) );

  \c$ds1_case_alt_selection_res\ <= tag = to_unsigned(113,8);

  \c$ds1_case_alt\ <= to_signed(0,64) when \c$ds1_case_alt_selection_res\ else
                      dta;

  dta <= \c$ds1_case_scrut\.Tuple2_2_sel1_signed;

  tag <= \c$ds1_case_scrut\.Tuple2_2_sel0_unsigned;

  \c$ds1_case_scrut\ <=  result_21(result_21'high) ;

  result_20 <= t_0 when b_0 else
               result_21;

  -- register begin
  result_21_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_21 <= Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_21 <= result_20;
      end if;
    end if;
  end process;
  -- register end

  b_0 <= result_22;

  \c$vec\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(result_21) & Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => ( Tuple2_2_sel0_unsigned => t_31
           , Tuple2_2_sel1_signed => \c$win_app_arg\ )))));

  t_projection <= (\c$vec\(0 to 1-1),\c$vec\(1 to \c$vec\'high));

  t_0 <= t_projection.Tuple2_11_sel1_array_of_Tuple2_2_1;

  -- delay begin
  cwin_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$win_app_arg\ <= inputs_3.Inputs_4_sel1_input1.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_0 : block
    signal iterateI_ho1_0_res_0 : boolean;
  begin
    r_block_238 : block
        signal \c$bb_res_res_7\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= pIn1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_0 <= \c$bb_res_res_7\;


      end block;

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_boolean'( pIn1
                                                           , iterateI_ho1_0_res_0 );


  end block;

  result_22 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_res_1 : boolean;
    signal iterateI_ho1_1_res_0 : boolean;
    signal iterateI_ho1_2_res_0 : boolean;
    signal iterateI_ho1_3_res_0 : boolean;
    signal iterateI_ho1_4_res_0 : boolean;
    signal iterateI_ho1_5_res_0 : boolean;
    signal iterateI_ho1_6_res_0 : boolean;
  begin
    r_block_239 : block
        signal \c$bb_res_res_8\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_8_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_8\ <= pOut10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_1 <= \c$bb_res_res_8\;


      end block;

    r_block_240 : block
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

        iterateI_ho1_1_res_0 <= \c$bb_res_res_9\;


      end block;

    r_block_241 : block
        signal \c$bb_res_res_10\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_10_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_10\ <= iterateI_ho1_1_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_0 <= \c$bb_res_res_10\;


      end block;

    r_block_242 : block
        signal \c$bb_res_res_11\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_11_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_11\ <= iterateI_ho1_2_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_0 <= \c$bb_res_res_11\;


      end block;

    r_block_243 : block
        signal \c$bb_res_res_12\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_12_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_12\ <= iterateI_ho1_3_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_0 <= \c$bb_res_res_12\;


      end block;

    r_block_244 : block
        signal \c$bb_res_res_13\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= iterateI_ho1_4_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_0 <= \c$bb_res_res_13\;


      end block;

    r_block_245 : block
        signal \c$bb_res_res_14\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_14_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_14\ <= iterateI_ho1_5_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_0 <= \c$bb_res_res_14\;


      end block;

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_boolean'( pOut10
                                                           , iterateI_ho1_0_res_1
                                                           , iterateI_ho1_1_res_0
                                                           , iterateI_ho1_2_res_0
                                                           , iterateI_ho1_3_res_0
                                                           , iterateI_ho1_4_res_0
                                                           , iterateI_ho1_5_res_0
                                                           , iterateI_ho1_6_res_0 );


  end block;

  result_23 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  iterateI_2 : block
    signal iterateI_ho1_0_arg0_0 : boolean;
    signal iterateI_ho1_0_res_2  : boolean;
    signal iterateI_ho1_1_res_1  : boolean;
    signal iterateI_ho1_2_res_1  : boolean;
    signal iterateI_ho1_3_res_1  : boolean;
    signal iterateI_ho1_4_res_1  : boolean;
    signal iterateI_ho1_5_res_1  : boolean;
    signal iterateI_ho1_6_res_1  : boolean;
    signal iterateI_ho1_7_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_0 <= pOut9;

    r_block_246 : block
        signal \c$bb_res_res_15\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_15_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_15\ <= iterateI_ho1_0_arg0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_2 <= \c$bb_res_res_15\;


      end block;

    r_block_247 : block
        signal \c$bb_res_res_16\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_16_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_16\ <= iterateI_ho1_0_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_1 <= \c$bb_res_res_16\;


      end block;

    r_block_248 : block
        signal \c$bb_res_res_17\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_17_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_17\ <= iterateI_ho1_1_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_1 <= \c$bb_res_res_17\;


      end block;

    r_block_249 : block
        signal \c$bb_res_res_18\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_18_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_18\ <= iterateI_ho1_2_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_1 <= \c$bb_res_res_18\;


      end block;

    r_block_250 : block
        signal \c$bb_res_res_19\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_19_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_19\ <= iterateI_ho1_3_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_1 <= \c$bb_res_res_19\;


      end block;

    r_block_251 : block
        signal \c$bb_res_res_20\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_20_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_20\ <= iterateI_ho1_4_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_1 <= \c$bb_res_res_20\;


      end block;

    r_block_252 : block
        signal \c$bb_res_res_21\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_21_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_21\ <= iterateI_ho1_5_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_1 <= \c$bb_res_res_21\;


      end block;

    r_block_253 : block
        signal \c$bb_res_res_22\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_22_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_22\ <= iterateI_ho1_6_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_0 <= \c$bb_res_res_22\;


      end block;

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_boolean'( pOut9
                                                           , iterateI_ho1_0_res_2
                                                           , iterateI_ho1_1_res_1
                                                           , iterateI_ho1_2_res_1
                                                           , iterateI_ho1_3_res_1
                                                           , iterateI_ho1_4_res_1
                                                           , iterateI_ho1_5_res_1
                                                           , iterateI_ho1_6_res_1
                                                           , iterateI_ho1_7_res_0 );


  end block;

  result_24 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  result_25 <= t_1 when b_1 else
               result_26;

  -- register begin
  result_26_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_26 <= (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
    elsif rising_edge(clk) then
      if en then
        result_26 <= result_25;
      end if;
    end if;
  end process;
  -- register end

  b_1 <= result_30;

  \c$vec_0\ <= window.Tuple2_3_sel1_array_of_signed_64;

  \t__dc_arg_1_1\ <= \c$vec_0\(1 to \c$vec_0\'high);

  t_1 <= ( Tuple2_1_sel0_unsigned => result_112.Tags_sel12_output9_0
         , Tuple2_1_sel1_boolean => \t__dc_arg_1_1\(0) < (to_signed(5,64)) );

  x_projection_1 <=  win(0) ;

  x_1 <= x_projection_1;

  win <= window.Tuple2_3_sel1_array_of_signed_64;

  -- register begin
  window_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window <= ( Tuple2_3_sel0_unsigned => to_unsigned(113,8)
  , Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window <= result_27;
      end if;
    end if;
  end process;
  -- register end

  result_27 <= t_2 when b_2 else
               window;

  t_2 <= ( Tuple2_3_sel0_unsigned => result_113.Tags_sel20_slide6_0
         , Tuple2_3_sel1_array_of_signed_64 => result_28 );

  result_28 <= \c$t_case_alt\ when result_29 else
               \c$t_case_alt_0\;

  \c$t_case_alt_selection_1\ <= result_58;

  \c$vec_1\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win)));

  \c$t_case_alt_sel_alt_0\ <= (\c$vec_1\(0 to 2-1),\c$vec_1\(2 to \c$vec_1\'high));

  \c$vec_2\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated)));

  \c$t_case_alt_sel_alt_2\ <= (\c$vec_2\(0 to 2-1),\c$vec_2\(2 to \c$vec_2\'high));

  \c$t_case_alt\ <= \c$t_case_alt_sel_alt_2\.Tuple2_12_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_1\ else
                    \c$t_case_alt_sel_alt_0\.Tuple2_12_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_4\ <= result_58;

  \c$t_case_alt_0\ <= lastBucketUpdated when \c$t_case_alt_selection_4\ else
                      win;

  -- replace begin
  replaceVec : block
    signal vec_index : integer range 0 to 2-1;
  begin
    vec_index <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;

    process(vec_index,win,x_1)
      variable ivec : Spec_topEntity_types.array_of_signed_64(0 to 1);
    begin
      ivec := win;
      ivec(vec_index) := ((x_1 + to_signed(1,64)));
      lastBucketUpdated <= ivec;
    end process;
  end block;
  -- replace end

  b_2 <= result_58 or result_29;

  iterateI_3 : block
    signal iterateI_ho1_0_arg0_1 : boolean;
    signal iterateI_ho1_0_res_3  : boolean;
    signal iterateI_ho1_1_res_2  : boolean;
    signal iterateI_ho1_2_res_2  : boolean;
    signal iterateI_ho1_3_res_2  : boolean;
    signal iterateI_ho1_4_res_2  : boolean;
    signal iterateI_ho1_5_res_2  : boolean;
  begin
    iterateI_ho1_0_arg0_1 <= slides.Slides_1_sel6_slide6;

    r_block_254 : block
        signal \c$bb_res_res_23\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_23_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_23\ <= iterateI_ho1_0_arg0_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_23\;


      end block;

    r_block_255 : block
        signal \c$bb_res_res_24\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_24_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_24\ <= iterateI_ho1_0_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_2 <= \c$bb_res_res_24\;


      end block;

    r_block_256 : block
        signal \c$bb_res_res_25\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_25_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_25\ <= iterateI_ho1_1_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_2 <= \c$bb_res_res_25\;


      end block;

    r_block_257 : block
        signal \c$bb_res_res_26\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_26_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_26\ <= iterateI_ho1_2_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_2 <= \c$bb_res_res_26\;


      end block;

    r_block_258 : block
        signal \c$bb_res_res_27\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_27_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_27\ <= iterateI_ho1_3_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_2 <= \c$bb_res_res_27\;


      end block;

    r_block_259 : block
        signal \c$bb_res_res_28\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_28_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_28\ <= iterateI_ho1_4_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_2 <= \c$bb_res_res_28\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel6_slide6
                                                           , iterateI_ho1_0_res_3
                                                           , iterateI_ho1_1_res_2
                                                           , iterateI_ho1_2_res_2
                                                           , iterateI_ho1_3_res_2
                                                           , iterateI_ho1_4_res_2
                                                           , iterateI_ho1_5_res_2 );


  end block;

  result_29 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  iterateI_4 : block
    signal iterateI_ho1_0_res_4 : boolean;
    signal iterateI_ho1_1_res_3 : boolean;
    signal iterateI_ho1_2_res_3 : boolean;
    signal iterateI_ho1_3_res_3 : boolean;
    signal iterateI_ho1_4_res_3 : boolean;
    signal iterateI_ho1_5_res_3 : boolean;
    signal iterateI_ho1_6_res_2 : boolean;
  begin
    r_block_260 : block
        signal \c$bb_res_res_29\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_29_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_29\ <= pOut9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_29\;


      end block;

    r_block_261 : block
        signal \c$bb_res_res_30\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_30_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_30\ <= iterateI_ho1_0_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_3 <= \c$bb_res_res_30\;


      end block;

    r_block_262 : block
        signal \c$bb_res_res_31\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_31_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_31\ <= iterateI_ho1_1_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_3 <= \c$bb_res_res_31\;


      end block;

    r_block_263 : block
        signal \c$bb_res_res_32\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_32_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_32\ <= iterateI_ho1_2_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_3 <= \c$bb_res_res_32\;


      end block;

    r_block_264 : block
        signal \c$bb_res_res_33\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_33_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_33\ <= iterateI_ho1_3_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_3 <= \c$bb_res_res_33\;


      end block;

    r_block_265 : block
        signal \c$bb_res_res_34\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_34_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_34\ <= iterateI_ho1_4_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_3 <= \c$bb_res_res_34\;


      end block;

    r_block_266 : block
        signal \c$bb_res_res_35\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_35_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_35\ <= iterateI_ho1_5_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_2 <= \c$bb_res_res_35\;


      end block;

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_boolean'( pOut9
                                                           , iterateI_ho1_0_res_4
                                                           , iterateI_ho1_1_res_3
                                                           , iterateI_ho1_2_res_3
                                                           , iterateI_ho1_3_res_3
                                                           , iterateI_ho1_4_res_3
                                                           , iterateI_ho1_5_res_3
                                                           , iterateI_ho1_6_res_2 );


  end block;

  result_30 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  iterateI_5 : block
    signal iterateI_ho1_0_arg0_2 : boolean;
    signal iterateI_ho1_0_res_5  : boolean;
    signal iterateI_ho1_1_res_4  : boolean;
    signal iterateI_ho1_2_res_4  : boolean;
    signal iterateI_ho1_3_res_4  : boolean;
    signal iterateI_ho1_4_res_4  : boolean;
    signal iterateI_ho1_5_res_4  : boolean;
    signal iterateI_ho1_6_res_3  : boolean;
    signal iterateI_ho1_7_res_1  : boolean;
  begin
    iterateI_ho1_0_arg0_2 <= pOut8;

    r_block_267 : block
        signal \c$bb_res_res_36\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_36_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_36\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_36\;


      end block;

    r_block_268 : block
        signal \c$bb_res_res_37\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_37_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_37\ <= iterateI_ho1_0_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_4 <= \c$bb_res_res_37\;


      end block;

    r_block_269 : block
        signal \c$bb_res_res_38\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_38_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_38\ <= iterateI_ho1_1_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_4 <= \c$bb_res_res_38\;


      end block;

    r_block_270 : block
        signal \c$bb_res_res_39\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_39_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_39\ <= iterateI_ho1_2_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_4 <= \c$bb_res_res_39\;


      end block;

    r_block_271 : block
        signal \c$bb_res_res_40\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_40_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_40\ <= iterateI_ho1_3_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_4 <= \c$bb_res_res_40\;


      end block;

    r_block_272 : block
        signal \c$bb_res_res_41\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_41_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_41\ <= iterateI_ho1_4_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_4 <= \c$bb_res_res_41\;


      end block;

    r_block_273 : block
        signal \c$bb_res_res_42\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_42_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_42\ <= iterateI_ho1_5_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_3 <= \c$bb_res_res_42\;


      end block;

    r_block_274 : block
        signal \c$bb_res_res_43\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_43_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_43\ <= iterateI_ho1_6_res_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_1 <= \c$bb_res_res_43\;


      end block;

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_boolean'( pOut8
                                                           , iterateI_ho1_0_res_5
                                                           , iterateI_ho1_1_res_4
                                                           , iterateI_ho1_2_res_4
                                                           , iterateI_ho1_3_res_4
                                                           , iterateI_ho1_4_res_4
                                                           , iterateI_ho1_5_res_4
                                                           , iterateI_ho1_6_res_3
                                                           , iterateI_ho1_7_res_1 );


  end block;

  result_31 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  result_32 <= t_3 when b_3 else
               result_33;

  -- register begin
  result_33_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_33 <= (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
    elsif rising_edge(clk) then
      if en then
        result_33 <= result_32;
      end if;
    end if;
  end process;
  -- register end

  b_3 <= result_40;

  \c$vec_3\ <= window_1.Tuple2_3_sel1_array_of_signed_64;

  \t__dc_arg_5_3\ <= \c$vec_3\(1 to \c$vec_3\'high);

  \c$vec_4\ <= window_0.Tuple2_3_sel1_array_of_signed_64;

  \t__dc_arg_5_5\ <= \c$vec_4\(1 to \c$vec_4\'high);

  t_3 <= ( Tuple2_1_sel0_unsigned => result_112.Tags_sel11_output8_0
         , Tuple2_1_sel1_boolean => (\t__dc_arg_5_3\(0) < (to_signed(10,64))) and (\t__dc_arg_5_5\(0) >= (to_signed(5,64))) );

  x_projection_2 <=  win_0(0) ;

  x_2 <= x_projection_2;

  win_0 <= window_0.Tuple2_3_sel1_array_of_signed_64;

  -- register begin
  window_0_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window_0 <= ( Tuple2_3_sel0_unsigned => to_unsigned(113,8)
  , Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window_0 <= result_34;
      end if;
    end if;
  end process;
  -- register end

  result_34 <= t_4 when b_4 else
               window_0;

  t_4 <= ( Tuple2_3_sel0_unsigned => result_113.Tags_sel19_slide5_0
         , Tuple2_3_sel1_array_of_signed_64 => result_35 );

  result_35 <= \c$t_case_alt_1\ when result_36 else
               \c$t_case_alt_2\;

  \c$t_case_alt_selection_7\ <= result_58;

  \c$vec_5\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win_0)));

  \c$t_case_alt_sel_alt_6\ <= (\c$vec_5\(0 to 2-1),\c$vec_5\(2 to \c$vec_5\'high));

  \c$vec_6\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated_0)));

  \c$t_case_alt_sel_alt_8\ <= (\c$vec_6\(0 to 2-1),\c$vec_6\(2 to \c$vec_6\'high));

  \c$t_case_alt_1\ <= \c$t_case_alt_sel_alt_8\.Tuple2_12_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_7\ else
                      \c$t_case_alt_sel_alt_6\.Tuple2_12_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_10\ <= result_58;

  \c$t_case_alt_2\ <= lastBucketUpdated_0 when \c$t_case_alt_selection_10\ else
                      win_0;

  -- replace begin
  replaceVec_0 : block
    signal vec_index_0 : integer range 0 to 2-1;
  begin
    vec_index_0 <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;

    process(vec_index_0,win_0,x_2)
      variable ivec_0 : Spec_topEntity_types.array_of_signed_64(0 to 1);
    begin
      ivec_0 := win_0;
      ivec_0(vec_index_0) := ((x_2 + to_signed(1,64)));
      lastBucketUpdated_0 <= ivec_0;
    end process;
  end block;
  -- replace end

  b_4 <= result_58 or result_36;

  iterateI_6 : block
    signal iterateI_ho1_0_arg0_3 : boolean;
    signal iterateI_ho1_0_res_6  : boolean;
    signal iterateI_ho1_1_res_5  : boolean;
    signal iterateI_ho1_2_res_5  : boolean;
    signal iterateI_ho1_3_res_5  : boolean;
    signal iterateI_ho1_4_res_5  : boolean;
    signal iterateI_ho1_5_res_5  : boolean;
  begin
    iterateI_ho1_0_arg0_3 <= slides.Slides_1_sel5_slide5;

    r_block_275 : block
        signal \c$bb_res_res_44\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_44_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_44\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_44\;


      end block;

    r_block_276 : block
        signal \c$bb_res_res_45\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_45_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_45\ <= iterateI_ho1_0_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_5 <= \c$bb_res_res_45\;


      end block;

    r_block_277 : block
        signal \c$bb_res_res_46\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_46_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_46\ <= iterateI_ho1_1_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_5 <= \c$bb_res_res_46\;


      end block;

    r_block_278 : block
        signal \c$bb_res_res_47\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_47_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_47\ <= iterateI_ho1_2_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_5 <= \c$bb_res_res_47\;


      end block;

    r_block_279 : block
        signal \c$bb_res_res_48\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_48_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_48\ <= iterateI_ho1_3_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_5 <= \c$bb_res_res_48\;


      end block;

    r_block_280 : block
        signal \c$bb_res_res_49\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_49_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_49\ <= iterateI_ho1_4_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_5 <= \c$bb_res_res_49\;


      end block;

    \c$app_arg_6\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel5_slide5
                                                           , iterateI_ho1_0_res_6
                                                           , iterateI_ho1_1_res_5
                                                           , iterateI_ho1_2_res_5
                                                           , iterateI_ho1_3_res_5
                                                           , iterateI_ho1_4_res_5
                                                           , iterateI_ho1_5_res_5 );


  end block;

  result_36 <=  \c$app_arg_6\(\c$app_arg_6\'high) ;

  x_projection_3 <=  win_1(0) ;

  x_3 <= x_projection_3;

  win_1 <= window_1.Tuple2_3_sel1_array_of_signed_64;

  -- register begin
  window_1_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window_1 <= ( Tuple2_3_sel0_unsigned => to_unsigned(113,8)
  , Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window_1 <= result_37;
      end if;
    end if;
  end process;
  -- register end

  result_37 <= t_5 when b_5 else
               window_1;

  t_5 <= ( Tuple2_3_sel0_unsigned => result_113.Tags_sel18_slide4_0
         , Tuple2_3_sel1_array_of_signed_64 => result_38 );

  result_38 <= \c$t_case_alt_3\ when result_39 else
               \c$t_case_alt_4\;

  \c$t_case_alt_selection_13\ <= result_58;

  \c$vec_7\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win_1)));

  \c$t_case_alt_sel_alt_12\ <= (\c$vec_7\(0 to 2-1),\c$vec_7\(2 to \c$vec_7\'high));

  \c$vec_8\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated_1)));

  \c$t_case_alt_sel_alt_14\ <= (\c$vec_8\(0 to 2-1),\c$vec_8\(2 to \c$vec_8\'high));

  \c$t_case_alt_3\ <= \c$t_case_alt_sel_alt_14\.Tuple2_12_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_13\ else
                      \c$t_case_alt_sel_alt_12\.Tuple2_12_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_16\ <= result_58;

  \c$t_case_alt_4\ <= lastBucketUpdated_1 when \c$t_case_alt_selection_16\ else
                      win_1;

  -- replace begin
  replaceVec_1 : block
    signal vec_index_1 : integer range 0 to 2-1;
  begin
    vec_index_1 <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;

    process(vec_index_1,win_1,x_3)
      variable ivec_1 : Spec_topEntity_types.array_of_signed_64(0 to 1);
    begin
      ivec_1 := win_1;
      ivec_1(vec_index_1) := ((x_3 + to_signed(1,64)));
      lastBucketUpdated_1 <= ivec_1;
    end process;
  end block;
  -- replace end

  b_5 <= result_58 or result_39;

  iterateI_7 : block
    signal iterateI_ho1_0_arg0_4 : boolean;
    signal iterateI_ho1_0_res_7  : boolean;
    signal iterateI_ho1_1_res_6  : boolean;
    signal iterateI_ho1_2_res_6  : boolean;
    signal iterateI_ho1_3_res_6  : boolean;
    signal iterateI_ho1_4_res_6  : boolean;
    signal iterateI_ho1_5_res_6  : boolean;
  begin
    iterateI_ho1_0_arg0_4 <= slides.Slides_1_sel4_slide4;

    r_block_281 : block
        signal \c$bb_res_res_50\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_50_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_50\ <= iterateI_ho1_0_arg0_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_50\;


      end block;

    r_block_282 : block
        signal \c$bb_res_res_51\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_51_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_51\ <= iterateI_ho1_0_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_6 <= \c$bb_res_res_51\;


      end block;

    r_block_283 : block
        signal \c$bb_res_res_52\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_52_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_52\ <= iterateI_ho1_1_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_6 <= \c$bb_res_res_52\;


      end block;

    r_block_284 : block
        signal \c$bb_res_res_53\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_53_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_53\ <= iterateI_ho1_2_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_6 <= \c$bb_res_res_53\;


      end block;

    r_block_285 : block
        signal \c$bb_res_res_54\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_54_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_54\ <= iterateI_ho1_3_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_6 <= \c$bb_res_res_54\;


      end block;

    r_block_286 : block
        signal \c$bb_res_res_55\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_55_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_55\ <= iterateI_ho1_4_res_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_6 <= \c$bb_res_res_55\;


      end block;

    \c$app_arg_7\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel4_slide4
                                                           , iterateI_ho1_0_res_7
                                                           , iterateI_ho1_1_res_6
                                                           , iterateI_ho1_2_res_6
                                                           , iterateI_ho1_3_res_6
                                                           , iterateI_ho1_4_res_6
                                                           , iterateI_ho1_5_res_6 );


  end block;

  result_39 <=  \c$app_arg_7\(\c$app_arg_7\'high) ;

  iterateI_8 : block
    signal iterateI_ho1_0_res_8 : boolean;
    signal iterateI_ho1_1_res_7 : boolean;
    signal iterateI_ho1_2_res_7 : boolean;
    signal iterateI_ho1_3_res_7 : boolean;
    signal iterateI_ho1_4_res_7 : boolean;
    signal iterateI_ho1_5_res_7 : boolean;
    signal iterateI_ho1_6_res_4 : boolean;
  begin
    r_block_287 : block
        signal \c$bb_res_res_56\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_56_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_56\ <= pOut8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_8 <= \c$bb_res_res_56\;


      end block;

    r_block_288 : block
        signal \c$bb_res_res_57\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_57_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_57\ <= iterateI_ho1_0_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_7 <= \c$bb_res_res_57\;


      end block;

    r_block_289 : block
        signal \c$bb_res_res_58\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_58_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_58\ <= iterateI_ho1_1_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_7 <= \c$bb_res_res_58\;


      end block;

    r_block_290 : block
        signal \c$bb_res_res_59\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_59_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_59\ <= iterateI_ho1_2_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_7 <= \c$bb_res_res_59\;


      end block;

    r_block_291 : block
        signal \c$bb_res_res_60\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_60_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_60\ <= iterateI_ho1_3_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_7 <= \c$bb_res_res_60\;


      end block;

    r_block_292 : block
        signal \c$bb_res_res_61\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_61_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_61\ <= iterateI_ho1_4_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_7 <= \c$bb_res_res_61\;


      end block;

    r_block_293 : block
        signal \c$bb_res_res_62\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_62_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_62\ <= iterateI_ho1_5_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_4 <= \c$bb_res_res_62\;


      end block;

    \c$app_arg_8\ <= Spec_topEntity_types.array_of_boolean'( pOut8
                                                           , iterateI_ho1_0_res_8
                                                           , iterateI_ho1_1_res_7
                                                           , iterateI_ho1_2_res_7
                                                           , iterateI_ho1_3_res_7
                                                           , iterateI_ho1_4_res_7
                                                           , iterateI_ho1_5_res_7
                                                           , iterateI_ho1_6_res_4 );


  end block;

  result_40 <=  \c$app_arg_8\(\c$app_arg_8\'high) ;

  iterateI_9 : block
    signal iterateI_ho1_0_arg0_5 : boolean;
    signal iterateI_ho1_0_res_9  : boolean;
    signal iterateI_ho1_1_res_8  : boolean;
    signal iterateI_ho1_2_res_8  : boolean;
    signal iterateI_ho1_3_res_8  : boolean;
    signal iterateI_ho1_4_res_8  : boolean;
    signal iterateI_ho1_5_res_8  : boolean;
    signal iterateI_ho1_6_res_5  : boolean;
    signal iterateI_ho1_7_res_2  : boolean;
  begin
    iterateI_ho1_0_arg0_5 <= pOut7;

    r_block_294 : block
        signal \c$bb_res_res_63\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_63_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_63\ <= iterateI_ho1_0_arg0_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_9 <= \c$bb_res_res_63\;


      end block;

    r_block_295 : block
        signal \c$bb_res_res_64\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_64_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_64\ <= iterateI_ho1_0_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_8 <= \c$bb_res_res_64\;


      end block;

    r_block_296 : block
        signal \c$bb_res_res_65\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_65_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_65\ <= iterateI_ho1_1_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_8 <= \c$bb_res_res_65\;


      end block;

    r_block_297 : block
        signal \c$bb_res_res_66\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_66_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_66\ <= iterateI_ho1_2_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_8 <= \c$bb_res_res_66\;


      end block;

    r_block_298 : block
        signal \c$bb_res_res_67\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_67_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_67\ <= iterateI_ho1_3_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_8 <= \c$bb_res_res_67\;


      end block;

    r_block_299 : block
        signal \c$bb_res_res_68\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_68_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_68\ <= iterateI_ho1_4_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_8 <= \c$bb_res_res_68\;


      end block;

    r_block_300 : block
        signal \c$bb_res_res_69\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_69_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_69\ <= iterateI_ho1_5_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_5 <= \c$bb_res_res_69\;


      end block;

    r_block_301 : block
        signal \c$bb_res_res_70\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_70_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_70\ <= iterateI_ho1_6_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_2 <= \c$bb_res_res_70\;


      end block;

    \c$app_arg_9\ <= Spec_topEntity_types.array_of_boolean'( pOut7
                                                           , iterateI_ho1_0_res_9
                                                           , iterateI_ho1_1_res_8
                                                           , iterateI_ho1_2_res_8
                                                           , iterateI_ho1_3_res_8
                                                           , iterateI_ho1_4_res_8
                                                           , iterateI_ho1_5_res_8
                                                           , iterateI_ho1_6_res_5
                                                           , iterateI_ho1_7_res_2 );


  end block;

  result_41 <=  \c$app_arg_9\(\c$app_arg_9\'high) ;

  result_42 <= t_6 when b_6 else
               result_43;

  -- register begin
  result_43_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_43 <= (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
    elsif rising_edge(clk) then
      if en then
        result_43 <= result_42;
      end if;
    end if;
  end process;
  -- register end

  b_6 <= result_50;

  \c$vec_9\ <= window_3.Tuple2_3_sel1_array_of_signed_64;

  \t__dc_arg_11_7\ <= \c$vec_9\(1 to \c$vec_9\'high);

  \c$vec_10\ <= window_2.Tuple2_3_sel1_array_of_signed_64;

  \t__dc_arg_11_9\ <= \c$vec_10\(1 to \c$vec_10\'high);

  t_6 <= ( Tuple2_1_sel0_unsigned => result_112.Tags_sel10_output7_0
         , Tuple2_1_sel1_boolean => (\t__dc_arg_11_7\(0) < (to_signed(15,64))) and (\t__dc_arg_11_9\(0) >= (to_signed(10,64))) );

  x_projection_4 <=  win_2(0) ;

  x_4 <= x_projection_4;

  win_2 <= window_2.Tuple2_3_sel1_array_of_signed_64;

  -- register begin
  window_2_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window_2 <= ( Tuple2_3_sel0_unsigned => to_unsigned(113,8)
  , Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window_2 <= result_44;
      end if;
    end if;
  end process;
  -- register end

  result_44 <= t_7 when b_7 else
               window_2;

  t_7 <= ( Tuple2_3_sel0_unsigned => result_113.Tags_sel17_slide3_0
         , Tuple2_3_sel1_array_of_signed_64 => result_45 );

  result_45 <= \c$t_case_alt_5\ when result_46 else
               \c$t_case_alt_6\;

  \c$t_case_alt_selection_19\ <= result_58;

  \c$vec_11\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win_2)));

  \c$t_case_alt_sel_alt_18\ <= (\c$vec_11\(0 to 2-1),\c$vec_11\(2 to \c$vec_11\'high));

  \c$vec_12\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated_2)));

  \c$t_case_alt_sel_alt_20\ <= (\c$vec_12\(0 to 2-1),\c$vec_12\(2 to \c$vec_12\'high));

  \c$t_case_alt_5\ <= \c$t_case_alt_sel_alt_20\.Tuple2_12_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_19\ else
                      \c$t_case_alt_sel_alt_18\.Tuple2_12_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_22\ <= result_58;

  \c$t_case_alt_6\ <= lastBucketUpdated_2 when \c$t_case_alt_selection_22\ else
                      win_2;

  -- replace begin
  replaceVec_2 : block
    signal vec_index_2 : integer range 0 to 2-1;
  begin
    vec_index_2 <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;

    process(vec_index_2,win_2,x_4)
      variable ivec_2 : Spec_topEntity_types.array_of_signed_64(0 to 1);
    begin
      ivec_2 := win_2;
      ivec_2(vec_index_2) := ((x_4 + to_signed(1,64)));
      lastBucketUpdated_2 <= ivec_2;
    end process;
  end block;
  -- replace end

  b_7 <= result_58 or result_46;

  iterateI_10 : block
    signal iterateI_ho1_0_arg0_6 : boolean;
    signal iterateI_ho1_0_res_10 : boolean;
    signal iterateI_ho1_1_res_9  : boolean;
    signal iterateI_ho1_2_res_9  : boolean;
    signal iterateI_ho1_3_res_9  : boolean;
    signal iterateI_ho1_4_res_9  : boolean;
    signal iterateI_ho1_5_res_9  : boolean;
  begin
    iterateI_ho1_0_arg0_6 <= slides.Slides_1_sel3_slide3;

    r_block_302 : block
        signal \c$bb_res_res_71\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_71_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_71\ <= iterateI_ho1_0_arg0_6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_10 <= \c$bb_res_res_71\;


      end block;

    r_block_303 : block
        signal \c$bb_res_res_72\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_72_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_72\ <= iterateI_ho1_0_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_9 <= \c$bb_res_res_72\;


      end block;

    r_block_304 : block
        signal \c$bb_res_res_73\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_73_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_73\ <= iterateI_ho1_1_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_9 <= \c$bb_res_res_73\;


      end block;

    r_block_305 : block
        signal \c$bb_res_res_74\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_74_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_74\ <= iterateI_ho1_2_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_9 <= \c$bb_res_res_74\;


      end block;

    r_block_306 : block
        signal \c$bb_res_res_75\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_75_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_75\ <= iterateI_ho1_3_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_9 <= \c$bb_res_res_75\;


      end block;

    r_block_307 : block
        signal \c$bb_res_res_76\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_76_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_76\ <= iterateI_ho1_4_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_9 <= \c$bb_res_res_76\;


      end block;

    \c$app_arg_10\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel3_slide3
                                                            , iterateI_ho1_0_res_10
                                                            , iterateI_ho1_1_res_9
                                                            , iterateI_ho1_2_res_9
                                                            , iterateI_ho1_3_res_9
                                                            , iterateI_ho1_4_res_9
                                                            , iterateI_ho1_5_res_9 );


  end block;

  result_46 <=  \c$app_arg_10\(\c$app_arg_10\'high) ;

  x_projection_5 <=  win_3(0) ;

  x_5 <= x_projection_5;

  win_3 <= window_3.Tuple2_3_sel1_array_of_signed_64;

  -- register begin
  window_3_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window_3 <= ( Tuple2_3_sel0_unsigned => to_unsigned(113,8)
  , Tuple2_3_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
                                                                                , to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        window_3 <= result_47;
      end if;
    end if;
  end process;
  -- register end

  result_47 <= t_8 when b_8 else
               window_3;

  t_8 <= ( Tuple2_3_sel0_unsigned => result_113.Tags_sel16_slide2_0
         , Tuple2_3_sel1_array_of_signed_64 => result_48 );

  result_48 <= \c$t_case_alt_7\ when result_49 else
               \c$t_case_alt_8\;

  \c$t_case_alt_selection_25\ <= result_58;

  \c$vec_13\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win_3)));

  \c$t_case_alt_sel_alt_24\ <= (\c$vec_13\(0 to 2-1),\c$vec_13\(2 to \c$vec_13\'high));

  \c$vec_14\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated_3)));

  \c$t_case_alt_sel_alt_26\ <= (\c$vec_14\(0 to 2-1),\c$vec_14\(2 to \c$vec_14\'high));

  \c$t_case_alt_7\ <= \c$t_case_alt_sel_alt_26\.Tuple2_12_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_25\ else
                      \c$t_case_alt_sel_alt_24\.Tuple2_12_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_28\ <= result_58;

  \c$t_case_alt_8\ <= lastBucketUpdated_3 when \c$t_case_alt_selection_28\ else
                      win_3;

  -- replace begin
  replaceVec_3 : block
    signal vec_index_3 : integer range 0 to 2-1;
  begin
    vec_index_3 <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;

    process(vec_index_3,win_3,x_5)
      variable ivec_3 : Spec_topEntity_types.array_of_signed_64(0 to 1);
    begin
      ivec_3 := win_3;
      ivec_3(vec_index_3) := ((x_5 + to_signed(1,64)));
      lastBucketUpdated_3 <= ivec_3;
    end process;
  end block;
  -- replace end

  b_8 <= result_58 or result_49;

  iterateI_11 : block
    signal iterateI_ho1_0_arg0_7 : boolean;
    signal iterateI_ho1_0_res_11 : boolean;
    signal iterateI_ho1_1_res_10 : boolean;
    signal iterateI_ho1_2_res_10 : boolean;
    signal iterateI_ho1_3_res_10 : boolean;
    signal iterateI_ho1_4_res_10 : boolean;
    signal iterateI_ho1_5_res_10 : boolean;
  begin
    iterateI_ho1_0_arg0_7 <= slides.Slides_1_sel2_slide2;

    r_block_308 : block
        signal \c$bb_res_res_77\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_77_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_77\ <= iterateI_ho1_0_arg0_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_11 <= \c$bb_res_res_77\;


      end block;

    r_block_309 : block
        signal \c$bb_res_res_78\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_78_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_78\ <= iterateI_ho1_0_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_10 <= \c$bb_res_res_78\;


      end block;

    r_block_310 : block
        signal \c$bb_res_res_79\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_79_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_79\ <= iterateI_ho1_1_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_10 <= \c$bb_res_res_79\;


      end block;

    r_block_311 : block
        signal \c$bb_res_res_80\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_80_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_80\ <= iterateI_ho1_2_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_10 <= \c$bb_res_res_80\;


      end block;

    r_block_312 : block
        signal \c$bb_res_res_81\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_81_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_81\ <= iterateI_ho1_3_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_10 <= \c$bb_res_res_81\;


      end block;

    r_block_313 : block
        signal \c$bb_res_res_82\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_82_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_82\ <= iterateI_ho1_4_res_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_10 <= \c$bb_res_res_82\;


      end block;

    \c$app_arg_11\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel2_slide2
                                                            , iterateI_ho1_0_res_11
                                                            , iterateI_ho1_1_res_10
                                                            , iterateI_ho1_2_res_10
                                                            , iterateI_ho1_3_res_10
                                                            , iterateI_ho1_4_res_10
                                                            , iterateI_ho1_5_res_10 );


  end block;

  result_49 <=  \c$app_arg_11\(\c$app_arg_11\'high) ;

  iterateI_12 : block
    signal iterateI_ho1_0_res_12 : boolean;
    signal iterateI_ho1_1_res_11 : boolean;
    signal iterateI_ho1_2_res_11 : boolean;
    signal iterateI_ho1_3_res_11 : boolean;
    signal iterateI_ho1_4_res_11 : boolean;
    signal iterateI_ho1_5_res_11 : boolean;
    signal iterateI_ho1_6_res_6  : boolean;
  begin
    r_block_314 : block
        signal \c$bb_res_res_83\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_83_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_83\ <= pOut7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_12 <= \c$bb_res_res_83\;


      end block;

    r_block_315 : block
        signal \c$bb_res_res_84\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_84_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_84\ <= iterateI_ho1_0_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_11 <= \c$bb_res_res_84\;


      end block;

    r_block_316 : block
        signal \c$bb_res_res_85\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_85_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_85\ <= iterateI_ho1_1_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_11 <= \c$bb_res_res_85\;


      end block;

    r_block_317 : block
        signal \c$bb_res_res_86\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_86_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_86\ <= iterateI_ho1_2_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_11 <= \c$bb_res_res_86\;


      end block;

    r_block_318 : block
        signal \c$bb_res_res_87\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_87_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_87\ <= iterateI_ho1_3_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_11 <= \c$bb_res_res_87\;


      end block;

    r_block_319 : block
        signal \c$bb_res_res_88\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_88_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_88\ <= iterateI_ho1_4_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_11 <= \c$bb_res_res_88\;


      end block;

    r_block_320 : block
        signal \c$bb_res_res_89\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_89_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_89\ <= iterateI_ho1_5_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_6 <= \c$bb_res_res_89\;


      end block;

    \c$app_arg_12\ <= Spec_topEntity_types.array_of_boolean'( pOut7
                                                            , iterateI_ho1_0_res_12
                                                            , iterateI_ho1_1_res_11
                                                            , iterateI_ho1_2_res_11
                                                            , iterateI_ho1_3_res_11
                                                            , iterateI_ho1_4_res_11
                                                            , iterateI_ho1_5_res_11
                                                            , iterateI_ho1_6_res_6 );


  end block;

  result_50 <=  \c$app_arg_12\(\c$app_arg_12\'high) ;

  iterateI_13 : block
    signal iterateI_ho1_0_arg0_8 : boolean;
    signal iterateI_ho1_0_res_13 : boolean;
    signal iterateI_ho1_1_res_12 : boolean;
    signal iterateI_ho1_2_res_12 : boolean;
    signal iterateI_ho1_3_res_12 : boolean;
    signal iterateI_ho1_4_res_12 : boolean;
    signal iterateI_ho1_5_res_12 : boolean;
    signal iterateI_ho1_6_res_7  : boolean;
    signal iterateI_ho1_7_res_3  : boolean;
  begin
    iterateI_ho1_0_arg0_8 <= pOut6;

    r_block_321 : block
        signal \c$bb_res_res_90\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_90_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_90\ <= iterateI_ho1_0_arg0_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_13 <= \c$bb_res_res_90\;


      end block;

    r_block_322 : block
        signal \c$bb_res_res_91\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_91_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_91\ <= iterateI_ho1_0_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_12 <= \c$bb_res_res_91\;


      end block;

    r_block_323 : block
        signal \c$bb_res_res_92\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_92_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_92\ <= iterateI_ho1_1_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_12 <= \c$bb_res_res_92\;


      end block;

    r_block_324 : block
        signal \c$bb_res_res_93\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_93_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_93\ <= iterateI_ho1_2_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_12 <= \c$bb_res_res_93\;


      end block;

    r_block_325 : block
        signal \c$bb_res_res_94\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_94_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_94\ <= iterateI_ho1_3_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_12 <= \c$bb_res_res_94\;


      end block;

    r_block_326 : block
        signal \c$bb_res_res_95\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_95_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_95\ <= iterateI_ho1_4_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_12 <= \c$bb_res_res_95\;


      end block;

    r_block_327 : block
        signal \c$bb_res_res_96\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_96_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_96\ <= iterateI_ho1_5_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_7 <= \c$bb_res_res_96\;


      end block;

    r_block_328 : block
        signal \c$bb_res_res_97\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_97_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_97\ <= iterateI_ho1_6_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_3 <= \c$bb_res_res_97\;


      end block;

    \c$app_arg_13\ <= Spec_topEntity_types.array_of_boolean'( pOut6
                                                            , iterateI_ho1_0_res_13
                                                            , iterateI_ho1_1_res_12
                                                            , iterateI_ho1_2_res_12
                                                            , iterateI_ho1_3_res_12
                                                            , iterateI_ho1_4_res_12
                                                            , iterateI_ho1_5_res_12
                                                            , iterateI_ho1_6_res_7
                                                            , iterateI_ho1_7_res_3 );


  end block;

  result_51 <=  \c$app_arg_13\(\c$app_arg_13\'high) ;

  \c$vec_15\ <= window_4.Tuple2_4_sel1_array_of_signed_64;

  fold : block
    signal vec      : Spec_topEntity_types.array_of_signed_64(0 to 109);
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
    signal acc_101  : signed(63 downto 0);
    signal acc_102  : signed(63 downto 0);
    signal acc_103  : signed(63 downto 0);
    signal acc_104  : signed(63 downto 0);
    signal acc_105  : signed(63 downto 0);
    signal acc_106  : signed(63 downto 0);
    signal acc_107  : signed(63 downto 0);
    signal acc_108  : signed(63 downto 0);
    signal acc_109  : signed(63 downto 0);
    signal acc_110  : signed(63 downto 0);
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
    signal acc_1_50 : signed(63 downto 0);
    signal acc_1_51 : signed(63 downto 0);
    signal acc_1_52 : signed(63 downto 0);
    signal acc_1_53 : signed(63 downto 0);
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
    signal acc_2_24 : signed(63 downto 0);
    signal acc_2_25 : signed(63 downto 0);
    signal acc_2_26 : signed(63 downto 0);
    signal acc_1_54 : signed(63 downto 0);
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
    signal acc_3_12 : signed(63 downto 0);
    signal acc_3_13 : signed(63 downto 0);
    signal acc_4_0  : signed(63 downto 0);
    signal acc_4_1  : signed(63 downto 0);
    signal acc_4_2  : signed(63 downto 0);
    signal acc_4_3  : signed(63 downto 0);
    signal acc_4_4  : signed(63 downto 0);
    signal acc_4_5  : signed(63 downto 0);
    signal acc_5_0  : signed(63 downto 0);
    signal acc_5_1  : signed(63 downto 0);
    signal acc_5_2  : signed(63 downto 0);
    signal acc_4_6  : signed(63 downto 0);
    signal acc_6_0  : signed(63 downto 0);
    signal acc_6_1  : signed(63 downto 0);
  begin
    \c$t_app_arg\ <= acc_7_0;

    vec <= (\c$vec_15\(1 to \c$vec_15\'high));

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

    acc_101 <= vec(100);

    acc_102 <= vec(101);

    acc_103 <= vec(102);

    acc_104 <= vec(103);

    acc_105 <= vec(104);

    acc_106 <= vec(105);

    acc_107 <= vec(106);

    acc_108 <= vec(107);

    acc_109 <= vec(108);

    acc_110 <= vec(109);

    fun_221 : block
        signal x_12            : signed(63 downto 0);
        signal y_1             : signed(63 downto 0);
        signal \c$case_alt_13\ : signed(63 downto 0);
      begin
        acc_1_0 <= \c$case_alt_13\;

        x_12 <= acc_1;

        y_1 <= acc_2;

        \c$case_alt_13\ <= (x_12 + y_1);


      end block;

    fun_222 : block
        signal x_13            : signed(63 downto 0);
        signal y_2             : signed(63 downto 0);
        signal \c$case_alt_14\ : signed(63 downto 0);
      begin
        acc_1_1 <= \c$case_alt_14\;

        x_13 <= acc_3;

        y_2 <= acc_4;

        \c$case_alt_14\ <= (x_13 + y_2);


      end block;

    fun_223 : block
        signal x_14            : signed(63 downto 0);
        signal y_3             : signed(63 downto 0);
        signal \c$case_alt_15\ : signed(63 downto 0);
      begin
        acc_1_2 <= \c$case_alt_15\;

        x_14 <= acc_5;

        y_3 <= acc_6;

        \c$case_alt_15\ <= (x_14 + y_3);


      end block;

    fun_224 : block
        signal x_15            : signed(63 downto 0);
        signal y_4             : signed(63 downto 0);
        signal \c$case_alt_16\ : signed(63 downto 0);
      begin
        acc_1_3 <= \c$case_alt_16\;

        x_15 <= acc_7;

        y_4 <= acc_8;

        \c$case_alt_16\ <= (x_15 + y_4);


      end block;

    fun_225 : block
        signal x_16            : signed(63 downto 0);
        signal y_5             : signed(63 downto 0);
        signal \c$case_alt_17\ : signed(63 downto 0);
      begin
        acc_1_4 <= \c$case_alt_17\;

        x_16 <= acc_9;

        y_5 <= acc_10;

        \c$case_alt_17\ <= (x_16 + y_5);


      end block;

    fun_226 : block
        signal x_17            : signed(63 downto 0);
        signal y_6             : signed(63 downto 0);
        signal \c$case_alt_18\ : signed(63 downto 0);
      begin
        acc_1_5 <= \c$case_alt_18\;

        x_17 <= acc_11;

        y_6 <= acc_12;

        \c$case_alt_18\ <= (x_17 + y_6);


      end block;

    fun_227 : block
        signal x_18            : signed(63 downto 0);
        signal y_7             : signed(63 downto 0);
        signal \c$case_alt_19\ : signed(63 downto 0);
      begin
        acc_1_6 <= \c$case_alt_19\;

        x_18 <= acc_13;

        y_7 <= acc_14;

        \c$case_alt_19\ <= (x_18 + y_7);


      end block;

    fun_228 : block
        signal x_19            : signed(63 downto 0);
        signal y_8             : signed(63 downto 0);
        signal \c$case_alt_20\ : signed(63 downto 0);
      begin
        acc_1_7 <= \c$case_alt_20\;

        x_19 <= acc_15;

        y_8 <= acc_16;

        \c$case_alt_20\ <= (x_19 + y_8);


      end block;

    fun_229 : block
        signal x_20            : signed(63 downto 0);
        signal y_9             : signed(63 downto 0);
        signal \c$case_alt_21\ : signed(63 downto 0);
      begin
        acc_1_8 <= \c$case_alt_21\;

        x_20 <= acc_17;

        y_9 <= acc_18;

        \c$case_alt_21\ <= (x_20 + y_9);


      end block;

    fun_230 : block
        signal x_21            : signed(63 downto 0);
        signal y_10            : signed(63 downto 0);
        signal \c$case_alt_22\ : signed(63 downto 0);
      begin
        acc_1_9 <= \c$case_alt_22\;

        x_21 <= acc_19;

        y_10 <= acc_20;

        \c$case_alt_22\ <= (x_21 + y_10);


      end block;

    fun_231 : block
        signal x_22            : signed(63 downto 0);
        signal y_11            : signed(63 downto 0);
        signal \c$case_alt_23\ : signed(63 downto 0);
      begin
        acc_1_10 <= \c$case_alt_23\;

        x_22 <= acc_21;

        y_11 <= acc_22;

        \c$case_alt_23\ <= (x_22 + y_11);


      end block;

    fun_232 : block
        signal x_23            : signed(63 downto 0);
        signal y_12            : signed(63 downto 0);
        signal \c$case_alt_24\ : signed(63 downto 0);
      begin
        acc_1_11 <= \c$case_alt_24\;

        x_23 <= acc_23;

        y_12 <= acc_24;

        \c$case_alt_24\ <= (x_23 + y_12);


      end block;

    fun_233 : block
        signal x_24            : signed(63 downto 0);
        signal y_13            : signed(63 downto 0);
        signal \c$case_alt_25\ : signed(63 downto 0);
      begin
        acc_1_12 <= \c$case_alt_25\;

        x_24 <= acc_25;

        y_13 <= acc_26;

        \c$case_alt_25\ <= (x_24 + y_13);


      end block;

    fun_234 : block
        signal x_25            : signed(63 downto 0);
        signal y_14            : signed(63 downto 0);
        signal \c$case_alt_26\ : signed(63 downto 0);
      begin
        acc_1_13 <= \c$case_alt_26\;

        x_25 <= acc_27;

        y_14 <= acc_28;

        \c$case_alt_26\ <= (x_25 + y_14);


      end block;

    fun_235 : block
        signal x_26            : signed(63 downto 0);
        signal y_15            : signed(63 downto 0);
        signal \c$case_alt_27\ : signed(63 downto 0);
      begin
        acc_1_14 <= \c$case_alt_27\;

        x_26 <= acc_29;

        y_15 <= acc_30;

        \c$case_alt_27\ <= (x_26 + y_15);


      end block;

    fun_236 : block
        signal x_27            : signed(63 downto 0);
        signal y_16            : signed(63 downto 0);
        signal \c$case_alt_28\ : signed(63 downto 0);
      begin
        acc_1_15 <= \c$case_alt_28\;

        x_27 <= acc_31;

        y_16 <= acc_32;

        \c$case_alt_28\ <= (x_27 + y_16);


      end block;

    fun_237 : block
        signal x_28            : signed(63 downto 0);
        signal y_17            : signed(63 downto 0);
        signal \c$case_alt_29\ : signed(63 downto 0);
      begin
        acc_1_16 <= \c$case_alt_29\;

        x_28 <= acc_33;

        y_17 <= acc_34;

        \c$case_alt_29\ <= (x_28 + y_17);


      end block;

    fun_238 : block
        signal x_29            : signed(63 downto 0);
        signal y_18            : signed(63 downto 0);
        signal \c$case_alt_30\ : signed(63 downto 0);
      begin
        acc_1_17 <= \c$case_alt_30\;

        x_29 <= acc_35;

        y_18 <= acc_36;

        \c$case_alt_30\ <= (x_29 + y_18);


      end block;

    fun_239 : block
        signal x_30            : signed(63 downto 0);
        signal y_19            : signed(63 downto 0);
        signal \c$case_alt_31\ : signed(63 downto 0);
      begin
        acc_1_18 <= \c$case_alt_31\;

        x_30 <= acc_37;

        y_19 <= acc_38;

        \c$case_alt_31\ <= (x_30 + y_19);


      end block;

    fun_240 : block
        signal x_31            : signed(63 downto 0);
        signal y_20            : signed(63 downto 0);
        signal \c$case_alt_32\ : signed(63 downto 0);
      begin
        acc_1_19 <= \c$case_alt_32\;

        x_31 <= acc_39;

        y_20 <= acc_40;

        \c$case_alt_32\ <= (x_31 + y_20);


      end block;

    fun_241 : block
        signal x_32            : signed(63 downto 0);
        signal y_21            : signed(63 downto 0);
        signal \c$case_alt_33\ : signed(63 downto 0);
      begin
        acc_1_20 <= \c$case_alt_33\;

        x_32 <= acc_41;

        y_21 <= acc_42;

        \c$case_alt_33\ <= (x_32 + y_21);


      end block;

    fun_242 : block
        signal x_33            : signed(63 downto 0);
        signal y_22            : signed(63 downto 0);
        signal \c$case_alt_34\ : signed(63 downto 0);
      begin
        acc_1_21 <= \c$case_alt_34\;

        x_33 <= acc_43;

        y_22 <= acc_44;

        \c$case_alt_34\ <= (x_33 + y_22);


      end block;

    fun_243 : block
        signal x_34            : signed(63 downto 0);
        signal y_23            : signed(63 downto 0);
        signal \c$case_alt_35\ : signed(63 downto 0);
      begin
        acc_1_22 <= \c$case_alt_35\;

        x_34 <= acc_45;

        y_23 <= acc_46;

        \c$case_alt_35\ <= (x_34 + y_23);


      end block;

    fun_244 : block
        signal x_35            : signed(63 downto 0);
        signal y_24            : signed(63 downto 0);
        signal \c$case_alt_36\ : signed(63 downto 0);
      begin
        acc_1_23 <= \c$case_alt_36\;

        x_35 <= acc_47;

        y_24 <= acc_48;

        \c$case_alt_36\ <= (x_35 + y_24);


      end block;

    fun_245 : block
        signal x_36            : signed(63 downto 0);
        signal y_25            : signed(63 downto 0);
        signal \c$case_alt_37\ : signed(63 downto 0);
      begin
        acc_1_24 <= \c$case_alt_37\;

        x_36 <= acc_49;

        y_25 <= acc_50;

        \c$case_alt_37\ <= (x_36 + y_25);


      end block;

    fun_246 : block
        signal x_37            : signed(63 downto 0);
        signal y_26            : signed(63 downto 0);
        signal \c$case_alt_38\ : signed(63 downto 0);
      begin
        acc_1_25 <= \c$case_alt_38\;

        x_37 <= acc_51;

        y_26 <= acc_52;

        \c$case_alt_38\ <= (x_37 + y_26);


      end block;

    fun_247 : block
        signal x_38            : signed(63 downto 0);
        signal y_27            : signed(63 downto 0);
        signal \c$case_alt_39\ : signed(63 downto 0);
      begin
        acc_1_26 <= \c$case_alt_39\;

        x_38 <= acc_53;

        y_27 <= acc_54;

        \c$case_alt_39\ <= (x_38 + y_27);


      end block;

    fun_248 : block
        signal x_39            : signed(63 downto 0);
        signal y_28            : signed(63 downto 0);
        signal \c$case_alt_40\ : signed(63 downto 0);
      begin
        acc_1_27 <= \c$case_alt_40\;

        x_39 <= acc_55;

        y_28 <= acc_56;

        \c$case_alt_40\ <= (x_39 + y_28);


      end block;

    fun_249 : block
        signal x_40            : signed(63 downto 0);
        signal y_29            : signed(63 downto 0);
        signal \c$case_alt_41\ : signed(63 downto 0);
      begin
        acc_1_28 <= \c$case_alt_41\;

        x_40 <= acc_57;

        y_29 <= acc_58;

        \c$case_alt_41\ <= (x_40 + y_29);


      end block;

    fun_250 : block
        signal x_41            : signed(63 downto 0);
        signal y_30            : signed(63 downto 0);
        signal \c$case_alt_42\ : signed(63 downto 0);
      begin
        acc_1_29 <= \c$case_alt_42\;

        x_41 <= acc_59;

        y_30 <= acc_60;

        \c$case_alt_42\ <= (x_41 + y_30);


      end block;

    fun_251 : block
        signal x_42            : signed(63 downto 0);
        signal y_31            : signed(63 downto 0);
        signal \c$case_alt_43\ : signed(63 downto 0);
      begin
        acc_1_30 <= \c$case_alt_43\;

        x_42 <= acc_61;

        y_31 <= acc_62;

        \c$case_alt_43\ <= (x_42 + y_31);


      end block;

    fun_252 : block
        signal x_43            : signed(63 downto 0);
        signal y_32            : signed(63 downto 0);
        signal \c$case_alt_44\ : signed(63 downto 0);
      begin
        acc_1_31 <= \c$case_alt_44\;

        x_43 <= acc_63;

        y_32 <= acc_64;

        \c$case_alt_44\ <= (x_43 + y_32);


      end block;

    fun_253 : block
        signal x_44            : signed(63 downto 0);
        signal y_33            : signed(63 downto 0);
        signal \c$case_alt_45\ : signed(63 downto 0);
      begin
        acc_1_32 <= \c$case_alt_45\;

        x_44 <= acc_65;

        y_33 <= acc_66;

        \c$case_alt_45\ <= (x_44 + y_33);


      end block;

    fun_254 : block
        signal x_45            : signed(63 downto 0);
        signal y_34            : signed(63 downto 0);
        signal \c$case_alt_46\ : signed(63 downto 0);
      begin
        acc_1_33 <= \c$case_alt_46\;

        x_45 <= acc_67;

        y_34 <= acc_68;

        \c$case_alt_46\ <= (x_45 + y_34);


      end block;

    fun_255 : block
        signal x_46            : signed(63 downto 0);
        signal y_35            : signed(63 downto 0);
        signal \c$case_alt_47\ : signed(63 downto 0);
      begin
        acc_1_34 <= \c$case_alt_47\;

        x_46 <= acc_69;

        y_35 <= acc_70;

        \c$case_alt_47\ <= (x_46 + y_35);


      end block;

    fun_256 : block
        signal x_47            : signed(63 downto 0);
        signal y_36            : signed(63 downto 0);
        signal \c$case_alt_48\ : signed(63 downto 0);
      begin
        acc_1_35 <= \c$case_alt_48\;

        x_47 <= acc_71;

        y_36 <= acc_72;

        \c$case_alt_48\ <= (x_47 + y_36);


      end block;

    fun_257 : block
        signal x_48            : signed(63 downto 0);
        signal y_37            : signed(63 downto 0);
        signal \c$case_alt_49\ : signed(63 downto 0);
      begin
        acc_1_36 <= \c$case_alt_49\;

        x_48 <= acc_73;

        y_37 <= acc_74;

        \c$case_alt_49\ <= (x_48 + y_37);


      end block;

    fun_258 : block
        signal x_49            : signed(63 downto 0);
        signal y_38            : signed(63 downto 0);
        signal \c$case_alt_50\ : signed(63 downto 0);
      begin
        acc_1_37 <= \c$case_alt_50\;

        x_49 <= acc_75;

        y_38 <= acc_76;

        \c$case_alt_50\ <= (x_49 + y_38);


      end block;

    fun_259 : block
        signal x_50            : signed(63 downto 0);
        signal y_39            : signed(63 downto 0);
        signal \c$case_alt_51\ : signed(63 downto 0);
      begin
        acc_1_38 <= \c$case_alt_51\;

        x_50 <= acc_77;

        y_39 <= acc_78;

        \c$case_alt_51\ <= (x_50 + y_39);


      end block;

    fun_260 : block
        signal x_51            : signed(63 downto 0);
        signal y_40            : signed(63 downto 0);
        signal \c$case_alt_52\ : signed(63 downto 0);
      begin
        acc_1_39 <= \c$case_alt_52\;

        x_51 <= acc_79;

        y_40 <= acc_80;

        \c$case_alt_52\ <= (x_51 + y_40);


      end block;

    fun_261 : block
        signal x_52            : signed(63 downto 0);
        signal y_41            : signed(63 downto 0);
        signal \c$case_alt_53\ : signed(63 downto 0);
      begin
        acc_1_40 <= \c$case_alt_53\;

        x_52 <= acc_81;

        y_41 <= acc_82;

        \c$case_alt_53\ <= (x_52 + y_41);


      end block;

    fun_262 : block
        signal x_53            : signed(63 downto 0);
        signal y_42            : signed(63 downto 0);
        signal \c$case_alt_54\ : signed(63 downto 0);
      begin
        acc_1_41 <= \c$case_alt_54\;

        x_53 <= acc_83;

        y_42 <= acc_84;

        \c$case_alt_54\ <= (x_53 + y_42);


      end block;

    fun_263 : block
        signal x_54            : signed(63 downto 0);
        signal y_43            : signed(63 downto 0);
        signal \c$case_alt_55\ : signed(63 downto 0);
      begin
        acc_1_42 <= \c$case_alt_55\;

        x_54 <= acc_85;

        y_43 <= acc_86;

        \c$case_alt_55\ <= (x_54 + y_43);


      end block;

    fun_264 : block
        signal x_55            : signed(63 downto 0);
        signal y_44            : signed(63 downto 0);
        signal \c$case_alt_56\ : signed(63 downto 0);
      begin
        acc_1_43 <= \c$case_alt_56\;

        x_55 <= acc_87;

        y_44 <= acc_88;

        \c$case_alt_56\ <= (x_55 + y_44);


      end block;

    fun_265 : block
        signal x_56            : signed(63 downto 0);
        signal y_45            : signed(63 downto 0);
        signal \c$case_alt_57\ : signed(63 downto 0);
      begin
        acc_1_44 <= \c$case_alt_57\;

        x_56 <= acc_89;

        y_45 <= acc_90;

        \c$case_alt_57\ <= (x_56 + y_45);


      end block;

    fun_266 : block
        signal x_57            : signed(63 downto 0);
        signal y_46            : signed(63 downto 0);
        signal \c$case_alt_58\ : signed(63 downto 0);
      begin
        acc_1_45 <= \c$case_alt_58\;

        x_57 <= acc_91;

        y_46 <= acc_92;

        \c$case_alt_58\ <= (x_57 + y_46);


      end block;

    fun_267 : block
        signal x_58            : signed(63 downto 0);
        signal y_47            : signed(63 downto 0);
        signal \c$case_alt_59\ : signed(63 downto 0);
      begin
        acc_1_46 <= \c$case_alt_59\;

        x_58 <= acc_93;

        y_47 <= acc_94;

        \c$case_alt_59\ <= (x_58 + y_47);


      end block;

    fun_268 : block
        signal x_59            : signed(63 downto 0);
        signal y_48            : signed(63 downto 0);
        signal \c$case_alt_60\ : signed(63 downto 0);
      begin
        acc_1_47 <= \c$case_alt_60\;

        x_59 <= acc_95;

        y_48 <= acc_96;

        \c$case_alt_60\ <= (x_59 + y_48);


      end block;

    fun_269 : block
        signal x_60            : signed(63 downto 0);
        signal y_49            : signed(63 downto 0);
        signal \c$case_alt_61\ : signed(63 downto 0);
      begin
        acc_1_48 <= \c$case_alt_61\;

        x_60 <= acc_97;

        y_49 <= acc_98;

        \c$case_alt_61\ <= (x_60 + y_49);


      end block;

    fun_270 : block
        signal x_61            : signed(63 downto 0);
        signal y_50            : signed(63 downto 0);
        signal \c$case_alt_62\ : signed(63 downto 0);
      begin
        acc_1_49 <= \c$case_alt_62\;

        x_61 <= acc_99;

        y_50 <= acc_100;

        \c$case_alt_62\ <= (x_61 + y_50);


      end block;

    fun_271 : block
        signal x_62            : signed(63 downto 0);
        signal y_51            : signed(63 downto 0);
        signal \c$case_alt_63\ : signed(63 downto 0);
      begin
        acc_1_50 <= \c$case_alt_63\;

        x_62 <= acc_101;

        y_51 <= acc_102;

        \c$case_alt_63\ <= (x_62 + y_51);


      end block;

    fun_272 : block
        signal x_63            : signed(63 downto 0);
        signal y_52            : signed(63 downto 0);
        signal \c$case_alt_64\ : signed(63 downto 0);
      begin
        acc_1_51 <= \c$case_alt_64\;

        x_63 <= acc_103;

        y_52 <= acc_104;

        \c$case_alt_64\ <= (x_63 + y_52);


      end block;

    fun_273 : block
        signal x_64            : signed(63 downto 0);
        signal y_53            : signed(63 downto 0);
        signal \c$case_alt_65\ : signed(63 downto 0);
      begin
        acc_1_52 <= \c$case_alt_65\;

        x_64 <= acc_105;

        y_53 <= acc_106;

        \c$case_alt_65\ <= (x_64 + y_53);


      end block;

    fun_274 : block
        signal x_65            : signed(63 downto 0);
        signal y_54            : signed(63 downto 0);
        signal \c$case_alt_66\ : signed(63 downto 0);
      begin
        acc_1_53 <= \c$case_alt_66\;

        x_65 <= acc_107;

        y_54 <= acc_108;

        \c$case_alt_66\ <= (x_65 + y_54);


      end block;

    fun_275 : block
        signal x_66            : signed(63 downto 0);
        signal y_55            : signed(63 downto 0);
        signal \c$case_alt_67\ : signed(63 downto 0);
      begin
        acc_1_54 <= \c$case_alt_67\;

        x_66 <= acc_109;

        y_55 <= acc_110;

        \c$case_alt_67\ <= (x_66 + y_55);


      end block;

    fun_276 : block
        signal x_67            : signed(63 downto 0);
        signal y_56            : signed(63 downto 0);
        signal \c$case_alt_68\ : signed(63 downto 0);
      begin
        acc_2_0 <= \c$case_alt_68\;

        x_67 <= acc_1_0;

        y_56 <= acc_1_1;

        \c$case_alt_68\ <= (x_67 + y_56);


      end block;

    fun_277 : block
        signal x_68            : signed(63 downto 0);
        signal y_57            : signed(63 downto 0);
        signal \c$case_alt_69\ : signed(63 downto 0);
      begin
        acc_2_1 <= \c$case_alt_69\;

        x_68 <= acc_1_2;

        y_57 <= acc_1_3;

        \c$case_alt_69\ <= (x_68 + y_57);


      end block;

    fun_278 : block
        signal x_69            : signed(63 downto 0);
        signal y_58            : signed(63 downto 0);
        signal \c$case_alt_70\ : signed(63 downto 0);
      begin
        acc_2_2 <= \c$case_alt_70\;

        x_69 <= acc_1_4;

        y_58 <= acc_1_5;

        \c$case_alt_70\ <= (x_69 + y_58);


      end block;

    fun_279 : block
        signal x_70            : signed(63 downto 0);
        signal y_59            : signed(63 downto 0);
        signal \c$case_alt_71\ : signed(63 downto 0);
      begin
        acc_2_3 <= \c$case_alt_71\;

        x_70 <= acc_1_6;

        y_59 <= acc_1_7;

        \c$case_alt_71\ <= (x_70 + y_59);


      end block;

    fun_280 : block
        signal x_71            : signed(63 downto 0);
        signal y_60            : signed(63 downto 0);
        signal \c$case_alt_72\ : signed(63 downto 0);
      begin
        acc_2_4 <= \c$case_alt_72\;

        x_71 <= acc_1_8;

        y_60 <= acc_1_9;

        \c$case_alt_72\ <= (x_71 + y_60);


      end block;

    fun_281 : block
        signal x_72            : signed(63 downto 0);
        signal y_61            : signed(63 downto 0);
        signal \c$case_alt_73\ : signed(63 downto 0);
      begin
        acc_2_5 <= \c$case_alt_73\;

        x_72 <= acc_1_10;

        y_61 <= acc_1_11;

        \c$case_alt_73\ <= (x_72 + y_61);


      end block;

    fun_282 : block
        signal x_73            : signed(63 downto 0);
        signal y_62            : signed(63 downto 0);
        signal \c$case_alt_74\ : signed(63 downto 0);
      begin
        acc_2_6 <= \c$case_alt_74\;

        x_73 <= acc_1_12;

        y_62 <= acc_1_13;

        \c$case_alt_74\ <= (x_73 + y_62);


      end block;

    fun_283 : block
        signal x_74            : signed(63 downto 0);
        signal y_63            : signed(63 downto 0);
        signal \c$case_alt_75\ : signed(63 downto 0);
      begin
        acc_2_7 <= \c$case_alt_75\;

        x_74 <= acc_1_14;

        y_63 <= acc_1_15;

        \c$case_alt_75\ <= (x_74 + y_63);


      end block;

    fun_284 : block
        signal x_75            : signed(63 downto 0);
        signal y_64            : signed(63 downto 0);
        signal \c$case_alt_76\ : signed(63 downto 0);
      begin
        acc_2_8 <= \c$case_alt_76\;

        x_75 <= acc_1_16;

        y_64 <= acc_1_17;

        \c$case_alt_76\ <= (x_75 + y_64);


      end block;

    fun_285 : block
        signal x_76            : signed(63 downto 0);
        signal y_65            : signed(63 downto 0);
        signal \c$case_alt_77\ : signed(63 downto 0);
      begin
        acc_2_9 <= \c$case_alt_77\;

        x_76 <= acc_1_18;

        y_65 <= acc_1_19;

        \c$case_alt_77\ <= (x_76 + y_65);


      end block;

    fun_286 : block
        signal x_77            : signed(63 downto 0);
        signal y_66            : signed(63 downto 0);
        signal \c$case_alt_78\ : signed(63 downto 0);
      begin
        acc_2_10 <= \c$case_alt_78\;

        x_77 <= acc_1_20;

        y_66 <= acc_1_21;

        \c$case_alt_78\ <= (x_77 + y_66);


      end block;

    fun_287 : block
        signal x_78            : signed(63 downto 0);
        signal y_67            : signed(63 downto 0);
        signal \c$case_alt_79\ : signed(63 downto 0);
      begin
        acc_2_11 <= \c$case_alt_79\;

        x_78 <= acc_1_22;

        y_67 <= acc_1_23;

        \c$case_alt_79\ <= (x_78 + y_67);


      end block;

    fun_288 : block
        signal x_79            : signed(63 downto 0);
        signal y_68            : signed(63 downto 0);
        signal \c$case_alt_80\ : signed(63 downto 0);
      begin
        acc_2_12 <= \c$case_alt_80\;

        x_79 <= acc_1_24;

        y_68 <= acc_1_25;

        \c$case_alt_80\ <= (x_79 + y_68);


      end block;

    fun_289 : block
        signal x_80            : signed(63 downto 0);
        signal y_69            : signed(63 downto 0);
        signal \c$case_alt_81\ : signed(63 downto 0);
      begin
        acc_2_13 <= \c$case_alt_81\;

        x_80 <= acc_1_26;

        y_69 <= acc_1_27;

        \c$case_alt_81\ <= (x_80 + y_69);


      end block;

    fun_290 : block
        signal x_81            : signed(63 downto 0);
        signal y_70            : signed(63 downto 0);
        signal \c$case_alt_82\ : signed(63 downto 0);
      begin
        acc_2_14 <= \c$case_alt_82\;

        x_81 <= acc_1_28;

        y_70 <= acc_1_29;

        \c$case_alt_82\ <= (x_81 + y_70);


      end block;

    fun_291 : block
        signal x_82            : signed(63 downto 0);
        signal y_71            : signed(63 downto 0);
        signal \c$case_alt_83\ : signed(63 downto 0);
      begin
        acc_2_15 <= \c$case_alt_83\;

        x_82 <= acc_1_30;

        y_71 <= acc_1_31;

        \c$case_alt_83\ <= (x_82 + y_71);


      end block;

    fun_292 : block
        signal x_83            : signed(63 downto 0);
        signal y_72            : signed(63 downto 0);
        signal \c$case_alt_84\ : signed(63 downto 0);
      begin
        acc_2_16 <= \c$case_alt_84\;

        x_83 <= acc_1_32;

        y_72 <= acc_1_33;

        \c$case_alt_84\ <= (x_83 + y_72);


      end block;

    fun_293 : block
        signal x_84            : signed(63 downto 0);
        signal y_73            : signed(63 downto 0);
        signal \c$case_alt_85\ : signed(63 downto 0);
      begin
        acc_2_17 <= \c$case_alt_85\;

        x_84 <= acc_1_34;

        y_73 <= acc_1_35;

        \c$case_alt_85\ <= (x_84 + y_73);


      end block;

    fun_294 : block
        signal x_85            : signed(63 downto 0);
        signal y_74            : signed(63 downto 0);
        signal \c$case_alt_86\ : signed(63 downto 0);
      begin
        acc_2_18 <= \c$case_alt_86\;

        x_85 <= acc_1_36;

        y_74 <= acc_1_37;

        \c$case_alt_86\ <= (x_85 + y_74);


      end block;

    fun_295 : block
        signal x_86            : signed(63 downto 0);
        signal y_75            : signed(63 downto 0);
        signal \c$case_alt_87\ : signed(63 downto 0);
      begin
        acc_2_19 <= \c$case_alt_87\;

        x_86 <= acc_1_38;

        y_75 <= acc_1_39;

        \c$case_alt_87\ <= (x_86 + y_75);


      end block;

    fun_296 : block
        signal x_87            : signed(63 downto 0);
        signal y_76            : signed(63 downto 0);
        signal \c$case_alt_88\ : signed(63 downto 0);
      begin
        acc_2_20 <= \c$case_alt_88\;

        x_87 <= acc_1_40;

        y_76 <= acc_1_41;

        \c$case_alt_88\ <= (x_87 + y_76);


      end block;

    fun_297 : block
        signal x_88            : signed(63 downto 0);
        signal y_77            : signed(63 downto 0);
        signal \c$case_alt_89\ : signed(63 downto 0);
      begin
        acc_2_21 <= \c$case_alt_89\;

        x_88 <= acc_1_42;

        y_77 <= acc_1_43;

        \c$case_alt_89\ <= (x_88 + y_77);


      end block;

    fun_298 : block
        signal x_89            : signed(63 downto 0);
        signal y_78            : signed(63 downto 0);
        signal \c$case_alt_90\ : signed(63 downto 0);
      begin
        acc_2_22 <= \c$case_alt_90\;

        x_89 <= acc_1_44;

        y_78 <= acc_1_45;

        \c$case_alt_90\ <= (x_89 + y_78);


      end block;

    fun_299 : block
        signal x_90            : signed(63 downto 0);
        signal y_79            : signed(63 downto 0);
        signal \c$case_alt_91\ : signed(63 downto 0);
      begin
        acc_2_23 <= \c$case_alt_91\;

        x_90 <= acc_1_46;

        y_79 <= acc_1_47;

        \c$case_alt_91\ <= (x_90 + y_79);


      end block;

    fun_300 : block
        signal x_91            : signed(63 downto 0);
        signal y_80            : signed(63 downto 0);
        signal \c$case_alt_92\ : signed(63 downto 0);
      begin
        acc_2_24 <= \c$case_alt_92\;

        x_91 <= acc_1_48;

        y_80 <= acc_1_49;

        \c$case_alt_92\ <= (x_91 + y_80);


      end block;

    fun_301 : block
        signal x_92            : signed(63 downto 0);
        signal y_81            : signed(63 downto 0);
        signal \c$case_alt_93\ : signed(63 downto 0);
      begin
        acc_2_25 <= \c$case_alt_93\;

        x_92 <= acc_1_50;

        y_81 <= acc_1_51;

        \c$case_alt_93\ <= (x_92 + y_81);


      end block;

    fun_302 : block
        signal x_93            : signed(63 downto 0);
        signal y_82            : signed(63 downto 0);
        signal \c$case_alt_94\ : signed(63 downto 0);
      begin
        acc_2_26 <= \c$case_alt_94\;

        x_93 <= acc_1_52;

        y_82 <= acc_1_53;

        \c$case_alt_94\ <= (x_93 + y_82);


      end block;

    fun_303 : block
        signal x_94            : signed(63 downto 0);
        signal y_83            : signed(63 downto 0);
        signal \c$case_alt_95\ : signed(63 downto 0);
      begin
        acc_3_0 <= \c$case_alt_95\;

        x_94 <= acc_2_0;

        y_83 <= acc_2_1;

        \c$case_alt_95\ <= (x_94 + y_83);


      end block;

    fun_304 : block
        signal x_95            : signed(63 downto 0);
        signal y_84            : signed(63 downto 0);
        signal \c$case_alt_96\ : signed(63 downto 0);
      begin
        acc_3_1 <= \c$case_alt_96\;

        x_95 <= acc_2_2;

        y_84 <= acc_2_3;

        \c$case_alt_96\ <= (x_95 + y_84);


      end block;

    fun_305 : block
        signal x_96            : signed(63 downto 0);
        signal y_85            : signed(63 downto 0);
        signal \c$case_alt_97\ : signed(63 downto 0);
      begin
        acc_3_2 <= \c$case_alt_97\;

        x_96 <= acc_2_4;

        y_85 <= acc_2_5;

        \c$case_alt_97\ <= (x_96 + y_85);


      end block;

    fun_306 : block
        signal x_97            : signed(63 downto 0);
        signal y_86            : signed(63 downto 0);
        signal \c$case_alt_98\ : signed(63 downto 0);
      begin
        acc_3_3 <= \c$case_alt_98\;

        x_97 <= acc_2_6;

        y_86 <= acc_2_7;

        \c$case_alt_98\ <= (x_97 + y_86);


      end block;

    fun_307 : block
        signal x_98            : signed(63 downto 0);
        signal y_87            : signed(63 downto 0);
        signal \c$case_alt_99\ : signed(63 downto 0);
      begin
        acc_3_4 <= \c$case_alt_99\;

        x_98 <= acc_2_8;

        y_87 <= acc_2_9;

        \c$case_alt_99\ <= (x_98 + y_87);


      end block;

    fun_308 : block
        signal x_99             : signed(63 downto 0);
        signal y_88             : signed(63 downto 0);
        signal \c$case_alt_100\ : signed(63 downto 0);
      begin
        acc_3_5 <= \c$case_alt_100\;

        x_99 <= acc_2_10;

        y_88 <= acc_2_11;

        \c$case_alt_100\ <= (x_99 + y_88);


      end block;

    fun_309 : block
        signal x_100            : signed(63 downto 0);
        signal y_89             : signed(63 downto 0);
        signal \c$case_alt_101\ : signed(63 downto 0);
      begin
        acc_3_6 <= \c$case_alt_101\;

        x_100 <= acc_2_12;

        y_89 <= acc_2_13;

        \c$case_alt_101\ <= (x_100 + y_89);


      end block;

    fun_310 : block
        signal x_101            : signed(63 downto 0);
        signal y_90             : signed(63 downto 0);
        signal \c$case_alt_102\ : signed(63 downto 0);
      begin
        acc_3_7 <= \c$case_alt_102\;

        x_101 <= acc_2_14;

        y_90 <= acc_2_15;

        \c$case_alt_102\ <= (x_101 + y_90);


      end block;

    fun_311 : block
        signal x_102            : signed(63 downto 0);
        signal y_91             : signed(63 downto 0);
        signal \c$case_alt_103\ : signed(63 downto 0);
      begin
        acc_3_8 <= \c$case_alt_103\;

        x_102 <= acc_2_16;

        y_91 <= acc_2_17;

        \c$case_alt_103\ <= (x_102 + y_91);


      end block;

    fun_312 : block
        signal x_103            : signed(63 downto 0);
        signal y_92             : signed(63 downto 0);
        signal \c$case_alt_104\ : signed(63 downto 0);
      begin
        acc_3_9 <= \c$case_alt_104\;

        x_103 <= acc_2_18;

        y_92 <= acc_2_19;

        \c$case_alt_104\ <= (x_103 + y_92);


      end block;

    fun_313 : block
        signal x_104            : signed(63 downto 0);
        signal y_93             : signed(63 downto 0);
        signal \c$case_alt_105\ : signed(63 downto 0);
      begin
        acc_3_10 <= \c$case_alt_105\;

        x_104 <= acc_2_20;

        y_93 <= acc_2_21;

        \c$case_alt_105\ <= (x_104 + y_93);


      end block;

    fun_314 : block
        signal x_105            : signed(63 downto 0);
        signal y_94             : signed(63 downto 0);
        signal \c$case_alt_106\ : signed(63 downto 0);
      begin
        acc_3_11 <= \c$case_alt_106\;

        x_105 <= acc_2_22;

        y_94 <= acc_2_23;

        \c$case_alt_106\ <= (x_105 + y_94);


      end block;

    fun_315 : block
        signal x_106            : signed(63 downto 0);
        signal y_95             : signed(63 downto 0);
        signal \c$case_alt_107\ : signed(63 downto 0);
      begin
        acc_3_12 <= \c$case_alt_107\;

        x_106 <= acc_2_24;

        y_95 <= acc_2_25;

        \c$case_alt_107\ <= (x_106 + y_95);


      end block;

    fun_316 : block
        signal x_107            : signed(63 downto 0);
        signal y_96             : signed(63 downto 0);
        signal \c$case_alt_108\ : signed(63 downto 0);
      begin
        acc_3_13 <= \c$case_alt_108\;

        x_107 <= acc_2_26;

        y_96 <= acc_1_54;

        \c$case_alt_108\ <= (x_107 + y_96);


      end block;

    fun_317 : block
        signal x_108            : signed(63 downto 0);
        signal y_97             : signed(63 downto 0);
        signal \c$case_alt_109\ : signed(63 downto 0);
      begin
        acc_4_0 <= \c$case_alt_109\;

        x_108 <= acc_3_0;

        y_97 <= acc_3_1;

        \c$case_alt_109\ <= (x_108 + y_97);


      end block;

    fun_318 : block
        signal x_109            : signed(63 downto 0);
        signal y_98             : signed(63 downto 0);
        signal \c$case_alt_110\ : signed(63 downto 0);
      begin
        acc_4_1 <= \c$case_alt_110\;

        x_109 <= acc_3_2;

        y_98 <= acc_3_3;

        \c$case_alt_110\ <= (x_109 + y_98);


      end block;

    fun_319 : block
        signal x_110            : signed(63 downto 0);
        signal y_99             : signed(63 downto 0);
        signal \c$case_alt_111\ : signed(63 downto 0);
      begin
        acc_4_2 <= \c$case_alt_111\;

        x_110 <= acc_3_4;

        y_99 <= acc_3_5;

        \c$case_alt_111\ <= (x_110 + y_99);


      end block;

    fun_320 : block
        signal x_111            : signed(63 downto 0);
        signal y_100            : signed(63 downto 0);
        signal \c$case_alt_112\ : signed(63 downto 0);
      begin
        acc_4_3 <= \c$case_alt_112\;

        x_111 <= acc_3_6;

        y_100 <= acc_3_7;

        \c$case_alt_112\ <= (x_111 + y_100);


      end block;

    fun_321 : block
        signal x_112            : signed(63 downto 0);
        signal y_101            : signed(63 downto 0);
        signal \c$case_alt_113\ : signed(63 downto 0);
      begin
        acc_4_4 <= \c$case_alt_113\;

        x_112 <= acc_3_8;

        y_101 <= acc_3_9;

        \c$case_alt_113\ <= (x_112 + y_101);


      end block;

    fun_322 : block
        signal x_113            : signed(63 downto 0);
        signal y_102            : signed(63 downto 0);
        signal \c$case_alt_114\ : signed(63 downto 0);
      begin
        acc_4_5 <= \c$case_alt_114\;

        x_113 <= acc_3_10;

        y_102 <= acc_3_11;

        \c$case_alt_114\ <= (x_113 + y_102);


      end block;

    fun_323 : block
        signal x_114            : signed(63 downto 0);
        signal y_103            : signed(63 downto 0);
        signal \c$case_alt_115\ : signed(63 downto 0);
      begin
        acc_4_6 <= \c$case_alt_115\;

        x_114 <= acc_3_12;

        y_103 <= acc_3_13;

        \c$case_alt_115\ <= (x_114 + y_103);


      end block;

    fun_324 : block
        signal x_115            : signed(63 downto 0);
        signal y_104            : signed(63 downto 0);
        signal \c$case_alt_116\ : signed(63 downto 0);
      begin
        acc_5_0 <= \c$case_alt_116\;

        x_115 <= acc_4_0;

        y_104 <= acc_4_1;

        \c$case_alt_116\ <= (x_115 + y_104);


      end block;

    fun_325 : block
        signal x_116            : signed(63 downto 0);
        signal y_105            : signed(63 downto 0);
        signal \c$case_alt_117\ : signed(63 downto 0);
      begin
        acc_5_1 <= \c$case_alt_117\;

        x_116 <= acc_4_2;

        y_105 <= acc_4_3;

        \c$case_alt_117\ <= (x_116 + y_105);


      end block;

    fun_326 : block
        signal x_117            : signed(63 downto 0);
        signal y_106            : signed(63 downto 0);
        signal \c$case_alt_118\ : signed(63 downto 0);
      begin
        acc_5_2 <= \c$case_alt_118\;

        x_117 <= acc_4_4;

        y_106 <= acc_4_5;

        \c$case_alt_118\ <= (x_117 + y_106);


      end block;

    fun_327 : block
        signal x_118            : signed(63 downto 0);
        signal y_107            : signed(63 downto 0);
        signal \c$case_alt_119\ : signed(63 downto 0);
      begin
        acc_6_0 <= \c$case_alt_119\;

        x_118 <= acc_5_0;

        y_107 <= acc_5_1;

        \c$case_alt_119\ <= (x_118 + y_107);


      end block;

    fun_328 : block
        signal x_119            : signed(63 downto 0);
        signal y_108            : signed(63 downto 0);
        signal \c$case_alt_120\ : signed(63 downto 0);
      begin
        acc_6_1 <= \c$case_alt_120\;

        x_119 <= acc_5_2;

        y_108 <= acc_4_6;

        \c$case_alt_120\ <= (x_119 + y_108);


      end block;

    fun_329 : block
        signal x_120            : signed(63 downto 0);
        signal y_109            : signed(63 downto 0);
        signal \c$case_alt_121\ : signed(63 downto 0);
      begin
        acc_7_0 <= \c$case_alt_121\;

        x_120 <= acc_6_0;

        y_109 <= acc_6_1;

        \c$case_alt_121\ <= (x_120 + y_109);


      end block;


  end block;

  result_52 <= t_9 when b_9 else
               result_53;

  -- register begin
  result_53_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_53 <= (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
    elsif rising_edge(clk) then
      if en then
        result_53 <= result_52;
      end if;
    end if;
  end process;
  -- register end

  b_9 <= result_59;

  t_9 <= ( Tuple2_1_sel0_unsigned => result_112.Tags_sel9_output6_0
         , Tuple2_1_sel1_boolean => \c$t_app_arg\ < (to_signed(1,64)) );

  x_projection_115 <=  win_4(0) ;

  x_6 <= x_projection_115;

  win_4 <= window_4.Tuple2_4_sel1_array_of_signed_64;

  -- register begin
  window_4_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window_4 <= ( Tuple2_4_sel0_unsigned => to_unsigned(113,8)
  , Tuple2_4_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
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
        window_4 <= result_54;
      end if;
    end if;
  end process;
  -- register end

  result_54 <= t_10 when b_10 else
               window_4;

  t_10 <= ( Tuple2_4_sel0_unsigned => result_113.Tags_sel15_slide1_0
          , Tuple2_4_sel1_array_of_signed_64 => result_55 );

  result_55 <= \c$t_case_alt_9\ when result_57 else
               \c$t_case_alt_10\;

  \c$t_case_alt_selection_31\ <= result_58;

  \c$vec_16\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win_4)));

  \c$t_case_alt_sel_alt_30\ <= (\c$vec_16\(0 to 111-1),\c$vec_16\(111 to \c$vec_16\'high));

  \c$vec_17\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated_4)));

  \c$t_case_alt_sel_alt_32\ <= (\c$vec_17\(0 to 111-1),\c$vec_17\(111 to \c$vec_17\'high));

  \c$t_case_alt_9\ <= \c$t_case_alt_sel_alt_32\.Tuple2_13_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_31\ else
                      \c$t_case_alt_sel_alt_30\.Tuple2_13_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_34\ <= result_58;

  \c$t_case_alt_10\ <= lastBucketUpdated_4 when \c$t_case_alt_selection_34\ else
                       win_4;

  -- replace begin
  replaceVec_4 : block
    signal vec_index_4 : integer range 0 to 111-1;
  begin
    vec_index_4 <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 111
    -- pragma translate_on
                 ;

    process(vec_index_4,win_4,x_6)
      variable ivec_4 : Spec_topEntity_types.array_of_signed_64(0 to 110);
    begin
      ivec_4 := win_4;
      ivec_4(vec_index_4) := ((x_6 + to_signed(1,64)));
      lastBucketUpdated_4 <= ivec_4;
    end process;
  end block;
  -- replace end

  b_10 <= result_58 or result_57;

  \c$vec2\ <= (ws(1 to ws'high));

  -- zipWith begin
  zipWith : for i_13 in \c$ws_app_arg\'range generate
  begin
    \c$ws_app_arg\(i_13) <= ( Tuple2_5_sel0_Tuple2_2 => result_76(i_13)
               , Tuple2_5_sel1_Maybe => \c$vec2\(i_13) );


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
    imap_0 : for i_14 in \c$ws_app_arg_0\'range generate
    begin
      fun_330 : block
      signal \c$case_alt_122\           : Spec_topEntity_types.Maybe;
      -- spec.hs:455:1-63
      signal t_34                       : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_121                      : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res\ : boolean;
    begin
      \c$ws_app_arg_0\(i_14) <= \c$case_alt_122\;

      \c$case_alt_selection_res\ <= t_34 = result_113.Tags_sel2_input2_0;

      \c$case_alt_122\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_14,max(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res\ else
                          \c$ws_app_arg\(i_14).Tuple2_5_sel1_Maybe;

      t_34 <= x_121.Tuple2_2_sel0_unsigned;

      x_121 <= \c$ws_app_arg\(i_14).Tuple2_5_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec : block
    signal vec_index_5 : integer range 0 to 5-1;
  begin
    vec_index_5 <= to_integer((signed(std_logic_vector(resize(i,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut\ <= result_76(vec_index_5);
  end block;
  -- index end

  \c$case_scrut_0\ <=  ws(0) ;

  with (\c$case_scrut_0\(3 downto 3)) select
    result_56 <= to_signed(0,64) when "0",
                 \c$case_scrut\.Tuple2_2_sel1_signed when others;

  i <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_0\(2 downto 0)));

  ws <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_0\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "---"))));

  iterateI_14 : block
    signal iterateI_ho1_0_arg0_9 : boolean;
    signal iterateI_ho1_0_res_14 : boolean;
    signal iterateI_ho1_1_res_13 : boolean;
    signal iterateI_ho1_2_res_13 : boolean;
    signal iterateI_ho1_3_res_13 : boolean;
    signal iterateI_ho1_4_res_13 : boolean;
    signal iterateI_ho1_5_res_13 : boolean;
  begin
    iterateI_ho1_0_arg0_9 <= slides.Slides_1_sel1_slide1;

    r_block_329 : block
        signal \c$bb_res_res_98\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_98_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_98\ <= iterateI_ho1_0_arg0_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_14 <= \c$bb_res_res_98\;


      end block;

    r_block_330 : block
        signal \c$bb_res_res_99\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_99_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_99\ <= iterateI_ho1_0_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_13 <= \c$bb_res_res_99\;


      end block;

    r_block_331 : block
        signal \c$bb_res_res_100\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_100_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_100\ <= iterateI_ho1_1_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_13 <= \c$bb_res_res_100\;


      end block;

    r_block_332 : block
        signal \c$bb_res_res_101\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_101_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_101\ <= iterateI_ho1_2_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_13 <= \c$bb_res_res_101\;


      end block;

    r_block_333 : block
        signal \c$bb_res_res_102\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_102_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_102\ <= iterateI_ho1_3_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_13 <= \c$bb_res_res_102\;


      end block;

    r_block_334 : block
        signal \c$bb_res_res_103\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_103_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_103\ <= iterateI_ho1_4_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_13 <= \c$bb_res_res_103\;


      end block;

    \c$app_arg_14\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel1_slide1
                                                            , iterateI_ho1_0_res_14
                                                            , iterateI_ho1_1_res_13
                                                            , iterateI_ho1_2_res_13
                                                            , iterateI_ho1_3_res_13
                                                            , iterateI_ho1_4_res_13
                                                            , iterateI_ho1_5_res_13 );


  end block;

  result_57 <=  \c$app_arg_14\(\c$app_arg_14\'high) ;

  iterateI_15 : block
    signal iterateI_ho1_0_res_15 : boolean;
    signal iterateI_ho1_1_res_14 : boolean;
    signal iterateI_ho1_2_res_14 : boolean;
    signal iterateI_ho1_3_res_14 : boolean;
    signal iterateI_ho1_4_res_14 : boolean;
    signal iterateI_ho1_5_res_14 : boolean;
  begin
    r_block_335 : block
        signal \c$bb_res_res_104\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_104_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_104\ <= pIn2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_15 <= \c$bb_res_res_104\;


      end block;

    r_block_336 : block
        signal \c$bb_res_res_105\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_105_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_105\ <= iterateI_ho1_0_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_14 <= \c$bb_res_res_105\;


      end block;

    r_block_337 : block
        signal \c$bb_res_res_106\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_106_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_106\ <= iterateI_ho1_1_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_14 <= \c$bb_res_res_106\;


      end block;

    r_block_338 : block
        signal \c$bb_res_res_107\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_107_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_107\ <= iterateI_ho1_2_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_14 <= \c$bb_res_res_107\;


      end block;

    r_block_339 : block
        signal \c$bb_res_res_108\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_108_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_108\ <= iterateI_ho1_3_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_14 <= \c$bb_res_res_108\;


      end block;

    r_block_340 : block
        signal \c$bb_res_res_109\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_109_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_109\ <= iterateI_ho1_4_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_14 <= \c$bb_res_res_109\;


      end block;

    \c$app_arg_15\ <= Spec_topEntity_types.array_of_boolean'( pIn2
                                                            , iterateI_ho1_0_res_15
                                                            , iterateI_ho1_1_res_14
                                                            , iterateI_ho1_2_res_14
                                                            , iterateI_ho1_3_res_14
                                                            , iterateI_ho1_4_res_14
                                                            , iterateI_ho1_5_res_14 );


  end block;

  result_58 <=  \c$app_arg_15\(\c$app_arg_15\'high) ;

  iterateI_16 : block
    signal iterateI_ho1_0_res_16 : boolean;
    signal iterateI_ho1_1_res_15 : boolean;
    signal iterateI_ho1_2_res_15 : boolean;
    signal iterateI_ho1_3_res_15 : boolean;
    signal iterateI_ho1_4_res_15 : boolean;
    signal iterateI_ho1_5_res_15 : boolean;
    signal iterateI_ho1_6_res_8  : boolean;
  begin
    r_block_341 : block
        signal \c$bb_res_res_110\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_110_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_110\ <= pOut6;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_16 <= \c$bb_res_res_110\;


      end block;

    r_block_342 : block
        signal \c$bb_res_res_111\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_111_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_111\ <= iterateI_ho1_0_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_15 <= \c$bb_res_res_111\;


      end block;

    r_block_343 : block
        signal \c$bb_res_res_112\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_112_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_112\ <= iterateI_ho1_1_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_15 <= \c$bb_res_res_112\;


      end block;

    r_block_344 : block
        signal \c$bb_res_res_113\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_113_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_113\ <= iterateI_ho1_2_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_15 <= \c$bb_res_res_113\;


      end block;

    r_block_345 : block
        signal \c$bb_res_res_114\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_114_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_114\ <= iterateI_ho1_3_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_15 <= \c$bb_res_res_114\;


      end block;

    r_block_346 : block
        signal \c$bb_res_res_115\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_115_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_115\ <= iterateI_ho1_4_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_15 <= \c$bb_res_res_115\;


      end block;

    r_block_347 : block
        signal \c$bb_res_res_116\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_116_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_116\ <= iterateI_ho1_5_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_8 <= \c$bb_res_res_116\;


      end block;

    \c$app_arg_16\ <= Spec_topEntity_types.array_of_boolean'( pOut6
                                                            , iterateI_ho1_0_res_16
                                                            , iterateI_ho1_1_res_15
                                                            , iterateI_ho1_2_res_15
                                                            , iterateI_ho1_3_res_15
                                                            , iterateI_ho1_4_res_15
                                                            , iterateI_ho1_5_res_15
                                                            , iterateI_ho1_6_res_8 );


  end block;

  result_59 <=  \c$app_arg_16\(\c$app_arg_16\'high) ;

  iterateI_17 : block
    signal iterateI_ho1_0_arg0_10 : boolean;
    signal iterateI_ho1_0_res_17  : boolean;
    signal iterateI_ho1_1_res_16  : boolean;
    signal iterateI_ho1_2_res_16  : boolean;
    signal iterateI_ho1_3_res_16  : boolean;
    signal iterateI_ho1_4_res_16  : boolean;
    signal iterateI_ho1_5_res_16  : boolean;
    signal iterateI_ho1_6_res_9   : boolean;
    signal iterateI_ho1_7_res_4   : boolean;
  begin
    iterateI_ho1_0_arg0_10 <= pOut5;

    r_block_348 : block
        signal \c$bb_res_res_117\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_117_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_117\ <= iterateI_ho1_0_arg0_10;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_17 <= \c$bb_res_res_117\;


      end block;

    r_block_349 : block
        signal \c$bb_res_res_118\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_118_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_118\ <= iterateI_ho1_0_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_16 <= \c$bb_res_res_118\;


      end block;

    r_block_350 : block
        signal \c$bb_res_res_119\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_119_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_119\ <= iterateI_ho1_1_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_16 <= \c$bb_res_res_119\;


      end block;

    r_block_351 : block
        signal \c$bb_res_res_120\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_120_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_120\ <= iterateI_ho1_2_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_16 <= \c$bb_res_res_120\;


      end block;

    r_block_352 : block
        signal \c$bb_res_res_121\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_121_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_121\ <= iterateI_ho1_3_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_16 <= \c$bb_res_res_121\;


      end block;

    r_block_353 : block
        signal \c$bb_res_res_122\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_122_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_122\ <= iterateI_ho1_4_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_16 <= \c$bb_res_res_122\;


      end block;

    r_block_354 : block
        signal \c$bb_res_res_123\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_123_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_123\ <= iterateI_ho1_5_res_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_9 <= \c$bb_res_res_123\;


      end block;

    r_block_355 : block
        signal \c$bb_res_res_124\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_124_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_124\ <= iterateI_ho1_6_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_4 <= \c$bb_res_res_124\;


      end block;

    \c$app_arg_17\ <= Spec_topEntity_types.array_of_boolean'( pOut5
                                                            , iterateI_ho1_0_res_17
                                                            , iterateI_ho1_1_res_16
                                                            , iterateI_ho1_2_res_16
                                                            , iterateI_ho1_3_res_16
                                                            , iterateI_ho1_4_res_16
                                                            , iterateI_ho1_5_res_16
                                                            , iterateI_ho1_6_res_9
                                                            , iterateI_ho1_7_res_4 );


  end block;

  result_60 <=  \c$app_arg_17\(\c$app_arg_17\'high) ;

  result_61 <= t_11 when b_11 else
               result_62;

  -- register begin
  result_62_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_62 <= (Tuple2_1_sel0_unsigned => to_unsigned(113,8), Tuple2_1_sel1_boolean => false);
    elsif rising_edge(clk) then
      if en then
        result_62 <= result_61;
      end if;
    end if;
  end process;
  -- register end

  b_11 <= result_64;

  t_11 <= ( Tuple2_1_sel0_unsigned => result_112.Tags_sel8_output5_0
          , Tuple2_1_sel1_boolean => result_63 > (to_signed(5,64)) );

  \c$vec2_0\ <= (ws_0(1 to ws_0'high));

  -- zipWith begin
  zipWith_0 : for i_16 in \c$ws_app_arg_1\'range generate
  begin
    \c$ws_app_arg_1\(i_16) <= ( Tuple2_6_sel0_Tuple2_2 => result_79(i_16)
               , Tuple2_6_sel1_Maybe_0 => \c$vec2_0\(i_16) );


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
    imap_2 : for i_17 in \c$ws_app_arg_2\'range generate
    begin
      fun_331 : block
      signal \c$case_alt_123\             : Spec_topEntity_types.Maybe_0;
      -- spec.hs:455:1-63
      signal t_35                         : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_122                        : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_2\ : boolean;
    begin
      \c$ws_app_arg_2\(i_17) <= \c$case_alt_123\;

      \c$case_alt_selection_res_2\ <= t_35 = result_112.Tags_sel7_output4_0;

      \c$case_alt_123\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_17,max_0(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_2\ else
                          \c$ws_app_arg_1\(i_17).Tuple2_6_sel1_Maybe_0;

      t_35 <= x_122.Tuple2_2_sel0_unsigned;

      x_122 <= \c$ws_app_arg_1\(i_17).Tuple2_6_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_0 : block
    signal vec_index_6 : integer range 0 to 2-1;
  begin
    vec_index_6 <= to_integer((signed(std_logic_vector(resize(i_0,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_1\ <= result_79(vec_index_6);
  end block;
  -- index end

  \c$case_scrut_2\ <=  ws_0(0) ;

  with (\c$case_scrut_2\(1 downto 1)) select
    result_63 <= to_signed(0,64) when "0",
                 \c$case_scrut_1\.Tuple2_2_sel1_signed when others;

  i_0 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_2\(0 downto 0)));

  ws_0 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_2\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "-"))));

  iterateI_18 : block
    signal iterateI_ho1_0_res_18 : boolean;
    signal iterateI_ho1_1_res_17 : boolean;
    signal iterateI_ho1_2_res_17 : boolean;
    signal iterateI_ho1_3_res_17 : boolean;
    signal iterateI_ho1_4_res_17 : boolean;
    signal iterateI_ho1_5_res_17 : boolean;
    signal iterateI_ho1_6_res_10 : boolean;
  begin
    r_block_356 : block
        signal \c$bb_res_res_125\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_125_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_125\ <= pOut5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_18 <= \c$bb_res_res_125\;


      end block;

    r_block_357 : block
        signal \c$bb_res_res_126\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_126_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_126\ <= iterateI_ho1_0_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_17 <= \c$bb_res_res_126\;


      end block;

    r_block_358 : block
        signal \c$bb_res_res_127\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_127_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_127\ <= iterateI_ho1_1_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_17 <= \c$bb_res_res_127\;


      end block;

    r_block_359 : block
        signal \c$bb_res_res_128\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_128_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_128\ <= iterateI_ho1_2_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_17 <= \c$bb_res_res_128\;


      end block;

    r_block_360 : block
        signal \c$bb_res_res_129\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_129_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_129\ <= iterateI_ho1_3_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_17 <= \c$bb_res_res_129\;


      end block;

    r_block_361 : block
        signal \c$bb_res_res_130\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_130_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_130\ <= iterateI_ho1_4_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_17 <= \c$bb_res_res_130\;


      end block;

    r_block_362 : block
        signal \c$bb_res_res_131\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_131_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_131\ <= iterateI_ho1_5_res_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_10 <= \c$bb_res_res_131\;


      end block;

    \c$app_arg_18\ <= Spec_topEntity_types.array_of_boolean'( pOut5
                                                            , iterateI_ho1_0_res_18
                                                            , iterateI_ho1_1_res_17
                                                            , iterateI_ho1_2_res_17
                                                            , iterateI_ho1_3_res_17
                                                            , iterateI_ho1_4_res_17
                                                            , iterateI_ho1_5_res_17
                                                            , iterateI_ho1_6_res_10 );


  end block;

  result_64 <=  \c$app_arg_18\(\c$app_arg_18\'high) ;

  \c$vec2_1\ <= (ws_1(1 to ws_1'high));

  -- zipWith begin
  zipWith_1 : for i_18 in \c$ws_app_arg_3\'range generate
  begin
    \c$ws_app_arg_3\(i_18) <= ( Tuple2_6_sel0_Tuple2_2 => result_79(i_18)
               , Tuple2_6_sel1_Maybe_0 => \c$vec2_1\(i_18) );


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
    imap_4 : for i_19 in \c$ws_app_arg_4\'range generate
    begin
      fun_332 : block
      signal \c$case_alt_124\             : Spec_topEntity_types.Maybe_0;
      -- spec.hs:455:1-63
      signal t_36                         : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_123                        : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_3\ : boolean;
    begin
      \c$ws_app_arg_4\(i_19) <= \c$case_alt_124\;

      \c$case_alt_selection_res_3\ <= t_36 = result_111.Tags_sel7_output4_0;

      \c$case_alt_124\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_19,max_1(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_3\ else
                          \c$ws_app_arg_3\(i_19).Tuple2_6_sel1_Maybe_0;

      t_36 <= x_123.Tuple2_2_sel0_unsigned;

      x_123 <= \c$ws_app_arg_3\(i_19).Tuple2_6_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_1 : block
    signal vec_index_7 : integer range 0 to 2-1;
  begin
    vec_index_7 <= to_integer((signed(std_logic_vector(resize(i_1,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_3\ <= result_79(vec_index_7);
  end block;
  -- index end

  \c$case_scrut_4\ <=  ws_1(0) ;

  with (\c$case_scrut_4\(1 downto 1)) select
    result_65 <= to_signed(0,64) when "0",
                 \c$case_scrut_3\.Tuple2_2_sel1_signed when others;

  i_1 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_4\(0 downto 0)));

  ws_1 <= Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(\c$ws_app_arg_4\) & Spec_topEntity_types.array_of_Maybe_0'(Spec_topEntity_types.array_of_Maybe_0'(0 => std_logic_vector'("0" & "-"))));

  iterateI_19 : block
    signal iterateI_ho1_0_arg0_11 : boolean;
    signal iterateI_ho1_0_res_19  : boolean;
    signal iterateI_ho1_1_res_18  : boolean;
    signal iterateI_ho1_2_res_18  : boolean;
    signal iterateI_ho1_3_res_18  : boolean;
    signal iterateI_ho1_4_res_18  : boolean;
    signal iterateI_ho1_5_res_18  : boolean;
    signal iterateI_ho1_6_res_11  : boolean;
    signal iterateI_ho1_7_res_5   : boolean;
  begin
    iterateI_ho1_0_arg0_11 <= pOut4;

    r_block_363 : block
        signal \c$bb_res_res_132\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_132_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_132\ <= iterateI_ho1_0_arg0_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_19 <= \c$bb_res_res_132\;


      end block;

    r_block_364 : block
        signal \c$bb_res_res_133\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_133_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_133\ <= iterateI_ho1_0_res_19;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_18 <= \c$bb_res_res_133\;


      end block;

    r_block_365 : block
        signal \c$bb_res_res_134\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_134_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_134\ <= iterateI_ho1_1_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_18 <= \c$bb_res_res_134\;


      end block;

    r_block_366 : block
        signal \c$bb_res_res_135\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_135_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_135\ <= iterateI_ho1_2_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_18 <= \c$bb_res_res_135\;


      end block;

    r_block_367 : block
        signal \c$bb_res_res_136\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_136_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_136\ <= iterateI_ho1_3_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_18 <= \c$bb_res_res_136\;


      end block;

    r_block_368 : block
        signal \c$bb_res_res_137\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_137_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_137\ <= iterateI_ho1_4_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_18 <= \c$bb_res_res_137\;


      end block;

    r_block_369 : block
        signal \c$bb_res_res_138\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_138_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_138\ <= iterateI_ho1_5_res_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_11 <= \c$bb_res_res_138\;


      end block;

    r_block_370 : block
        signal \c$bb_res_res_139\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_139_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_139\ <= iterateI_ho1_6_res_11;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_5 <= \c$bb_res_res_139\;


      end block;

    \c$app_arg_19\ <= Spec_topEntity_types.array_of_boolean'( pOut4
                                                            , iterateI_ho1_0_res_19
                                                            , iterateI_ho1_1_res_18
                                                            , iterateI_ho1_2_res_18
                                                            , iterateI_ho1_3_res_18
                                                            , iterateI_ho1_4_res_18
                                                            , iterateI_ho1_5_res_18
                                                            , iterateI_ho1_6_res_11
                                                            , iterateI_ho1_7_res_5 );


  end block;

  result_66 <=  \c$app_arg_19\(\c$app_arg_19\'high) ;

  \c$vec2_2\ <= (ws_2(1 to ws_2'high));

  -- zipWith begin
  zipWith_2 : for i_20 in \c$ws_app_arg_5\'range generate
  begin
    \c$ws_app_arg_5\(i_20) <= ( Tuple2_7_sel0_Tuple2_1 => result_87(i_20)
               , Tuple2_7_sel1_Maybe_1 => \c$vec2_2\(i_20) );


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
    imap_6 : for i_21 in \c$ws_app_arg_6\'range generate
    begin
      fun_333 : block
      signal \c$case_alt_125\             : Spec_topEntity_types.Maybe_1;
      -- spec.hs:455:1-63
      signal t_37                         : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_124                        : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_4\ : boolean;
    begin
      \c$ws_app_arg_6\(i_21) <= \c$case_alt_125\;

      \c$case_alt_selection_res_4\ <= t_37 = result_111.Tags_sel6_output3_0;

      \c$case_alt_125\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_21,max_2(1,integer(ceil(log2(real(4))))))))) when \c$case_alt_selection_res_4\ else
                          \c$ws_app_arg_5\(i_21).Tuple2_7_sel1_Maybe_1;

      t_37 <= x_124.Tuple2_1_sel0_unsigned;

      x_124 <= \c$ws_app_arg_5\(i_21).Tuple2_7_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_2 : block
    signal vec_index_8 : integer range 0 to 4-1;
  begin
    vec_index_8 <= to_integer((signed(std_logic_vector(resize(i_2,64)))))
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
    \c$case_scrut_5\ <= result_87(vec_index_8);
  end block;
  -- index end

  \c$case_scrut_6\ <=  ws_2(0) ;

  with (\c$case_scrut_6\(2 downto 2)) select
    result_67 <= false when "0",
                 \c$case_scrut_5\.Tuple2_1_sel1_boolean when others;

  i_2 <= Spec_topEntity_types.index_4'(spec_topentity_types.fromSLV(\c$case_scrut_6\(1 downto 0)));

  ws_2 <= Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(\c$ws_app_arg_6\) & Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(0 => std_logic_vector'("0" & "--"))));

  iterateI_20 : block
    signal iterateI_ho1_0_arg0_12 : boolean;
    signal iterateI_ho1_0_res_20  : boolean;
    signal iterateI_ho1_1_res_19  : boolean;
    signal iterateI_ho1_2_res_19  : boolean;
    signal iterateI_ho1_3_res_19  : boolean;
    signal iterateI_ho1_4_res_19  : boolean;
    signal iterateI_ho1_5_res_19  : boolean;
    signal iterateI_ho1_6_res_12  : boolean;
    signal iterateI_ho1_7_res_6   : boolean;
  begin
    iterateI_ho1_0_arg0_12 <= pOut3;

    r_block_371 : block
        signal \c$bb_res_res_140\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_140_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_140\ <= iterateI_ho1_0_arg0_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_20 <= \c$bb_res_res_140\;


      end block;

    r_block_372 : block
        signal \c$bb_res_res_141\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_141_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_141\ <= iterateI_ho1_0_res_20;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_19 <= \c$bb_res_res_141\;


      end block;

    r_block_373 : block
        signal \c$bb_res_res_142\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_142_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_142\ <= iterateI_ho1_1_res_19;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_19 <= \c$bb_res_res_142\;


      end block;

    r_block_374 : block
        signal \c$bb_res_res_143\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_143_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_143\ <= iterateI_ho1_2_res_19;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_19 <= \c$bb_res_res_143\;


      end block;

    r_block_375 : block
        signal \c$bb_res_res_144\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_144_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_144\ <= iterateI_ho1_3_res_19;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_19 <= \c$bb_res_res_144\;


      end block;

    r_block_376 : block
        signal \c$bb_res_res_145\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_145_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_145\ <= iterateI_ho1_4_res_19;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_19 <= \c$bb_res_res_145\;


      end block;

    r_block_377 : block
        signal \c$bb_res_res_146\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_146_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_146\ <= iterateI_ho1_5_res_19;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_12 <= \c$bb_res_res_146\;


      end block;

    r_block_378 : block
        signal \c$bb_res_res_147\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_147_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_147\ <= iterateI_ho1_6_res_12;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_6 <= \c$bb_res_res_147\;


      end block;

    \c$app_arg_20\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                            , iterateI_ho1_0_res_20
                                                            , iterateI_ho1_1_res_19
                                                            , iterateI_ho1_2_res_19
                                                            , iterateI_ho1_3_res_19
                                                            , iterateI_ho1_4_res_19
                                                            , iterateI_ho1_5_res_19
                                                            , iterateI_ho1_6_res_12
                                                            , iterateI_ho1_7_res_6 );


  end block;

  result_68 <=  \c$app_arg_20\(\c$app_arg_20\'high) ;

  \c$vec2_3\ <= (ws_3(1 to ws_3'high));

  -- zipWith begin
  zipWith_3 : for i_22 in \c$ws_app_arg_7\'range generate
  begin
    \c$ws_app_arg_7\(i_22) <= ( Tuple2_8_sel0_Tuple2_1 => result_96(i_22)
               , Tuple2_8_sel1_Maybe => \c$vec2_3\(i_22) );


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
    imap_8 : for i_23 in \c$ws_app_arg_8\'range generate
    begin
      fun_334 : block
      signal \c$case_alt_126\             : Spec_topEntity_types.Maybe;
      -- spec.hs:455:1-63
      signal t_38                         : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_125                        : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_5\ : boolean;
    begin
      \c$ws_app_arg_8\(i_23) <= \c$case_alt_126\;

      \c$case_alt_selection_res_5\ <= t_38 = result_111.Tags_sel5_output2_0;

      \c$case_alt_126\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_23,max_3(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res_5\ else
                          \c$ws_app_arg_7\(i_23).Tuple2_8_sel1_Maybe;

      t_38 <= x_125.Tuple2_1_sel0_unsigned;

      x_125 <= \c$ws_app_arg_7\(i_23).Tuple2_8_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_3 : block
    signal vec_index_9 : integer range 0 to 5-1;
  begin
    vec_index_9 <= to_integer((signed(std_logic_vector(resize(i_3,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut_7\ <= result_96(vec_index_9);
  end block;
  -- index end

  \c$case_scrut_8\ <=  ws_3(0) ;

  with (\c$case_scrut_8\(3 downto 3)) select
    result_69 <= false when "0",
                 \c$case_scrut_7\.Tuple2_1_sel1_boolean when others;

  i_3 <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_8\(2 downto 0)));

  ws_3 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_8\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "---"))));

  iterateI_21 : block
    signal iterateI_ho1_0_arg0_13 : boolean;
    signal iterateI_ho1_0_res_21  : boolean;
    signal iterateI_ho1_1_res_20  : boolean;
    signal iterateI_ho1_2_res_20  : boolean;
    signal iterateI_ho1_3_res_20  : boolean;
    signal iterateI_ho1_4_res_20  : boolean;
    signal iterateI_ho1_5_res_20  : boolean;
    signal iterateI_ho1_6_res_13  : boolean;
    signal iterateI_ho1_7_res_7   : boolean;
  begin
    iterateI_ho1_0_arg0_13 <= pOut2;

    r_block_379 : block
        signal \c$bb_res_res_148\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_148_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_148\ <= iterateI_ho1_0_arg0_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_21 <= \c$bb_res_res_148\;


      end block;

    r_block_380 : block
        signal \c$bb_res_res_149\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_149_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_149\ <= iterateI_ho1_0_res_21;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_20 <= \c$bb_res_res_149\;


      end block;

    r_block_381 : block
        signal \c$bb_res_res_150\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_150_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_150\ <= iterateI_ho1_1_res_20;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_20 <= \c$bb_res_res_150\;


      end block;

    r_block_382 : block
        signal \c$bb_res_res_151\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_151_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_151\ <= iterateI_ho1_2_res_20;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_20 <= \c$bb_res_res_151\;


      end block;

    r_block_383 : block
        signal \c$bb_res_res_152\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_152_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_152\ <= iterateI_ho1_3_res_20;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_20 <= \c$bb_res_res_152\;


      end block;

    r_block_384 : block
        signal \c$bb_res_res_153\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_153_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_153\ <= iterateI_ho1_4_res_20;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_20 <= \c$bb_res_res_153\;


      end block;

    r_block_385 : block
        signal \c$bb_res_res_154\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_154_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_154\ <= iterateI_ho1_5_res_20;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_13 <= \c$bb_res_res_154\;


      end block;

    r_block_386 : block
        signal \c$bb_res_res_155\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_155_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_155\ <= iterateI_ho1_6_res_13;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_7 <= \c$bb_res_res_155\;


      end block;

    \c$app_arg_21\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                            , iterateI_ho1_0_res_21
                                                            , iterateI_ho1_1_res_20
                                                            , iterateI_ho1_2_res_20
                                                            , iterateI_ho1_3_res_20
                                                            , iterateI_ho1_4_res_20
                                                            , iterateI_ho1_5_res_20
                                                            , iterateI_ho1_6_res_13
                                                            , iterateI_ho1_7_res_7 );


  end block;

  result_70 <=  \c$app_arg_21\(\c$app_arg_21\'high) ;

  \c$vec2_4\ <= (ws_4(1 to ws_4'high));

  -- zipWith begin
  zipWith_4 : for i_24 in \c$ws_app_arg_9\'range generate
  begin
    \c$ws_app_arg_9\(i_24) <= ( Tuple2_8_sel0_Tuple2_1 => result_99(i_24)
               , Tuple2_8_sel1_Maybe => \c$vec2_4\(i_24) );


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
    imap_10 : for i_25 in \c$ws_app_arg_10\'range generate
    begin
      fun_335 : block
      signal \c$case_alt_127\             : Spec_topEntity_types.Maybe;
      -- spec.hs:455:1-63
      signal t_39                         : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_126                        : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_6\ : boolean;
    begin
      \c$ws_app_arg_10\(i_25) <= \c$case_alt_127\;

      \c$case_alt_selection_res_6\ <= t_39 = result_111.Tags_sel4_output1_0;

      \c$case_alt_127\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_25,max_4(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res_6\ else
                          \c$ws_app_arg_9\(i_25).Tuple2_8_sel1_Maybe;

      t_39 <= x_126.Tuple2_1_sel0_unsigned;

      x_126 <= \c$ws_app_arg_9\(i_25).Tuple2_8_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_4 : block
    signal vec_index_10 : integer range 0 to 5-1;
  begin
    vec_index_10 <= to_integer((signed(std_logic_vector(resize(i_4,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut_9\ <= result_99(vec_index_10);
  end block;
  -- index end

  \c$case_scrut_10\ <=  ws_4(0) ;

  with (\c$case_scrut_10\(3 downto 3)) select
    result_71 <= false when "0",
                 \c$case_scrut_9\.Tuple2_1_sel1_boolean when others;

  i_4 <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_10\(2 downto 0)));

  ws_4 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_10\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "---"))));

  iterateI_22 : block
    signal iterateI_ho1_0_arg0_14 : boolean;
    signal iterateI_ho1_0_res_22  : boolean;
    signal iterateI_ho1_1_res_21  : boolean;
    signal iterateI_ho1_2_res_21  : boolean;
    signal iterateI_ho1_3_res_21  : boolean;
    signal iterateI_ho1_4_res_21  : boolean;
    signal iterateI_ho1_5_res_21  : boolean;
    signal iterateI_ho1_6_res_14  : boolean;
    signal iterateI_ho1_7_res_8   : boolean;
  begin
    iterateI_ho1_0_arg0_14 <= pOut1;

    r_block_387 : block
        signal \c$bb_res_res_156\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_156_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_156\ <= iterateI_ho1_0_arg0_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_22 <= \c$bb_res_res_156\;


      end block;

    r_block_388 : block
        signal \c$bb_res_res_157\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_157_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_157\ <= iterateI_ho1_0_res_22;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_21 <= \c$bb_res_res_157\;


      end block;

    r_block_389 : block
        signal \c$bb_res_res_158\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_158_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_158\ <= iterateI_ho1_1_res_21;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_21 <= \c$bb_res_res_158\;


      end block;

    r_block_390 : block
        signal \c$bb_res_res_159\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_159_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_159\ <= iterateI_ho1_2_res_21;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_21 <= \c$bb_res_res_159\;


      end block;

    r_block_391 : block
        signal \c$bb_res_res_160\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_160_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_160\ <= iterateI_ho1_3_res_21;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_21 <= \c$bb_res_res_160\;


      end block;

    r_block_392 : block
        signal \c$bb_res_res_161\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_161_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_161\ <= iterateI_ho1_4_res_21;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_21 <= \c$bb_res_res_161\;


      end block;

    r_block_393 : block
        signal \c$bb_res_res_162\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_162_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_162\ <= iterateI_ho1_5_res_21;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_14 <= \c$bb_res_res_162\;


      end block;

    r_block_394 : block
        signal \c$bb_res_res_163\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_163_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_163\ <= iterateI_ho1_6_res_14;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_8 <= \c$bb_res_res_163\;


      end block;

    \c$app_arg_22\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                            , iterateI_ho1_0_res_22
                                                            , iterateI_ho1_1_res_21
                                                            , iterateI_ho1_2_res_21
                                                            , iterateI_ho1_3_res_21
                                                            , iterateI_ho1_4_res_21
                                                            , iterateI_ho1_5_res_21
                                                            , iterateI_ho1_6_res_14
                                                            , iterateI_ho1_7_res_8 );


  end block;

  result_72 <=  \c$app_arg_22\(\c$app_arg_22\'high) ;

  \c$vec2_5\ <= (ws_5(1 to ws_5'high));

  -- zipWith begin
  zipWith_5 : for i_26 in \c$ws_app_arg_11\'range generate
  begin
    \c$ws_app_arg_11\(i_26) <= ( Tuple2_9_sel0_Tuple2_2 => result_105(i_26)
               , Tuple2_9_sel1_Maybe_2 => \c$vec2_5\(i_26) );


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
    imap_12 : for i_27 in \c$ws_app_arg_12\'range generate
    begin
      fun_336 : block
      signal \c$case_alt_128\             : Spec_topEntity_types.Maybe_2;
      -- spec.hs:455:1-63
      signal t_40                         : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_127                        : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_7\ : boolean;
    begin
      \c$ws_app_arg_12\(i_27) <= \c$case_alt_128\;

      \c$case_alt_selection_res_7\ <= t_40 = result_111.Tags_sel3_output0_0;

      \c$case_alt_128\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_27,max_5(1,integer(ceil(log2(real(6))))))))) when \c$case_alt_selection_res_7\ else
                          \c$ws_app_arg_11\(i_27).Tuple2_9_sel1_Maybe_2;

      t_40 <= x_127.Tuple2_2_sel0_unsigned;

      x_127 <= \c$ws_app_arg_11\(i_27).Tuple2_9_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_5 : block
    signal vec_index_11 : integer range 0 to 6-1;
  begin
    vec_index_11 <= to_integer((signed(std_logic_vector(resize(i_5,64)))))
    -- pragma translate_off
                 mod 6
    -- pragma translate_on
                 ;
    \c$case_scrut_11\ <= result_105(vec_index_11);
  end block;
  -- index end

  \c$case_scrut_12\ <=  ws_5(0) ;

  with (\c$case_scrut_12\(3 downto 3)) select
    result_73 <= to_signed(0,64) when "0",
                 \c$case_scrut_11\.Tuple2_2_sel1_signed when others;

  i_5 <= Spec_topEntity_types.index_6'(spec_topentity_types.fromSLV(\c$case_scrut_12\(2 downto 0)));

  ws_5 <= Spec_topEntity_types.array_of_Maybe_2'(Spec_topEntity_types.array_of_Maybe_2'(\c$ws_app_arg_12\) & Spec_topEntity_types.array_of_Maybe_2'(Spec_topEntity_types.array_of_Maybe_2'(0 => std_logic_vector'("0" & "---"))));

  iterateI_23 : block
    signal iterateI_ho1_0_arg0_15 : boolean;
    signal iterateI_ho1_0_res_23  : boolean;
    signal iterateI_ho1_1_res_22  : boolean;
    signal iterateI_ho1_2_res_22  : boolean;
    signal iterateI_ho1_3_res_22  : boolean;
    signal iterateI_ho1_4_res_22  : boolean;
    signal iterateI_ho1_5_res_22  : boolean;
    signal iterateI_ho1_6_res_15  : boolean;
    signal iterateI_ho1_7_res_9   : boolean;
  begin
    iterateI_ho1_0_arg0_15 <= pOut0;

    r_block_395 : block
        signal \c$bb_res_res_164\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_164_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_164\ <= iterateI_ho1_0_arg0_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_23 <= \c$bb_res_res_164\;


      end block;

    r_block_396 : block
        signal \c$bb_res_res_165\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_165_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_165\ <= iterateI_ho1_0_res_23;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_22 <= \c$bb_res_res_165\;


      end block;

    r_block_397 : block
        signal \c$bb_res_res_166\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_166_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_166\ <= iterateI_ho1_1_res_22;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_22 <= \c$bb_res_res_166\;


      end block;

    r_block_398 : block
        signal \c$bb_res_res_167\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_167_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_167\ <= iterateI_ho1_2_res_22;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_22 <= \c$bb_res_res_167\;


      end block;

    r_block_399 : block
        signal \c$bb_res_res_168\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_168_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_168\ <= iterateI_ho1_3_res_22;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_22 <= \c$bb_res_res_168\;


      end block;

    r_block_400 : block
        signal \c$bb_res_res_169\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_169_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_169\ <= iterateI_ho1_4_res_22;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_22 <= \c$bb_res_res_169\;


      end block;

    r_block_401 : block
        signal \c$bb_res_res_170\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_170_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_170\ <= iterateI_ho1_5_res_22;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_15 <= \c$bb_res_res_170\;


      end block;

    r_block_402 : block
        signal \c$bb_res_res_171\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_171_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_171\ <= iterateI_ho1_6_res_15;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_7_res_9 <= \c$bb_res_res_171\;


      end block;

    \c$app_arg_23\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                            , iterateI_ho1_0_res_23
                                                            , iterateI_ho1_1_res_22
                                                            , iterateI_ho1_2_res_22
                                                            , iterateI_ho1_3_res_22
                                                            , iterateI_ho1_4_res_22
                                                            , iterateI_ho1_5_res_22
                                                            , iterateI_ho1_6_res_15
                                                            , iterateI_ho1_7_res_9 );


  end block;

  result_74 <=  \c$app_arg_23\(\c$app_arg_23\'high) ;

  result_75 <= t_12 when b_12 else
               result_76;

  -- register begin
  result_76_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_76 <= Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_76 <= result_75;
      end if;
    end if;
  end process;
  -- register end

  b_12 <= result_77;

  \c$vec_18\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(result_76) & Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => ( Tuple2_2_sel0_unsigned => t_30
              , Tuple2_2_sel1_signed => \c$input2Win_app_arg\ )))));

  t_projection_7 <= (\c$vec_18\(0 to 1-1),\c$vec_18\(1 to \c$vec_18\'high));

  t_12 <= t_projection_7.Tuple2_14_sel1_array_of_Tuple2_2_1;

  -- delay begin
  cinput2Win_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$input2Win_app_arg\ <= inputs_3.Inputs_4_sel2_input2.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_24 : block
    signal iterateI_ho1_0_res_24 : boolean;
  begin
    r_block_403 : block
        signal \c$bb_res_res_172\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_172_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_172\ <= pIn2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_24 <= \c$bb_res_res_172\;


      end block;

    \c$app_arg_24\ <= Spec_topEntity_types.array_of_boolean'( pIn2
                                                            , iterateI_ho1_0_res_24 );


  end block;

  result_77 <=  \c$app_arg_24\(\c$app_arg_24\'high) ;

  \c$vec_19\ <= window_5.Tuple2_10_sel1_array_of_signed_64;

  fold_0 : block
    signal vec_0       : Spec_topEntity_types.array_of_signed_64(0 to 99);
    signal acc_7_0_98  : signed(63 downto 0);
    signal acc_0_56    : signed(63 downto 0);
    signal acc_0_57    : signed(63 downto 0);
    signal acc_0_58    : signed(63 downto 0);
    signal acc_0_59    : signed(63 downto 0);
    signal acc_0_60    : signed(63 downto 0);
    signal acc_0_61    : signed(63 downto 0);
    signal acc_0_62    : signed(63 downto 0);
    signal acc_0_63    : signed(63 downto 0);
    signal acc_0_64    : signed(63 downto 0);
    signal acc_0_65    : signed(63 downto 0);
    signal acc_0_66    : signed(63 downto 0);
    signal acc_0_67    : signed(63 downto 0);
    signal acc_0_68    : signed(63 downto 0);
    signal acc_0_69    : signed(63 downto 0);
    signal acc_0_70    : signed(63 downto 0);
    signal acc_0_71    : signed(63 downto 0);
    signal acc_0_72    : signed(63 downto 0);
    signal acc_0_73    : signed(63 downto 0);
    signal acc_0_74    : signed(63 downto 0);
    signal acc_0_75    : signed(63 downto 0);
    signal acc_0_76    : signed(63 downto 0);
    signal acc_0_77    : signed(63 downto 0);
    signal acc_0_78    : signed(63 downto 0);
    signal acc_0_79    : signed(63 downto 0);
    signal acc_0_80    : signed(63 downto 0);
    signal acc_0_81    : signed(63 downto 0);
    signal acc_0_82    : signed(63 downto 0);
    signal acc_0_83    : signed(63 downto 0);
    signal acc_0_84    : signed(63 downto 0);
    signal acc_0_85    : signed(63 downto 0);
    signal acc_0_86    : signed(63 downto 0);
    signal acc_0_87    : signed(63 downto 0);
    signal acc_0_88    : signed(63 downto 0);
    signal acc_0_89    : signed(63 downto 0);
    signal acc_0_90    : signed(63 downto 0);
    signal acc_0_91    : signed(63 downto 0);
    signal acc_0_92    : signed(63 downto 0);
    signal acc_0_93    : signed(63 downto 0);
    signal acc_0_94    : signed(63 downto 0);
    signal acc_0_95    : signed(63 downto 0);
    signal acc_0_96    : signed(63 downto 0);
    signal acc_0_97    : signed(63 downto 0);
    signal acc_0_98    : signed(63 downto 0);
    signal acc_0_99    : signed(63 downto 0);
    signal acc_0_100   : signed(63 downto 0);
    signal acc_0_101   : signed(63 downto 0);
    signal acc_0_102   : signed(63 downto 0);
    signal acc_0_103   : signed(63 downto 0);
    signal acc_0_104   : signed(63 downto 0);
    signal acc_0_105   : signed(63 downto 0);
    signal acc_0_106   : signed(63 downto 0);
    signal acc_0_107   : signed(63 downto 0);
    signal acc_0_108   : signed(63 downto 0);
    signal acc_0_109   : signed(63 downto 0);
    signal acc_0_110   : signed(63 downto 0);
    signal acc_0_111   : signed(63 downto 0);
    signal acc_0_112   : signed(63 downto 0);
    signal acc_0_113   : signed(63 downto 0);
    signal acc_0_114   : signed(63 downto 0);
    signal acc_0_115   : signed(63 downto 0);
    signal acc_0_116   : signed(63 downto 0);
    signal acc_0_117   : signed(63 downto 0);
    signal acc_0_118   : signed(63 downto 0);
    signal acc_0_119   : signed(63 downto 0);
    signal acc_0_120   : signed(63 downto 0);
    signal acc_0_121   : signed(63 downto 0);
    signal acc_0_122   : signed(63 downto 0);
    signal acc_0_123   : signed(63 downto 0);
    signal acc_0_124   : signed(63 downto 0);
    signal acc_0_125   : signed(63 downto 0);
    signal acc_0_126   : signed(63 downto 0);
    signal acc_0_127   : signed(63 downto 0);
    signal acc_0_128   : signed(63 downto 0);
    signal acc_0_129   : signed(63 downto 0);
    signal acc_0_130   : signed(63 downto 0);
    signal acc_0_131   : signed(63 downto 0);
    signal acc_0_132   : signed(63 downto 0);
    signal acc_0_133   : signed(63 downto 0);
    signal acc_0_134   : signed(63 downto 0);
    signal acc_0_135   : signed(63 downto 0);
    signal acc_0_136   : signed(63 downto 0);
    signal acc_0_137   : signed(63 downto 0);
    signal acc_0_138   : signed(63 downto 0);
    signal acc_0_139   : signed(63 downto 0);
    signal acc_0_140   : signed(63 downto 0);
    signal acc_0_141   : signed(63 downto 0);
    signal acc_0_142   : signed(63 downto 0);
    signal acc_0_143   : signed(63 downto 0);
    signal acc_0_144   : signed(63 downto 0);
    signal acc_0_145   : signed(63 downto 0);
    signal acc_0_146   : signed(63 downto 0);
    signal acc_0_147   : signed(63 downto 0);
    signal acc_0_148   : signed(63 downto 0);
    signal acc_0_149   : signed(63 downto 0);
    signal acc_0_150   : signed(63 downto 0);
    signal acc_0_151   : signed(63 downto 0);
    signal acc_0_152   : signed(63 downto 0);
    signal acc_0_153   : signed(63 downto 0);
    signal acc_0_154   : signed(63 downto 0);
    signal acc_0_155   : signed(63 downto 0);
    signal acc_1_0_0   : signed(63 downto 0);
    signal acc_1_1_1   : signed(63 downto 0);
    signal acc_1_2_2   : signed(63 downto 0);
    signal acc_1_3_3   : signed(63 downto 0);
    signal acc_1_4_4   : signed(63 downto 0);
    signal acc_1_5_5   : signed(63 downto 0);
    signal acc_1_6_6   : signed(63 downto 0);
    signal acc_1_7_7   : signed(63 downto 0);
    signal acc_1_8_8   : signed(63 downto 0);
    signal acc_1_9_9   : signed(63 downto 0);
    signal acc_1_10_10 : signed(63 downto 0);
    signal acc_1_11_11 : signed(63 downto 0);
    signal acc_1_12_12 : signed(63 downto 0);
    signal acc_1_13_13 : signed(63 downto 0);
    signal acc_1_14_14 : signed(63 downto 0);
    signal acc_1_15_15 : signed(63 downto 0);
    signal acc_1_16_16 : signed(63 downto 0);
    signal acc_1_17_17 : signed(63 downto 0);
    signal acc_1_18_18 : signed(63 downto 0);
    signal acc_1_19_19 : signed(63 downto 0);
    signal acc_1_20_20 : signed(63 downto 0);
    signal acc_1_21_21 : signed(63 downto 0);
    signal acc_1_22_22 : signed(63 downto 0);
    signal acc_1_23_23 : signed(63 downto 0);
    signal acc_1_24_24 : signed(63 downto 0);
    signal acc_1_25_25 : signed(63 downto 0);
    signal acc_1_26_26 : signed(63 downto 0);
    signal acc_1_27_27 : signed(63 downto 0);
    signal acc_1_28_28 : signed(63 downto 0);
    signal acc_1_29_29 : signed(63 downto 0);
    signal acc_1_30_30 : signed(63 downto 0);
    signal acc_1_31_31 : signed(63 downto 0);
    signal acc_1_32_32 : signed(63 downto 0);
    signal acc_1_33_33 : signed(63 downto 0);
    signal acc_1_34_34 : signed(63 downto 0);
    signal acc_1_35_35 : signed(63 downto 0);
    signal acc_1_36_36 : signed(63 downto 0);
    signal acc_1_37_37 : signed(63 downto 0);
    signal acc_1_38_38 : signed(63 downto 0);
    signal acc_1_39_39 : signed(63 downto 0);
    signal acc_1_40_40 : signed(63 downto 0);
    signal acc_1_41_41 : signed(63 downto 0);
    signal acc_1_42_42 : signed(63 downto 0);
    signal acc_1_43_43 : signed(63 downto 0);
    signal acc_1_44_44 : signed(63 downto 0);
    signal acc_1_45_45 : signed(63 downto 0);
    signal acc_1_46_46 : signed(63 downto 0);
    signal acc_1_47_47 : signed(63 downto 0);
    signal acc_1_48_48 : signed(63 downto 0);
    signal acc_1_49_49 : signed(63 downto 0);
    signal acc_2_0_50  : signed(63 downto 0);
    signal acc_2_1_51  : signed(63 downto 0);
    signal acc_2_2_52  : signed(63 downto 0);
    signal acc_2_3_53  : signed(63 downto 0);
    signal acc_2_4_54  : signed(63 downto 0);
    signal acc_2_5_55  : signed(63 downto 0);
    signal acc_2_6_56  : signed(63 downto 0);
    signal acc_2_7_57  : signed(63 downto 0);
    signal acc_2_8_58  : signed(63 downto 0);
    signal acc_2_9_59  : signed(63 downto 0);
    signal acc_2_10_60 : signed(63 downto 0);
    signal acc_2_11_61 : signed(63 downto 0);
    signal acc_2_12_62 : signed(63 downto 0);
    signal acc_2_13_63 : signed(63 downto 0);
    signal acc_2_14_64 : signed(63 downto 0);
    signal acc_2_15_65 : signed(63 downto 0);
    signal acc_2_16_66 : signed(63 downto 0);
    signal acc_2_17_67 : signed(63 downto 0);
    signal acc_2_18_68 : signed(63 downto 0);
    signal acc_2_19_69 : signed(63 downto 0);
    signal acc_2_20_70 : signed(63 downto 0);
    signal acc_2_21_71 : signed(63 downto 0);
    signal acc_2_22_72 : signed(63 downto 0);
    signal acc_2_23_73 : signed(63 downto 0);
    signal acc_3_0_75  : signed(63 downto 0);
    signal acc_3_1_76  : signed(63 downto 0);
    signal acc_3_2_77  : signed(63 downto 0);
    signal acc_3_3_78  : signed(63 downto 0);
    signal acc_3_4_79  : signed(63 downto 0);
    signal acc_3_5_80  : signed(63 downto 0);
    signal acc_3_6_81  : signed(63 downto 0);
    signal acc_3_7_82  : signed(63 downto 0);
    signal acc_3_8_83  : signed(63 downto 0);
    signal acc_3_9_84  : signed(63 downto 0);
    signal acc_3_10_85 : signed(63 downto 0);
    signal acc_3_11_86 : signed(63 downto 0);
    signal acc_4_0_87  : signed(63 downto 0);
    signal acc_4_1_88  : signed(63 downto 0);
    signal acc_4_2_89  : signed(63 downto 0);
    signal acc_4_3_90  : signed(63 downto 0);
    signal acc_4_4_91  : signed(63 downto 0);
    signal acc_4_5_92  : signed(63 downto 0);
    signal acc_5_0_93  : signed(63 downto 0);
    signal acc_5_1_94  : signed(63 downto 0);
    signal acc_5_2_95  : signed(63 downto 0);
    signal acc_2_24_74 : signed(63 downto 0);
    signal acc_6_0_96  : signed(63 downto 0);
    signal acc_6_1_97  : signed(63 downto 0);
  begin
    \c$t_app_arg_0\ <= acc_7_0_98;

    vec_0 <= (\c$vec_19\(1 to \c$vec_19\'high));

    acc_0_56 <= vec_0(0);

    acc_0_57 <= vec_0(1);

    acc_0_58 <= vec_0(2);

    acc_0_59 <= vec_0(3);

    acc_0_60 <= vec_0(4);

    acc_0_61 <= vec_0(5);

    acc_0_62 <= vec_0(6);

    acc_0_63 <= vec_0(7);

    acc_0_64 <= vec_0(8);

    acc_0_65 <= vec_0(9);

    acc_0_66 <= vec_0(10);

    acc_0_67 <= vec_0(11);

    acc_0_68 <= vec_0(12);

    acc_0_69 <= vec_0(13);

    acc_0_70 <= vec_0(14);

    acc_0_71 <= vec_0(15);

    acc_0_72 <= vec_0(16);

    acc_0_73 <= vec_0(17);

    acc_0_74 <= vec_0(18);

    acc_0_75 <= vec_0(19);

    acc_0_76 <= vec_0(20);

    acc_0_77 <= vec_0(21);

    acc_0_78 <= vec_0(22);

    acc_0_79 <= vec_0(23);

    acc_0_80 <= vec_0(24);

    acc_0_81 <= vec_0(25);

    acc_0_82 <= vec_0(26);

    acc_0_83 <= vec_0(27);

    acc_0_84 <= vec_0(28);

    acc_0_85 <= vec_0(29);

    acc_0_86 <= vec_0(30);

    acc_0_87 <= vec_0(31);

    acc_0_88 <= vec_0(32);

    acc_0_89 <= vec_0(33);

    acc_0_90 <= vec_0(34);

    acc_0_91 <= vec_0(35);

    acc_0_92 <= vec_0(36);

    acc_0_93 <= vec_0(37);

    acc_0_94 <= vec_0(38);

    acc_0_95 <= vec_0(39);

    acc_0_96 <= vec_0(40);

    acc_0_97 <= vec_0(41);

    acc_0_98 <= vec_0(42);

    acc_0_99 <= vec_0(43);

    acc_0_100 <= vec_0(44);

    acc_0_101 <= vec_0(45);

    acc_0_102 <= vec_0(46);

    acc_0_103 <= vec_0(47);

    acc_0_104 <= vec_0(48);

    acc_0_105 <= vec_0(49);

    acc_0_106 <= vec_0(50);

    acc_0_107 <= vec_0(51);

    acc_0_108 <= vec_0(52);

    acc_0_109 <= vec_0(53);

    acc_0_110 <= vec_0(54);

    acc_0_111 <= vec_0(55);

    acc_0_112 <= vec_0(56);

    acc_0_113 <= vec_0(57);

    acc_0_114 <= vec_0(58);

    acc_0_115 <= vec_0(59);

    acc_0_116 <= vec_0(60);

    acc_0_117 <= vec_0(61);

    acc_0_118 <= vec_0(62);

    acc_0_119 <= vec_0(63);

    acc_0_120 <= vec_0(64);

    acc_0_121 <= vec_0(65);

    acc_0_122 <= vec_0(66);

    acc_0_123 <= vec_0(67);

    acc_0_124 <= vec_0(68);

    acc_0_125 <= vec_0(69);

    acc_0_126 <= vec_0(70);

    acc_0_127 <= vec_0(71);

    acc_0_128 <= vec_0(72);

    acc_0_129 <= vec_0(73);

    acc_0_130 <= vec_0(74);

    acc_0_131 <= vec_0(75);

    acc_0_132 <= vec_0(76);

    acc_0_133 <= vec_0(77);

    acc_0_134 <= vec_0(78);

    acc_0_135 <= vec_0(79);

    acc_0_136 <= vec_0(80);

    acc_0_137 <= vec_0(81);

    acc_0_138 <= vec_0(82);

    acc_0_139 <= vec_0(83);

    acc_0_140 <= vec_0(84);

    acc_0_141 <= vec_0(85);

    acc_0_142 <= vec_0(86);

    acc_0_143 <= vec_0(87);

    acc_0_144 <= vec_0(88);

    acc_0_145 <= vec_0(89);

    acc_0_146 <= vec_0(90);

    acc_0_147 <= vec_0(91);

    acc_0_148 <= vec_0(92);

    acc_0_149 <= vec_0(93);

    acc_0_150 <= vec_0(94);

    acc_0_151 <= vec_0(95);

    acc_0_152 <= vec_0(96);

    acc_0_153 <= vec_0(97);

    acc_0_154 <= vec_0(98);

    acc_0_155 <= vec_0(99);

    fun_337 : block
        signal x_128            : signed(63 downto 0);
        signal y_110            : signed(63 downto 0);
        signal \c$case_alt_129\ : signed(63 downto 0);
      begin
        acc_1_0_0 <= \c$case_alt_129\;

        x_128 <= acc_0_56;

        y_110 <= acc_0_57;

        \c$case_alt_129\ <= (x_128 + y_110);


      end block;

    fun_338 : block
        signal x_129            : signed(63 downto 0);
        signal y_111            : signed(63 downto 0);
        signal \c$case_alt_130\ : signed(63 downto 0);
      begin
        acc_1_1_1 <= \c$case_alt_130\;

        x_129 <= acc_0_58;

        y_111 <= acc_0_59;

        \c$case_alt_130\ <= (x_129 + y_111);


      end block;

    fun_339 : block
        signal x_130            : signed(63 downto 0);
        signal y_112            : signed(63 downto 0);
        signal \c$case_alt_131\ : signed(63 downto 0);
      begin
        acc_1_2_2 <= \c$case_alt_131\;

        x_130 <= acc_0_60;

        y_112 <= acc_0_61;

        \c$case_alt_131\ <= (x_130 + y_112);


      end block;

    fun_340 : block
        signal x_131            : signed(63 downto 0);
        signal y_113            : signed(63 downto 0);
        signal \c$case_alt_132\ : signed(63 downto 0);
      begin
        acc_1_3_3 <= \c$case_alt_132\;

        x_131 <= acc_0_62;

        y_113 <= acc_0_63;

        \c$case_alt_132\ <= (x_131 + y_113);


      end block;

    fun_341 : block
        signal x_132            : signed(63 downto 0);
        signal y_114            : signed(63 downto 0);
        signal \c$case_alt_133\ : signed(63 downto 0);
      begin
        acc_1_4_4 <= \c$case_alt_133\;

        x_132 <= acc_0_64;

        y_114 <= acc_0_65;

        \c$case_alt_133\ <= (x_132 + y_114);


      end block;

    fun_342 : block
        signal x_133            : signed(63 downto 0);
        signal y_115            : signed(63 downto 0);
        signal \c$case_alt_134\ : signed(63 downto 0);
      begin
        acc_1_5_5 <= \c$case_alt_134\;

        x_133 <= acc_0_66;

        y_115 <= acc_0_67;

        \c$case_alt_134\ <= (x_133 + y_115);


      end block;

    fun_343 : block
        signal x_134            : signed(63 downto 0);
        signal y_116            : signed(63 downto 0);
        signal \c$case_alt_135\ : signed(63 downto 0);
      begin
        acc_1_6_6 <= \c$case_alt_135\;

        x_134 <= acc_0_68;

        y_116 <= acc_0_69;

        \c$case_alt_135\ <= (x_134 + y_116);


      end block;

    fun_344 : block
        signal x_135            : signed(63 downto 0);
        signal y_117            : signed(63 downto 0);
        signal \c$case_alt_136\ : signed(63 downto 0);
      begin
        acc_1_7_7 <= \c$case_alt_136\;

        x_135 <= acc_0_70;

        y_117 <= acc_0_71;

        \c$case_alt_136\ <= (x_135 + y_117);


      end block;

    fun_345 : block
        signal x_136            : signed(63 downto 0);
        signal y_118            : signed(63 downto 0);
        signal \c$case_alt_137\ : signed(63 downto 0);
      begin
        acc_1_8_8 <= \c$case_alt_137\;

        x_136 <= acc_0_72;

        y_118 <= acc_0_73;

        \c$case_alt_137\ <= (x_136 + y_118);


      end block;

    fun_346 : block
        signal x_137            : signed(63 downto 0);
        signal y_119            : signed(63 downto 0);
        signal \c$case_alt_138\ : signed(63 downto 0);
      begin
        acc_1_9_9 <= \c$case_alt_138\;

        x_137 <= acc_0_74;

        y_119 <= acc_0_75;

        \c$case_alt_138\ <= (x_137 + y_119);


      end block;

    fun_347 : block
        signal x_138            : signed(63 downto 0);
        signal y_120            : signed(63 downto 0);
        signal \c$case_alt_139\ : signed(63 downto 0);
      begin
        acc_1_10_10 <= \c$case_alt_139\;

        x_138 <= acc_0_76;

        y_120 <= acc_0_77;

        \c$case_alt_139\ <= (x_138 + y_120);


      end block;

    fun_348 : block
        signal x_139            : signed(63 downto 0);
        signal y_121            : signed(63 downto 0);
        signal \c$case_alt_140\ : signed(63 downto 0);
      begin
        acc_1_11_11 <= \c$case_alt_140\;

        x_139 <= acc_0_78;

        y_121 <= acc_0_79;

        \c$case_alt_140\ <= (x_139 + y_121);


      end block;

    fun_349 : block
        signal x_140            : signed(63 downto 0);
        signal y_122            : signed(63 downto 0);
        signal \c$case_alt_141\ : signed(63 downto 0);
      begin
        acc_1_12_12 <= \c$case_alt_141\;

        x_140 <= acc_0_80;

        y_122 <= acc_0_81;

        \c$case_alt_141\ <= (x_140 + y_122);


      end block;

    fun_350 : block
        signal x_141            : signed(63 downto 0);
        signal y_123            : signed(63 downto 0);
        signal \c$case_alt_142\ : signed(63 downto 0);
      begin
        acc_1_13_13 <= \c$case_alt_142\;

        x_141 <= acc_0_82;

        y_123 <= acc_0_83;

        \c$case_alt_142\ <= (x_141 + y_123);


      end block;

    fun_351 : block
        signal x_142            : signed(63 downto 0);
        signal y_124            : signed(63 downto 0);
        signal \c$case_alt_143\ : signed(63 downto 0);
      begin
        acc_1_14_14 <= \c$case_alt_143\;

        x_142 <= acc_0_84;

        y_124 <= acc_0_85;

        \c$case_alt_143\ <= (x_142 + y_124);


      end block;

    fun_352 : block
        signal x_143            : signed(63 downto 0);
        signal y_125            : signed(63 downto 0);
        signal \c$case_alt_144\ : signed(63 downto 0);
      begin
        acc_1_15_15 <= \c$case_alt_144\;

        x_143 <= acc_0_86;

        y_125 <= acc_0_87;

        \c$case_alt_144\ <= (x_143 + y_125);


      end block;

    fun_353 : block
        signal x_144            : signed(63 downto 0);
        signal y_126            : signed(63 downto 0);
        signal \c$case_alt_145\ : signed(63 downto 0);
      begin
        acc_1_16_16 <= \c$case_alt_145\;

        x_144 <= acc_0_88;

        y_126 <= acc_0_89;

        \c$case_alt_145\ <= (x_144 + y_126);


      end block;

    fun_354 : block
        signal x_145            : signed(63 downto 0);
        signal y_127            : signed(63 downto 0);
        signal \c$case_alt_146\ : signed(63 downto 0);
      begin
        acc_1_17_17 <= \c$case_alt_146\;

        x_145 <= acc_0_90;

        y_127 <= acc_0_91;

        \c$case_alt_146\ <= (x_145 + y_127);


      end block;

    fun_355 : block
        signal x_146            : signed(63 downto 0);
        signal y_128            : signed(63 downto 0);
        signal \c$case_alt_147\ : signed(63 downto 0);
      begin
        acc_1_18_18 <= \c$case_alt_147\;

        x_146 <= acc_0_92;

        y_128 <= acc_0_93;

        \c$case_alt_147\ <= (x_146 + y_128);


      end block;

    fun_356 : block
        signal x_147            : signed(63 downto 0);
        signal y_129            : signed(63 downto 0);
        signal \c$case_alt_148\ : signed(63 downto 0);
      begin
        acc_1_19_19 <= \c$case_alt_148\;

        x_147 <= acc_0_94;

        y_129 <= acc_0_95;

        \c$case_alt_148\ <= (x_147 + y_129);


      end block;

    fun_357 : block
        signal x_148            : signed(63 downto 0);
        signal y_130            : signed(63 downto 0);
        signal \c$case_alt_149\ : signed(63 downto 0);
      begin
        acc_1_20_20 <= \c$case_alt_149\;

        x_148 <= acc_0_96;

        y_130 <= acc_0_97;

        \c$case_alt_149\ <= (x_148 + y_130);


      end block;

    fun_358 : block
        signal x_149            : signed(63 downto 0);
        signal y_131            : signed(63 downto 0);
        signal \c$case_alt_150\ : signed(63 downto 0);
      begin
        acc_1_21_21 <= \c$case_alt_150\;

        x_149 <= acc_0_98;

        y_131 <= acc_0_99;

        \c$case_alt_150\ <= (x_149 + y_131);


      end block;

    fun_359 : block
        signal x_150            : signed(63 downto 0);
        signal y_132            : signed(63 downto 0);
        signal \c$case_alt_151\ : signed(63 downto 0);
      begin
        acc_1_22_22 <= \c$case_alt_151\;

        x_150 <= acc_0_100;

        y_132 <= acc_0_101;

        \c$case_alt_151\ <= (x_150 + y_132);


      end block;

    fun_360 : block
        signal x_151            : signed(63 downto 0);
        signal y_133            : signed(63 downto 0);
        signal \c$case_alt_152\ : signed(63 downto 0);
      begin
        acc_1_23_23 <= \c$case_alt_152\;

        x_151 <= acc_0_102;

        y_133 <= acc_0_103;

        \c$case_alt_152\ <= (x_151 + y_133);


      end block;

    fun_361 : block
        signal x_152            : signed(63 downto 0);
        signal y_134            : signed(63 downto 0);
        signal \c$case_alt_153\ : signed(63 downto 0);
      begin
        acc_1_24_24 <= \c$case_alt_153\;

        x_152 <= acc_0_104;

        y_134 <= acc_0_105;

        \c$case_alt_153\ <= (x_152 + y_134);


      end block;

    fun_362 : block
        signal x_153            : signed(63 downto 0);
        signal y_135            : signed(63 downto 0);
        signal \c$case_alt_154\ : signed(63 downto 0);
      begin
        acc_1_25_25 <= \c$case_alt_154\;

        x_153 <= acc_0_106;

        y_135 <= acc_0_107;

        \c$case_alt_154\ <= (x_153 + y_135);


      end block;

    fun_363 : block
        signal x_154            : signed(63 downto 0);
        signal y_136            : signed(63 downto 0);
        signal \c$case_alt_155\ : signed(63 downto 0);
      begin
        acc_1_26_26 <= \c$case_alt_155\;

        x_154 <= acc_0_108;

        y_136 <= acc_0_109;

        \c$case_alt_155\ <= (x_154 + y_136);


      end block;

    fun_364 : block
        signal x_155            : signed(63 downto 0);
        signal y_137            : signed(63 downto 0);
        signal \c$case_alt_156\ : signed(63 downto 0);
      begin
        acc_1_27_27 <= \c$case_alt_156\;

        x_155 <= acc_0_110;

        y_137 <= acc_0_111;

        \c$case_alt_156\ <= (x_155 + y_137);


      end block;

    fun_365 : block
        signal x_156            : signed(63 downto 0);
        signal y_138            : signed(63 downto 0);
        signal \c$case_alt_157\ : signed(63 downto 0);
      begin
        acc_1_28_28 <= \c$case_alt_157\;

        x_156 <= acc_0_112;

        y_138 <= acc_0_113;

        \c$case_alt_157\ <= (x_156 + y_138);


      end block;

    fun_366 : block
        signal x_157            : signed(63 downto 0);
        signal y_139            : signed(63 downto 0);
        signal \c$case_alt_158\ : signed(63 downto 0);
      begin
        acc_1_29_29 <= \c$case_alt_158\;

        x_157 <= acc_0_114;

        y_139 <= acc_0_115;

        \c$case_alt_158\ <= (x_157 + y_139);


      end block;

    fun_367 : block
        signal x_158            : signed(63 downto 0);
        signal y_140            : signed(63 downto 0);
        signal \c$case_alt_159\ : signed(63 downto 0);
      begin
        acc_1_30_30 <= \c$case_alt_159\;

        x_158 <= acc_0_116;

        y_140 <= acc_0_117;

        \c$case_alt_159\ <= (x_158 + y_140);


      end block;

    fun_368 : block
        signal x_159            : signed(63 downto 0);
        signal y_141            : signed(63 downto 0);
        signal \c$case_alt_160\ : signed(63 downto 0);
      begin
        acc_1_31_31 <= \c$case_alt_160\;

        x_159 <= acc_0_118;

        y_141 <= acc_0_119;

        \c$case_alt_160\ <= (x_159 + y_141);


      end block;

    fun_369 : block
        signal x_160            : signed(63 downto 0);
        signal y_142            : signed(63 downto 0);
        signal \c$case_alt_161\ : signed(63 downto 0);
      begin
        acc_1_32_32 <= \c$case_alt_161\;

        x_160 <= acc_0_120;

        y_142 <= acc_0_121;

        \c$case_alt_161\ <= (x_160 + y_142);


      end block;

    fun_370 : block
        signal x_161            : signed(63 downto 0);
        signal y_143            : signed(63 downto 0);
        signal \c$case_alt_162\ : signed(63 downto 0);
      begin
        acc_1_33_33 <= \c$case_alt_162\;

        x_161 <= acc_0_122;

        y_143 <= acc_0_123;

        \c$case_alt_162\ <= (x_161 + y_143);


      end block;

    fun_371 : block
        signal x_162            : signed(63 downto 0);
        signal y_144            : signed(63 downto 0);
        signal \c$case_alt_163\ : signed(63 downto 0);
      begin
        acc_1_34_34 <= \c$case_alt_163\;

        x_162 <= acc_0_124;

        y_144 <= acc_0_125;

        \c$case_alt_163\ <= (x_162 + y_144);


      end block;

    fun_372 : block
        signal x_163            : signed(63 downto 0);
        signal y_145            : signed(63 downto 0);
        signal \c$case_alt_164\ : signed(63 downto 0);
      begin
        acc_1_35_35 <= \c$case_alt_164\;

        x_163 <= acc_0_126;

        y_145 <= acc_0_127;

        \c$case_alt_164\ <= (x_163 + y_145);


      end block;

    fun_373 : block
        signal x_164            : signed(63 downto 0);
        signal y_146            : signed(63 downto 0);
        signal \c$case_alt_165\ : signed(63 downto 0);
      begin
        acc_1_36_36 <= \c$case_alt_165\;

        x_164 <= acc_0_128;

        y_146 <= acc_0_129;

        \c$case_alt_165\ <= (x_164 + y_146);


      end block;

    fun_374 : block
        signal x_165            : signed(63 downto 0);
        signal y_147            : signed(63 downto 0);
        signal \c$case_alt_166\ : signed(63 downto 0);
      begin
        acc_1_37_37 <= \c$case_alt_166\;

        x_165 <= acc_0_130;

        y_147 <= acc_0_131;

        \c$case_alt_166\ <= (x_165 + y_147);


      end block;

    fun_375 : block
        signal x_166            : signed(63 downto 0);
        signal y_148            : signed(63 downto 0);
        signal \c$case_alt_167\ : signed(63 downto 0);
      begin
        acc_1_38_38 <= \c$case_alt_167\;

        x_166 <= acc_0_132;

        y_148 <= acc_0_133;

        \c$case_alt_167\ <= (x_166 + y_148);


      end block;

    fun_376 : block
        signal x_167            : signed(63 downto 0);
        signal y_149            : signed(63 downto 0);
        signal \c$case_alt_168\ : signed(63 downto 0);
      begin
        acc_1_39_39 <= \c$case_alt_168\;

        x_167 <= acc_0_134;

        y_149 <= acc_0_135;

        \c$case_alt_168\ <= (x_167 + y_149);


      end block;

    fun_377 : block
        signal x_168            : signed(63 downto 0);
        signal y_150            : signed(63 downto 0);
        signal \c$case_alt_169\ : signed(63 downto 0);
      begin
        acc_1_40_40 <= \c$case_alt_169\;

        x_168 <= acc_0_136;

        y_150 <= acc_0_137;

        \c$case_alt_169\ <= (x_168 + y_150);


      end block;

    fun_378 : block
        signal x_169            : signed(63 downto 0);
        signal y_151            : signed(63 downto 0);
        signal \c$case_alt_170\ : signed(63 downto 0);
      begin
        acc_1_41_41 <= \c$case_alt_170\;

        x_169 <= acc_0_138;

        y_151 <= acc_0_139;

        \c$case_alt_170\ <= (x_169 + y_151);


      end block;

    fun_379 : block
        signal x_170            : signed(63 downto 0);
        signal y_152            : signed(63 downto 0);
        signal \c$case_alt_171\ : signed(63 downto 0);
      begin
        acc_1_42_42 <= \c$case_alt_171\;

        x_170 <= acc_0_140;

        y_152 <= acc_0_141;

        \c$case_alt_171\ <= (x_170 + y_152);


      end block;

    fun_380 : block
        signal x_171            : signed(63 downto 0);
        signal y_153            : signed(63 downto 0);
        signal \c$case_alt_172\ : signed(63 downto 0);
      begin
        acc_1_43_43 <= \c$case_alt_172\;

        x_171 <= acc_0_142;

        y_153 <= acc_0_143;

        \c$case_alt_172\ <= (x_171 + y_153);


      end block;

    fun_381 : block
        signal x_172            : signed(63 downto 0);
        signal y_154            : signed(63 downto 0);
        signal \c$case_alt_173\ : signed(63 downto 0);
      begin
        acc_1_44_44 <= \c$case_alt_173\;

        x_172 <= acc_0_144;

        y_154 <= acc_0_145;

        \c$case_alt_173\ <= (x_172 + y_154);


      end block;

    fun_382 : block
        signal x_173            : signed(63 downto 0);
        signal y_155            : signed(63 downto 0);
        signal \c$case_alt_174\ : signed(63 downto 0);
      begin
        acc_1_45_45 <= \c$case_alt_174\;

        x_173 <= acc_0_146;

        y_155 <= acc_0_147;

        \c$case_alt_174\ <= (x_173 + y_155);


      end block;

    fun_383 : block
        signal x_174            : signed(63 downto 0);
        signal y_156            : signed(63 downto 0);
        signal \c$case_alt_175\ : signed(63 downto 0);
      begin
        acc_1_46_46 <= \c$case_alt_175\;

        x_174 <= acc_0_148;

        y_156 <= acc_0_149;

        \c$case_alt_175\ <= (x_174 + y_156);


      end block;

    fun_384 : block
        signal x_175            : signed(63 downto 0);
        signal y_157            : signed(63 downto 0);
        signal \c$case_alt_176\ : signed(63 downto 0);
      begin
        acc_1_47_47 <= \c$case_alt_176\;

        x_175 <= acc_0_150;

        y_157 <= acc_0_151;

        \c$case_alt_176\ <= (x_175 + y_157);


      end block;

    fun_385 : block
        signal x_176            : signed(63 downto 0);
        signal y_158            : signed(63 downto 0);
        signal \c$case_alt_177\ : signed(63 downto 0);
      begin
        acc_1_48_48 <= \c$case_alt_177\;

        x_176 <= acc_0_152;

        y_158 <= acc_0_153;

        \c$case_alt_177\ <= (x_176 + y_158);


      end block;

    fun_386 : block
        signal x_177            : signed(63 downto 0);
        signal y_159            : signed(63 downto 0);
        signal \c$case_alt_178\ : signed(63 downto 0);
      begin
        acc_1_49_49 <= \c$case_alt_178\;

        x_177 <= acc_0_154;

        y_159 <= acc_0_155;

        \c$case_alt_178\ <= (x_177 + y_159);


      end block;

    fun_387 : block
        signal x_178            : signed(63 downto 0);
        signal y_160            : signed(63 downto 0);
        signal \c$case_alt_179\ : signed(63 downto 0);
      begin
        acc_2_0_50 <= \c$case_alt_179\;

        x_178 <= acc_1_0_0;

        y_160 <= acc_1_1_1;

        \c$case_alt_179\ <= (x_178 + y_160);


      end block;

    fun_388 : block
        signal x_179            : signed(63 downto 0);
        signal y_161            : signed(63 downto 0);
        signal \c$case_alt_180\ : signed(63 downto 0);
      begin
        acc_2_1_51 <= \c$case_alt_180\;

        x_179 <= acc_1_2_2;

        y_161 <= acc_1_3_3;

        \c$case_alt_180\ <= (x_179 + y_161);


      end block;

    fun_389 : block
        signal x_180            : signed(63 downto 0);
        signal y_162            : signed(63 downto 0);
        signal \c$case_alt_181\ : signed(63 downto 0);
      begin
        acc_2_2_52 <= \c$case_alt_181\;

        x_180 <= acc_1_4_4;

        y_162 <= acc_1_5_5;

        \c$case_alt_181\ <= (x_180 + y_162);


      end block;

    fun_390 : block
        signal x_181            : signed(63 downto 0);
        signal y_163            : signed(63 downto 0);
        signal \c$case_alt_182\ : signed(63 downto 0);
      begin
        acc_2_3_53 <= \c$case_alt_182\;

        x_181 <= acc_1_6_6;

        y_163 <= acc_1_7_7;

        \c$case_alt_182\ <= (x_181 + y_163);


      end block;

    fun_391 : block
        signal x_182            : signed(63 downto 0);
        signal y_164            : signed(63 downto 0);
        signal \c$case_alt_183\ : signed(63 downto 0);
      begin
        acc_2_4_54 <= \c$case_alt_183\;

        x_182 <= acc_1_8_8;

        y_164 <= acc_1_9_9;

        \c$case_alt_183\ <= (x_182 + y_164);


      end block;

    fun_392 : block
        signal x_183            : signed(63 downto 0);
        signal y_165            : signed(63 downto 0);
        signal \c$case_alt_184\ : signed(63 downto 0);
      begin
        acc_2_5_55 <= \c$case_alt_184\;

        x_183 <= acc_1_10_10;

        y_165 <= acc_1_11_11;

        \c$case_alt_184\ <= (x_183 + y_165);


      end block;

    fun_393 : block
        signal x_184            : signed(63 downto 0);
        signal y_166            : signed(63 downto 0);
        signal \c$case_alt_185\ : signed(63 downto 0);
      begin
        acc_2_6_56 <= \c$case_alt_185\;

        x_184 <= acc_1_12_12;

        y_166 <= acc_1_13_13;

        \c$case_alt_185\ <= (x_184 + y_166);


      end block;

    fun_394 : block
        signal x_185            : signed(63 downto 0);
        signal y_167            : signed(63 downto 0);
        signal \c$case_alt_186\ : signed(63 downto 0);
      begin
        acc_2_7_57 <= \c$case_alt_186\;

        x_185 <= acc_1_14_14;

        y_167 <= acc_1_15_15;

        \c$case_alt_186\ <= (x_185 + y_167);


      end block;

    fun_395 : block
        signal x_186            : signed(63 downto 0);
        signal y_168            : signed(63 downto 0);
        signal \c$case_alt_187\ : signed(63 downto 0);
      begin
        acc_2_8_58 <= \c$case_alt_187\;

        x_186 <= acc_1_16_16;

        y_168 <= acc_1_17_17;

        \c$case_alt_187\ <= (x_186 + y_168);


      end block;

    fun_396 : block
        signal x_187            : signed(63 downto 0);
        signal y_169            : signed(63 downto 0);
        signal \c$case_alt_188\ : signed(63 downto 0);
      begin
        acc_2_9_59 <= \c$case_alt_188\;

        x_187 <= acc_1_18_18;

        y_169 <= acc_1_19_19;

        \c$case_alt_188\ <= (x_187 + y_169);


      end block;

    fun_397 : block
        signal x_188            : signed(63 downto 0);
        signal y_170            : signed(63 downto 0);
        signal \c$case_alt_189\ : signed(63 downto 0);
      begin
        acc_2_10_60 <= \c$case_alt_189\;

        x_188 <= acc_1_20_20;

        y_170 <= acc_1_21_21;

        \c$case_alt_189\ <= (x_188 + y_170);


      end block;

    fun_398 : block
        signal x_189            : signed(63 downto 0);
        signal y_171            : signed(63 downto 0);
        signal \c$case_alt_190\ : signed(63 downto 0);
      begin
        acc_2_11_61 <= \c$case_alt_190\;

        x_189 <= acc_1_22_22;

        y_171 <= acc_1_23_23;

        \c$case_alt_190\ <= (x_189 + y_171);


      end block;

    fun_399 : block
        signal x_190            : signed(63 downto 0);
        signal y_172            : signed(63 downto 0);
        signal \c$case_alt_191\ : signed(63 downto 0);
      begin
        acc_2_12_62 <= \c$case_alt_191\;

        x_190 <= acc_1_24_24;

        y_172 <= acc_1_25_25;

        \c$case_alt_191\ <= (x_190 + y_172);


      end block;

    fun_400 : block
        signal x_191            : signed(63 downto 0);
        signal y_173            : signed(63 downto 0);
        signal \c$case_alt_192\ : signed(63 downto 0);
      begin
        acc_2_13_63 <= \c$case_alt_192\;

        x_191 <= acc_1_26_26;

        y_173 <= acc_1_27_27;

        \c$case_alt_192\ <= (x_191 + y_173);


      end block;

    fun_401 : block
        signal x_192            : signed(63 downto 0);
        signal y_174            : signed(63 downto 0);
        signal \c$case_alt_193\ : signed(63 downto 0);
      begin
        acc_2_14_64 <= \c$case_alt_193\;

        x_192 <= acc_1_28_28;

        y_174 <= acc_1_29_29;

        \c$case_alt_193\ <= (x_192 + y_174);


      end block;

    fun_402 : block
        signal x_193            : signed(63 downto 0);
        signal y_175            : signed(63 downto 0);
        signal \c$case_alt_194\ : signed(63 downto 0);
      begin
        acc_2_15_65 <= \c$case_alt_194\;

        x_193 <= acc_1_30_30;

        y_175 <= acc_1_31_31;

        \c$case_alt_194\ <= (x_193 + y_175);


      end block;

    fun_403 : block
        signal x_194            : signed(63 downto 0);
        signal y_176            : signed(63 downto 0);
        signal \c$case_alt_195\ : signed(63 downto 0);
      begin
        acc_2_16_66 <= \c$case_alt_195\;

        x_194 <= acc_1_32_32;

        y_176 <= acc_1_33_33;

        \c$case_alt_195\ <= (x_194 + y_176);


      end block;

    fun_404 : block
        signal x_195            : signed(63 downto 0);
        signal y_177            : signed(63 downto 0);
        signal \c$case_alt_196\ : signed(63 downto 0);
      begin
        acc_2_17_67 <= \c$case_alt_196\;

        x_195 <= acc_1_34_34;

        y_177 <= acc_1_35_35;

        \c$case_alt_196\ <= (x_195 + y_177);


      end block;

    fun_405 : block
        signal x_196            : signed(63 downto 0);
        signal y_178            : signed(63 downto 0);
        signal \c$case_alt_197\ : signed(63 downto 0);
      begin
        acc_2_18_68 <= \c$case_alt_197\;

        x_196 <= acc_1_36_36;

        y_178 <= acc_1_37_37;

        \c$case_alt_197\ <= (x_196 + y_178);


      end block;

    fun_406 : block
        signal x_197            : signed(63 downto 0);
        signal y_179            : signed(63 downto 0);
        signal \c$case_alt_198\ : signed(63 downto 0);
      begin
        acc_2_19_69 <= \c$case_alt_198\;

        x_197 <= acc_1_38_38;

        y_179 <= acc_1_39_39;

        \c$case_alt_198\ <= (x_197 + y_179);


      end block;

    fun_407 : block
        signal x_198            : signed(63 downto 0);
        signal y_180            : signed(63 downto 0);
        signal \c$case_alt_199\ : signed(63 downto 0);
      begin
        acc_2_20_70 <= \c$case_alt_199\;

        x_198 <= acc_1_40_40;

        y_180 <= acc_1_41_41;

        \c$case_alt_199\ <= (x_198 + y_180);


      end block;

    fun_408 : block
        signal x_199            : signed(63 downto 0);
        signal y_181            : signed(63 downto 0);
        signal \c$case_alt_200\ : signed(63 downto 0);
      begin
        acc_2_21_71 <= \c$case_alt_200\;

        x_199 <= acc_1_42_42;

        y_181 <= acc_1_43_43;

        \c$case_alt_200\ <= (x_199 + y_181);


      end block;

    fun_409 : block
        signal x_200            : signed(63 downto 0);
        signal y_182            : signed(63 downto 0);
        signal \c$case_alt_201\ : signed(63 downto 0);
      begin
        acc_2_22_72 <= \c$case_alt_201\;

        x_200 <= acc_1_44_44;

        y_182 <= acc_1_45_45;

        \c$case_alt_201\ <= (x_200 + y_182);


      end block;

    fun_410 : block
        signal x_201            : signed(63 downto 0);
        signal y_183            : signed(63 downto 0);
        signal \c$case_alt_202\ : signed(63 downto 0);
      begin
        acc_2_23_73 <= \c$case_alt_202\;

        x_201 <= acc_1_46_46;

        y_183 <= acc_1_47_47;

        \c$case_alt_202\ <= (x_201 + y_183);


      end block;

    fun_411 : block
        signal x_202            : signed(63 downto 0);
        signal y_184            : signed(63 downto 0);
        signal \c$case_alt_203\ : signed(63 downto 0);
      begin
        acc_2_24_74 <= \c$case_alt_203\;

        x_202 <= acc_1_48_48;

        y_184 <= acc_1_49_49;

        \c$case_alt_203\ <= (x_202 + y_184);


      end block;

    fun_412 : block
        signal x_203            : signed(63 downto 0);
        signal y_185            : signed(63 downto 0);
        signal \c$case_alt_204\ : signed(63 downto 0);
      begin
        acc_3_0_75 <= \c$case_alt_204\;

        x_203 <= acc_2_0_50;

        y_185 <= acc_2_1_51;

        \c$case_alt_204\ <= (x_203 + y_185);


      end block;

    fun_413 : block
        signal x_204            : signed(63 downto 0);
        signal y_186            : signed(63 downto 0);
        signal \c$case_alt_205\ : signed(63 downto 0);
      begin
        acc_3_1_76 <= \c$case_alt_205\;

        x_204 <= acc_2_2_52;

        y_186 <= acc_2_3_53;

        \c$case_alt_205\ <= (x_204 + y_186);


      end block;

    fun_414 : block
        signal x_205            : signed(63 downto 0);
        signal y_187            : signed(63 downto 0);
        signal \c$case_alt_206\ : signed(63 downto 0);
      begin
        acc_3_2_77 <= \c$case_alt_206\;

        x_205 <= acc_2_4_54;

        y_187 <= acc_2_5_55;

        \c$case_alt_206\ <= (x_205 + y_187);


      end block;

    fun_415 : block
        signal x_206            : signed(63 downto 0);
        signal y_188            : signed(63 downto 0);
        signal \c$case_alt_207\ : signed(63 downto 0);
      begin
        acc_3_3_78 <= \c$case_alt_207\;

        x_206 <= acc_2_6_56;

        y_188 <= acc_2_7_57;

        \c$case_alt_207\ <= (x_206 + y_188);


      end block;

    fun_416 : block
        signal x_207            : signed(63 downto 0);
        signal y_189            : signed(63 downto 0);
        signal \c$case_alt_208\ : signed(63 downto 0);
      begin
        acc_3_4_79 <= \c$case_alt_208\;

        x_207 <= acc_2_8_58;

        y_189 <= acc_2_9_59;

        \c$case_alt_208\ <= (x_207 + y_189);


      end block;

    fun_417 : block
        signal x_208            : signed(63 downto 0);
        signal y_190            : signed(63 downto 0);
        signal \c$case_alt_209\ : signed(63 downto 0);
      begin
        acc_3_5_80 <= \c$case_alt_209\;

        x_208 <= acc_2_10_60;

        y_190 <= acc_2_11_61;

        \c$case_alt_209\ <= (x_208 + y_190);


      end block;

    fun_418 : block
        signal x_209            : signed(63 downto 0);
        signal y_191            : signed(63 downto 0);
        signal \c$case_alt_210\ : signed(63 downto 0);
      begin
        acc_3_6_81 <= \c$case_alt_210\;

        x_209 <= acc_2_12_62;

        y_191 <= acc_2_13_63;

        \c$case_alt_210\ <= (x_209 + y_191);


      end block;

    fun_419 : block
        signal x_210            : signed(63 downto 0);
        signal y_192            : signed(63 downto 0);
        signal \c$case_alt_211\ : signed(63 downto 0);
      begin
        acc_3_7_82 <= \c$case_alt_211\;

        x_210 <= acc_2_14_64;

        y_192 <= acc_2_15_65;

        \c$case_alt_211\ <= (x_210 + y_192);


      end block;

    fun_420 : block
        signal x_211            : signed(63 downto 0);
        signal y_193            : signed(63 downto 0);
        signal \c$case_alt_212\ : signed(63 downto 0);
      begin
        acc_3_8_83 <= \c$case_alt_212\;

        x_211 <= acc_2_16_66;

        y_193 <= acc_2_17_67;

        \c$case_alt_212\ <= (x_211 + y_193);


      end block;

    fun_421 : block
        signal x_212            : signed(63 downto 0);
        signal y_194            : signed(63 downto 0);
        signal \c$case_alt_213\ : signed(63 downto 0);
      begin
        acc_3_9_84 <= \c$case_alt_213\;

        x_212 <= acc_2_18_68;

        y_194 <= acc_2_19_69;

        \c$case_alt_213\ <= (x_212 + y_194);


      end block;

    fun_422 : block
        signal x_213            : signed(63 downto 0);
        signal y_195            : signed(63 downto 0);
        signal \c$case_alt_214\ : signed(63 downto 0);
      begin
        acc_3_10_85 <= \c$case_alt_214\;

        x_213 <= acc_2_20_70;

        y_195 <= acc_2_21_71;

        \c$case_alt_214\ <= (x_213 + y_195);


      end block;

    fun_423 : block
        signal x_214            : signed(63 downto 0);
        signal y_196            : signed(63 downto 0);
        signal \c$case_alt_215\ : signed(63 downto 0);
      begin
        acc_3_11_86 <= \c$case_alt_215\;

        x_214 <= acc_2_22_72;

        y_196 <= acc_2_23_73;

        \c$case_alt_215\ <= (x_214 + y_196);


      end block;

    fun_424 : block
        signal x_215            : signed(63 downto 0);
        signal y_197            : signed(63 downto 0);
        signal \c$case_alt_216\ : signed(63 downto 0);
      begin
        acc_4_0_87 <= \c$case_alt_216\;

        x_215 <= acc_3_0_75;

        y_197 <= acc_3_1_76;

        \c$case_alt_216\ <= (x_215 + y_197);


      end block;

    fun_425 : block
        signal x_216            : signed(63 downto 0);
        signal y_198            : signed(63 downto 0);
        signal \c$case_alt_217\ : signed(63 downto 0);
      begin
        acc_4_1_88 <= \c$case_alt_217\;

        x_216 <= acc_3_2_77;

        y_198 <= acc_3_3_78;

        \c$case_alt_217\ <= (x_216 + y_198);


      end block;

    fun_426 : block
        signal x_217            : signed(63 downto 0);
        signal y_199            : signed(63 downto 0);
        signal \c$case_alt_218\ : signed(63 downto 0);
      begin
        acc_4_2_89 <= \c$case_alt_218\;

        x_217 <= acc_3_4_79;

        y_199 <= acc_3_5_80;

        \c$case_alt_218\ <= (x_217 + y_199);


      end block;

    fun_427 : block
        signal x_218            : signed(63 downto 0);
        signal y_200            : signed(63 downto 0);
        signal \c$case_alt_219\ : signed(63 downto 0);
      begin
        acc_4_3_90 <= \c$case_alt_219\;

        x_218 <= acc_3_6_81;

        y_200 <= acc_3_7_82;

        \c$case_alt_219\ <= (x_218 + y_200);


      end block;

    fun_428 : block
        signal x_219            : signed(63 downto 0);
        signal y_201            : signed(63 downto 0);
        signal \c$case_alt_220\ : signed(63 downto 0);
      begin
        acc_4_4_91 <= \c$case_alt_220\;

        x_219 <= acc_3_8_83;

        y_201 <= acc_3_9_84;

        \c$case_alt_220\ <= (x_219 + y_201);


      end block;

    fun_429 : block
        signal x_220            : signed(63 downto 0);
        signal y_202            : signed(63 downto 0);
        signal \c$case_alt_221\ : signed(63 downto 0);
      begin
        acc_4_5_92 <= \c$case_alt_221\;

        x_220 <= acc_3_10_85;

        y_202 <= acc_3_11_86;

        \c$case_alt_221\ <= (x_220 + y_202);


      end block;

    fun_430 : block
        signal x_221            : signed(63 downto 0);
        signal y_203            : signed(63 downto 0);
        signal \c$case_alt_222\ : signed(63 downto 0);
      begin
        acc_5_0_93 <= \c$case_alt_222\;

        x_221 <= acc_4_0_87;

        y_203 <= acc_4_1_88;

        \c$case_alt_222\ <= (x_221 + y_203);


      end block;

    fun_431 : block
        signal x_222            : signed(63 downto 0);
        signal y_204            : signed(63 downto 0);
        signal \c$case_alt_223\ : signed(63 downto 0);
      begin
        acc_5_1_94 <= \c$case_alt_223\;

        x_222 <= acc_4_2_89;

        y_204 <= acc_4_3_90;

        \c$case_alt_223\ <= (x_222 + y_204);


      end block;

    fun_432 : block
        signal x_223            : signed(63 downto 0);
        signal y_205            : signed(63 downto 0);
        signal \c$case_alt_224\ : signed(63 downto 0);
      begin
        acc_5_2_95 <= \c$case_alt_224\;

        x_223 <= acc_4_4_91;

        y_205 <= acc_4_5_92;

        \c$case_alt_224\ <= (x_223 + y_205);


      end block;

    fun_433 : block
        signal x_224            : signed(63 downto 0);
        signal y_206            : signed(63 downto 0);
        signal \c$case_alt_225\ : signed(63 downto 0);
      begin
        acc_6_0_96 <= \c$case_alt_225\;

        x_224 <= acc_5_0_93;

        y_206 <= acc_5_1_94;

        \c$case_alt_225\ <= (x_224 + y_206);


      end block;

    fun_434 : block
        signal x_225            : signed(63 downto 0);
        signal y_207            : signed(63 downto 0);
        signal \c$case_alt_226\ : signed(63 downto 0);
      begin
        acc_6_1_97 <= \c$case_alt_226\;

        x_225 <= acc_5_2_95;

        y_207 <= acc_2_24_74;

        \c$case_alt_226\ <= (x_225 + y_207);


      end block;

    fun_435 : block
        signal x_226            : signed(63 downto 0);
        signal y_208            : signed(63 downto 0);
        signal \c$case_alt_227\ : signed(63 downto 0);
      begin
        acc_7_0_98 <= \c$case_alt_227\;

        x_226 <= acc_6_0_96;

        y_208 <= acc_6_1_97;

        \c$case_alt_227\ <= (x_226 + y_208);


      end block;


  end block;

  result_78 <= t_13 when b_13 else
               result_79;

  -- register begin
  result_79_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_79 <= Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_79 <= result_78;
      end if;
    end if;
  end process;
  -- register end

  b_13 <= result_85;

  \c$vec_20\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(result_79) & Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => ( Tuple2_2_sel0_unsigned => result_113.Tags_sel7_output4_0
              , Tuple2_2_sel1_signed => \c$t_app_arg_0\ )))));

  t_projection_8 <= (\c$vec_20\(0 to 1-1),\c$vec_20\(1 to \c$vec_20\'high));

  t_13 <= t_projection_8.Tuple2_15_sel1_array_of_Tuple2_2_1;

  x_projection_222 <=  win_5(0) ;

  x_7 <= x_projection_222;

  win_5 <= window_5.Tuple2_10_sel1_array_of_signed_64;

  -- register begin
  window_5_register : process(clk,rst)
  begin
    if rst =  '1'  then
      window_5 <= ( Tuple2_10_sel0_unsigned => to_unsigned(113,8)
  , Tuple2_10_sel1_array_of_signed_64 => Spec_topEntity_types.array_of_signed_64'( to_signed(0,64)
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
        window_5 <= result_80;
      end if;
    end if;
  end process;
  -- register end

  result_80 <= t_14 when b_14 else
               window_5;

  t_14 <= ( Tuple2_10_sel0_unsigned => result_114.Tags_sel14_slide0_0
          , Tuple2_10_sel1_array_of_signed_64 => result_81 );

  result_81 <= \c$t_case_alt_11\ when result_83 else
               \c$t_case_alt_12\;

  \c$t_case_alt_selection_37\ <= result_84;

  \c$vec_21\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(win_5)));

  \c$t_case_alt_sel_alt_36\ <= (\c$vec_21\(0 to 101-1),\c$vec_21\(101 to \c$vec_21\'high));

  \c$vec_22\ <= (Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(Spec_topEntity_types.array_of_signed_64'(0 => to_signed(0,64))) & Spec_topEntity_types.array_of_signed_64'(lastBucketUpdated_5)));

  \c$t_case_alt_sel_alt_38\ <= (\c$vec_22\(0 to 101-1),\c$vec_22\(101 to \c$vec_22\'high));

  \c$t_case_alt_11\ <= \c$t_case_alt_sel_alt_38\.Tuple2_16_sel0_array_of_signed_64_0 when \c$t_case_alt_selection_37\ else
                       \c$t_case_alt_sel_alt_36\.Tuple2_16_sel0_array_of_signed_64_0;

  \c$t_case_alt_selection_40\ <= result_84;

  \c$t_case_alt_12\ <= lastBucketUpdated_5 when \c$t_case_alt_selection_40\ else
                       win_5;

  -- replace begin
  replaceVec_5 : block
    signal vec_index_12 : integer range 0 to 101-1;
  begin
    vec_index_12 <= to_integer((to_signed(0,64)))
    -- pragma translate_off
                 mod 101
    -- pragma translate_on
                 ;

    process(vec_index_12,win_5,x_7)
      variable ivec_5 : Spec_topEntity_types.array_of_signed_64(0 to 100);
    begin
      ivec_5 := win_5;
      ivec_5(vec_index_12) := ((x_7 + to_signed(1,64)));
      lastBucketUpdated_5 <= ivec_5;
    end process;
  end block;
  -- replace end

  b_14 <= result_84 or result_83;

  \c$vec2_6\ <= (ws_6(1 to ws_6'high));

  -- zipWith begin
  zipWith_6 : for i_28 in \c$ws_app_arg_13\'range generate
  begin
    \c$ws_app_arg_13\(i_28) <= ( Tuple2_7_sel0_Tuple2_1 => result_87(i_28)
               , Tuple2_7_sel1_Maybe_1 => \c$vec2_6\(i_28) );


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
    imap_14 : for i_29 in \c$ws_app_arg_14\'range generate
    begin
      fun_436 : block
      signal \c$case_alt_228\             : Spec_topEntity_types.Maybe_1;
      -- spec.hs:455:1-63
      signal t_41                         : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_227                        : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_8\ : boolean;
    begin
      \c$ws_app_arg_14\(i_29) <= \c$case_alt_228\;

      \c$case_alt_selection_res_8\ <= t_41 = result_114.Tags_sel6_output3_0;

      \c$case_alt_228\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_29,max_6(1,integer(ceil(log2(real(4))))))))) when \c$case_alt_selection_res_8\ else
                          \c$ws_app_arg_13\(i_29).Tuple2_7_sel1_Maybe_1;

      t_41 <= x_227.Tuple2_1_sel0_unsigned;

      x_227 <= \c$ws_app_arg_13\(i_29).Tuple2_7_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_6 : block
    signal vec_index_13 : integer range 0 to 4-1;
  begin
    vec_index_13 <= to_integer((signed(std_logic_vector(resize(i_6,64)))))
    -- pragma translate_off
                 mod 4
    -- pragma translate_on
                 ;
    \c$case_scrut_13\ <= result_87(vec_index_13);
  end block;
  -- index end

  \c$case_scrut_14\ <=  ws_6(0) ;

  with (\c$case_scrut_14\(2 downto 2)) select
    result_82 <= false when "0",
                 \c$case_scrut_13\.Tuple2_1_sel1_boolean when others;

  i_6 <= Spec_topEntity_types.index_4'(spec_topentity_types.fromSLV(\c$case_scrut_14\(1 downto 0)));

  ws_6 <= Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(\c$ws_app_arg_14\) & Spec_topEntity_types.array_of_Maybe_1'(Spec_topEntity_types.array_of_Maybe_1'(0 => std_logic_vector'("0" & "--"))));

  iterateI_25 : block
    signal iterateI_ho1_0_arg0_16 : boolean;
    signal iterateI_ho1_0_res_25  : boolean;
    signal iterateI_ho1_1_res_23  : boolean;
    signal iterateI_ho1_2_res_23  : boolean;
    signal iterateI_ho1_3_res_23  : boolean;
    signal iterateI_ho1_4_res_23  : boolean;
  begin
    iterateI_ho1_0_arg0_16 <= slides.Slides_1_sel0_slide0;

    r_block_404 : block
        signal \c$bb_res_res_173\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_173_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_173\ <= iterateI_ho1_0_arg0_16;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_25 <= \c$bb_res_res_173\;


      end block;

    r_block_405 : block
        signal \c$bb_res_res_174\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_174_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_174\ <= iterateI_ho1_0_res_25;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_23 <= \c$bb_res_res_174\;


      end block;

    r_block_406 : block
        signal \c$bb_res_res_175\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_175_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_175\ <= iterateI_ho1_1_res_23;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_23 <= \c$bb_res_res_175\;


      end block;

    r_block_407 : block
        signal \c$bb_res_res_176\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_176_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_176\ <= iterateI_ho1_2_res_23;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_23 <= \c$bb_res_res_176\;


      end block;

    r_block_408 : block
        signal \c$bb_res_res_177\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_177_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_177\ <= iterateI_ho1_3_res_23;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_23 <= \c$bb_res_res_177\;


      end block;

    \c$app_arg_25\ <= Spec_topEntity_types.array_of_boolean'( slides.Slides_1_sel0_slide0
                                                            , iterateI_ho1_0_res_25
                                                            , iterateI_ho1_1_res_23
                                                            , iterateI_ho1_2_res_23
                                                            , iterateI_ho1_3_res_23
                                                            , iterateI_ho1_4_res_23 );


  end block;

  result_83 <=  \c$app_arg_25\(\c$app_arg_25\'high) ;

  slides <= result_145.Tuple3_sel2_Tuple3_0.Tuple3_0_sel1_Slides_1;

  iterateI_26 : block
    signal iterateI_ho1_0_res_26 : boolean;
    signal iterateI_ho1_1_res_24 : boolean;
    signal iterateI_ho1_2_res_24 : boolean;
    signal iterateI_ho1_3_res_24 : boolean;
    signal iterateI_ho1_4_res_24 : boolean;
  begin
    r_block_409 : block
        signal \c$bb_res_res_178\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_178_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_178\ <= pOut3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_26 <= \c$bb_res_res_178\;


      end block;

    r_block_410 : block
        signal \c$bb_res_res_179\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_179_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_179\ <= iterateI_ho1_0_res_26;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_24 <= \c$bb_res_res_179\;


      end block;

    r_block_411 : block
        signal \c$bb_res_res_180\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_180_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_180\ <= iterateI_ho1_1_res_24;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_24 <= \c$bb_res_res_180\;


      end block;

    r_block_412 : block
        signal \c$bb_res_res_181\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_181_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_181\ <= iterateI_ho1_2_res_24;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_24 <= \c$bb_res_res_181\;


      end block;

    r_block_413 : block
        signal \c$bb_res_res_182\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_182_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_182\ <= iterateI_ho1_3_res_24;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_24 <= \c$bb_res_res_182\;


      end block;

    \c$app_arg_26\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                            , iterateI_ho1_0_res_26
                                                            , iterateI_ho1_1_res_24
                                                            , iterateI_ho1_2_res_24
                                                            , iterateI_ho1_3_res_24
                                                            , iterateI_ho1_4_res_24 );


  end block;

  result_84 <=  \c$app_arg_26\(\c$app_arg_26\'high) ;

  iterateI_27 : block
    signal iterateI_ho1_0_res_27 : boolean;
    signal iterateI_ho1_1_res_25 : boolean;
    signal iterateI_ho1_2_res_25 : boolean;
    signal iterateI_ho1_3_res_25 : boolean;
    signal iterateI_ho1_4_res_25 : boolean;
    signal iterateI_ho1_5_res_23 : boolean;
  begin
    r_block_414 : block
        signal \c$bb_res_res_183\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_183_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_183\ <= pOut4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_27 <= \c$bb_res_res_183\;


      end block;

    r_block_415 : block
        signal \c$bb_res_res_184\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_184_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_184\ <= iterateI_ho1_0_res_27;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_25 <= \c$bb_res_res_184\;


      end block;

    r_block_416 : block
        signal \c$bb_res_res_185\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_185_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_185\ <= iterateI_ho1_1_res_25;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_25 <= \c$bb_res_res_185\;


      end block;

    r_block_417 : block
        signal \c$bb_res_res_186\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_186_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_186\ <= iterateI_ho1_2_res_25;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_25 <= \c$bb_res_res_186\;


      end block;

    r_block_418 : block
        signal \c$bb_res_res_187\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_187_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_187\ <= iterateI_ho1_3_res_25;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_25 <= \c$bb_res_res_187\;


      end block;

    r_block_419 : block
        signal \c$bb_res_res_188\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_188_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_188\ <= iterateI_ho1_4_res_25;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_23 <= \c$bb_res_res_188\;


      end block;

    \c$app_arg_27\ <= Spec_topEntity_types.array_of_boolean'( pOut4
                                                            , iterateI_ho1_0_res_27
                                                            , iterateI_ho1_1_res_25
                                                            , iterateI_ho1_2_res_25
                                                            , iterateI_ho1_3_res_25
                                                            , iterateI_ho1_4_res_25
                                                            , iterateI_ho1_5_res_23 );


  end block;

  result_85 <=  \c$app_arg_27\(\c$app_arg_27\'high) ;

  result_86 <= t_15 when b_15 else
               result_87;

  -- register begin
  result_87_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_87 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false ) );
    elsif rising_edge(clk) then
      if en then
        result_87 <= result_86;
      end if;
    end if;
  end process;
  -- register end

  b_15 <= result_94;

  \c$vec_23\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_87) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => result_115.Tags_sel6_output3_0
              , Tuple2_1_sel1_boolean => (result_93 and result_91) or (result_90 and result_88) )))));

  t_projection_10 <= (\c$vec_23\(0 to 1-1),\c$vec_23\(1 to \c$vec_23\'high));

  t_15 <= t_projection_10.Tuple2_17_sel1_array_of_Tuple2_1_1;

  \c$vec2_7\ <= (ws_7(1 to ws_7'high));

  -- zipWith begin
  zipWith_7 : for i_30 in \c$ws_app_arg_15\'range generate
  begin
    \c$ws_app_arg_15\(i_30) <= ( Tuple2_8_sel0_Tuple2_1 => result_99(i_30)
               , Tuple2_8_sel1_Maybe => \c$vec2_7\(i_30) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_15 : block
    function max_7 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_16 : for i_31 in \c$ws_app_arg_16\'range generate
    begin
      fun_437 : block
      signal \c$case_alt_229\             : Spec_topEntity_types.Maybe;
      -- spec.hs:455:1-63
      signal t_42                         : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_228                        : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_9\ : boolean;
    begin
      \c$ws_app_arg_16\(i_31) <= \c$case_alt_229\;

      \c$case_alt_selection_res_9\ <= t_42 = result_89;

      \c$case_alt_229\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_31,max_7(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res_9\ else
                          \c$ws_app_arg_15\(i_31).Tuple2_8_sel1_Maybe;

      t_42 <= x_228.Tuple2_1_sel0_unsigned;

      x_228 <= \c$ws_app_arg_15\(i_31).Tuple2_8_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_7 : block
    signal vec_index_14 : integer range 0 to 5-1;
  begin
    vec_index_14 <= to_integer((signed(std_logic_vector(resize(i_7,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut_15\ <= result_99(vec_index_14);
  end block;
  -- index end

  \c$case_scrut_16\ <=  ws_7(0) ;

  with (\c$case_scrut_16\(3 downto 3)) select
    result_88 <= false when "0",
                 \c$case_scrut_15\.Tuple2_1_sel1_boolean when others;

  result_selection_res <= result_115.Tags_sel4_output1_0 > to_unsigned(1,8);

  result_89 <= \c$app_arg_28\ when result_selection_res else
               \c$app_arg_28\ + to_unsigned(112,8);

  \c$app_arg_28\ <= result_115.Tags_sel4_output1_0 - to_unsigned(1,8);

  i_7 <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_16\(2 downto 0)));

  ws_7 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_16\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "---"))));

  \c$vec2_8\ <= (ws_8(1 to ws_8'high));

  -- zipWith begin
  zipWith_8 : for i_32 in \c$ws_app_arg_17\'range generate
  begin
    \c$ws_app_arg_17\(i_32) <= ( Tuple2_8_sel0_Tuple2_1 => result_96(i_32)
               , Tuple2_8_sel1_Maybe => \c$vec2_8\(i_32) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_17 : block
    function max_8 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_18 : for i_33 in \c$ws_app_arg_18\'range generate
    begin
      fun_438 : block
      signal \c$case_alt_230\              : Spec_topEntity_types.Maybe;
      -- spec.hs:455:1-63
      signal t_43                          : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_229                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_10\ : boolean;
    begin
      \c$ws_app_arg_18\(i_33) <= \c$case_alt_230\;

      \c$case_alt_selection_res_10\ <= t_43 = result_115.Tags_sel5_output2_0;

      \c$case_alt_230\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_33,max_8(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res_10\ else
                          \c$ws_app_arg_17\(i_33).Tuple2_8_sel1_Maybe;

      t_43 <= x_229.Tuple2_1_sel0_unsigned;

      x_229 <= \c$ws_app_arg_17\(i_33).Tuple2_8_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_8 : block
    signal vec_index_15 : integer range 0 to 5-1;
  begin
    vec_index_15 <= to_integer((signed(std_logic_vector(resize(i_8,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut_17\ <= result_96(vec_index_15);
  end block;
  -- index end

  \c$case_scrut_18\ <=  ws_8(0) ;

  with (\c$case_scrut_18\(3 downto 3)) select
    result_90 <= false when "0",
                 \c$case_scrut_17\.Tuple2_1_sel1_boolean when others;

  i_8 <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_18\(2 downto 0)));

  ws_8 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_18\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "---"))));

  \c$vec2_9\ <= (ws_9(1 to ws_9'high));

  -- zipWith begin
  zipWith_9 : for i_34 in \c$ws_app_arg_19\'range generate
  begin
    \c$ws_app_arg_19\(i_34) <= ( Tuple2_8_sel0_Tuple2_1 => result_96(i_34)
               , Tuple2_8_sel1_Maybe => \c$vec2_9\(i_34) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_19 : block
    function max_9 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_20 : for i_35 in \c$ws_app_arg_20\'range generate
    begin
      fun_439 : block
      signal \c$case_alt_231\              : Spec_topEntity_types.Maybe;
      -- spec.hs:455:1-63
      signal t_44                          : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_230                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_11\ : boolean;
    begin
      \c$ws_app_arg_20\(i_35) <= \c$case_alt_231\;

      \c$case_alt_selection_res_11\ <= t_44 = result_92;

      \c$case_alt_231\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_35,max_9(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res_11\ else
                          \c$ws_app_arg_19\(i_35).Tuple2_8_sel1_Maybe;

      t_44 <= x_230.Tuple2_1_sel0_unsigned;

      x_230 <= \c$ws_app_arg_19\(i_35).Tuple2_8_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_9 : block
    signal vec_index_16 : integer range 0 to 5-1;
  begin
    vec_index_16 <= to_integer((signed(std_logic_vector(resize(i_9,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut_19\ <= result_96(vec_index_16);
  end block;
  -- index end

  \c$case_scrut_20\ <=  ws_9(0) ;

  with (\c$case_scrut_20\(3 downto 3)) select
    result_91 <= false when "0",
                 \c$case_scrut_19\.Tuple2_1_sel1_boolean when others;

  result_selection_res_2 <= result_115.Tags_sel5_output2_0 > to_unsigned(1,8);

  result_92 <= \c$app_arg_29\ when result_selection_res_2 else
               \c$app_arg_29\ + to_unsigned(112,8);

  \c$app_arg_29\ <= result_115.Tags_sel5_output2_0 - to_unsigned(1,8);

  i_9 <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_20\(2 downto 0)));

  ws_9 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_20\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "---"))));

  \c$vec2_10\ <= (ws_10(1 to ws_10'high));

  -- zipWith begin
  zipWith_10 : for i_36 in \c$ws_app_arg_21\'range generate
  begin
    \c$ws_app_arg_21\(i_36) <= ( Tuple2_8_sel0_Tuple2_1 => result_99(i_36)
               , Tuple2_8_sel1_Maybe => \c$vec2_10\(i_36) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_21 : block
    function max_10 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_22 : for i_37 in \c$ws_app_arg_22\'range generate
    begin
      fun_440 : block
      signal \c$case_alt_232\              : Spec_topEntity_types.Maybe;
      -- spec.hs:455:1-63
      signal t_45                          : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_231                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_12\ : boolean;
    begin
      \c$ws_app_arg_22\(i_37) <= \c$case_alt_232\;

      \c$case_alt_selection_res_12\ <= t_45 = result_115.Tags_sel4_output1_0;

      \c$case_alt_232\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_37,max_10(1,integer(ceil(log2(real(5))))))))) when \c$case_alt_selection_res_12\ else
                          \c$ws_app_arg_21\(i_37).Tuple2_8_sel1_Maybe;

      t_45 <= x_231.Tuple2_1_sel0_unsigned;

      x_231 <= \c$ws_app_arg_21\(i_37).Tuple2_8_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_10 : block
    signal vec_index_17 : integer range 0 to 5-1;
  begin
    vec_index_17 <= to_integer((signed(std_logic_vector(resize(i_10,64)))))
    -- pragma translate_off
                 mod 5
    -- pragma translate_on
                 ;
    \c$case_scrut_21\ <= result_99(vec_index_17);
  end block;
  -- index end

  \c$case_scrut_22\ <=  ws_10(0) ;

  with (\c$case_scrut_22\(3 downto 3)) select
    result_93 <= false when "0",
                 \c$case_scrut_21\.Tuple2_1_sel1_boolean when others;

  i_10 <= Spec_topEntity_types.index_5'(spec_topentity_types.fromSLV(\c$case_scrut_22\(2 downto 0)));

  ws_10 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_22\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "---"))));

  iterateI_28 : block
    signal iterateI_ho1_0_res_28 : boolean;
    signal iterateI_ho1_1_res_26 : boolean;
    signal iterateI_ho1_2_res_26 : boolean;
    signal iterateI_ho1_3_res_26 : boolean;
  begin
    r_block_420 : block
        signal \c$bb_res_res_189\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_189_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_189\ <= pOut3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_28 <= \c$bb_res_res_189\;


      end block;

    r_block_421 : block
        signal \c$bb_res_res_190\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_190_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_190\ <= iterateI_ho1_0_res_28;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_26 <= \c$bb_res_res_190\;


      end block;

    r_block_422 : block
        signal \c$bb_res_res_191\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_191_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_191\ <= iterateI_ho1_1_res_26;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_26 <= \c$bb_res_res_191\;


      end block;

    r_block_423 : block
        signal \c$bb_res_res_192\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_192_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_192\ <= iterateI_ho1_2_res_26;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_26 <= \c$bb_res_res_192\;


      end block;

    \c$app_arg_30\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                            , iterateI_ho1_0_res_28
                                                            , iterateI_ho1_1_res_26
                                                            , iterateI_ho1_2_res_26
                                                            , iterateI_ho1_3_res_26 );


  end block;

  result_94 <=  \c$app_arg_30\(\c$app_arg_30\'high) ;

  result_95 <= t_16 when b_16 else
               result_96;

  -- register begin
  result_96_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_96 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false ) );
    elsif rising_edge(clk) then
      if en then
        result_96 <= result_95;
      end if;
    end if;
  end process;
  -- register end

  b_16 <= result_97;

  \c$vec_24\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_96) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => result_116.Tags_sel5_output2_0
              , Tuple2_1_sel1_boolean => ((x_8 - y)) < (to_signed(-5,64)) )))));

  t_projection_15 <= (\c$vec_24\(0 to 1-1),\c$vec_24\(1 to \c$vec_24\'high));

  t_16 <= t_projection_15.Tuple2_18_sel1_array_of_Tuple2_1_1;

  x_8 <= result_102;

  y <= result_100;

  iterateI_29 : block
    signal iterateI_ho1_0_res_29 : boolean;
    signal iterateI_ho1_1_res_27 : boolean;
    signal iterateI_ho1_2_res_27 : boolean;
  begin
    r_block_424 : block
        signal \c$bb_res_res_193\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_193_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_193\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_29 <= \c$bb_res_res_193\;


      end block;

    r_block_425 : block
        signal \c$bb_res_res_194\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_194_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_194\ <= iterateI_ho1_0_res_29;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_27 <= \c$bb_res_res_194\;


      end block;

    r_block_426 : block
        signal \c$bb_res_res_195\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_195_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_195\ <= iterateI_ho1_1_res_27;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_27 <= \c$bb_res_res_195\;


      end block;

    \c$app_arg_31\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                            , iterateI_ho1_0_res_29
                                                            , iterateI_ho1_1_res_27
                                                            , iterateI_ho1_2_res_27 );


  end block;

  result_97 <=  \c$app_arg_31\(\c$app_arg_31\'high) ;

  result_98 <= t_17 when b_17 else
               result_99;

  -- register begin
  result_99_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_99 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_1_sel1_boolean => false ) );
    elsif rising_edge(clk) then
      if en then
        result_99 <= result_98;
      end if;
    end if;
  end process;
  -- register end

  b_17 <= result_103;

  \c$vec_25\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_99) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => result_116.Tags_sel4_output1_0
              , Tuple2_1_sel1_boolean => ((x_9 - y_0)) > (to_signed(5,64)) )))));

  t_projection_16 <= (\c$vec_25\(0 to 1-1),\c$vec_25\(1 to \c$vec_25\'high));

  t_17 <= t_projection_16.Tuple2_18_sel1_array_of_Tuple2_1_1;

  x_9 <= result_102;

  y_0 <= result_100;

  \c$vec2_11\ <= (ws_11(1 to ws_11'high));

  -- zipWith begin
  zipWith_11 : for i_38 in \c$ws_app_arg_23\'range generate
  begin
    \c$ws_app_arg_23\(i_38) <= ( Tuple2_9_sel0_Tuple2_2 => result_105(i_38)
               , Tuple2_9_sel1_Maybe_2 => \c$vec2_11\(i_38) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_23 : block
    function max_11 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_24 : for i_39 in \c$ws_app_arg_24\'range generate
    begin
      fun_441 : block
      signal \c$case_alt_233\              : Spec_topEntity_types.Maybe_2;
      -- spec.hs:455:1-63
      signal t_46                          : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_232                         : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_13\ : boolean;
    begin
      \c$ws_app_arg_24\(i_39) <= \c$case_alt_233\;

      \c$case_alt_selection_res_13\ <= t_46 = result_101;

      \c$case_alt_233\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_39,max_11(1,integer(ceil(log2(real(6))))))))) when \c$case_alt_selection_res_13\ else
                          \c$ws_app_arg_23\(i_39).Tuple2_9_sel1_Maybe_2;

      t_46 <= x_232.Tuple2_2_sel0_unsigned;

      x_232 <= \c$ws_app_arg_23\(i_39).Tuple2_9_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_11 : block
    signal vec_index_18 : integer range 0 to 6-1;
  begin
    vec_index_18 <= to_integer((signed(std_logic_vector(resize(i_11,64)))))
    -- pragma translate_off
                 mod 6
    -- pragma translate_on
                 ;
    \c$case_scrut_23\ <= result_105(vec_index_18);
  end block;
  -- index end

  \c$case_scrut_24\ <=  ws_11(0) ;

  with (\c$case_scrut_24\(3 downto 3)) select
    result_100 <= result_102 when "0",
                  \c$case_scrut_23\.Tuple2_2_sel1_signed when others;

  result_selection_res_3 <= result_116.Tags_sel3_output0_0 > to_unsigned(1,8);

  result_101 <= \c$app_arg_32\ when result_selection_res_3 else
                \c$app_arg_32\ + to_unsigned(112,8);

  \c$app_arg_32\ <= result_116.Tags_sel3_output0_0 - to_unsigned(1,8);

  i_11 <= Spec_topEntity_types.index_6'(spec_topentity_types.fromSLV(\c$case_scrut_24\(2 downto 0)));

  ws_11 <= Spec_topEntity_types.array_of_Maybe_2'(Spec_topEntity_types.array_of_Maybe_2'(\c$ws_app_arg_24\) & Spec_topEntity_types.array_of_Maybe_2'(Spec_topEntity_types.array_of_Maybe_2'(0 => std_logic_vector'("0" & "---"))));

  \c$vec2_12\ <= (ws_12(1 to ws_12'high));

  -- zipWith begin
  zipWith_12 : for i_40 in \c$ws_app_arg_25\'range generate
  begin
    \c$ws_app_arg_25\(i_40) <= ( Tuple2_9_sel0_Tuple2_2 => result_105(i_40)
               , Tuple2_9_sel1_Maybe_2 => \c$vec2_12\(i_40) );


  end generate;
  -- zipWith end

  -- imap begin
  imap_25 : block
    function max_12 (l,r : in natural) return natural is
    begin
      if l > r then return l;
      else return r;
      end if;
    end function;
  begin
    imap_26 : for i_41 in \c$ws_app_arg_26\'range generate
    begin
      fun_442 : block
      signal \c$case_alt_234\              : Spec_topEntity_types.Maybe_2;
      -- spec.hs:455:1-63
      signal t_47                          : unsigned(7 downto 0);
      -- spec.hs:455:1-63
      signal x_233                         : Spec_topEntity_types.Tuple2_2;
      signal \c$case_alt_selection_res_14\ : boolean;
    begin
      \c$ws_app_arg_26\(i_41) <= \c$case_alt_234\;

      \c$case_alt_selection_res_14\ <= t_47 = result_116.Tags_sel3_output0_0;

      \c$case_alt_234\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_41,max_12(1,integer(ceil(log2(real(6))))))))) when \c$case_alt_selection_res_14\ else
                          \c$ws_app_arg_25\(i_41).Tuple2_9_sel1_Maybe_2;

      t_47 <= x_233.Tuple2_2_sel0_unsigned;

      x_233 <= \c$ws_app_arg_25\(i_41).Tuple2_9_sel0_Tuple2_2;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_12 : block
    signal vec_index_19 : integer range 0 to 6-1;
  begin
    vec_index_19 <= to_integer((signed(std_logic_vector(resize(i_12,64)))))
    -- pragma translate_off
                 mod 6
    -- pragma translate_on
                 ;
    \c$case_scrut_25\ <= result_105(vec_index_19);
  end block;
  -- index end

  \c$case_scrut_26\ <=  ws_12(0) ;

  with (\c$case_scrut_26\(3 downto 3)) select
    result_102 <= to_signed(0,64) when "0",
                  \c$case_scrut_25\.Tuple2_2_sel1_signed when others;

  i_12 <= Spec_topEntity_types.index_6'(spec_topentity_types.fromSLV(\c$case_scrut_26\(2 downto 0)));

  ws_12 <= Spec_topEntity_types.array_of_Maybe_2'(Spec_topEntity_types.array_of_Maybe_2'(\c$ws_app_arg_26\) & Spec_topEntity_types.array_of_Maybe_2'(Spec_topEntity_types.array_of_Maybe_2'(0 => std_logic_vector'("0" & "---"))));

  iterateI_30 : block
    signal iterateI_ho1_0_res_30 : boolean;
    signal iterateI_ho1_1_res_28 : boolean;
    signal iterateI_ho1_2_res_28 : boolean;
  begin
    r_block_427 : block
        signal \c$bb_res_res_196\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_196_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_196\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_30 <= \c$bb_res_res_196\;


      end block;

    r_block_428 : block
        signal \c$bb_res_res_197\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_197_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_197\ <= iterateI_ho1_0_res_30;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_28 <= \c$bb_res_res_197\;


      end block;

    r_block_429 : block
        signal \c$bb_res_res_198\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_198_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_198\ <= iterateI_ho1_1_res_28;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_28 <= \c$bb_res_res_198\;


      end block;

    \c$app_arg_33\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                            , iterateI_ho1_0_res_30
                                                            , iterateI_ho1_1_res_28
                                                            , iterateI_ho1_2_res_28 );


  end block;

  result_103 <=  \c$app_arg_33\(\c$app_arg_33\'high) ;

  result_104 <= t_18 when b_18 else
                result_105;

  -- register begin
  result_105_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_105 <= Spec_topEntity_types.array_of_Tuple2_2'( ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
                                         , Tuple2_2_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_105 <= result_104;
      end if;
    end if;
  end process;
  -- register end

  b_18 <= result_110;

  \c$vec_26\ <= (Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(result_105) & Spec_topEntity_types.array_of_Tuple2_2'(Spec_topEntity_types.array_of_Tuple2_2'(0 => ( Tuple2_2_sel0_unsigned => result_109.Tags_sel3_output0_0
              , Tuple2_2_sel1_signed => \c$out0_case_alt\ )))));

  t_projection_19 <= (\c$vec_26\(0 to 1-1),\c$vec_26\(1 to \c$vec_26\'high));

  t_18 <= t_projection_19.Tuple2_11_sel1_array_of_Tuple2_2_1;

  \c$out0_case_alt_selection_res\ <= tag_0 = to_unsigned(113,8);

  \c$out0_case_alt\ <= to_signed(0,64) when \c$out0_case_alt_selection_res\ else
                       dta_0;

  dta_0 <= result_107.Tuple2_2_sel1_signed;

  tag_0 <= result_107.Tuple2_2_sel0_unsigned;

  result_106 <= ( Tuple2_2_sel0_unsigned => t_32
                , Tuple2_2_sel1_signed => \c$ds_app_arg\ ) when b_19 else
                result_107;

  -- register begin
  result_107_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_107 <= ( Tuple2_2_sel0_unsigned => to_unsigned(113,8)
  , Tuple2_2_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_107 <= result_106;
      end if;
    end if;
  end process;
  -- register end

  b_19 <= result_108;

  -- delay begin
  cds_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$ds_app_arg\ <= inputs_3.Inputs_4_sel0_input0.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  inputs_3 <= result_145.Tuple3_sel2_Tuple3_0.Tuple3_0_sel0_Inputs_4;

  iterateI_31 : block
    signal iterateI_ho1_0_res_31 : boolean;
  begin
    r_block_430 : block
        signal \c$bb_res_res_199\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_199_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_199\ <= pIn0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_31 <= \c$bb_res_res_199\;


      end block;

    \c$app_arg_34\ <= Spec_topEntity_types.array_of_boolean'( pIn0
                                                            , iterateI_ho1_0_res_31 );


  end block;

  result_108 <=  \c$app_arg_34\(\c$app_arg_34\'high) ;

  iterateI_32 : block
    signal iterateI_ho1_0_arg0_17 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_32  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_17 <= ( Tags_sel0_input0_0 => t_32
                              , Tags_sel1_input1_0 => t_31
                              , Tags_sel2_input2_0 => t_30
                              , Tags_sel3_output0_0 => t_29
                              , Tags_sel4_output1_0 => t_28
                              , Tags_sel5_output2_0 => t_27
                              , Tags_sel6_output3_0 => t_26
                              , Tags_sel7_output4_0 => t_25
                              , Tags_sel8_output5_0 => t_24
                              , Tags_sel9_output6_0 => t_23
                              , Tags_sel10_output7_0 => t_22
                              , Tags_sel11_output8_0 => t_21
                              , Tags_sel12_output9_0 => t_20
                              , Tags_sel13_output10_0 => t_19
                              , Tags_sel14_slide0_0 => t_26
                              , Tags_sel15_slide1_0 => t_30
                              , Tags_sel16_slide2_0 => t_30
                              , Tags_sel17_slide3_0 => t_30
                              , Tags_sel18_slide4_0 => t_30
                              , Tags_sel19_slide5_0 => t_30
                              , Tags_sel20_slide6_0 => t_30 );

    r_block_431 : block
        signal \c$bb_res_res_200\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_200_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_200\ <= iterateI_ho1_0_arg0_17;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_32 <= \c$bb_res_res_200\;


      end block;

    \c$app_arg_35\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_32
                                                         , Tags_sel1_input1_0 => t_31
                                                         , Tags_sel2_input2_0 => t_30
                                                         , Tags_sel3_output0_0 => t_29
                                                         , Tags_sel4_output1_0 => t_28
                                                         , Tags_sel5_output2_0 => t_27
                                                         , Tags_sel6_output3_0 => t_26
                                                         , Tags_sel7_output4_0 => t_25
                                                         , Tags_sel8_output5_0 => t_24
                                                         , Tags_sel9_output6_0 => t_23
                                                         , Tags_sel10_output7_0 => t_22
                                                         , Tags_sel11_output8_0 => t_21
                                                         , Tags_sel12_output9_0 => t_20
                                                         , Tags_sel13_output10_0 => t_19
                                                         , Tags_sel14_slide0_0 => t_26
                                                         , Tags_sel15_slide1_0 => t_30
                                                         , Tags_sel16_slide2_0 => t_30
                                                         , Tags_sel17_slide3_0 => t_30
                                                         , Tags_sel18_slide4_0 => t_30
                                                         , Tags_sel19_slide5_0 => t_30
                                                         , Tags_sel20_slide6_0 => t_30 )
                                                         , iterateI_ho1_0_res_32 );


  end block;

  result_109 <=  \c$app_arg_35\(\c$app_arg_35\'high) ;

  iterateI_33 : block
    signal iterateI_ho1_0_res_33 : boolean;
    signal iterateI_ho1_1_res_29 : boolean;
  begin
    r_block_432 : block
        signal \c$bb_res_res_201\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_201_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_201\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_33 <= \c$bb_res_res_201\;


      end block;

    r_block_433 : block
        signal \c$bb_res_res_202\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_202_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_202\ <= iterateI_ho1_0_res_33;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_29 <= \c$bb_res_res_202\;


      end block;

    \c$app_arg_36\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                            , iterateI_ho1_0_res_33
                                                            , iterateI_ho1_1_res_29 );


  end block;

  result_110 <=  \c$app_arg_36\(\c$app_arg_36\'high) ;

  iterateI_34 : block
    signal iterateI_ho1_0_arg0_18 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_34  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_30  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_29  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_27  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_4_res_26  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_5_res_24  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_6_res_16  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_18 <= ( Tags_sel0_input0_0 => t_32
                              , Tags_sel1_input1_0 => t_31
                              , Tags_sel2_input2_0 => t_30
                              , Tags_sel3_output0_0 => t_29
                              , Tags_sel4_output1_0 => t_28
                              , Tags_sel5_output2_0 => t_27
                              , Tags_sel6_output3_0 => t_26
                              , Tags_sel7_output4_0 => t_25
                              , Tags_sel8_output5_0 => t_24
                              , Tags_sel9_output6_0 => t_23
                              , Tags_sel10_output7_0 => t_22
                              , Tags_sel11_output8_0 => t_21
                              , Tags_sel12_output9_0 => t_20
                              , Tags_sel13_output10_0 => t_19
                              , Tags_sel14_slide0_0 => t_26
                              , Tags_sel15_slide1_0 => t_30
                              , Tags_sel16_slide2_0 => t_30
                              , Tags_sel17_slide3_0 => t_30
                              , Tags_sel18_slide4_0 => t_30
                              , Tags_sel19_slide5_0 => t_30
                              , Tags_sel20_slide6_0 => t_30 );

    r_block_434 : block
        signal \c$bb_res_res_203\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_203_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_203\ <= iterateI_ho1_0_arg0_18;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_34 <= \c$bb_res_res_203\;


      end block;

    r_block_435 : block
        signal \c$bb_res_res_204\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_204_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_204\ <= iterateI_ho1_0_res_34;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_30 <= \c$bb_res_res_204\;


      end block;

    r_block_436 : block
        signal \c$bb_res_res_205\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_205_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_205\ <= iterateI_ho1_1_res_30;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_29 <= \c$bb_res_res_205\;


      end block;

    r_block_437 : block
        signal \c$bb_res_res_206\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_206_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_206\ <= iterateI_ho1_2_res_29;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_27 <= \c$bb_res_res_206\;


      end block;

    r_block_438 : block
        signal \c$bb_res_res_207\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_207_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_207\ <= iterateI_ho1_3_res_27;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_26 <= \c$bb_res_res_207\;


      end block;

    r_block_439 : block
        signal \c$bb_res_res_208\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_208_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_208\ <= iterateI_ho1_4_res_26;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_24 <= \c$bb_res_res_208\;


      end block;

    r_block_440 : block
        signal \c$bb_res_res_209\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_209_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_209\ <= iterateI_ho1_5_res_24;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_6_res_16 <= \c$bb_res_res_209\;


      end block;

    \c$app_arg_37\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_32
                                                         , Tags_sel1_input1_0 => t_31
                                                         , Tags_sel2_input2_0 => t_30
                                                         , Tags_sel3_output0_0 => t_29
                                                         , Tags_sel4_output1_0 => t_28
                                                         , Tags_sel5_output2_0 => t_27
                                                         , Tags_sel6_output3_0 => t_26
                                                         , Tags_sel7_output4_0 => t_25
                                                         , Tags_sel8_output5_0 => t_24
                                                         , Tags_sel9_output6_0 => t_23
                                                         , Tags_sel10_output7_0 => t_22
                                                         , Tags_sel11_output8_0 => t_21
                                                         , Tags_sel12_output9_0 => t_20
                                                         , Tags_sel13_output10_0 => t_19
                                                         , Tags_sel14_slide0_0 => t_26
                                                         , Tags_sel15_slide1_0 => t_30
                                                         , Tags_sel16_slide2_0 => t_30
                                                         , Tags_sel17_slide3_0 => t_30
                                                         , Tags_sel18_slide4_0 => t_30
                                                         , Tags_sel19_slide5_0 => t_30
                                                         , Tags_sel20_slide6_0 => t_30 )
                                                         , iterateI_ho1_0_res_34
                                                         , iterateI_ho1_1_res_30
                                                         , iterateI_ho1_2_res_29
                                                         , iterateI_ho1_3_res_27
                                                         , iterateI_ho1_4_res_26
                                                         , iterateI_ho1_5_res_24
                                                         , iterateI_ho1_6_res_16 );


  end block;

  result_111 <=  \c$app_arg_37\(\c$app_arg_37\'high) ;

  iterateI_35 : block
    signal iterateI_ho1_0_arg0_19 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_35  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_31  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_30  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_28  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_4_res_27  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_5_res_25  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_19 <= ( Tags_sel0_input0_0 => t_32
                              , Tags_sel1_input1_0 => t_31
                              , Tags_sel2_input2_0 => t_30
                              , Tags_sel3_output0_0 => t_29
                              , Tags_sel4_output1_0 => t_28
                              , Tags_sel5_output2_0 => t_27
                              , Tags_sel6_output3_0 => t_26
                              , Tags_sel7_output4_0 => t_25
                              , Tags_sel8_output5_0 => t_24
                              , Tags_sel9_output6_0 => t_23
                              , Tags_sel10_output7_0 => t_22
                              , Tags_sel11_output8_0 => t_21
                              , Tags_sel12_output9_0 => t_20
                              , Tags_sel13_output10_0 => t_19
                              , Tags_sel14_slide0_0 => t_26
                              , Tags_sel15_slide1_0 => t_30
                              , Tags_sel16_slide2_0 => t_30
                              , Tags_sel17_slide3_0 => t_30
                              , Tags_sel18_slide4_0 => t_30
                              , Tags_sel19_slide5_0 => t_30
                              , Tags_sel20_slide6_0 => t_30 );

    r_block_441 : block
        signal \c$bb_res_res_210\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_210_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_210\ <= iterateI_ho1_0_arg0_19;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_35 <= \c$bb_res_res_210\;


      end block;

    r_block_442 : block
        signal \c$bb_res_res_211\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_211_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_211\ <= iterateI_ho1_0_res_35;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_31 <= \c$bb_res_res_211\;


      end block;

    r_block_443 : block
        signal \c$bb_res_res_212\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_212_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_212\ <= iterateI_ho1_1_res_31;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_30 <= \c$bb_res_res_212\;


      end block;

    r_block_444 : block
        signal \c$bb_res_res_213\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_213_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_213\ <= iterateI_ho1_2_res_30;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_28 <= \c$bb_res_res_213\;


      end block;

    r_block_445 : block
        signal \c$bb_res_res_214\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_214_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_214\ <= iterateI_ho1_3_res_28;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_27 <= \c$bb_res_res_214\;


      end block;

    r_block_446 : block
        signal \c$bb_res_res_215\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_215_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_215\ <= iterateI_ho1_4_res_27;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_5_res_25 <= \c$bb_res_res_215\;


      end block;

    \c$app_arg_38\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_32
                                                         , Tags_sel1_input1_0 => t_31
                                                         , Tags_sel2_input2_0 => t_30
                                                         , Tags_sel3_output0_0 => t_29
                                                         , Tags_sel4_output1_0 => t_28
                                                         , Tags_sel5_output2_0 => t_27
                                                         , Tags_sel6_output3_0 => t_26
                                                         , Tags_sel7_output4_0 => t_25
                                                         , Tags_sel8_output5_0 => t_24
                                                         , Tags_sel9_output6_0 => t_23
                                                         , Tags_sel10_output7_0 => t_22
                                                         , Tags_sel11_output8_0 => t_21
                                                         , Tags_sel12_output9_0 => t_20
                                                         , Tags_sel13_output10_0 => t_19
                                                         , Tags_sel14_slide0_0 => t_26
                                                         , Tags_sel15_slide1_0 => t_30
                                                         , Tags_sel16_slide2_0 => t_30
                                                         , Tags_sel17_slide3_0 => t_30
                                                         , Tags_sel18_slide4_0 => t_30
                                                         , Tags_sel19_slide5_0 => t_30
                                                         , Tags_sel20_slide6_0 => t_30 )
                                                         , iterateI_ho1_0_res_35
                                                         , iterateI_ho1_1_res_31
                                                         , iterateI_ho1_2_res_30
                                                         , iterateI_ho1_3_res_28
                                                         , iterateI_ho1_4_res_27
                                                         , iterateI_ho1_5_res_25 );


  end block;

  result_112 <=  \c$app_arg_38\(\c$app_arg_38\'high) ;

  iterateI_36 : block
    signal iterateI_ho1_0_arg0_20 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_36  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_32  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_31  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_29  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_4_res_28  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_20 <= ( Tags_sel0_input0_0 => t_32
                              , Tags_sel1_input1_0 => t_31
                              , Tags_sel2_input2_0 => t_30
                              , Tags_sel3_output0_0 => t_29
                              , Tags_sel4_output1_0 => t_28
                              , Tags_sel5_output2_0 => t_27
                              , Tags_sel6_output3_0 => t_26
                              , Tags_sel7_output4_0 => t_25
                              , Tags_sel8_output5_0 => t_24
                              , Tags_sel9_output6_0 => t_23
                              , Tags_sel10_output7_0 => t_22
                              , Tags_sel11_output8_0 => t_21
                              , Tags_sel12_output9_0 => t_20
                              , Tags_sel13_output10_0 => t_19
                              , Tags_sel14_slide0_0 => t_26
                              , Tags_sel15_slide1_0 => t_30
                              , Tags_sel16_slide2_0 => t_30
                              , Tags_sel17_slide3_0 => t_30
                              , Tags_sel18_slide4_0 => t_30
                              , Tags_sel19_slide5_0 => t_30
                              , Tags_sel20_slide6_0 => t_30 );

    r_block_447 : block
        signal \c$bb_res_res_216\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_216_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_216\ <= iterateI_ho1_0_arg0_20;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_36 <= \c$bb_res_res_216\;


      end block;

    r_block_448 : block
        signal \c$bb_res_res_217\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_217_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_217\ <= iterateI_ho1_0_res_36;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_32 <= \c$bb_res_res_217\;


      end block;

    r_block_449 : block
        signal \c$bb_res_res_218\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_218_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_218\ <= iterateI_ho1_1_res_32;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_31 <= \c$bb_res_res_218\;


      end block;

    r_block_450 : block
        signal \c$bb_res_res_219\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_219_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_219\ <= iterateI_ho1_2_res_31;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_29 <= \c$bb_res_res_219\;


      end block;

    r_block_451 : block
        signal \c$bb_res_res_220\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_220_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_220\ <= iterateI_ho1_3_res_29;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_4_res_28 <= \c$bb_res_res_220\;


      end block;

    \c$app_arg_39\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_32
                                                         , Tags_sel1_input1_0 => t_31
                                                         , Tags_sel2_input2_0 => t_30
                                                         , Tags_sel3_output0_0 => t_29
                                                         , Tags_sel4_output1_0 => t_28
                                                         , Tags_sel5_output2_0 => t_27
                                                         , Tags_sel6_output3_0 => t_26
                                                         , Tags_sel7_output4_0 => t_25
                                                         , Tags_sel8_output5_0 => t_24
                                                         , Tags_sel9_output6_0 => t_23
                                                         , Tags_sel10_output7_0 => t_22
                                                         , Tags_sel11_output8_0 => t_21
                                                         , Tags_sel12_output9_0 => t_20
                                                         , Tags_sel13_output10_0 => t_19
                                                         , Tags_sel14_slide0_0 => t_26
                                                         , Tags_sel15_slide1_0 => t_30
                                                         , Tags_sel16_slide2_0 => t_30
                                                         , Tags_sel17_slide3_0 => t_30
                                                         , Tags_sel18_slide4_0 => t_30
                                                         , Tags_sel19_slide5_0 => t_30
                                                         , Tags_sel20_slide6_0 => t_30 )
                                                         , iterateI_ho1_0_res_36
                                                         , iterateI_ho1_1_res_32
                                                         , iterateI_ho1_2_res_31
                                                         , iterateI_ho1_3_res_29
                                                         , iterateI_ho1_4_res_28 );


  end block;

  result_113 <=  \c$app_arg_39\(\c$app_arg_39\'high) ;

  iterateI_37 : block
    signal iterateI_ho1_0_arg0_21 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_37  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_33  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_32  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_3_res_30  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_21 <= ( Tags_sel0_input0_0 => t_32
                              , Tags_sel1_input1_0 => t_31
                              , Tags_sel2_input2_0 => t_30
                              , Tags_sel3_output0_0 => t_29
                              , Tags_sel4_output1_0 => t_28
                              , Tags_sel5_output2_0 => t_27
                              , Tags_sel6_output3_0 => t_26
                              , Tags_sel7_output4_0 => t_25
                              , Tags_sel8_output5_0 => t_24
                              , Tags_sel9_output6_0 => t_23
                              , Tags_sel10_output7_0 => t_22
                              , Tags_sel11_output8_0 => t_21
                              , Tags_sel12_output9_0 => t_20
                              , Tags_sel13_output10_0 => t_19
                              , Tags_sel14_slide0_0 => t_26
                              , Tags_sel15_slide1_0 => t_30
                              , Tags_sel16_slide2_0 => t_30
                              , Tags_sel17_slide3_0 => t_30
                              , Tags_sel18_slide4_0 => t_30
                              , Tags_sel19_slide5_0 => t_30
                              , Tags_sel20_slide6_0 => t_30 );

    r_block_452 : block
        signal \c$bb_res_res_221\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_221_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_221\ <= iterateI_ho1_0_arg0_21;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_37 <= \c$bb_res_res_221\;


      end block;

    r_block_453 : block
        signal \c$bb_res_res_222\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_222_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_222\ <= iterateI_ho1_0_res_37;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_33 <= \c$bb_res_res_222\;


      end block;

    r_block_454 : block
        signal \c$bb_res_res_223\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_223_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_223\ <= iterateI_ho1_1_res_33;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_32 <= \c$bb_res_res_223\;


      end block;

    r_block_455 : block
        signal \c$bb_res_res_224\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_224_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_224\ <= iterateI_ho1_2_res_32;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_30 <= \c$bb_res_res_224\;


      end block;

    \c$app_arg_40\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_32
                                                         , Tags_sel1_input1_0 => t_31
                                                         , Tags_sel2_input2_0 => t_30
                                                         , Tags_sel3_output0_0 => t_29
                                                         , Tags_sel4_output1_0 => t_28
                                                         , Tags_sel5_output2_0 => t_27
                                                         , Tags_sel6_output3_0 => t_26
                                                         , Tags_sel7_output4_0 => t_25
                                                         , Tags_sel8_output5_0 => t_24
                                                         , Tags_sel9_output6_0 => t_23
                                                         , Tags_sel10_output7_0 => t_22
                                                         , Tags_sel11_output8_0 => t_21
                                                         , Tags_sel12_output9_0 => t_20
                                                         , Tags_sel13_output10_0 => t_19
                                                         , Tags_sel14_slide0_0 => t_26
                                                         , Tags_sel15_slide1_0 => t_30
                                                         , Tags_sel16_slide2_0 => t_30
                                                         , Tags_sel17_slide3_0 => t_30
                                                         , Tags_sel18_slide4_0 => t_30
                                                         , Tags_sel19_slide5_0 => t_30
                                                         , Tags_sel20_slide6_0 => t_30 )
                                                         , iterateI_ho1_0_res_37
                                                         , iterateI_ho1_1_res_33
                                                         , iterateI_ho1_2_res_32
                                                         , iterateI_ho1_3_res_30 );


  end block;

  result_114 <=  \c$app_arg_40\(\c$app_arg_40\'high) ;

  iterateI_38 : block
    signal iterateI_ho1_0_arg0_22 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_38  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_34  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_33  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_22 <= ( Tags_sel0_input0_0 => t_32
                              , Tags_sel1_input1_0 => t_31
                              , Tags_sel2_input2_0 => t_30
                              , Tags_sel3_output0_0 => t_29
                              , Tags_sel4_output1_0 => t_28
                              , Tags_sel5_output2_0 => t_27
                              , Tags_sel6_output3_0 => t_26
                              , Tags_sel7_output4_0 => t_25
                              , Tags_sel8_output5_0 => t_24
                              , Tags_sel9_output6_0 => t_23
                              , Tags_sel10_output7_0 => t_22
                              , Tags_sel11_output8_0 => t_21
                              , Tags_sel12_output9_0 => t_20
                              , Tags_sel13_output10_0 => t_19
                              , Tags_sel14_slide0_0 => t_26
                              , Tags_sel15_slide1_0 => t_30
                              , Tags_sel16_slide2_0 => t_30
                              , Tags_sel17_slide3_0 => t_30
                              , Tags_sel18_slide4_0 => t_30
                              , Tags_sel19_slide5_0 => t_30
                              , Tags_sel20_slide6_0 => t_30 );

    r_block_456 : block
        signal \c$bb_res_res_225\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_225_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_225\ <= iterateI_ho1_0_arg0_22;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_38 <= \c$bb_res_res_225\;


      end block;

    r_block_457 : block
        signal \c$bb_res_res_226\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_226_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_226\ <= iterateI_ho1_0_res_38;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_34 <= \c$bb_res_res_226\;


      end block;

    r_block_458 : block
        signal \c$bb_res_res_227\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_227_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_227\ <= iterateI_ho1_1_res_34;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_33 <= \c$bb_res_res_227\;


      end block;

    \c$app_arg_41\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_32
                                                         , Tags_sel1_input1_0 => t_31
                                                         , Tags_sel2_input2_0 => t_30
                                                         , Tags_sel3_output0_0 => t_29
                                                         , Tags_sel4_output1_0 => t_28
                                                         , Tags_sel5_output2_0 => t_27
                                                         , Tags_sel6_output3_0 => t_26
                                                         , Tags_sel7_output4_0 => t_25
                                                         , Tags_sel8_output5_0 => t_24
                                                         , Tags_sel9_output6_0 => t_23
                                                         , Tags_sel10_output7_0 => t_22
                                                         , Tags_sel11_output8_0 => t_21
                                                         , Tags_sel12_output9_0 => t_20
                                                         , Tags_sel13_output10_0 => t_19
                                                         , Tags_sel14_slide0_0 => t_26
                                                         , Tags_sel15_slide1_0 => t_30
                                                         , Tags_sel16_slide2_0 => t_30
                                                         , Tags_sel17_slide3_0 => t_30
                                                         , Tags_sel18_slide4_0 => t_30
                                                         , Tags_sel19_slide5_0 => t_30
                                                         , Tags_sel20_slide6_0 => t_30 )
                                                         , iterateI_ho1_0_res_38
                                                         , iterateI_ho1_1_res_34
                                                         , iterateI_ho1_2_res_33 );


  end block;

  result_115 <=  \c$app_arg_41\(\c$app_arg_41\'high) ;

  iterateI_39 : block
    signal iterateI_ho1_0_arg0_23 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_39  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_35  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_23 <= ( Tags_sel0_input0_0 => t_32
                              , Tags_sel1_input1_0 => t_31
                              , Tags_sel2_input2_0 => t_30
                              , Tags_sel3_output0_0 => t_29
                              , Tags_sel4_output1_0 => t_28
                              , Tags_sel5_output2_0 => t_27
                              , Tags_sel6_output3_0 => t_26
                              , Tags_sel7_output4_0 => t_25
                              , Tags_sel8_output5_0 => t_24
                              , Tags_sel9_output6_0 => t_23
                              , Tags_sel10_output7_0 => t_22
                              , Tags_sel11_output8_0 => t_21
                              , Tags_sel12_output9_0 => t_20
                              , Tags_sel13_output10_0 => t_19
                              , Tags_sel14_slide0_0 => t_26
                              , Tags_sel15_slide1_0 => t_30
                              , Tags_sel16_slide2_0 => t_30
                              , Tags_sel17_slide3_0 => t_30
                              , Tags_sel18_slide4_0 => t_30
                              , Tags_sel19_slide5_0 => t_30
                              , Tags_sel20_slide6_0 => t_30 );

    r_block_459 : block
        signal \c$bb_res_res_228\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_228_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_228\ <= iterateI_ho1_0_arg0_23;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_39 <= \c$bb_res_res_228\;


      end block;

    r_block_460 : block
        signal \c$bb_res_res_229\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0_0 => to_unsigned(113,8)
      , Tags_sel1_input1_0 => to_unsigned(113,8)
      , Tags_sel2_input2_0 => to_unsigned(113,8)
      , Tags_sel3_output0_0 => to_unsigned(113,8)
      , Tags_sel4_output1_0 => to_unsigned(113,8)
      , Tags_sel5_output2_0 => to_unsigned(113,8)
      , Tags_sel6_output3_0 => to_unsigned(113,8)
      , Tags_sel7_output4_0 => to_unsigned(113,8)
      , Tags_sel8_output5_0 => to_unsigned(113,8)
      , Tags_sel9_output6_0 => to_unsigned(113,8)
      , Tags_sel10_output7_0 => to_unsigned(113,8)
      , Tags_sel11_output8_0 => to_unsigned(113,8)
      , Tags_sel12_output9_0 => to_unsigned(113,8)
      , Tags_sel13_output10_0 => to_unsigned(113,8)
      , Tags_sel14_slide0_0 => to_unsigned(113,8)
      , Tags_sel15_slide1_0 => to_unsigned(113,8)
      , Tags_sel16_slide2_0 => to_unsigned(113,8)
      , Tags_sel17_slide3_0 => to_unsigned(113,8)
      , Tags_sel18_slide4_0 => to_unsigned(113,8)
      , Tags_sel19_slide5_0 => to_unsigned(113,8)
      , Tags_sel20_slide6_0 => to_unsigned(113,8) );
      begin
        -- delay begin
        cbb_res_res_229_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_229\ <= iterateI_ho1_0_res_39;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_35 <= \c$bb_res_res_229\;


      end block;

    \c$app_arg_42\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0_0 => t_32
                                                         , Tags_sel1_input1_0 => t_31
                                                         , Tags_sel2_input2_0 => t_30
                                                         , Tags_sel3_output0_0 => t_29
                                                         , Tags_sel4_output1_0 => t_28
                                                         , Tags_sel5_output2_0 => t_27
                                                         , Tags_sel6_output3_0 => t_26
                                                         , Tags_sel7_output4_0 => t_25
                                                         , Tags_sel8_output5_0 => t_24
                                                         , Tags_sel9_output6_0 => t_23
                                                         , Tags_sel10_output7_0 => t_22
                                                         , Tags_sel11_output8_0 => t_21
                                                         , Tags_sel12_output9_0 => t_20
                                                         , Tags_sel13_output10_0 => t_19
                                                         , Tags_sel14_slide0_0 => t_26
                                                         , Tags_sel15_slide1_0 => t_30
                                                         , Tags_sel16_slide2_0 => t_30
                                                         , Tags_sel17_slide3_0 => t_30
                                                         , Tags_sel18_slide4_0 => t_30
                                                         , Tags_sel19_slide5_0 => t_30
                                                         , Tags_sel20_slide6_0 => t_30 )
                                                         , iterateI_ho1_0_res_39
                                                         , iterateI_ho1_1_res_35 );


  end block;

  result_116 <=  \c$app_arg_42\(\c$app_arg_42\'high) ;

  -- register begin
  t_19_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_19 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_19 <= result_117;
      end if;
    end if;
  end process;
  -- register end

  result_selection_48 <= pOut10;

  result_117 <= result_118 when result_selection_48 else
                t_19;

  b_20 <= t_19 = to_unsigned(112,8);

  f1 <= t_19 + to_unsigned(1,8);

  result_118 <= to_unsigned(1,8) when b_20 else
                f1;

  pOut10 <= pacings.Pacings_1_sel13_pacingOut10;

  -- register begin
  t_20_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_20 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_20 <= result_119;
      end if;
    end if;
  end process;
  -- register end

  result_selection_52 <= pOut9;

  result_119 <= result_120 when result_selection_52 else
                t_20;

  b_21 <= t_20 = to_unsigned(112,8);

  f1_0 <= t_20 + to_unsigned(1,8);

  result_120 <= to_unsigned(1,8) when b_21 else
                f1_0;

  pOut9 <= pacings.Pacings_1_sel12_pacingOut9;

  -- register begin
  t_21_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_21 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_21 <= result_121;
      end if;
    end if;
  end process;
  -- register end

  result_selection_56 <= pOut8;

  result_121 <= result_122 when result_selection_56 else
                t_21;

  b_22 <= t_21 = to_unsigned(112,8);

  f1_1 <= t_21 + to_unsigned(1,8);

  result_122 <= to_unsigned(1,8) when b_22 else
                f1_1;

  pOut8 <= pacings.Pacings_1_sel11_pacingOut8;

  -- register begin
  t_22_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_22 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_22 <= result_123;
      end if;
    end if;
  end process;
  -- register end

  result_selection_60 <= pOut7;

  result_123 <= result_124 when result_selection_60 else
                t_22;

  b_23 <= t_22 = to_unsigned(112,8);

  f1_2 <= t_22 + to_unsigned(1,8);

  result_124 <= to_unsigned(1,8) when b_23 else
                f1_2;

  pOut7 <= pacings.Pacings_1_sel10_pacingOut7;

  -- register begin
  t_23_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_23 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_23 <= result_125;
      end if;
    end if;
  end process;
  -- register end

  result_selection_64 <= pOut6;

  result_125 <= result_126 when result_selection_64 else
                t_23;

  b_24 <= t_23 = to_unsigned(112,8);

  f1_3 <= t_23 + to_unsigned(1,8);

  result_126 <= to_unsigned(1,8) when b_24 else
                f1_3;

  pOut6 <= pacings.Pacings_1_sel9_pacingOut6;

  -- register begin
  t_24_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_24 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_24 <= result_127;
      end if;
    end if;
  end process;
  -- register end

  result_selection_68 <= pOut5;

  result_127 <= result_128 when result_selection_68 else
                t_24;

  b_25 <= t_24 = to_unsigned(112,8);

  f1_4 <= t_24 + to_unsigned(1,8);

  result_128 <= to_unsigned(1,8) when b_25 else
                f1_4;

  pOut5 <= pacings.Pacings_1_sel8_pacingOut5;

  -- register begin
  t_25_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_25 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_25 <= result_129;
      end if;
    end if;
  end process;
  -- register end

  result_selection_72 <= pOut4;

  result_129 <= result_130 when result_selection_72 else
                t_25;

  b_26 <= t_25 = to_unsigned(112,8);

  f1_5 <= t_25 + to_unsigned(1,8);

  result_130 <= to_unsigned(1,8) when b_26 else
                f1_5;

  pOut4 <= pacings.Pacings_1_sel7_pacingOut4;

  -- register begin
  t_26_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_26 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_26 <= result_131;
      end if;
    end if;
  end process;
  -- register end

  result_selection_76 <= pOut3;

  result_131 <= result_132 when result_selection_76 else
                t_26;

  b_27 <= t_26 = to_unsigned(112,8);

  f1_6 <= t_26 + to_unsigned(1,8);

  result_132 <= to_unsigned(1,8) when b_27 else
                f1_6;

  pOut3 <= pacings.Pacings_1_sel6_pacingOut3;

  -- register begin
  t_27_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_27 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_27 <= result_133;
      end if;
    end if;
  end process;
  -- register end

  result_selection_80 <= pOut2;

  result_133 <= result_134 when result_selection_80 else
                t_27;

  b_28 <= t_27 = to_unsigned(112,8);

  f1_7 <= t_27 + to_unsigned(1,8);

  result_134 <= to_unsigned(1,8) when b_28 else
                f1_7;

  pOut2 <= pacings.Pacings_1_sel5_pacingOut2;

  -- register begin
  t_28_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_28 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_28 <= result_135;
      end if;
    end if;
  end process;
  -- register end

  result_selection_84 <= pOut1;

  result_135 <= result_136 when result_selection_84 else
                t_28;

  b_29 <= t_28 = to_unsigned(112,8);

  f1_8 <= t_28 + to_unsigned(1,8);

  result_136 <= to_unsigned(1,8) when b_29 else
                f1_8;

  pOut1 <= pacings.Pacings_1_sel4_pacingOut1;

  -- register begin
  t_29_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_29 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_29 <= result_137;
      end if;
    end if;
  end process;
  -- register end

  result_selection_88 <= pOut0;

  result_137 <= result_138 when result_selection_88 else
                t_29;

  b_30 <= t_29 = to_unsigned(112,8);

  f1_9 <= t_29 + to_unsigned(1,8);

  result_138 <= to_unsigned(1,8) when b_30 else
                f1_9;

  pOut0 <= pacings.Pacings_1_sel3_pacingOut0;

  -- register begin
  t_30_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_30 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_30 <= result_139;
      end if;
    end if;
  end process;
  -- register end

  result_selection_92 <= pIn2;

  result_139 <= result_140 when result_selection_92 else
                t_30;

  b_31 <= t_30 = to_unsigned(112,8);

  f1_10 <= t_30 + to_unsigned(1,8);

  result_140 <= to_unsigned(1,8) when b_31 else
                f1_10;

  pIn2 <= pacings.Pacings_1_sel2_pacingIn2;

  -- register begin
  t_31_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_31 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_31 <= result_141;
      end if;
    end if;
  end process;
  -- register end

  result_selection_96 <= pIn1;

  result_141 <= result_142 when result_selection_96 else
                t_31;

  b_32 <= t_31 = to_unsigned(112,8);

  f1_11 <= t_31 + to_unsigned(1,8);

  result_142 <= to_unsigned(1,8) when b_32 else
                f1_11;

  pIn1 <= pacings.Pacings_1_sel1_pacingIn1;

  -- register begin
  t_32_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_32 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_32 <= result_143;
      end if;
    end if;
  end process;
  -- register end

  result_selection_100 <= pIn0;

  result_143 <= result_144 when result_selection_100 else
                t_32;

  b_33 <= t_32 = to_unsigned(112,8);

  f1_12 <= t_32 + to_unsigned(1,8);

  result_144 <= to_unsigned(1,8) when b_33 else
                f1_12;

  pIn0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_145.Tuple3_sel2_Tuple3_0.Tuple3_0_sel2_Pacings_1;

  result_145 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_45\
                , Tuple3_sel1_boolean_1 => \c$app_arg_44\
                , Tuple3_sel2_Tuple3_0 => \c$app_arg_43\ );

  -- register begin
  capp_arg_43_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_43\ <= ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false ) )
  , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
  , Slides_1_sel1_slide1 => false
  , Slides_1_sel2_slide2 => false
  , Slides_1_sel3_slide3 => false
  , Slides_1_sel4_slide4 => false
  , Slides_1_sel5_slide5 => false
  , Slides_1_sel6_slide6 => false )
  , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingIn1 => false
  , Pacings_1_sel2_pacingIn2 => false
  , Pacings_1_sel3_pacingOut0 => false
  , Pacings_1_sel4_pacingOut1 => false
  , Pacings_1_sel5_pacingOut2 => false
  , Pacings_1_sel6_pacingOut3 => false
  , Pacings_1_sel7_pacingOut4 => false
  , Pacings_1_sel8_pacingOut5 => false
  , Pacings_1_sel9_pacingOut6 => false
  , Pacings_1_sel10_pacingOut7 => false
  , Pacings_1_sel11_pacingOut8 => false
  , Pacings_1_sel12_pacingOut9 => false
  , Pacings_1_sel13_pacingOut10 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_43\ <= \c$case_alt\;
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
                    , Slides_1_sel1_slide1 => false
                    , Slides_1_sel2_slide2 => false
                    , Slides_1_sel3_slide3 => false
                    , Slides_1_sel4_slide4 => false
                    , Slides_1_sel5_slide5 => false
                    , Slides_1_sel6_slide6 => false )
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingIn1 => false
                    , Pacings_1_sel2_pacingIn2 => false
                    , Pacings_1_sel3_pacingOut0 => false
                    , Pacings_1_sel4_pacingOut1 => false
                    , Pacings_1_sel5_pacingOut2 => false
                    , Pacings_1_sel6_pacingOut3 => false
                    , Pacings_1_sel7_pacingOut4 => false
                    , Pacings_1_sel8_pacingOut5 => false
                    , Pacings_1_sel9_pacingOut6 => false
                    , Pacings_1_sel10_pacingOut7 => false
                    , Pacings_1_sel11_pacingOut8 => false
                    , Pacings_1_sel12_pacingOut9 => false
                    , Pacings_1_sel13_pacingOut10 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false ) )
                    , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                    , Slides_1_sel1_slide1 => false
                    , Slides_1_sel2_slide2 => false
                    , Slides_1_sel3_slide3 => false
                    , Slides_1_sel4_slide4 => false
                    , Slides_1_sel5_slide5 => false
                    , Slides_1_sel6_slide6 => false )
                    , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingIn1 => false
                    , Pacings_1_sel2_pacingIn2 => false
                    , Pacings_1_sel3_pacingOut0 => false
                    , Pacings_1_sel4_pacingOut1 => false
                    , Pacings_1_sel5_pacingOut2 => false
                    , Pacings_1_sel6_pacingOut3 => false
                    , Pacings_1_sel7_pacingOut4 => false
                    , Pacings_1_sel8_pacingOut5 => false
                    , Pacings_1_sel9_pacingOut6 => false
                    , Pacings_1_sel10_pacingOut7 => false
                    , Pacings_1_sel11_pacingOut8 => false
                    , Pacings_1_sel12_pacingOut9 => false
                    , Pacings_1_sel13_pacingOut10 => false ) );

  with (x_10) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_10) select
    \c$case_alt_3\ <= ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false ) )
                      , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                      , Slides_1_sel1_slide1 => false
                      , Slides_1_sel2_slide2 => false
                      , Slides_1_sel3_slide3 => false
                      , Slides_1_sel4_slide4 => false
                      , Slides_1_sel5_slide5 => false
                      , Slides_1_sel6_slide6 => false )
                      , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingIn1 => false
                      , Pacings_1_sel2_pacingIn2 => false
                      , Pacings_1_sel3_pacingOut0 => false
                      , Pacings_1_sel4_pacingOut1 => false
                      , Pacings_1_sel5_pacingOut2 => false
                      , Pacings_1_sel6_pacingOut3 => false
                      , Pacings_1_sel7_pacingOut4 => false
                      , Pacings_1_sel8_pacingOut5 => false
                      , Pacings_1_sel9_pacingOut6 => false
                      , Pacings_1_sel10_pacingOut7 => false
                      , Pacings_1_sel11_pacingOut8 => false
                      , Pacings_1_sel12_pacingOut9 => false
                      , Pacings_1_sel13_pacingOut10 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec_13 : block
    signal vec_index_20 : integer range 0 to 2-1;
  begin
    vec_index_20 <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index_20);
  end block;
  -- index end

  -- register begin
  capp_arg_44_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_44\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_44\ <= \c$case_alt_5\;
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

  with (x_10) select
    \c$case_alt_9\ <= false when x"0000000000000000",
                      true when others;

  -- register begin
  capp_arg_45_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_45\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_45\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_46\);

  \c$app_arg_46\ <= to_signed(1,64) when x_10 /= to_signed(2,64) else to_signed(0,64);

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
                                         , Slides_1_sel1_slide1 => false
                                         , Slides_1_sel2_slide2 => false
                                         , Slides_1_sel3_slide3 => false
                                         , Slides_1_sel4_slide4 => false
                                         , Slides_1_sel5_slide5 => false
                                         , Slides_1_sel6_slide6 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingIn2 => false
                                         , Pacings_1_sel3_pacingOut0 => false
                                         , Pacings_1_sel4_pacingOut1 => false
                                         , Pacings_1_sel5_pacingOut2 => false
                                         , Pacings_1_sel6_pacingOut3 => false
                                         , Pacings_1_sel7_pacingOut4 => false
                                         , Pacings_1_sel8_pacingOut5 => false
                                         , Pacings_1_sel9_pacingOut6 => false
                                         , Pacings_1_sel10_pacingOut7 => false
                                         , Pacings_1_sel11_pacingOut8 => false
                                         , Pacings_1_sel12_pacingOut9 => false
                                         , Pacings_1_sel13_pacingOut10 => false ) )
                                         , ( Tuple3_0_sel0_Inputs_4 => ( Inputs_4_sel0_input0 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_4_sel1_input1 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Inputs_4_sel2_input2 => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false ) )
                                         , Tuple3_0_sel1_Slides_1 => ( Slides_1_sel0_slide0 => false
                                         , Slides_1_sel1_slide1 => false
                                         , Slides_1_sel2_slide2 => false
                                         , Slides_1_sel3_slide3 => false
                                         , Slides_1_sel4_slide4 => false
                                         , Slides_1_sel5_slide5 => false
                                         , Slides_1_sel6_slide6 => false )
                                         , Tuple3_0_sel2_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingIn1 => false
                                         , Pacings_1_sel2_pacingIn2 => false
                                         , Pacings_1_sel3_pacingOut0 => false
                                         , Pacings_1_sel4_pacingOut1 => false
                                         , Pacings_1_sel5_pacingOut2 => false
                                         , Pacings_1_sel6_pacingOut3 => false
                                         , Pacings_1_sel7_pacingOut4 => false
                                         , Pacings_1_sel8_pacingOut5 => false
                                         , Pacings_1_sel9_pacingOut6 => false
                                         , Pacings_1_sel10_pacingOut7 => false
                                         , Pacings_1_sel11_pacingOut8 => false
                                         , Pacings_1_sel12_pacingOut9 => false
                                         , Pacings_1_sel13_pacingOut10 => false ) ) );
    elsif rising_edge(clk) then
      if en then
        \buffer\ <= \c$buffer_case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$buffer_case_alt\ <= \c$buffer_case_alt_0\ when push else
                         \buffer\;

  \c$vec_27\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec_27\(0 to 2-1),\c$vec_27\(2 to \c$vec_27\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_19_sel0_array_of_Tuple3_0_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_28\ <= (Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(Spec_topEntity_types.array_of_Tuple3_0'(0 => qData)) & Spec_topEntity_types.array_of_Tuple3_0'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_28\(0 to 2-1),\c$vec_28\(2 to \c$vec_28\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_19_sel0_array_of_Tuple3_0_0;

  qData <= result_15.Tuple2_sel1_Tuple3_0;

  x_10 <= cursor;

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

  push <= result_15.Tuple2_sel0_boolean;

  \c$cursor_case_alt_0\ <= cursor when pop else
                           \c$cursor_case_alt_1\;

  \c$cursor_case_alt_1\ <= cursor when \c$cursor_case_scrut\ else
                           (x_10 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_10 = to_signed(2,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_16.Tuple2_0_sel0_boolean;

  with (x_10) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_10 - to_signed(1,64));

  result <= result_16.Tuple2_0_sel1_Outputs;

  result_0 <= result.Outputs_sel0_output0;

  result_1 <= result.Outputs_sel1_output1;

  result_2 <= result.Outputs_sel2_output2;

  result_3 <= result.Outputs_sel3_output3;

  result_4 <= result.Outputs_sel4_output4;

  result_5 <= result.Outputs_sel5_output5;

  result_6 <= result.Outputs_sel6_output6;

  result_7 <= result.Outputs_sel7_output7;

  result_8 <= result.Outputs_sel8_output8;

  result_9 <= result.Outputs_sel9_output9;

  result_10 <= result.Outputs_sel10_output10;

  result_0_0 <= result_0.ValidInt_sel0_value;

  result_0_1 <= result_0.ValidInt_sel1_valid;

  result_1_0 <= result_1.ValidBool_sel0_value_0;

  result_1_1 <= result_1.ValidBool_sel1_valid_0;

  result_2_0 <= result_2.ValidBool_sel0_value_0;

  result_2_1 <= result_2.ValidBool_sel1_valid_0;

  result_3_0 <= result_3.ValidBool_sel0_value_0;

  result_3_1 <= result_3.ValidBool_sel1_valid_0;

  result_4_0 <= result_4.ValidInt_sel0_value;

  result_4_1 <= result_4.ValidInt_sel1_valid;

  result_5_0 <= result_5.ValidBool_sel0_value_0;

  result_5_1 <= result_5.ValidBool_sel1_valid_0;

  result_6_0 <= result_6.ValidBool_sel0_value_0;

  result_6_1 <= result_6.ValidBool_sel1_valid_0;

  result_7_0 <= result_7.ValidBool_sel0_value_0;

  result_7_1 <= result_7.ValidBool_sel1_valid_0;

  result_8_0 <= result_8.ValidBool_sel0_value_0;

  result_8_1 <= result_8.ValidBool_sel1_valid_0;

  result_9_0 <= result_9.ValidBool_sel0_value_0;

  result_9_1 <= result_9.ValidBool_sel1_valid_0;

  result_10_0 <= result_10.ValidBool_sel0_value_0;

  result_10_1 <= result_10.ValidBool_sel1_valid_0;


end;

