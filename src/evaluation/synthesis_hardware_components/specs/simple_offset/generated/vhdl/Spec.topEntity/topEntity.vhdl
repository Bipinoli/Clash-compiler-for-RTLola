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
       result_2_1 : out boolean;
       result_3_0 : out signed(63 downto 0);
       result_3_1 : out boolean);
end;

architecture structural of topEntity is
  signal result_4                         : signed(63 downto 0);
  signal x                                : signed(63 downto 0);
  signal result_5                         : signed(63 downto 0) := (to_signed(0,64));
  signal result_6                         : Spec_topEntity_types.Tuple2;
  -- spec.hs:197:1-82
  signal hasInput0                        : boolean;
  -- spec.hs:197:1-82
  signal pIn0                             : boolean;
  -- spec.hs:197:1-82
  signal timer0Over                       : boolean;
  signal result_7                         : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                      : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_8                         : boolean;
  signal result_9                         : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:406:1-129
  signal result_10                        : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:406:1-129
  signal b                                : boolean;
  -- spec.hs:276:1-91
  signal \c$ds2_case_alt\                 : signed(63 downto 0);
  signal \c$app_arg_0\                    : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_11                        : boolean;
  signal \c$app_arg_1\                    : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_12                        : boolean;
  signal result_13                        : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:77:34-42
  signal x_0                              : boolean;
  -- spec.hs:398:1-147
  signal result_14                        : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:398:1-147
  signal t                                : Spec_topEntity_types.Tuple2_1;
  signal x_1                              : signed(63 downto 0);
  signal y                                : signed(63 downto 0);
  -- spec.hs:242:1-63
  signal \c$ws_app_arg\                   : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_0\                 : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut\                   : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_0\                 : Spec_topEntity_types.Maybe;
  signal result_15                        : signed(63 downto 0);
  -- spec.hs:242:1-63
  signal i                                : Spec_topEntity_types.index_2;
  -- spec.hs:242:1-63
  signal ws                               : Spec_topEntity_types.array_of_Maybe(0 to 2);
  -- spec.hs:276:1-91
  signal \c$ds2_case_alt_0\               : signed(63 downto 0);
  -- spec.hs:255:1-53
  signal tagToMatch                       : unsigned(7 downto 0);
  signal \c$app_arg_2\                    : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_16                        : boolean;
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_1\                 : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:242:1-63
  signal \c$ws_app_arg_2\                 : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut_1\                 : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_2\                 : Spec_topEntity_types.Maybe;
  signal result_17                        : signed(63 downto 0);
  -- spec.hs:242:1-63
  signal i_0                              : Spec_topEntity_types.index_2;
  -- spec.hs:242:1-63
  signal ws_0                             : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$app_arg_3\                    : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_18                        : boolean;
  signal \c$app_arg_4\                    : Spec_topEntity_types.array_of_Tags(0 to 3);
  signal result_19                        : Spec_topEntity_types.Tags;
  signal \c$app_arg_5\                    : Spec_topEntity_types.array_of_boolean(0 to 4);
  signal result_20                        : boolean;
  signal result_21                        : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:76:34-42
  signal x_2                              : boolean;
  -- spec.hs:389:1-137
  signal result_22                        : Spec_topEntity_types.array_of_Tuple2_1(0 to 1) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:389:1-137
  signal t_0                              : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  signal x_3                              : signed(63 downto 0);
  -- spec.hs:276:1-91
  signal \c$out1_case_alt\                : signed(63 downto 0);
  -- spec.hs:249:1-55
  signal winData                          : signed(63 downto 0);
  -- spec.hs:249:1-55
  signal winTag                           : unsigned(7 downto 0);
  signal result_23                        : unsigned(7 downto 0);
  signal \c$app_arg_6\                    : unsigned(7 downto 0);
  signal \c$app_arg_7\                    : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_24                        : boolean;
  signal result_25                        : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:75:34-42
  signal x_4                              : boolean;
  -- spec.hs:381:1-129
  signal result_26                        : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:381:1-129
  signal t_1                              : Spec_topEntity_types.Tuple2_1;
  signal x_5                              : signed(63 downto 0);
  -- spec.hs:276:1-91
  signal \c$out0_case_alt\                : signed(63 downto 0);
  -- spec.hs:255:1-53
  signal dta                              : signed(63 downto 0);
  -- spec.hs:255:1-53
  signal tagToMatch_0                     : unsigned(7 downto 0);
  -- spec.hs:255:1-53
  signal tag                              : unsigned(7 downto 0);
  signal result_27                        : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:375:1-127
  signal result_28                        : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:375:1-127
  signal b_0                              : boolean;
  -- spec.hs:255:1-53
  signal \c$ds_app_arg\                   : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_8\                    : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_29                        : boolean;
  signal \c$app_arg_9\                    : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_30                        : boolean;
  signal \c$app_arg_10\                   : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_31                        : Spec_topEntity_types.Tags;
  signal \c$app_arg_11\                   : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_32                        : Spec_topEntity_types.Tags;
  -- spec.hs:359:9-81
  signal t_2                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_33                        : unsigned(7 downto 0);
  -- spec.hs:359:9-81
  signal b_1                              : boolean;
  -- spec.hs:359:9-81
  signal f1                               : unsigned(7 downto 0);
  signal result_34                        : unsigned(7 downto 0);
  -- spec.hs:276:1-91
  signal pOut3                            : boolean;
  -- spec.hs:359:9-81
  signal t_3                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_35                        : unsigned(7 downto 0);
  -- spec.hs:359:9-81
  signal b_2                              : boolean;
  -- spec.hs:359:9-81
  signal f1_0                             : unsigned(7 downto 0);
  signal result_36                        : unsigned(7 downto 0);
  -- spec.hs:77:34-42
  signal x_6                              : boolean;
  -- spec.hs:276:1-91
  signal pOut2                            : boolean;
  -- spec.hs:359:9-81
  signal t_4                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_37                        : unsigned(7 downto 0);
  -- spec.hs:359:9-81
  signal b_3                              : boolean;
  -- spec.hs:359:9-81
  signal f1_1                             : unsigned(7 downto 0);
  signal result_38                        : unsigned(7 downto 0);
  -- spec.hs:76:34-42
  signal x_7                              : boolean;
  -- spec.hs:276:1-91
  signal pOut1                            : boolean;
  -- spec.hs:359:9-81
  signal t_5                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_39                        : unsigned(7 downto 0);
  -- spec.hs:359:9-81
  signal b_4                              : boolean;
  -- spec.hs:359:9-81
  signal f1_2                             : unsigned(7 downto 0);
  signal result_40                        : unsigned(7 downto 0);
  -- spec.hs:75:34-42
  signal x_8                              : boolean;
  -- spec.hs:276:1-91
  signal pOut0                            : boolean;
  -- spec.hs:359:9-81
  signal t_6                              : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_41                        : unsigned(7 downto 0);
  -- spec.hs:359:9-81
  signal b_5                              : boolean;
  -- spec.hs:359:9-81
  signal f1_3                             : unsigned(7 downto 0);
  signal result_42                        : unsigned(7 downto 0);
  -- spec.hs:276:1-91
  signal pIn0_0                           : boolean;
  -- spec.hs:276:1-91
  signal pacings                          : Spec_topEntity_types.Pacings_1;
  -- spec.hs:366:1-81
  signal toWait                           : signed(63 downto 0) := (to_signed(0,64));
  signal result_43                        : boolean;
  signal result_44                        : signed(63 downto 0);
  -- spec.hs:366:1-81
  signal b_6                              : boolean;
  signal result_45                        : signed(63 downto 0);
  signal x_9                              : signed(63 downto 0);
  signal result_46                        : boolean;
  -- spec.hs:276:1-91
  signal b_7                              : boolean;
  signal result_47                        : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_12\                   : Spec_topEntity_types.Tuple2_3 := ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingOut0 => false
, Pacings_1_sel2_pacingOut1 => false
, Pacings_1_sel3_pacingOut2 => false
, Pacings_1_sel4_pacingOut3 => false ) );
  signal \c$case_alt\                     : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_0\                   : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_1\                   : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_2\                   : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_3\                   : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_4\                   : Spec_topEntity_types.Tuple2_3;
  signal \c$app_arg_13\                   : boolean := false;
  signal \c$case_alt_5\                   : boolean;
  signal \c$case_alt_6\                   : boolean;
  signal \c$case_alt_7\                   : boolean;
  signal \c$case_alt_8\                   : boolean;
  signal \c$case_alt_9\                   : boolean;
  signal \c$app_arg_14\                   : boolean := false;
  signal \c$case_alt_10\                  : boolean;
  signal \c$case_alt_11\                  : boolean;
  signal \c$case_alt_12\                  : boolean;
  signal \c$app_arg_15\                   : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal \buffer\                         : Spec_topEntity_types.array_of_Tuple2_3(0 to 2) := Spec_topEntity_types.array_of_Tuple2_3'( ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false
                                       , Pacings_1_sel4_pacingOut3 => false ) )
                                       , ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false
                                       , Pacings_1_sel4_pacingOut3 => false ) )
                                       , ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false
                                       , Pacings_1_sel4_pacingOut3 => false ) ) );
  -- spec.hs:128:1-78
  signal \c$buffer_case_alt\              : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:128:1-78
  signal \c$buffer_case_alt_0\            : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:128:1-78
  signal \c$buffer_case_alt_1\            : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  -- spec.hs:128:1-78
  signal qData                            : Spec_topEntity_types.Tuple2_3;
  -- spec.hs:128:1-78
  signal x_10                             : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal cursor                           : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt\              : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal push                             : boolean;
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt_0\            : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt_1\            : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal \c$cursor_case_scrut\            : boolean;
  -- spec.hs:128:1-78
  signal \c$cursor_app_arg\               : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt_2\            : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal pop                              : boolean;
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt_3\            : signed(63 downto 0);
  -- spec.hs:128:1-78
  signal \c$cursor_case_alt_4\            : signed(63 downto 0);
  signal inputs                           : Spec_topEntity_types.ValidInt;
  signal \c$ds2_case_alt_selection_res\   : boolean;
  signal result_selection_3               : boolean;
  signal \c$vec2\                         : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$ds2_case_alt_selection_res_0\ : boolean;
  signal \c$vec2_0\                       : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal result_selection_8               : boolean;
  signal \c$vec\                          : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  signal t_projection_1                   : Spec_topEntity_types.Tuple2_4;
  signal \c$out1_case_alt_selection_res\  : boolean;
  signal result_selection_res             : boolean;
  signal result_selection_12              : boolean;
  signal \c$out0_case_alt_selection_res\  : boolean;
  signal result_selection_16              : boolean;
  signal result_selection_20              : boolean;
  signal result_selection_24              : boolean;
  signal result_selection_28              : boolean;
  signal result_selection_32              : boolean;
  signal \c$vec_0\                        : Spec_topEntity_types.array_of_Tuple2_3(0 to 3);
  signal \c$buffer_case_alt_sel_alt_3\    : Spec_topEntity_types.Tuple2_5;
  signal \c$vec_1\                        : Spec_topEntity_types.array_of_Tuple2_3(0 to 3);
  signal \c$buffer_case_alt_sel_alt_5\    : Spec_topEntity_types.Tuple2_5;
  signal result                           : Spec_topEntity_types.Outputs;
  signal result_0                         : Spec_topEntity_types.ValidInt;
  signal result_1                         : Spec_topEntity_types.ValidInt;
  signal result_2                         : Spec_topEntity_types.ValidInt;
  signal result_3                         : Spec_topEntity_types.ValidInt;

begin
  inputs <= ( ValidInt_sel0_value => inputs_0
            , ValidInt_sel1_valid => inputs_1 );

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

  result_6 <= ( Tuple2_sel0_boolean => hasInput0 or timer0Over
              , Tuple2_sel1_Tuple2_3 => ( Tuple2_3_sel0_ValidInt => inputs
              , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => pIn0
              , Pacings_1_sel1_pacingOut0 => pIn0
              , Pacings_1_sel2_pacingOut1 => pIn0
              , Pacings_1_sel3_pacingOut2 => pIn0
              , Pacings_1_sel4_pacingOut3 => timer0Over ) ) );

  hasInput0 <= inputs.ValidInt_sel1_valid;

  pIn0 <= hasInput0;

  timer0Over <= result_5 >= (to_signed(1000000,64));

  result_7 <= ( Tuple2_0_sel0_boolean => result_43 and (not result_46)
              , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidInt_sel0_value => winData
              , ValidInt_sel1_valid => result_20 )
              , Outputs_sel1_output1 => ( ValidInt_sel0_value => result_17
              , ValidInt_sel1_valid => result_18 )
              , Outputs_sel2_output2 => ( ValidInt_sel0_value => result_14.Tuple2_1_sel1_signed
              , ValidInt_sel1_valid => result_12 )
              , Outputs_sel3_output3 => ( ValidInt_sel0_value => result_10.Tuple2_1_sel1_signed
              , ValidInt_sel1_valid => result_8 ) ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
    signal iterateI_ho1_3_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut3;

    r_block_31 : block
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

    r_block_32 : block
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

    r_block_33 : block
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

    r_block_34 : block
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

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res
                                                         , iterateI_ho1_3_res );


  end block;

  result_8 <=  \c$app_arg\(\c$app_arg\'high) ;

  result_9 <= ( Tuple2_1_sel0_unsigned => result_31.Tags_sel4_output3_0
              , Tuple2_1_sel1_signed => \c$ds2_case_alt\ ) when b else
              result_10;

  -- register begin
  result_10_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_10 <= ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_10 <= result_9;
      end if;
    end if;
  end process;
  -- register end

  b <= result_11;

  \c$ds2_case_alt_selection_res\ <= winTag = to_unsigned(4,8);

  \c$ds2_case_alt\ <= to_signed(0,64) when \c$ds2_case_alt_selection_res\ else
                      winData;

  iterateI_0 : block
    signal iterateI_ho1_0_res_0 : boolean;
    signal iterateI_ho1_1_res_0 : boolean;
    signal iterateI_ho1_2_res_0 : boolean;
  begin
    r_block_35 : block
        signal \c$bb_res_res_3\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_3_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_3\ <= pOut3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_0 <= \c$bb_res_res_3\;


      end block;

    r_block_36 : block
        signal \c$bb_res_res_4\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_4_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_4\ <= iterateI_ho1_0_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_0 <= \c$bb_res_res_4\;


      end block;

    r_block_37 : block
        signal \c$bb_res_res_5\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_5_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_5\ <= iterateI_ho1_1_res_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_0 <= \c$bb_res_res_5\;


      end block;

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_boolean'( pOut3
                                                           , iterateI_ho1_0_res_0
                                                           , iterateI_ho1_1_res_0
                                                           , iterateI_ho1_2_res_0 );


  end block;

  result_11 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_arg0_0 : boolean;
    signal iterateI_ho1_0_res_1  : boolean;
    signal iterateI_ho1_1_res_1  : boolean;
    signal iterateI_ho1_2_res_1  : boolean;
    signal iterateI_ho1_3_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_0 <= x_6;

    r_block_38 : block
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

        iterateI_ho1_0_res_1 <= \c$bb_res_res_6\;


      end block;

    r_block_39 : block
        signal \c$bb_res_res_7\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= iterateI_ho1_0_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_1 <= \c$bb_res_res_7\;


      end block;

    r_block_40 : block
        signal \c$bb_res_res_8\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_8_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_8\ <= iterateI_ho1_1_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_1 <= \c$bb_res_res_8\;


      end block;

    r_block_41 : block
        signal \c$bb_res_res_9\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_9_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_9\ <= iterateI_ho1_2_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_0 <= \c$bb_res_res_9\;


      end block;

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_boolean'( x_6
                                                           , iterateI_ho1_0_res_1
                                                           , iterateI_ho1_1_res_1
                                                           , iterateI_ho1_2_res_1
                                                           , iterateI_ho1_3_res_0 );


  end block;

  result_12 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  result_selection_3 <= x_0;

  result_13 <= t when result_selection_3 else
               result_14;

  x_0 <= result_16;

  -- register begin
  result_14_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_14 <= ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_14 <= result_13;
      end if;
    end if;
  end process;
  -- register end

  t <= ( Tuple2_1_sel0_unsigned => result_31.Tags_sel3_output2_0
       , Tuple2_1_sel1_signed => (x_1 + y) );

  x_1 <= \c$ds2_case_alt_0\;

  y <= result_15;

  \c$vec2\ <= (ws(1 to ws'high));

  -- zipWith begin
  zipWith : for i_1 in \c$ws_app_arg\'range generate
  begin
    \c$ws_app_arg\(i_1) <= ( Tuple2_2_sel0_Tuple2_1 => result_22(i_1)
               , Tuple2_2_sel1_Maybe => \c$vec2\(i_1) );


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
    imap_0 : for i_2 in \c$ws_app_arg_0\'range generate
    begin
      fun_1 : block
      signal \c$case_alt_13\            : Spec_topEntity_types.Maybe;
      -- spec.hs:242:1-63
      signal t_8                        : unsigned(7 downto 0);
      -- spec.hs:242:1-63
      signal x_12                       : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res\ : boolean;
    begin
      \c$ws_app_arg_0\(i_2) <= \c$case_alt_13\;

      \c$case_alt_selection_res\ <= t_8 = result_31.Tags_sel2_output1_0;

      \c$case_alt_13\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_2,max(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res\ else
                         \c$ws_app_arg\(i_2).Tuple2_2_sel1_Maybe;

      t_8 <= x_12.Tuple2_1_sel0_unsigned;

      x_12 <= \c$ws_app_arg\(i_2).Tuple2_2_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec : block
    signal vec_index : integer range 0 to 2-1;
  begin
    vec_index <= to_integer((signed(std_logic_vector(resize(i,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut\ <= result_22(vec_index);
  end block;
  -- index end

  \c$case_scrut_0\ <=  ws(0) ;

  with (\c$case_scrut_0\(1 downto 1)) select
    result_15 <= to_signed(0,64) when "0",
                 \c$case_scrut\.Tuple2_1_sel1_signed when others;

  i <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_0\(0 downto 0)));

  ws <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_0\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  \c$ds2_case_alt_selection_res_0\ <= winTag = tagToMatch;

  \c$ds2_case_alt_0\ <= winData when \c$ds2_case_alt_selection_res_0\ else
                        to_signed(0,64);

  tagToMatch <= result_31.Tags_sel1_output0_0;

  iterateI_2 : block
    signal iterateI_ho1_0_res_2 : boolean;
    signal iterateI_ho1_1_res_2 : boolean;
    signal iterateI_ho1_2_res_2 : boolean;
  begin
    r_block_42 : block
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

    r_block_43 : block
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

    r_block_44 : block
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

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                           , iterateI_ho1_0_res_2
                                                           , iterateI_ho1_1_res_2
                                                           , iterateI_ho1_2_res_2 );


  end block;

  result_16 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  \c$vec2_0\ <= (ws_0(1 to ws_0'high));

  -- zipWith begin
  zipWith_0 : for i_4 in \c$ws_app_arg_1\'range generate
  begin
    \c$ws_app_arg_1\(i_4) <= ( Tuple2_2_sel0_Tuple2_1 => result_22(i_4)
               , Tuple2_2_sel1_Maybe => \c$vec2_0\(i_4) );


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
    imap_2 : for i_5 in \c$ws_app_arg_2\'range generate
    begin
      fun_2 : block
      signal \c$case_alt_14\              : Spec_topEntity_types.Maybe;
      -- spec.hs:242:1-63
      signal t_9                          : unsigned(7 downto 0);
      -- spec.hs:242:1-63
      signal x_13                         : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_2\ : boolean;
    begin
      \c$ws_app_arg_2\(i_5) <= \c$case_alt_14\;

      \c$case_alt_selection_res_2\ <= t_9 = result_19.Tags_sel2_output1_0;

      \c$case_alt_14\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_5,max_0(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_2\ else
                         \c$ws_app_arg_1\(i_5).Tuple2_2_sel1_Maybe;

      t_9 <= x_13.Tuple2_1_sel0_unsigned;

      x_13 <= \c$ws_app_arg_1\(i_5).Tuple2_2_sel0_Tuple2_1;


    end block;
    end generate;
  end block;
  -- imap end

  -- index begin
  indexVec_0 : block
    signal vec_index_0 : integer range 0 to 2-1;
  begin
    vec_index_0 <= to_integer((signed(std_logic_vector(resize(i_0,64)))))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_scrut_1\ <= result_22(vec_index_0);
  end block;
  -- index end

  \c$case_scrut_2\ <=  ws_0(0) ;

  with (\c$case_scrut_2\(1 downto 1)) select
    result_17 <= to_signed(0,64) when "0",
                 \c$case_scrut_1\.Tuple2_1_sel1_signed when others;

  i_0 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_2\(0 downto 0)));

  ws_0 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_2\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  iterateI_3 : block
    signal iterateI_ho1_0_arg0_1 : boolean;
    signal iterateI_ho1_0_res_3  : boolean;
    signal iterateI_ho1_1_res_3  : boolean;
    signal iterateI_ho1_2_res_3  : boolean;
    signal iterateI_ho1_3_res_1  : boolean;
  begin
    iterateI_ho1_0_arg0_1 <= x_7;

    r_block_45 : block
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

        iterateI_ho1_0_res_3 <= \c$bb_res_res_13\;


      end block;

    r_block_46 : block
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

    r_block_47 : block
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

    r_block_48 : block
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

        iterateI_ho1_3_res_1 <= \c$bb_res_res_16\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( x_7
                                                           , iterateI_ho1_0_res_3
                                                           , iterateI_ho1_1_res_3
                                                           , iterateI_ho1_2_res_3
                                                           , iterateI_ho1_3_res_1 );


  end block;

  result_18 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  iterateI_4 : block
    signal iterateI_ho1_0_arg0_2 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_4  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_4  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_2_res_4  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_2 <= ( Tags_sel0_input0 => t_6
                             , Tags_sel1_output0_0 => t_5
                             , Tags_sel2_output1_0 => t_4
                             , Tags_sel3_output2_0 => t_3
                             , Tags_sel4_output3_0 => t_2 );

    r_block_49 : block
        signal \c$bb_res_res_17\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
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

    r_block_50 : block
        signal \c$bb_res_res_18\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
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

    r_block_51 : block
        signal \c$bb_res_res_19\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
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

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_6
                                                        , Tags_sel1_output0_0 => t_5
                                                        , Tags_sel2_output1_0 => t_4
                                                        , Tags_sel3_output2_0 => t_3
                                                        , Tags_sel4_output3_0 => t_2 )
                                                        , iterateI_ho1_0_res_4
                                                        , iterateI_ho1_1_res_4
                                                        , iterateI_ho1_2_res_4 );


  end block;

  result_19 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  iterateI_5 : block
    signal iterateI_ho1_0_arg0_3 : boolean;
    signal iterateI_ho1_0_res_5  : boolean;
    signal iterateI_ho1_1_res_5  : boolean;
    signal iterateI_ho1_2_res_5  : boolean;
    signal iterateI_ho1_3_res_2  : boolean;
  begin
    iterateI_ho1_0_arg0_3 <= x_8;

    r_block_52 : block
        signal \c$bb_res_res_20\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_20_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_20\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_20\;


      end block;

    r_block_53 : block
        signal \c$bb_res_res_21\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_21_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_21\ <= iterateI_ho1_0_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_5 <= \c$bb_res_res_21\;


      end block;

    r_block_54 : block
        signal \c$bb_res_res_22\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_22_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_22\ <= iterateI_ho1_1_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_5 <= \c$bb_res_res_22\;


      end block;

    r_block_55 : block
        signal \c$bb_res_res_23\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_23_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_23\ <= iterateI_ho1_2_res_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_3_res_2 <= \c$bb_res_res_23\;


      end block;

    \c$app_arg_5\ <= Spec_topEntity_types.array_of_boolean'( x_8
                                                           , iterateI_ho1_0_res_5
                                                           , iterateI_ho1_1_res_5
                                                           , iterateI_ho1_2_res_5
                                                           , iterateI_ho1_3_res_2 );


  end block;

  result_20 <=  \c$app_arg_5\(\c$app_arg_5\'high) ;

  result_selection_8 <= x_2;

  result_21 <= t_0 when result_selection_8 else
               result_22;

  x_2 <= result_24;

  -- register begin
  result_22_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_22 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_22 <= result_21;
      end if;
    end if;
  end process;
  -- register end

  \c$vec\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_22) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => t_4
           , Tuple2_1_sel1_signed => (x_3 + to_signed(1,64)) )))));

  t_projection_1 <= (\c$vec\(0 to 1-1),\c$vec\(1 to \c$vec\'high));

  t_0 <= t_projection_1.Tuple2_4_sel1_array_of_Tuple2_1_1;

  x_3 <= \c$out1_case_alt\;

  \c$out1_case_alt_selection_res\ <= result_23 = winTag;

  \c$out1_case_alt\ <= winData when \c$out1_case_alt_selection_res\ else
                       to_signed(0,64);

  winData <= result_26.Tuple2_1_sel1_signed;

  winTag <= result_26.Tuple2_1_sel0_unsigned;

  result_selection_res <= t_5 > to_unsigned(1,8);

  result_23 <= \c$app_arg_6\ when result_selection_res else
               \c$app_arg_6\ + to_unsigned(3,8);

  \c$app_arg_6\ <= t_5 - to_unsigned(1,8);

  iterateI_6 : block
    signal iterateI_ho1_0_res_6 : boolean;
  begin
    r_block_56 : block
        signal \c$bb_res_res_24\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_24_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_24\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_24\;


      end block;

    \c$app_arg_7\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                           , iterateI_ho1_0_res_6 );


  end block;

  result_24 <=  \c$app_arg_7\(\c$app_arg_7\'high) ;

  result_selection_12 <= x_4;

  result_25 <= t_1 when result_selection_12 else
               result_26;

  x_4 <= result_30;

  -- register begin
  result_26_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_26 <= ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_26 <= result_25;
      end if;
    end if;
  end process;
  -- register end

  t_1 <= ( Tuple2_1_sel0_unsigned => result_32.Tags_sel1_output0_0
         , Tuple2_1_sel1_signed => (x_5 + to_signed(1,64)) );

  x_5 <= \c$out0_case_alt\;

  \c$out0_case_alt_selection_res\ <= tag = tagToMatch_0;

  \c$out0_case_alt\ <= dta when \c$out0_case_alt_selection_res\ else
                       to_signed(0,64);

  dta <= result_28.Tuple2_1_sel1_signed;

  tagToMatch_0 <= result_32.Tags_sel0_input0;

  tag <= result_28.Tuple2_1_sel0_unsigned;

  result_27 <= ( Tuple2_1_sel0_unsigned => t_6
               , Tuple2_1_sel1_signed => \c$ds_app_arg\ ) when b_0 else
               result_28;

  -- register begin
  result_28_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_28 <= ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_28 <= result_27;
      end if;
    end if;
  end process;
  -- register end

  b_0 <= result_29;

  -- delay begin
  cds_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$ds_app_arg\ <= result_47.Tuple3_sel2_Tuple2_3.Tuple2_3_sel0_ValidInt.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_7 : block
    signal iterateI_ho1_0_res_7 : boolean;
  begin
    r_block_57 : block
        signal \c$bb_res_res_25\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_25_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_25\ <= pIn0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_25\;


      end block;

    \c$app_arg_8\ <= Spec_topEntity_types.array_of_boolean'( pIn0_0
                                                           , iterateI_ho1_0_res_7 );


  end block;

  result_29 <=  \c$app_arg_8\(\c$app_arg_8\'high) ;

  iterateI_8 : block
    signal iterateI_ho1_0_res_8 : boolean;
    signal iterateI_ho1_1_res_6 : boolean;
  begin
    r_block_58 : block
        signal \c$bb_res_res_26\ : boolean := false;
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

        iterateI_ho1_0_res_8 <= \c$bb_res_res_26\;


      end block;

    r_block_59 : block
        signal \c$bb_res_res_27\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_27_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_27\ <= iterateI_ho1_0_res_8;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_6 <= \c$bb_res_res_27\;


      end block;

    \c$app_arg_9\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                           , iterateI_ho1_0_res_8
                                                           , iterateI_ho1_1_res_6 );


  end block;

  result_30 <=  \c$app_arg_9\(\c$app_arg_9\'high) ;

  iterateI_9 : block
    signal iterateI_ho1_0_arg0_4 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_9  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_7  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_4 <= ( Tags_sel0_input0 => t_6
                             , Tags_sel1_output0_0 => t_5
                             , Tags_sel2_output1_0 => t_4
                             , Tags_sel3_output2_0 => t_3
                             , Tags_sel4_output3_0 => t_2 );

    r_block_60 : block
        signal \c$bb_res_res_28\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_28_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_28\ <= iterateI_ho1_0_arg0_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_9 <= \c$bb_res_res_28\;


      end block;

    r_block_61 : block
        signal \c$bb_res_res_29\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_29_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_29\ <= iterateI_ho1_0_res_9;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_7 <= \c$bb_res_res_29\;


      end block;

    \c$app_arg_10\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_6
                                                         , Tags_sel1_output0_0 => t_5
                                                         , Tags_sel2_output1_0 => t_4
                                                         , Tags_sel3_output2_0 => t_3
                                                         , Tags_sel4_output3_0 => t_2 )
                                                         , iterateI_ho1_0_res_9
                                                         , iterateI_ho1_1_res_7 );


  end block;

  result_31 <=  \c$app_arg_10\(\c$app_arg_10\'high) ;

  iterateI_10 : block
    signal iterateI_ho1_0_arg0_5 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_10 : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_5 <= ( Tags_sel0_input0 => t_6
                             , Tags_sel1_output0_0 => t_5
                             , Tags_sel2_output1_0 => t_4
                             , Tags_sel3_output2_0 => t_3
                             , Tags_sel4_output3_0 => t_2 );

    r_block_62 : block
        signal \c$bb_res_res_30\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8)
      , Tags_sel4_output3_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_30_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_30\ <= iterateI_ho1_0_arg0_5;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_10 <= \c$bb_res_res_30\;


      end block;

    \c$app_arg_11\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_6
                                                         , Tags_sel1_output0_0 => t_5
                                                         , Tags_sel2_output1_0 => t_4
                                                         , Tags_sel3_output2_0 => t_3
                                                         , Tags_sel4_output3_0 => t_2 )
                                                         , iterateI_ho1_0_res_10 );


  end block;

  result_32 <=  \c$app_arg_11\(\c$app_arg_11\'high) ;

  -- register begin
  t_2_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_2 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_2 <= result_33;
      end if;
    end if;
  end process;
  -- register end

  result_selection_16 <= pOut3;

  result_33 <= result_34 when result_selection_16 else
               t_2;

  b_1 <= t_2 = to_unsigned(3,8);

  f1 <= t_2 + to_unsigned(1,8);

  result_34 <= to_unsigned(1,8) when b_1 else
               f1;

  pOut3 <= pacings.Pacings_1_sel4_pacingOut3;

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

  result_selection_20 <= x_6;

  result_35 <= result_36 when result_selection_20 else
               t_3;

  b_2 <= t_3 = to_unsigned(3,8);

  f1_0 <= t_3 + to_unsigned(1,8);

  result_36 <= to_unsigned(1,8) when b_2 else
               f1_0;

  x_6 <= pOut2;

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

  result_selection_24 <= x_7;

  result_37 <= result_38 when result_selection_24 else
               t_4;

  b_3 <= t_4 = to_unsigned(3,8);

  f1_1 <= t_4 + to_unsigned(1,8);

  result_38 <= to_unsigned(1,8) when b_3 else
               f1_1;

  x_7 <= pOut1;

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

  result_selection_28 <= x_8;

  result_39 <= result_40 when result_selection_28 else
               t_5;

  b_4 <= t_5 = to_unsigned(3,8);

  f1_2 <= t_5 + to_unsigned(1,8);

  result_40 <= to_unsigned(1,8) when b_4 else
               f1_2;

  x_8 <= pOut0;

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

  result_selection_32 <= pIn0_0;

  result_41 <= result_42 when result_selection_32 else
               t_6;

  b_5 <= t_6 = to_unsigned(3,8);

  f1_3 <= t_6 + to_unsigned(1,8);

  result_42 <= to_unsigned(1,8) when b_5 else
               f1_3;

  pIn0_0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_47.Tuple3_sel2_Tuple2_3.Tuple2_3_sel1_Pacings_1;

  -- register begin
  toWait_register : process(clk,rst)
  begin
    if rst =  '1'  then
      toWait <= (to_signed(0,64));
    elsif rising_edge(clk) then
      if en then
        toWait <= result_44;
      end if;
    end if;
  end process;
  -- register end

  result_43 <= toWait = (to_signed(0,64));

  result_44 <= to_signed(1,64) when result_46 else
               result_45;

  b_6 <= toWait > (to_signed(0,64));

  result_45 <= (x_9 - to_signed(1,64)) when b_6 else
               toWait;

  x_9 <= toWait;

  result_46 <= true when b_7 else
               false;

  b_7 <= result_43 and result_47.Tuple3_sel1_boolean_1;

  result_47 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_14\
               , Tuple3_sel1_boolean_1 => \c$app_arg_13\
               , Tuple3_sel2_Tuple2_3 => \c$app_arg_12\ );

  -- register begin
  capp_arg_12_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_12\ <= ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingOut0 => false
  , Pacings_1_sel2_pacingOut1 => false
  , Pacings_1_sel3_pacingOut2 => false
  , Pacings_1_sel4_pacingOut3 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_12\ <= \c$case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt\ <= \c$case_alt_0\ when push else
                  \c$case_alt_1\;

  \c$case_alt_0\ <= \c$case_alt_2\ when pop else
                    ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false
                    , Pacings_1_sel2_pacingOut1 => false
                    , Pacings_1_sel3_pacingOut2 => false
                    , Pacings_1_sel4_pacingOut3 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false
                    , Pacings_1_sel2_pacingOut1 => false
                    , Pacings_1_sel3_pacingOut2 => false
                    , Pacings_1_sel4_pacingOut3 => false ) );

  with (x_10) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_10) select
    \c$case_alt_3\ <= ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingOut0 => false
                      , Pacings_1_sel2_pacingOut1 => false
                      , Pacings_1_sel3_pacingOut2 => false
                      , Pacings_1_sel4_pacingOut3 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec_1 : block
    signal vec_index_1 : integer range 0 to 3-1;
  begin
    vec_index_1 <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 3
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index_1);
  end block;
  -- index end

  -- register begin
  capp_arg_13_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_13\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_13\ <= \c$case_alt_5\;
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
  capp_arg_14_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_14\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_14\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_15\);

  \c$app_arg_15\ <= to_signed(1,64) when x_10 /= to_signed(3,64) else to_signed(0,64);

  -- register begin
  buffer_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \buffer\ <= Spec_topEntity_types.array_of_Tuple2_3'( ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false
                                         , Pacings_1_sel4_pacingOut3 => false ) )
                                         , ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false
                                         , Pacings_1_sel4_pacingOut3 => false ) )
                                         , ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false
                                         , Pacings_1_sel4_pacingOut3 => false ) ) );
    elsif rising_edge(clk) then
      if en then
        \buffer\ <= \c$buffer_case_alt\;
      end if;
    end if;
  end process;
  -- register end

  \c$buffer_case_alt\ <= \c$buffer_case_alt_0\ when push else
                         \buffer\;

  \c$vec_0\ <= (Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(0 => qData)) & Spec_topEntity_types.array_of_Tuple2_3'(\buffer\)));

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec_0\(0 to 3-1),\c$vec_0\(3 to \c$vec_0\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_5_sel0_array_of_Tuple2_3_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_1\ <= (Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(0 => qData)) & Spec_topEntity_types.array_of_Tuple2_3'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_1\(0 to 3-1),\c$vec_1\(3 to \c$vec_1\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_5_sel0_array_of_Tuple2_3_0;

  qData <= result_6.Tuple2_sel1_Tuple2_3;

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

  push <= result_6.Tuple2_sel0_boolean;

  \c$cursor_case_alt_0\ <= cursor when pop else
                           \c$cursor_case_alt_1\;

  \c$cursor_case_alt_1\ <= cursor when \c$cursor_case_scrut\ else
                           (x_10 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_10 = to_signed(3,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_7.Tuple2_0_sel0_boolean;

  with (x_10) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_10 - to_signed(1,64));

  result <= result_7.Tuple2_0_sel1_Outputs;

  result_0 <= result.Outputs_sel0_output0;

  result_1 <= result.Outputs_sel1_output1;

  result_2 <= result.Outputs_sel2_output2;

  result_3 <= result.Outputs_sel3_output3;

  result_0_0 <= result_0.ValidInt_sel0_value;

  result_0_1 <= result_0.ValidInt_sel1_valid;

  result_1_0 <= result_1.ValidInt_sel0_value;

  result_1_1 <= result_1.ValidInt_sel1_valid;

  result_2_0 <= result_2.ValidInt_sel0_value;

  result_2_1 <= result_2.ValidInt_sel1_valid;

  result_3_0 <= result_3.ValidInt_sel0_value;

  result_3_1 <= result_3.ValidInt_sel1_valid;


end;

