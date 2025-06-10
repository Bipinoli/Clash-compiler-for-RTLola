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
  signal result_3                       : signed(63 downto 0);
  signal x                              : signed(63 downto 0);
  signal result_4                       : signed(63 downto 0) := (to_signed(0,64));
  signal result_5                       : Spec_topEntity_types.Tuple2;
  -- spec.hs:186:1-82
  signal hasInput0                      : boolean;
  -- spec.hs:186:1-82
  signal pIn0                           : boolean;
  -- spec.hs:186:1-82
  signal timer0Over                     : boolean;
  signal result_6                       : Spec_topEntity_types.Tuple2_0;
  signal \c$app_arg\                    : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_7                       : boolean;
  signal result_8                       : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:368:1-129
  signal result_9                       : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:368:1-129
  signal b                              : boolean;
  -- spec.hs:264:1-91
  signal \c$ds1_case_alt\               : signed(63 downto 0);
  signal \c$app_arg_0\                  : Spec_topEntity_types.array_of_Tags(0 to 1);
  signal result_10                      : Spec_topEntity_types.Tags;
  signal \c$app_arg_1\                  : Spec_topEntity_types.array_of_boolean(0 to 2);
  signal result_11                      : boolean;
  -- spec.hs:230:1-63
  signal \c$ws_app_arg\                 : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:230:1-63
  signal \c$ws_app_arg_0\               : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut\                 : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_0\               : Spec_topEntity_types.Maybe;
  signal result_12                      : signed(63 downto 0);
  -- spec.hs:230:1-63
  signal i                              : Spec_topEntity_types.index_2;
  -- spec.hs:230:1-63
  signal ws                             : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$app_arg_2\                  : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_13                      : boolean;
  signal result_14                      : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:69:34-42
  signal x_0                            : boolean;
  -- spec.hs:359:1-137
  signal result_15                      : Spec_topEntity_types.array_of_Tuple2_1(0 to 1) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:359:1-137
  signal t                              : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:264:1-91
  signal \c$ds1_case_alt_0\             : signed(63 downto 0);
  signal \c$app_arg_3\                  : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_16                      : boolean;
  -- spec.hs:230:1-63
  signal \c$ws_app_arg_1\               : Spec_topEntity_types.array_of_Tuple2_2(0 to 1);
  -- spec.hs:230:1-63
  signal \c$ws_app_arg_2\               : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal \c$case_scrut_1\               : Spec_topEntity_types.Tuple2_1;
  signal \c$case_scrut_2\               : Spec_topEntity_types.Maybe;
  signal result_17                      : signed(63 downto 0);
  -- spec.hs:230:1-63
  signal i_0                            : Spec_topEntity_types.index_2;
  -- spec.hs:230:1-63
  signal ws_0                           : Spec_topEntity_types.array_of_Maybe(0 to 2);
  signal \c$app_arg_4\                  : Spec_topEntity_types.array_of_boolean(0 to 3);
  signal result_18                      : boolean;
  signal result_19                      : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:68:34-42
  signal x_1                            : boolean;
  -- spec.hs:350:1-137
  signal result_20                      : Spec_topEntity_types.array_of_Tuple2_1(0 to 1) := Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) )
                                       , ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                       , Tuple2_1_sel1_signed => to_signed(0,64) ) );
  -- spec.hs:350:1-137
  signal t_0                            : Spec_topEntity_types.array_of_Tuple2_1(0 to 1);
  -- spec.hs:264:1-91
  signal \c$ds1_case_alt_1\             : signed(63 downto 0);
  -- spec.hs:237:1-55
  signal winData                        : signed(63 downto 0);
  -- spec.hs:264:1-91
  signal \c$ds1_case_scrut\             : boolean;
  -- spec.hs:237:1-55
  signal winTag                         : unsigned(7 downto 0);
  signal result_21                      : unsigned(7 downto 0);
  signal \c$app_arg_5\                  : unsigned(7 downto 0);
  signal \c$app_arg_6\                  : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_22                      : boolean;
  signal result_23                      : Spec_topEntity_types.Tuple2_1;
  -- spec.hs:344:1-127
  signal result_24                      : Spec_topEntity_types.Tuple2_1 := ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
, Tuple2_1_sel1_signed => to_signed(0,64) );
  -- spec.hs:344:1-127
  signal b_0                            : boolean;
  -- spec.hs:264:1-91
  signal \c$input0Win_app_arg\          : signed(63 downto 0) := (to_signed(0,64));
  signal \c$app_arg_7\                  : Spec_topEntity_types.array_of_boolean(0 to 1);
  signal result_25                      : boolean;
  signal \c$app_arg_8\                  : Spec_topEntity_types.array_of_Tags(0 to 2);
  signal result_26                      : Spec_topEntity_types.Tags;
  -- spec.hs:334:9-81
  signal t_1                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_27                      : unsigned(7 downto 0);
  -- spec.hs:334:9-81
  signal b_1                            : boolean;
  -- spec.hs:334:9-81
  signal f1                             : unsigned(7 downto 0);
  signal result_28                      : unsigned(7 downto 0);
  -- spec.hs:264:1-91
  signal pOut2                          : boolean;
  -- spec.hs:334:9-81
  signal t_2                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_29                      : unsigned(7 downto 0);
  -- spec.hs:334:9-81
  signal b_2                            : boolean;
  -- spec.hs:334:9-81
  signal f1_0                           : unsigned(7 downto 0);
  signal result_30                      : unsigned(7 downto 0);
  -- spec.hs:69:34-42
  signal x_2                            : boolean;
  -- spec.hs:264:1-91
  signal pOut1                          : boolean;
  -- spec.hs:334:9-81
  signal t_3                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_31                      : unsigned(7 downto 0);
  -- spec.hs:334:9-81
  signal b_3                            : boolean;
  -- spec.hs:334:9-81
  signal f1_1                           : unsigned(7 downto 0);
  signal result_32                      : unsigned(7 downto 0);
  -- spec.hs:68:34-42
  signal x_3                            : boolean;
  -- spec.hs:264:1-91
  signal pOut0                          : boolean;
  -- spec.hs:334:9-81
  signal t_4                            : unsigned(7 downto 0) := to_unsigned(1,8);
  signal result_33                      : unsigned(7 downto 0);
  -- spec.hs:334:9-81
  signal b_4                            : boolean;
  -- spec.hs:334:9-81
  signal f1_2                           : unsigned(7 downto 0);
  signal result_34                      : unsigned(7 downto 0);
  -- spec.hs:264:1-91
  signal pIn0_0                         : boolean;
  -- spec.hs:264:1-91
  signal pacings                        : Spec_topEntity_types.Pacings_1;
  signal result_35                      : Spec_topEntity_types.Tuple3;
  signal \c$app_arg_9\                  : Spec_topEntity_types.Tuple2_3 := ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
, ValidInt_sel1_valid => false )
, Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
, Pacings_1_sel1_pacingOut0 => false
, Pacings_1_sel2_pacingOut1 => false
, Pacings_1_sel3_pacingOut2 => false ) );
  signal \c$case_alt\                   : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_0\                 : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_1\                 : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_2\                 : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_3\                 : Spec_topEntity_types.Tuple2_3;
  signal \c$case_alt_4\                 : Spec_topEntity_types.Tuple2_3;
  signal \c$app_arg_10\                 : boolean := false;
  signal \c$case_alt_5\                 : boolean;
  signal \c$case_alt_6\                 : boolean;
  signal \c$case_alt_7\                 : boolean;
  signal \c$case_alt_8\                 : boolean;
  signal \c$case_alt_9\                 : boolean;
  signal \c$app_arg_11\                 : boolean := false;
  signal \c$case_alt_10\                : boolean;
  signal \c$case_alt_11\                : boolean;
  signal \c$case_alt_12\                : boolean;
  signal \c$app_arg_12\                 : signed(63 downto 0);
  -- spec.hs:117:1-78
  signal \buffer\                       : Spec_topEntity_types.array_of_Tuple2_3(0 to 1) := Spec_topEntity_types.array_of_Tuple2_3'( ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false ) )
                                       , ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                       , ValidInt_sel1_valid => false )
                                       , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                       , Pacings_1_sel1_pacingOut0 => false
                                       , Pacings_1_sel2_pacingOut1 => false
                                       , Pacings_1_sel3_pacingOut2 => false ) ) );
  -- spec.hs:117:1-78
  signal \c$buffer_case_alt\            : Spec_topEntity_types.array_of_Tuple2_3(0 to 1);
  -- spec.hs:117:1-78
  signal \c$buffer_case_alt_0\          : Spec_topEntity_types.array_of_Tuple2_3(0 to 1);
  -- spec.hs:117:1-78
  signal \c$buffer_case_alt_1\          : Spec_topEntity_types.array_of_Tuple2_3(0 to 1);
  -- spec.hs:117:1-78
  signal qData                          : Spec_topEntity_types.Tuple2_3;
  -- spec.hs:117:1-78
  signal x_4                            : signed(63 downto 0);
  -- spec.hs:117:1-78
  signal cursor                         : signed(63 downto 0) := (to_signed(0,64));
  -- spec.hs:117:1-78
  signal \c$cursor_case_alt\            : signed(63 downto 0);
  -- spec.hs:117:1-78
  signal push                           : boolean;
  -- spec.hs:117:1-78
  signal \c$cursor_case_alt_0\          : signed(63 downto 0);
  -- spec.hs:117:1-78
  signal \c$cursor_case_alt_1\          : signed(63 downto 0);
  -- spec.hs:117:1-78
  signal \c$cursor_case_scrut\          : boolean;
  -- spec.hs:117:1-78
  signal \c$cursor_app_arg\             : signed(63 downto 0);
  -- spec.hs:117:1-78
  signal \c$cursor_case_alt_2\          : signed(63 downto 0);
  -- spec.hs:117:1-78
  signal pop                            : boolean;
  -- spec.hs:117:1-78
  signal \c$cursor_case_alt_3\          : signed(63 downto 0);
  -- spec.hs:117:1-78
  signal \c$cursor_case_alt_4\          : signed(63 downto 0);
  signal inputs                         : Spec_topEntity_types.ValidInt;
  signal \c$ds1_case_alt_selection_res\ : boolean;
  signal \c$vec2\                       : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal result_selection_4             : boolean;
  signal \c$vec\                        : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  signal t_projection_0                 : Spec_topEntity_types.Tuple2_4;
  signal \c$vec2_0\                     : Spec_topEntity_types.array_of_Maybe(0 to 1);
  signal result_selection_8             : boolean;
  signal \c$vec_0\                      : Spec_topEntity_types.array_of_Tuple2_1(0 to 2);
  signal t_projection_2                 : Spec_topEntity_types.Tuple2_4;
  signal result_selection_res           : boolean;
  signal result_selection_13            : boolean;
  signal result_selection_17            : boolean;
  signal result_selection_21            : boolean;
  signal result_selection_25            : boolean;
  signal \c$vec_1\                      : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  signal \c$buffer_case_alt_sel_alt_3\  : Spec_topEntity_types.Tuple2_5;
  signal \c$vec_2\                      : Spec_topEntity_types.array_of_Tuple2_3(0 to 2);
  signal \c$buffer_case_alt_sel_alt_5\  : Spec_topEntity_types.Tuple2_5;
  signal result                         : Spec_topEntity_types.Outputs;
  signal result_0                       : Spec_topEntity_types.ValidInt;
  signal result_1                       : Spec_topEntity_types.ValidInt;
  signal result_2                       : Spec_topEntity_types.ValidInt;

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
              , Tuple2_sel1_Tuple2_3 => ( Tuple2_3_sel0_ValidInt => inputs
              , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => pIn0
              , Pacings_1_sel1_pacingOut0 => pIn0
              , Pacings_1_sel2_pacingOut1 => pIn0
              , Pacings_1_sel3_pacingOut2 => timer0Over ) ) );

  hasInput0 <= inputs.ValidInt_sel1_valid;

  pIn0 <= hasInput0;

  timer0Over <= result_4 >= (to_signed(1000000000,64));

  result_6 <= ( Tuple2_0_sel0_boolean => true
              , Tuple2_0_sel1_Outputs => ( Outputs_sel0_output0 => ( ValidInt_sel0_value => result_17
              , ValidInt_sel1_valid => result_18 )
              , Outputs_sel1_output1 => ( ValidInt_sel0_value => result_12
              , ValidInt_sel1_valid => result_13 )
              , Outputs_sel2_output2 => ( ValidInt_sel0_value => result_9.Tuple2_1_sel1_signed
              , ValidInt_sel1_valid => result_7 ) ) );

  iterateI : block
    signal iterateI_ho1_0_arg0 : boolean;
    signal iterateI_ho1_0_res  : boolean;
    signal iterateI_ho1_1_res  : boolean;
    signal iterateI_ho1_2_res  : boolean;
  begin
    iterateI_ho1_0_arg0 <= pOut2;

    r_block_16 : block
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

    r_block_17 : block
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

    r_block_18 : block
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

    \c$app_arg\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                         , iterateI_ho1_0_res
                                                         , iterateI_ho1_1_res
                                                         , iterateI_ho1_2_res );


  end block;

  result_7 <=  \c$app_arg\(\c$app_arg\'high) ;

  result_8 <= ( Tuple2_1_sel0_unsigned => result_10.Tags_sel3_output2_0
              , Tuple2_1_sel1_signed => \c$ds1_case_alt\ ) when b else
              result_9;

  -- register begin
  result_9_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_9 <= ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_9 <= result_8;
      end if;
    end if;
  end process;
  -- register end

  b <= result_11;

  \c$ds1_case_alt_selection_res\ <= winTag = to_unsigned(4,8);

  \c$ds1_case_alt\ <= to_signed(6,64) when \c$ds1_case_alt_selection_res\ else
                      winData;

  iterateI_0 : block
    signal iterateI_ho1_0_arg0_0 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_0  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_0 <= ( Tags_sel0_input0 => t_4
                             , Tags_sel1_output0_0 => t_3
                             , Tags_sel2_output1_0 => t_2
                             , Tags_sel3_output2_0 => t_1 );

    r_block_19 : block
        signal \c$bb_res_res_2\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_2_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_2\ <= iterateI_ho1_0_arg0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_0 <= \c$bb_res_res_2\;


      end block;

    \c$app_arg_0\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_4
                                                        , Tags_sel1_output0_0 => t_3
                                                        , Tags_sel2_output1_0 => t_2
                                                        , Tags_sel3_output2_0 => t_1 )
                                                        , iterateI_ho1_0_res_0 );


  end block;

  result_10 <=  \c$app_arg_0\(\c$app_arg_0\'high) ;

  iterateI_1 : block
    signal iterateI_ho1_0_res_1 : boolean;
    signal iterateI_ho1_1_res_0 : boolean;
  begin
    r_block_20 : block
        signal \c$bb_res_res_3\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_3_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_3\ <= pOut2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_1 <= \c$bb_res_res_3\;


      end block;

    r_block_21 : block
        signal \c$bb_res_res_4\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_4_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_4\ <= iterateI_ho1_0_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_0 <= \c$bb_res_res_4\;


      end block;

    \c$app_arg_1\ <= Spec_topEntity_types.array_of_boolean'( pOut2
                                                           , iterateI_ho1_0_res_1
                                                           , iterateI_ho1_1_res_0 );


  end block;

  result_11 <=  \c$app_arg_1\(\c$app_arg_1\'high) ;

  \c$vec2\ <= (ws(1 to ws'high));

  -- zipWith begin
  zipWith : for i_1 in \c$ws_app_arg\'range generate
  begin
    \c$ws_app_arg\(i_1) <= ( Tuple2_2_sel0_Tuple2_1 => result_15(i_1)
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
      -- spec.hs:230:1-63
      signal t_5                        : unsigned(7 downto 0);
      -- spec.hs:230:1-63
      signal x_6                        : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res\ : boolean;
    begin
      \c$ws_app_arg_0\(i_2) <= \c$case_alt_13\;

      \c$case_alt_selection_res\ <= t_5 = result_26.Tags_sel2_output1_0;

      \c$case_alt_13\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_2,max(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res\ else
                         \c$ws_app_arg\(i_2).Tuple2_2_sel1_Maybe;

      t_5 <= x_6.Tuple2_1_sel0_unsigned;

      x_6 <= \c$ws_app_arg\(i_2).Tuple2_2_sel0_Tuple2_1;


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
    \c$case_scrut\ <= result_15(vec_index);
  end block;
  -- index end

  \c$case_scrut_0\ <=  ws(0) ;

  with (\c$case_scrut_0\(1 downto 1)) select
    result_12 <= to_signed(0,64) when "0",
                 \c$case_scrut\.Tuple2_1_sel1_signed when others;

  i <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_0\(0 downto 0)));

  ws <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_0\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  iterateI_2 : block
    signal iterateI_ho1_0_arg0_1 : boolean;
    signal iterateI_ho1_0_res_2  : boolean;
    signal iterateI_ho1_1_res_1  : boolean;
    signal iterateI_ho1_2_res_0  : boolean;
  begin
    iterateI_ho1_0_arg0_1 <= x_2;

    r_block_22 : block
        signal \c$bb_res_res_5\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_5_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_5\ <= iterateI_ho1_0_arg0_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_2 <= \c$bb_res_res_5\;


      end block;

    r_block_23 : block
        signal \c$bb_res_res_6\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_6_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_6\ <= iterateI_ho1_0_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_1 <= \c$bb_res_res_6\;


      end block;

    r_block_24 : block
        signal \c$bb_res_res_7\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_7_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_7\ <= iterateI_ho1_1_res_1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_0 <= \c$bb_res_res_7\;


      end block;

    \c$app_arg_2\ <= Spec_topEntity_types.array_of_boolean'( x_2
                                                           , iterateI_ho1_0_res_2
                                                           , iterateI_ho1_1_res_1
                                                           , iterateI_ho1_2_res_0 );


  end block;

  result_13 <=  \c$app_arg_2\(\c$app_arg_2\'high) ;

  result_selection_4 <= x_0;

  result_14 <= t when result_selection_4 else
               result_15;

  x_0 <= result_16;

  -- register begin
  result_15_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_15 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_15 <= result_14;
      end if;
    end if;
  end process;
  -- register end

  \c$vec\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_15) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => t_2
           , Tuple2_1_sel1_signed => \c$ds1_case_alt_0\ )))));

  t_projection_0 <= (\c$vec\(0 to 1-1),\c$vec\(1 to \c$vec\'high));

  t <= t_projection_0.Tuple2_4_sel1_array_of_Tuple2_1_1;

  \c$ds1_case_alt_0\ <= winData when \c$ds1_case_scrut\ else
                        to_signed(100,64);

  iterateI_3 : block
    signal iterateI_ho1_0_res_3 : boolean;
  begin
    r_block_25 : block
        signal \c$bb_res_res_8\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_8_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_8\ <= pOut1;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_3 <= \c$bb_res_res_8\;


      end block;

    \c$app_arg_3\ <= Spec_topEntity_types.array_of_boolean'( pOut1
                                                           , iterateI_ho1_0_res_3 );


  end block;

  result_16 <=  \c$app_arg_3\(\c$app_arg_3\'high) ;

  \c$vec2_0\ <= (ws_0(1 to ws_0'high));

  -- zipWith begin
  zipWith_0 : for i_4 in \c$ws_app_arg_1\'range generate
  begin
    \c$ws_app_arg_1\(i_4) <= ( Tuple2_2_sel0_Tuple2_1 => result_20(i_4)
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
      -- spec.hs:230:1-63
      signal t_7                          : unsigned(7 downto 0);
      -- spec.hs:230:1-63
      signal x_7                          : Spec_topEntity_types.Tuple2_1;
      signal \c$case_alt_selection_res_2\ : boolean;
    begin
      \c$ws_app_arg_2\(i_5) <= \c$case_alt_14\;

      \c$case_alt_selection_res_2\ <= t_7 = result_26.Tags_sel1_output0_0;

      \c$case_alt_14\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(i_5,max_0(1,integer(ceil(log2(real(2))))))))) when \c$case_alt_selection_res_2\ else
                         \c$ws_app_arg_1\(i_5).Tuple2_2_sel1_Maybe;

      t_7 <= x_7.Tuple2_1_sel0_unsigned;

      x_7 <= \c$ws_app_arg_1\(i_5).Tuple2_2_sel0_Tuple2_1;


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
    \c$case_scrut_1\ <= result_20(vec_index_0);
  end block;
  -- index end

  \c$case_scrut_2\ <=  ws_0(0) ;

  with (\c$case_scrut_2\(1 downto 1)) select
    result_17 <= to_signed(0,64) when "0",
                 \c$case_scrut_1\.Tuple2_1_sel1_signed when others;

  i_0 <= Spec_topEntity_types.index_2'(spec_topentity_types.fromSLV(\c$case_scrut_2\(0 downto 0)));

  ws_0 <= Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(\c$ws_app_arg_2\) & Spec_topEntity_types.array_of_Maybe'(Spec_topEntity_types.array_of_Maybe'(0 => std_logic_vector'("0" & "-"))));

  iterateI_4 : block
    signal iterateI_ho1_0_arg0_2 : boolean;
    signal iterateI_ho1_0_res_4  : boolean;
    signal iterateI_ho1_1_res_2  : boolean;
    signal iterateI_ho1_2_res_1  : boolean;
  begin
    iterateI_ho1_0_arg0_2 <= x_3;

    r_block_26 : block
        signal \c$bb_res_res_9\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_9_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_9\ <= iterateI_ho1_0_arg0_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_4 <= \c$bb_res_res_9\;


      end block;

    r_block_27 : block
        signal \c$bb_res_res_10\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_10_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_10\ <= iterateI_ho1_0_res_4;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_2 <= \c$bb_res_res_10\;


      end block;

    r_block_28 : block
        signal \c$bb_res_res_11\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_11_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_11\ <= iterateI_ho1_1_res_2;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_2_res_1 <= \c$bb_res_res_11\;


      end block;

    \c$app_arg_4\ <= Spec_topEntity_types.array_of_boolean'( x_3
                                                           , iterateI_ho1_0_res_4
                                                           , iterateI_ho1_1_res_2
                                                           , iterateI_ho1_2_res_1 );


  end block;

  result_18 <=  \c$app_arg_4\(\c$app_arg_4\'high) ;

  result_selection_8 <= x_1;

  result_19 <= t_0 when result_selection_8 else
               result_20;

  x_1 <= result_22;

  -- register begin
  result_20_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_20 <= Spec_topEntity_types.array_of_Tuple2_1'( ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) )
                                         , ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
                                         , Tuple2_1_sel1_signed => to_signed(0,64) ) );
    elsif rising_edge(clk) then
      if en then
        result_20 <= result_19;
      end if;
    end if;
  end process;
  -- register end

  \c$vec_0\ <= (Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(result_20) & Spec_topEntity_types.array_of_Tuple2_1'(Spec_topEntity_types.array_of_Tuple2_1'(0 => ( Tuple2_1_sel0_unsigned => t_3
             , Tuple2_1_sel1_signed => \c$ds1_case_alt_1\ )))));

  t_projection_2 <= (\c$vec_0\(0 to 1-1),\c$vec_0\(1 to \c$vec_0\'high));

  t_0 <= t_projection_2.Tuple2_4_sel1_array_of_Tuple2_1_1;

  \c$ds1_case_alt_1\ <= winData when \c$ds1_case_scrut\ else
                        to_signed(10,64);

  winData <= result_24.Tuple2_1_sel1_signed;

  \c$ds1_case_scrut\ <= result_21 = winTag;

  winTag <= result_24.Tuple2_1_sel0_unsigned;

  result_selection_res <= t_4 > to_unsigned(1,8);

  result_21 <= \c$app_arg_5\ when result_selection_res else
               \c$app_arg_5\ + to_unsigned(3,8);

  \c$app_arg_5\ <= t_4 - to_unsigned(1,8);

  iterateI_5 : block
    signal iterateI_ho1_0_res_5 : boolean;
  begin
    r_block_29 : block
        signal \c$bb_res_res_12\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_12_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_12\ <= pOut0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_5 <= \c$bb_res_res_12\;


      end block;

    \c$app_arg_6\ <= Spec_topEntity_types.array_of_boolean'( pOut0
                                                           , iterateI_ho1_0_res_5 );


  end block;

  result_22 <=  \c$app_arg_6\(\c$app_arg_6\'high) ;

  result_23 <= ( Tuple2_1_sel0_unsigned => t_4
               , Tuple2_1_sel1_signed => \c$input0Win_app_arg\ ) when b_0 else
               result_24;

  -- register begin
  result_24_register : process(clk,rst)
  begin
    if rst =  '1'  then
      result_24 <= ( Tuple2_1_sel0_unsigned => to_unsigned(4,8)
  , Tuple2_1_sel1_signed => to_signed(0,64) );
    elsif rising_edge(clk) then
      if en then
        result_24 <= result_23;
      end if;
    end if;
  end process;
  -- register end

  b_0 <= result_25;

  -- delay begin
  cinput0Win_app_arg_delay : process(clk)
  begin
    if rising_edge(clk) then
      if en then
        \c$input0Win_app_arg\ <= result_35.Tuple3_sel2_Tuple2_3.Tuple2_3_sel0_ValidInt.ValidInt_sel0_value;
      end if;
    end if;
  end process;
  -- delay end

  iterateI_6 : block
    signal iterateI_ho1_0_res_6 : boolean;
  begin
    r_block_30 : block
        signal \c$bb_res_res_13\ : boolean := false;
      begin
        -- delay begin
        cbb_res_res_13_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_13\ <= pIn0_0;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_6 <= \c$bb_res_res_13\;


      end block;

    \c$app_arg_7\ <= Spec_topEntity_types.array_of_boolean'( pIn0_0
                                                           , iterateI_ho1_0_res_6 );


  end block;

  result_25 <=  \c$app_arg_7\(\c$app_arg_7\'high) ;

  iterateI_7 : block
    signal iterateI_ho1_0_arg0_3 : Spec_topEntity_types.Tags;
    signal iterateI_ho1_0_res_7  : Spec_topEntity_types.Tags;
    signal iterateI_ho1_1_res_3  : Spec_topEntity_types.Tags;
  begin
    iterateI_ho1_0_arg0_3 <= ( Tags_sel0_input0 => t_4
                             , Tags_sel1_output0_0 => t_3
                             , Tags_sel2_output1_0 => t_2
                             , Tags_sel3_output2_0 => t_1 );

    r_block_31 : block
        signal \c$bb_res_res_14\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_14_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_14\ <= iterateI_ho1_0_arg0_3;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_0_res_7 <= \c$bb_res_res_14\;


      end block;

    r_block_32 : block
        signal \c$bb_res_res_15\ : Spec_topEntity_types.Tags := ( Tags_sel0_input0 => to_unsigned(4,8)
      , Tags_sel1_output0_0 => to_unsigned(4,8)
      , Tags_sel2_output1_0 => to_unsigned(4,8)
      , Tags_sel3_output2_0 => to_unsigned(4,8) );
      begin
        -- delay begin
        cbb_res_res_15_delay : process(clk)
        begin
          if rising_edge(clk) then
            if en then
              \c$bb_res_res_15\ <= iterateI_ho1_0_res_7;
            end if;
          end if;
        end process;
        -- delay end

        iterateI_ho1_1_res_3 <= \c$bb_res_res_15\;


      end block;

    \c$app_arg_8\ <= Spec_topEntity_types.array_of_Tags'( ( Tags_sel0_input0 => t_4
                                                        , Tags_sel1_output0_0 => t_3
                                                        , Tags_sel2_output1_0 => t_2
                                                        , Tags_sel3_output2_0 => t_1 )
                                                        , iterateI_ho1_0_res_7
                                                        , iterateI_ho1_1_res_3 );


  end block;

  result_26 <=  \c$app_arg_8\(\c$app_arg_8\'high) ;

  -- register begin
  t_1_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_1 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_1 <= result_27;
      end if;
    end if;
  end process;
  -- register end

  result_selection_13 <= pOut2;

  result_27 <= result_28 when result_selection_13 else
               t_1;

  b_1 <= t_1 = to_unsigned(3,8);

  f1 <= t_1 + to_unsigned(1,8);

  result_28 <= to_unsigned(1,8) when b_1 else
               f1;

  pOut2 <= pacings.Pacings_1_sel3_pacingOut2;

  -- register begin
  t_2_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_2 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_2 <= result_29;
      end if;
    end if;
  end process;
  -- register end

  result_selection_17 <= x_2;

  result_29 <= result_30 when result_selection_17 else
               t_2;

  b_2 <= t_2 = to_unsigned(3,8);

  f1_0 <= t_2 + to_unsigned(1,8);

  result_30 <= to_unsigned(1,8) when b_2 else
               f1_0;

  x_2 <= pOut1;

  pOut1 <= pacings.Pacings_1_sel2_pacingOut1;

  -- register begin
  t_3_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_3 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_3 <= result_31;
      end if;
    end if;
  end process;
  -- register end

  result_selection_21 <= x_3;

  result_31 <= result_32 when result_selection_21 else
               t_3;

  b_3 <= t_3 = to_unsigned(3,8);

  f1_1 <= t_3 + to_unsigned(1,8);

  result_32 <= to_unsigned(1,8) when b_3 else
               f1_1;

  x_3 <= pOut0;

  pOut0 <= pacings.Pacings_1_sel1_pacingOut0;

  -- register begin
  t_4_register : process(clk,rst)
  begin
    if rst =  '1'  then
      t_4 <= to_unsigned(1,8);
    elsif rising_edge(clk) then
      if en then
        t_4 <= result_33;
      end if;
    end if;
  end process;
  -- register end

  result_selection_25 <= pIn0_0;

  result_33 <= result_34 when result_selection_25 else
               t_4;

  b_4 <= t_4 = to_unsigned(3,8);

  f1_2 <= t_4 + to_unsigned(1,8);

  result_34 <= to_unsigned(1,8) when b_4 else
               f1_2;

  pIn0_0 <= pacings.Pacings_1_sel0_pacingIn0;

  pacings <= result_35.Tuple3_sel2_Tuple2_3.Tuple2_3_sel1_Pacings_1;

  result_35 <= ( Tuple3_sel0_boolean_0 => \c$app_arg_11\
               , Tuple3_sel1_boolean_1 => \c$app_arg_10\
               , Tuple3_sel2_Tuple2_3 => \c$app_arg_9\ );

  -- register begin
  capp_arg_9_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_9\ <= ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
  , ValidInt_sel1_valid => false )
  , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
  , Pacings_1_sel1_pacingOut0 => false
  , Pacings_1_sel2_pacingOut1 => false
  , Pacings_1_sel3_pacingOut2 => false ) );
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_9\ <= \c$case_alt\;
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
                    , Pacings_1_sel3_pacingOut2 => false ) );

  \c$case_alt_1\ <= \c$case_alt_3\ when pop else
                    ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                    , ValidInt_sel1_valid => false )
                    , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                    , Pacings_1_sel1_pacingOut0 => false
                    , Pacings_1_sel2_pacingOut1 => false
                    , Pacings_1_sel3_pacingOut2 => false ) );

  with (x_4) select
    \c$case_alt_2\ <= qData when x"0000000000000000",
                      \c$case_alt_4\ when others;

  with (x_4) select
    \c$case_alt_3\ <= ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                      , ValidInt_sel1_valid => false )
                      , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                      , Pacings_1_sel1_pacingOut0 => false
                      , Pacings_1_sel2_pacingOut1 => false
                      , Pacings_1_sel3_pacingOut2 => false ) ) when x"0000000000000000",
                      \c$case_alt_4\ when others;

  -- index begin
  indexVec_1 : block
    signal vec_index_1 : integer range 0 to 2-1;
  begin
    vec_index_1 <= to_integer(\c$cursor_case_alt_4\)
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;
    \c$case_alt_4\ <= \buffer\(vec_index_1);
  end block;
  -- index end

  -- register begin
  capp_arg_10_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_10\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_10\ <= \c$case_alt_5\;
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

  with (x_4) select
    \c$case_alt_9\ <= false when x"0000000000000000",
                      true when others;

  -- register begin
  capp_arg_11_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \c$app_arg_11\ <= false;
    elsif rising_edge(clk) then
      if en then
        \c$app_arg_11\ <= \c$case_alt_10\;
      end if;
    end if;
  end process;
  -- register end

  \c$case_alt_10\ <= \c$case_alt_11\ when push else
                     false;

  \c$case_alt_11\ <= true when pop else
                     \c$case_alt_12\;

  \c$case_alt_12\ <= tagToEnum(\c$app_arg_12\);

  \c$app_arg_12\ <= to_signed(1,64) when x_4 /= to_signed(2,64) else to_signed(0,64);

  -- register begin
  buffer_register : process(clk,rst)
  begin
    if rst =  '1'  then
      \buffer\ <= Spec_topEntity_types.array_of_Tuple2_3'( ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
                                         , Pacings_1_sel1_pacingOut0 => false
                                         , Pacings_1_sel2_pacingOut1 => false
                                         , Pacings_1_sel3_pacingOut2 => false ) )
                                         , ( Tuple2_3_sel0_ValidInt => ( ValidInt_sel0_value => to_signed(0,64)
                                         , ValidInt_sel1_valid => false )
                                         , Tuple2_3_sel1_Pacings_1 => ( Pacings_1_sel0_pacingIn0 => false
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

  \c$vec_1\ <= (Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(0 => qData)) & Spec_topEntity_types.array_of_Tuple2_3'(\buffer\)));

  \c$buffer_case_alt_sel_alt_3\ <= (\c$vec_1\(0 to 2-1),\c$vec_1\(2 to \c$vec_1\'high));

  \c$buffer_case_alt_0\ <= \c$buffer_case_alt_sel_alt_3\.Tuple2_5_sel0_array_of_Tuple2_3_0 when pop else
                           \c$buffer_case_alt_1\;

  \c$vec_2\ <= (Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(Spec_topEntity_types.array_of_Tuple2_3'(0 => qData)) & Spec_topEntity_types.array_of_Tuple2_3'(\buffer\)));

  \c$buffer_case_alt_sel_alt_5\ <= (\c$vec_2\(0 to 2-1),\c$vec_2\(2 to \c$vec_2\'high));

  \c$buffer_case_alt_1\ <= \buffer\ when \c$cursor_case_scrut\ else
                           \c$buffer_case_alt_sel_alt_5\.Tuple2_5_sel0_array_of_Tuple2_3_0;

  qData <= result_5.Tuple2_sel1_Tuple2_3;

  x_4 <= cursor;

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
                           (x_4 + to_signed(1,64));

  \c$cursor_case_scrut\ <= tagToEnum(\c$cursor_app_arg\);

  \c$cursor_app_arg\ <= to_signed(1,64) when x_4 = to_signed(2,64) else to_signed(0,64);

  \c$cursor_case_alt_2\ <= \c$cursor_case_alt_3\ when pop else
                           cursor;

  pop <= result_6.Tuple2_0_sel0_boolean;

  with (x_4) select
    \c$cursor_case_alt_3\ <= to_signed(0,64) when x"0000000000000000",
                             \c$cursor_case_alt_4\ when others;

  \c$cursor_case_alt_4\ <= (x_4 - to_signed(1,64));

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

