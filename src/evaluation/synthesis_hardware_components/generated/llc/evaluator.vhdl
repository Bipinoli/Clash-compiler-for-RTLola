library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity evaluator is
    port (
        clk, input_clk, rst : in std_logic;
        input_time : in unsigned(63 downto 0);
		a : in signed(63 downto 0);
		a_en : in std_logic;
		b_en : in std_logic;
		c_en : in std_logic;
		xx_en : in std_logic;
		b : out signed(63 downto 0);
		c : out signed(63 downto 0);
		xx : out signed(63 downto 0);
        done : out std_logic;
        valid : out std_logic
    );
end evaluator;

--* Specification:
--* input a : Int64
--* output b := a.offset(by: neg1).defaults(to: 10)
--* output c := a.offset(by: neg1).defaults(to: 100)
--* output xx := a.hold().defaults(to: 6)


architecture mixed of evaluator is

    -- Component Declaration
	--* input a : Int64
    component a_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(1 downto 0);
		    data_valid_out : out bit_array(1 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* output b := a.offset(by: neg1).defaults(to: 10)
    component b_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			a_neg1 : in signed(63 downto 0);
			a_data_valid_neg1 : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output c := a.offset(by: neg1).defaults(to: 100)
    component c_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			a_neg1 : in signed(63 downto 0);
			a_data_valid_neg1 : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output xx := a.hold().defaults(to: 6)
    component xx_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			a_0 : in signed(63 downto 0);
			a_data_valid_0 : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;


    -- Internal Signal Declarations
	signal a_upd : std_logic;
	signal a_upd_done : std_logic;
	signal a_entity_data_0 : signed(63 downto 0);
	signal a_entity_data_valid_0 : std_logic;
	signal a_entity_data_1 : signed(63 downto 0);
	signal a_entity_data_valid_1 : std_logic;
	signal b_pe : std_logic;
	signal b_eval : std_logic;
	signal b_pe_done : std_logic;
	signal b_eval_done : std_logic;
	signal b_entity_data_0 : signed(63 downto 0);
	signal b_entity_data_valid_0 : std_logic;
	signal c_pe : std_logic;
	signal c_eval : std_logic;
	signal c_pe_done : std_logic;
	signal c_eval_done : std_logic;
	signal c_entity_data_0 : signed(63 downto 0);
	signal c_entity_data_valid_0 : std_logic;
	signal xx_pe : std_logic;
	signal xx_eval : std_logic;
	signal xx_pe_done : std_logic;
	signal xx_eval_done : std_logic;
	signal xx_entity_data_0 : signed(63 downto 0);
	signal xx_entity_data_valid_0 : std_logic;

    signal upd_and_pe_done : std_logic;
    signal evaluator_done : std_logic;
    signal valid_reg : std_logic;
    signal rst_en_done : std_logic;

begin
    -- Component Instantiation
	--* input a : Int64
    a_entity_instance: a_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => a_upd,
            data_in => a,
			data_out(0) => a_entity_data_0,
			data_out(1) => a_entity_data_1,
			data_valid_out(0) => a_entity_data_valid_0,
			data_valid_out(1) => a_entity_data_valid_1,
            done_out => a_upd_done
         );

	--* output b := a.offset(by: neg1).defaults(to: 10)
    b_entity_instance: b_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => b_pe,
            eval => b_eval,
			a_neg1 => a_entity_data_1,
			a_data_valid_neg1 => a_entity_data_valid_1,
			data_out(0) => b_entity_data_0,
			data_valid_out(0) => b_entity_data_valid_0,
            pe_done_out => b_pe_done,
            eval_done_out => b_eval_done
        );

	--* output c := a.offset(by: neg1).defaults(to: 100)
    c_entity_instance: c_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => c_pe,
            eval => c_eval,
			a_neg1 => a_entity_data_1,
			a_data_valid_neg1 => a_entity_data_valid_1,
			data_out(0) => c_entity_data_0,
			data_valid_out(0) => c_entity_data_valid_0,
            pe_done_out => c_pe_done,
            eval_done_out => c_eval_done
        );

	--* output xx := a.hold().defaults(to: 6)
    xx_entity_instance: xx_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => xx_pe,
            eval => xx_eval,
			a_0 => a_entity_data_0,
			a_data_valid_0 => a_entity_data_valid_0,
			data_out(0) => xx_entity_data_0,
			data_valid_out(0) => xx_entity_data_valid_0,
            pe_done_out => xx_pe_done,
            eval_done_out => xx_eval_done
        );


    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            valid_reg <= '0';
				a_upd <= '0';
				b_pe <= '0';
				b_eval <= '0';
				c_pe <= '0';
				c_eval <= '0';
				xx_pe <= '0';
				xx_eval <= '0';
            upd_and_pe_done <= '1';
            evaluator_done <= '1';
            rst_en_done <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase
            if input_clk = '1' then
                if upd_and_pe_done = '0' then
                    -- Input Stream Updates
                    --* Input Streams in Specification 
					--* - a 
					a_upd <= a_en;
                    -- Pseudo Evaluation Phase
                    --* Output Streams in Specification 
					--* - b
					--* - c
					--* - xx
					b_pe <= b_en;
					c_pe <= c_en;
					xx_pe <= xx_en;
                    -- Evict Phase
                    --* Sliding Windows in Specification 
                    upd_and_pe_done <= '1';
                    evaluator_done <= '0';
                else
                    -- Eval Phase
					--* output b := a.offset(by: neg1).defaults(to: 10)
					--* Evaluation Phase of Output Stream b is Influenced by No Lookup
					b_eval <= b_en and upd_and_pe_done;
					--* output c := a.offset(by: neg1).defaults(to: 100)
					--* Evaluation Phase of Output Stream c is Influenced by No Lookup
					c_eval <= c_en and upd_and_pe_done;
					--* output xx := a.hold().defaults(to: 6)
					--* Evaluation Phase of Output Stream xx is Influenced by No Lookup
					xx_eval <= xx_en and upd_and_pe_done;
                    -- SW Update Phase
                    -- SW Request Phase
                    -- Valid Assignment
					valid_reg <= '1' and b_entity_data_valid_0 and c_entity_data_valid_0 and xx_entity_data_valid_0;
                    -- Evaluator Done assignment
					upd_and_pe_done <= '1' and (not a_en or a_upd_done) and (not b_en or b_pe_done) and (not c_en or c_pe_done) and (not xx_en or xx_pe_done);
					evaluator_done <= upd_and_pe_done and (not b_en or b_eval_done) and (not c_en or c_eval_done) and (not xx_en or xx_eval_done);
                end if;
            else
                upd_and_pe_done <= '0';
				a_upd <= '0';
				b_pe <= '0';
				b_eval <= '0';
				c_pe <= '0';
				c_eval <= '0';
				xx_pe <= '0';
				xx_eval <= '0';
            end if;
        end if;
    end process;

    -- output port assignment
	b <= b_entity_data_0;
	c <= c_entity_data_0;
	xx <= xx_entity_data_0;
    valid <= valid_reg;
    done <= evaluator_done;

end mixed;