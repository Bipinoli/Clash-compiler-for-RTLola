library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity evaluator is
    port (
        clk, input_clk, rst : in std_logic;
        input_time : in unsigned(63 downto 0);
		x : in signed(63 downto 0);
		x_en : in std_logic;
		a_en : in std_logic;
		a : out signed(63 downto 0);
        done : out std_logic;
        valid : out std_logic
    );
end evaluator;

--* Specification:
--* input x : Int64
--* output a := x.aggregate(over: 0.01s, using: sum)


architecture mixed of evaluator is

    -- Component Declaration
	--* input x : Int64
    component x_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* output a := x.aggregate(over: 0.01s, using: sum)
    component a_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			x_sum_0_sw : in signed(63 downto 0);
			x_sum_0_sw_data_valid : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* x.aggregate(over: 0.01 s, using: sum)
    component x_sum_0_sliding_window_entity
	    port (
		    clk, rst : in std_logic;
		    evict, upd, request : in std_logic;
            time_in : in unsigned(63 downto 0);
            data_in : in signed(63 downto 0);
            data_out : out signed(63 downto 0);
            data_valid_out : out std_logic;
            evict_done_out : out std_logic;
            upd_done_out : out std_logic;
            request_done_out : out std_logic
	    );
    end component;


    -- Internal Signal Declarations
	signal x_upd : std_logic;
	signal x_upd_done : std_logic;
	signal x_entity_data_0 : signed(63 downto 0);
	signal x_entity_data_valid_0 : std_logic;
	signal a_pe : std_logic;
	signal a_eval : std_logic;
	signal a_pe_done : std_logic;
	signal a_eval_done : std_logic;
	signal a_entity_data_0 : signed(63 downto 0);
	signal a_entity_data_valid_0 : std_logic;
	signal x_sum_0_evict : std_logic;
	signal x_sum_0_upd : std_logic;
	signal x_sum_0_request : std_logic;
	signal x_sum_0_entity_data : signed(63 downto 0);
	signal x_sum_0_entity_data_valid : std_logic;
	signal x_sum_0_evict_done : std_logic;
	signal x_sum_0_upd_done : std_logic;
	signal x_sum_0_request_done : std_logic;

    signal upd_and_pe_done : std_logic;
    signal evaluator_done : std_logic;
    signal valid_reg : std_logic;
    signal rst_en_done : std_logic;

begin
    -- Component Instantiation
	--* input x : Int64
    x_entity_instance: x_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => x_upd,
            data_in => x,
			data_out(0) => x_entity_data_0,
			data_valid_out(0) => x_entity_data_valid_0,
            done_out => x_upd_done
         );

	--* output a := x.aggregate(over: 0.01s, using: sum)
    a_entity_instance: a_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => a_pe,
            eval => a_eval,
			x_sum_0_sw => x_sum_0_entity_data,
			x_sum_0_sw_data_valid => x_sum_0_entity_data_valid,
			data_out(0) => a_entity_data_0,
			data_valid_out(0) => a_entity_data_valid_0,
            pe_done_out => a_pe_done,
            eval_done_out => a_eval_done
        );

	--* x.aggregate(over: 0.01 s, using: sum)
    x_sum_0_sliding_window_entity_instance: x_sum_0_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => x_sum_0_evict,
            upd => x_sum_0_upd,
            request => x_sum_0_request,
            time_in => input_time,
            data_in => x_entity_data_0,
            data_out => x_sum_0_entity_data,
            data_valid_out => x_sum_0_entity_data_valid,
            evict_done_out => x_sum_0_evict_done,
            upd_done_out => x_sum_0_upd_done,
            request_done_out => x_sum_0_request_done
        );


    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            valid_reg <= '0';
				x_upd <= '0';
				a_pe <= '0';
				a_eval <= '0';
				x_sum_0_evict <= '0';
				x_sum_0_upd <= '0';
				x_sum_0_request <= '0';
            upd_and_pe_done <= '1';
            evaluator_done <= '1';
            rst_en_done <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase
            if input_clk = '1' then
                if upd_and_pe_done = '0' then
                    -- Input Stream Updates
                    --* Input Streams in Specification 
					--* - x 
					x_upd <= x_en;
                    -- Pseudo Evaluation Phase
                    --* Output Streams in Specification 
					--* - a
					a_pe <= a_en;
                    -- Evict Phase
                    --* Sliding Windows in Specification 
					--* - x.aggregate(over: 0.01 s, using: sum)
					x_sum_0_evict <= '1';
                    upd_and_pe_done <= '1';
                    evaluator_done <= '0';
                else
                    -- Eval Phase
					--* output a := x.aggregate(over: 0.01s, using: sum)
					--* Evaluation Phase of Output Stream a is Influenced by the following Lookups: 
					--* - Window Lookup: x.aggregate(over: 0.01 s, using: sum)
					a_eval <= a_en and upd_and_pe_done and x_sum_0_request_done;
                    -- SW Update Phase
					--* - x.aggregate(over: 0.01 s, using: sum) aggregates over x
					x_sum_0_upd <= x_upd_done and upd_and_pe_done;
                    -- SW Request Phase
					--* x.aggregate(over: 0.01 s, using: sum) has Source a
					x_sum_0_request <= a_en and upd_and_pe_done and (not x_en or x_sum_0_upd_done);
                    -- Valid Assignment
					valid_reg <= '1' and a_entity_data_valid_0;
                    -- Evaluator Done assignment
					upd_and_pe_done <= '1' and (not x_en or x_upd_done) and (not a_en or a_pe_done) and (not x_en or x_sum_0_evict_done);
					evaluator_done <= upd_and_pe_done and (not a_en or x_sum_0_request_done) and (not a_en or a_eval_done) and (not x_en or x_sum_0_upd_done);
                end if;
            else
                upd_and_pe_done <= '0';
				x_upd <= '0';
				a_pe <= '0';
				a_eval <= '0';
				x_sum_0_evict <= '0';
				x_sum_0_upd <= '0';
				x_sum_0_request <= '0';
            end if;
        end if;
    end process;

    -- output port assignment
	a <= a_entity_data_0;
    valid <= valid_reg;
    done <= evaluator_done;

end mixed;