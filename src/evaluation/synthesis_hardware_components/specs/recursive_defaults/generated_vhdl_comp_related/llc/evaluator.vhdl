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
		y : in signed(63 downto 0);
		y_en : in std_logic;
		a_en : in std_logic;
		counts_en : in std_logic;
		a : out signed(63 downto 0);
		counts : out unsigned(63 downto 0);
        done : out std_logic;
        valid : out std_logic
    );
end evaluator;

--* Specification:
--* input x : Int64
--* input y : Int64
--* output a := (y + x.offset(by: neg2).defaults(to: (y.offset(by: neg1).defaults(to: 10) + a.offset(by: neg1).defaults(to: x.offset(by: neg1).defaults(to: 20)))))
--* output counts := a.aggregate(over: 0.01s, using: count)


architecture mixed of evaluator is

    -- Component Declaration
	--* input x : Int64
    component x_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(2 downto 0);
		    data_valid_out : out bit_array(2 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* input y : Int64
    component y_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(1 downto 0);
		    data_valid_out : out bit_array(1 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* output a := (y + x.offset(by: neg2).defaults(to: (y.offset(by: neg1).defaults(to: 10) + a.offset(by: neg1).defaults(to: x.offset(by: neg1).defaults(to: 20)))))
    component a_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			x_neg2 : in signed(63 downto 0);
			x_data_valid_neg2 : in std_logic;
			x_neg1 : in signed(63 downto 0);
			x_data_valid_neg1 : in std_logic;
			y_0 : in signed(63 downto 0);
			y_data_valid_0 : in std_logic;
			y_neg1 : in signed(63 downto 0);
			y_data_valid_neg1 : in std_logic;
			a_neg1 : in signed(63 downto 0);
			a_data_valid_neg1 : in std_logic;
		    data_out : out signed64_array(1 downto 0);
		    data_valid_out : out bit_array(1 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output counts := a.aggregate(over: 0.01s, using: count)
    component counts_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			a_count_0_sw : in unsigned(63 downto 0);
			a_count_0_sw_data_valid : in std_logic;
		    data_out : out unsigned64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* a.aggregate(over: 0.01 s, using: count)
    component a_count_0_sliding_window_entity
	    port (
		    clk, rst : in std_logic;
		    evict, upd, request : in std_logic;
            time_in : in unsigned(63 downto 0);
            data_in : in signed(63 downto 0);
            data_out : out unsigned(63 downto 0);
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
	signal x_entity_data_1 : signed(63 downto 0);
	signal x_entity_data_valid_1 : std_logic;
	signal x_entity_data_2 : signed(63 downto 0);
	signal x_entity_data_valid_2 : std_logic;
	signal y_upd : std_logic;
	signal y_upd_done : std_logic;
	signal y_entity_data_0 : signed(63 downto 0);
	signal y_entity_data_valid_0 : std_logic;
	signal y_entity_data_1 : signed(63 downto 0);
	signal y_entity_data_valid_1 : std_logic;
	signal a_pe : std_logic;
	signal a_eval : std_logic;
	signal a_pe_done : std_logic;
	signal a_eval_done : std_logic;
	signal a_entity_data_0 : signed(63 downto 0);
	signal a_entity_data_valid_0 : std_logic;
	signal a_entity_data_1 : signed(63 downto 0);
	signal a_entity_data_valid_1 : std_logic;
	signal counts_pe : std_logic;
	signal counts_eval : std_logic;
	signal counts_pe_done : std_logic;
	signal counts_eval_done : std_logic;
	signal counts_entity_data_0 : unsigned(63 downto 0);
	signal counts_entity_data_valid_0 : std_logic;
	signal a_count_0_evict : std_logic;
	signal a_count_0_upd : std_logic;
	signal a_count_0_request : std_logic;
	signal a_count_0_entity_data : unsigned(63 downto 0);
	signal a_count_0_entity_data_valid : std_logic;
	signal a_count_0_evict_done : std_logic;
	signal a_count_0_upd_done : std_logic;
	signal a_count_0_request_done : std_logic;

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
			data_out(1) => x_entity_data_1,
			data_out(2) => x_entity_data_2,
			data_valid_out(0) => x_entity_data_valid_0,
			data_valid_out(1) => x_entity_data_valid_1,
			data_valid_out(2) => x_entity_data_valid_2,
            done_out => x_upd_done
         );

	--* input y : Int64
    y_entity_instance: y_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => y_upd,
            data_in => y,
			data_out(0) => y_entity_data_0,
			data_out(1) => y_entity_data_1,
			data_valid_out(0) => y_entity_data_valid_0,
			data_valid_out(1) => y_entity_data_valid_1,
            done_out => y_upd_done
         );

	--* output a := (y + x.offset(by: neg2).defaults(to: (y.offset(by: neg1).defaults(to: 10) + a.offset(by: neg1).defaults(to: x.offset(by: neg1).defaults(to: 20)))))
    a_entity_instance: a_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => a_pe,
            eval => a_eval,
			x_neg2 => x_entity_data_2,
			x_data_valid_neg2 => x_entity_data_valid_2,
			x_neg1 => x_entity_data_1,
			x_data_valid_neg1 => x_entity_data_valid_1,
			y_0 => y_entity_data_0,
			y_data_valid_0 => y_entity_data_valid_0,
			y_neg1 => y_entity_data_1,
			y_data_valid_neg1 => y_entity_data_valid_1,
			a_neg1 => a_entity_data_1,
			a_data_valid_neg1 => a_entity_data_valid_1,
			data_out(0) => a_entity_data_0,
			data_out(1) => a_entity_data_1,
			data_valid_out(0) => a_entity_data_valid_0,
			data_valid_out(1) => a_entity_data_valid_1,
            pe_done_out => a_pe_done,
            eval_done_out => a_eval_done
        );

	--* output counts := a.aggregate(over: 0.01s, using: count)
    counts_entity_instance: counts_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => counts_pe,
            eval => counts_eval,
			a_count_0_sw => a_count_0_entity_data,
			a_count_0_sw_data_valid => a_count_0_entity_data_valid,
			data_out(0) => counts_entity_data_0,
			data_valid_out(0) => counts_entity_data_valid_0,
            pe_done_out => counts_pe_done,
            eval_done_out => counts_eval_done
        );

	--* a.aggregate(over: 0.01 s, using: count)
    a_count_0_sliding_window_entity_instance: a_count_0_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => a_count_0_evict,
            upd => a_count_0_upd,
            request => a_count_0_request,
            time_in => input_time,
            data_in => a_entity_data_0,
            data_out => a_count_0_entity_data,
            data_valid_out => a_count_0_entity_data_valid,
            evict_done_out => a_count_0_evict_done,
            upd_done_out => a_count_0_upd_done,
            request_done_out => a_count_0_request_done
        );


    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            valid_reg <= '0';
				x_upd <= '0';
				y_upd <= '0';
				a_pe <= '0';
				a_eval <= '0';
				counts_pe <= '0';
				counts_eval <= '0';
				a_count_0_evict <= '0';
				a_count_0_upd <= '0';
				a_count_0_request <= '0';
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
					--* - y 
					x_upd <= x_en;
					y_upd <= y_en;
                    -- Pseudo Evaluation Phase
                    --* Output Streams in Specification 
					--* - a
					--* - counts
					a_pe <= a_en;
					counts_pe <= counts_en;
                    -- Evict Phase
                    --* Sliding Windows in Specification 
					--* - a.aggregate(over: 0.01 s, using: count)
					a_count_0_evict <= '1';
                    upd_and_pe_done <= '1';
                    evaluator_done <= '0';
                else
                    -- Eval Phase
					--* output a := (y + x.offset(by: neg2).defaults(to: (y.offset(by: neg1).defaults(to: 10) + a.offset(by: neg1).defaults(to: x.offset(by: neg1).defaults(to: 20)))))
					--* Evaluation Phase of Output Stream a is Influenced by No Lookup
					a_eval <= a_en and upd_and_pe_done;
					--* output counts := a.aggregate(over: 0.01s, using: count)
					--* Evaluation Phase of Output Stream counts is Influenced by the following Lookups: 
					--* - Window Lookup: a.aggregate(over: 0.01 s, using: count)
					counts_eval <= counts_en and upd_and_pe_done and a_count_0_request_done;
                    -- SW Update Phase
					--* - a.aggregate(over: 0.01 s, using: count) aggregates over a
					a_count_0_upd <= a_eval_done and upd_and_pe_done;
                    -- SW Request Phase
					--* a.aggregate(over: 0.01 s, using: count) has Source counts
					a_count_0_request <= counts_en and upd_and_pe_done and (not a_en or a_count_0_upd_done);
                    -- Valid Assignment
					valid_reg <= '1' and a_entity_data_valid_0 and counts_entity_data_valid_0;
                    -- Evaluator Done assignment
					upd_and_pe_done <= '1' and (not x_en or x_upd_done) and (not y_en or y_upd_done) and (not a_en or a_pe_done) and (not counts_en or counts_pe_done) and (not a_en or a_count_0_evict_done);
					evaluator_done <= upd_and_pe_done and (not a_en or a_eval_done) and (not counts_en or a_count_0_request_done) and (not counts_en or counts_eval_done) and (not a_en or a_count_0_upd_done);
                end if;
            else
                upd_and_pe_done <= '0';
				x_upd <= '0';
				y_upd <= '0';
				a_pe <= '0';
				a_eval <= '0';
				counts_pe <= '0';
				counts_eval <= '0';
				a_count_0_evict <= '0';
				a_count_0_upd <= '0';
				a_count_0_request <= '0';
            end if;
        end if;
    end process;

    -- output port assignment
	a <= a_entity_data_0;
	counts <= counts_entity_data_0;
    valid <= valid_reg;
    done <= evaluator_done;

end mixed;