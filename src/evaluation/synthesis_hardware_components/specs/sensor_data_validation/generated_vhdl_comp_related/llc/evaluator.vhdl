library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity evaluator is
    port (
        clk, input_clk, rst : in std_logic;
        input_time : in unsigned(63 downto 0);
		gps_x : in signed(63 downto 0);
		gps_x_en : in std_logic;
		num_satellites : in signed(63 downto 0);
		num_satellites_en : in std_logic;
		imu_acc_x : in signed(63 downto 0);
		imu_acc_x_en : in std_logic;
		gps_emitted_enough_en : in std_logic;
		few_satellites_en : in std_logic;
		is_unreliable_gps_data_en : in std_logic;
		gps_emitted_enough : out std_logic;
		few_satellites : out std_logic;
		is_unreliable_gps_data : out std_logic;
        done : out std_logic;
        valid : out std_logic
    );
end evaluator;

--* Specification:
--* input gps_x : Int64
--* input num_satellites : Int64
--* input imu_acc_x : Int64
--* output gps_emitted_enough := (gps_x.aggregate(over: 3s, using: count) < 10)
--* output few_satellites := (num_satellites < 9)
--* output is_unreliable_gps_data := (few_satellites.aggregate(over: 5s, using: count) > 12)


architecture mixed of evaluator is

    -- Component Declaration
	--* input gps_x : Int64
    component gps_x_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* input num_satellites : Int64
    component num_satellites_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* input imu_acc_x : Int64
    component imu_acc_x_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* output gps_emitted_enough := (gps_x.aggregate(over: 3s, using: count) < 10)
    component gps_emitted_enough_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			gps_x_count_0_sw : in unsigned(63 downto 0);
			gps_x_count_0_sw_data_valid : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output few_satellites := (num_satellites < 9)
    component few_satellites_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			num_satellites_0 : in signed(63 downto 0);
			num_satellites_data_valid_0 : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output is_unreliable_gps_data := (few_satellites.aggregate(over: 5s, using: count) > 12)
    component is_unreliable_gps_data_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			few_satellites_count_1_sw : in unsigned(63 downto 0);
			few_satellites_count_1_sw_data_valid : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* gps_x.aggregate(over: 3 s, using: count)
    component gps_x_count_0_sliding_window_entity
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

	--* few_satellites.aggregate(over: 5 s, using: count)
    component few_satellites_count_1_sliding_window_entity
	    port (
		    clk, rst : in std_logic;
		    evict, upd, request : in std_logic;
            time_in : in unsigned(63 downto 0);
            data_in : in std_logic;
            data_out : out unsigned(63 downto 0);
            data_valid_out : out std_logic;
            evict_done_out : out std_logic;
            upd_done_out : out std_logic;
            request_done_out : out std_logic
	    );
    end component;


    -- Internal Signal Declarations
	signal gps_x_upd : std_logic;
	signal gps_x_upd_done : std_logic;
	signal gps_x_entity_data_0 : signed(63 downto 0);
	signal gps_x_entity_data_valid_0 : std_logic;
	signal num_satellites_upd : std_logic;
	signal num_satellites_upd_done : std_logic;
	signal num_satellites_entity_data_0 : signed(63 downto 0);
	signal num_satellites_entity_data_valid_0 : std_logic;
	signal imu_acc_x_upd : std_logic;
	signal imu_acc_x_upd_done : std_logic;
	signal imu_acc_x_entity_data_0 : signed(63 downto 0);
	signal imu_acc_x_entity_data_valid_0 : std_logic;
	signal gps_emitted_enough_pe : std_logic;
	signal gps_emitted_enough_eval : std_logic;
	signal gps_emitted_enough_pe_done : std_logic;
	signal gps_emitted_enough_eval_done : std_logic;
	signal gps_emitted_enough_entity_data_0 : std_logic;
	signal gps_emitted_enough_entity_data_valid_0 : std_logic;
	signal few_satellites_pe : std_logic;
	signal few_satellites_eval : std_logic;
	signal few_satellites_pe_done : std_logic;
	signal few_satellites_eval_done : std_logic;
	signal few_satellites_entity_data_0 : std_logic;
	signal few_satellites_entity_data_valid_0 : std_logic;
	signal is_unreliable_gps_data_pe : std_logic;
	signal is_unreliable_gps_data_eval : std_logic;
	signal is_unreliable_gps_data_pe_done : std_logic;
	signal is_unreliable_gps_data_eval_done : std_logic;
	signal is_unreliable_gps_data_entity_data_0 : std_logic;
	signal is_unreliable_gps_data_entity_data_valid_0 : std_logic;
	signal gps_x_count_0_evict : std_logic;
	signal gps_x_count_0_upd : std_logic;
	signal gps_x_count_0_request : std_logic;
	signal gps_x_count_0_entity_data : unsigned(63 downto 0);
	signal gps_x_count_0_entity_data_valid : std_logic;
	signal gps_x_count_0_evict_done : std_logic;
	signal gps_x_count_0_upd_done : std_logic;
	signal gps_x_count_0_request_done : std_logic;
	signal few_satellites_count_1_evict : std_logic;
	signal few_satellites_count_1_upd : std_logic;
	signal few_satellites_count_1_request : std_logic;
	signal few_satellites_count_1_entity_data : unsigned(63 downto 0);
	signal few_satellites_count_1_entity_data_valid : std_logic;
	signal few_satellites_count_1_evict_done : std_logic;
	signal few_satellites_count_1_upd_done : std_logic;
	signal few_satellites_count_1_request_done : std_logic;

    signal upd_and_pe_done : std_logic;
    signal evaluator_done : std_logic;
    signal valid_reg : std_logic;
    signal rst_en_done : std_logic;

begin
    -- Component Instantiation
	--* input gps_x : Int64
    gps_x_entity_instance: gps_x_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => gps_x_upd,
            data_in => gps_x,
			data_out(0) => gps_x_entity_data_0,
			data_valid_out(0) => gps_x_entity_data_valid_0,
            done_out => gps_x_upd_done
         );

	--* input num_satellites : Int64
    num_satellites_entity_instance: num_satellites_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => num_satellites_upd,
            data_in => num_satellites,
			data_out(0) => num_satellites_entity_data_0,
			data_valid_out(0) => num_satellites_entity_data_valid_0,
            done_out => num_satellites_upd_done
         );

	--* input imu_acc_x : Int64
    imu_acc_x_entity_instance: imu_acc_x_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => imu_acc_x_upd,
            data_in => imu_acc_x,
			data_out(0) => imu_acc_x_entity_data_0,
			data_valid_out(0) => imu_acc_x_entity_data_valid_0,
            done_out => imu_acc_x_upd_done
         );

	--* output gps_emitted_enough := (gps_x.aggregate(over: 3s, using: count) < 10)
    gps_emitted_enough_entity_instance: gps_emitted_enough_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => gps_emitted_enough_pe,
            eval => gps_emitted_enough_eval,
			gps_x_count_0_sw => gps_x_count_0_entity_data,
			gps_x_count_0_sw_data_valid => gps_x_count_0_entity_data_valid,
			data_out(0) => gps_emitted_enough_entity_data_0,
			data_valid_out(0) => gps_emitted_enough_entity_data_valid_0,
            pe_done_out => gps_emitted_enough_pe_done,
            eval_done_out => gps_emitted_enough_eval_done
        );

	--* output few_satellites := (num_satellites < 9)
    few_satellites_entity_instance: few_satellites_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => few_satellites_pe,
            eval => few_satellites_eval,
			num_satellites_0 => num_satellites_entity_data_0,
			num_satellites_data_valid_0 => num_satellites_entity_data_valid_0,
			data_out(0) => few_satellites_entity_data_0,
			data_valid_out(0) => few_satellites_entity_data_valid_0,
            pe_done_out => few_satellites_pe_done,
            eval_done_out => few_satellites_eval_done
        );

	--* output is_unreliable_gps_data := (few_satellites.aggregate(over: 5s, using: count) > 12)
    is_unreliable_gps_data_entity_instance: is_unreliable_gps_data_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => is_unreliable_gps_data_pe,
            eval => is_unreliable_gps_data_eval,
			few_satellites_count_1_sw => few_satellites_count_1_entity_data,
			few_satellites_count_1_sw_data_valid => few_satellites_count_1_entity_data_valid,
			data_out(0) => is_unreliable_gps_data_entity_data_0,
			data_valid_out(0) => is_unreliable_gps_data_entity_data_valid_0,
            pe_done_out => is_unreliable_gps_data_pe_done,
            eval_done_out => is_unreliable_gps_data_eval_done
        );

	--* gps_x.aggregate(over: 3 s, using: count)
    gps_x_count_0_sliding_window_entity_instance: gps_x_count_0_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => gps_x_count_0_evict,
            upd => gps_x_count_0_upd,
            request => gps_x_count_0_request,
            time_in => input_time,
            data_in => gps_x_entity_data_0,
            data_out => gps_x_count_0_entity_data,
            data_valid_out => gps_x_count_0_entity_data_valid,
            evict_done_out => gps_x_count_0_evict_done,
            upd_done_out => gps_x_count_0_upd_done,
            request_done_out => gps_x_count_0_request_done
        );

	--* few_satellites.aggregate(over: 5 s, using: count)
    few_satellites_count_1_sliding_window_entity_instance: few_satellites_count_1_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => few_satellites_count_1_evict,
            upd => few_satellites_count_1_upd,
            request => few_satellites_count_1_request,
            time_in => input_time,
            data_in => few_satellites_entity_data_0,
            data_out => few_satellites_count_1_entity_data,
            data_valid_out => few_satellites_count_1_entity_data_valid,
            evict_done_out => few_satellites_count_1_evict_done,
            upd_done_out => few_satellites_count_1_upd_done,
            request_done_out => few_satellites_count_1_request_done
        );


    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            valid_reg <= '0';
				gps_x_upd <= '0';
				num_satellites_upd <= '0';
				imu_acc_x_upd <= '0';
				gps_emitted_enough_pe <= '0';
				gps_emitted_enough_eval <= '0';
				few_satellites_pe <= '0';
				few_satellites_eval <= '0';
				is_unreliable_gps_data_pe <= '0';
				is_unreliable_gps_data_eval <= '0';
				gps_x_count_0_evict <= '0';
				gps_x_count_0_upd <= '0';
				gps_x_count_0_request <= '0';
				few_satellites_count_1_evict <= '0';
				few_satellites_count_1_upd <= '0';
				few_satellites_count_1_request <= '0';
            upd_and_pe_done <= '1';
            evaluator_done <= '1';
            rst_en_done <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase
            if input_clk = '1' then
                if upd_and_pe_done = '0' then
                    -- Input Stream Updates
                    --* Input Streams in Specification 
					--* - gps_x 
					--* - num_satellites 
					--* - imu_acc_x 
					gps_x_upd <= gps_x_en;
					num_satellites_upd <= num_satellites_en;
					imu_acc_x_upd <= imu_acc_x_en;
                    -- Pseudo Evaluation Phase
                    --* Output Streams in Specification 
					--* - gps_emitted_enough
					--* - few_satellites
					--* - is_unreliable_gps_data
					gps_emitted_enough_pe <= gps_emitted_enough_en;
					few_satellites_pe <= few_satellites_en;
					is_unreliable_gps_data_pe <= is_unreliable_gps_data_en;
                    -- Evict Phase
                    --* Sliding Windows in Specification 
					--* - gps_x.aggregate(over: 3 s, using: count)
					--* - few_satellites.aggregate(over: 5 s, using: count)
					gps_x_count_0_evict <= '1';
					few_satellites_count_1_evict <= '1';
                    upd_and_pe_done <= '1';
                    evaluator_done <= '0';
                else
                    -- Eval Phase
					--* output gps_emitted_enough := (gps_x.aggregate(over: 3s, using: count) < 10)
					--* Evaluation Phase of Output Stream gps_emitted_enough is Influenced by the following Lookups: 
					--* - Window Lookup: gps_x.aggregate(over: 3 s, using: count)
					gps_emitted_enough_eval <= gps_emitted_enough_en and upd_and_pe_done and gps_x_count_0_request_done;
					--* output few_satellites := (num_satellites < 9)
					--* Evaluation Phase of Output Stream few_satellites is Influenced by No Lookup
					few_satellites_eval <= few_satellites_en and upd_and_pe_done;
					--* output is_unreliable_gps_data := (few_satellites.aggregate(over: 5s, using: count) > 12)
					--* Evaluation Phase of Output Stream is_unreliable_gps_data is Influenced by the following Lookups: 
					--* - Window Lookup: few_satellites.aggregate(over: 5 s, using: count)
					is_unreliable_gps_data_eval <= is_unreliable_gps_data_en and upd_and_pe_done and few_satellites_count_1_request_done;
                    -- SW Update Phase
					--* - gps_x.aggregate(over: 3 s, using: count) aggregates over gps_x
					gps_x_count_0_upd <= gps_x_upd_done and upd_and_pe_done;
					--* - few_satellites.aggregate(over: 5 s, using: count) aggregates over few_satellites
					few_satellites_count_1_upd <= few_satellites_eval_done and upd_and_pe_done;
                    -- SW Request Phase
					--* gps_x.aggregate(over: 3 s, using: count) has Source gps_emitted_enough
					gps_x_count_0_request <= gps_emitted_enough_en and upd_and_pe_done and (not gps_x_en or gps_x_count_0_upd_done);
					--* few_satellites.aggregate(over: 5 s, using: count) has Source is_unreliable_gps_data
					few_satellites_count_1_request <= is_unreliable_gps_data_en and upd_and_pe_done and (not few_satellites_en or few_satellites_count_1_upd_done);
                    -- Valid Assignment
					valid_reg <= '1' and gps_emitted_enough_entity_data_valid_0 and few_satellites_entity_data_valid_0 and is_unreliable_gps_data_entity_data_valid_0;
                    -- Evaluator Done assignment
					upd_and_pe_done <= '1' and (not gps_x_en or gps_x_upd_done) and (not num_satellites_en or num_satellites_upd_done) and (not imu_acc_x_en or imu_acc_x_upd_done) and (not gps_emitted_enough_en or gps_emitted_enough_pe_done) and (not few_satellites_en or few_satellites_pe_done) and (not is_unreliable_gps_data_en or is_unreliable_gps_data_pe_done) and (not gps_x_en or gps_x_count_0_evict_done) and (not few_satellites_en or few_satellites_count_1_evict_done);
					evaluator_done <= upd_and_pe_done and (not gps_emitted_enough_en or gps_x_count_0_request_done) and (not gps_emitted_enough_en or gps_emitted_enough_eval_done) and (not few_satellites_en or few_satellites_eval_done) and (not is_unreliable_gps_data_en or few_satellites_count_1_request_done) and (not is_unreliable_gps_data_en or is_unreliable_gps_data_eval_done) and (not gps_x_en or gps_x_count_0_upd_done) and (not few_satellites_en or few_satellites_count_1_upd_done);
                end if;
            else
                upd_and_pe_done <= '0';
				gps_x_upd <= '0';
				num_satellites_upd <= '0';
				imu_acc_x_upd <= '0';
				gps_emitted_enough_pe <= '0';
				gps_emitted_enough_eval <= '0';
				few_satellites_pe <= '0';
				few_satellites_eval <= '0';
				is_unreliable_gps_data_pe <= '0';
				is_unreliable_gps_data_eval <= '0';
				gps_x_count_0_evict <= '0';
				gps_x_count_0_upd <= '0';
				gps_x_count_0_request <= '0';
				few_satellites_count_1_evict <= '0';
				few_satellites_count_1_upd <= '0';
				few_satellites_count_1_request <= '0';
            end if;
        end if;
    end process;

    -- output port assignment
	gps_emitted_enough <= gps_emitted_enough_entity_data_0;
	few_satellites <= few_satellites_entity_data_0;
	is_unreliable_gps_data <= is_unreliable_gps_data_entity_data_0;
    valid <= valid_reg;
    done <= evaluator_done;

end mixed;