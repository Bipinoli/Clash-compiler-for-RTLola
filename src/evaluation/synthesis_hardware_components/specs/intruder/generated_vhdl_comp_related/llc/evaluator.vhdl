library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity evaluator is
    port (
        clk, input_clk, rst : in std_logic;
        input_time : in unsigned(63 downto 0);
		lat : in signed(63 downto 0);
		lat_en : in std_logic;
		lon : in signed(63 downto 0);
		lon_en : in std_logic;
		distance_en : in std_logic;
		closer_en : in std_logic;
		trigger_closer_en : in std_logic;
		is_good_en : in std_logic;
		distance : out signed(63 downto 0);
		closer : out std_logic;
		trigger_closer : out std_logic;
		is_good : out std_logic;
        done : out std_logic;
        valid : out std_logic
    );
end evaluator;

--* Specification:
--* input lat : Int64
--* input lon : Int64
--* output distance := ((249 - lat) + (23 - lon))
--* output closer := (distance.offset(by: neg1).defaults(to: distance) >= distance)
--* output trigger_closer := (closer and (distance < 1))
--* output is_good := (trigger_closer.aggregate(over: 0.01s, using: count) < 5)


architecture mixed of evaluator is

    -- Component Declaration
	--* input lat : Int64
    component lat_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* input lon : Int64
    component lon_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* output distance := ((249 - lat) + (23 - lon))
    component distance_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			lat_0 : in signed(63 downto 0);
			lat_data_valid_0 : in std_logic;
			lon_0 : in signed(63 downto 0);
			lon_data_valid_0 : in std_logic;
		    data_out : out signed64_array(1 downto 0);
		    data_valid_out : out bit_array(1 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output closer := (distance.offset(by: neg1).defaults(to: distance) >= distance)
    component closer_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			distance_neg1 : in signed(63 downto 0);
			distance_data_valid_neg1 : in std_logic;
			distance_0 : in signed(63 downto 0);
			distance_data_valid_0 : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output trigger_closer := (closer and (distance < 1))
    component trigger_closer_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			distance_0 : in signed(63 downto 0);
			distance_data_valid_0 : in std_logic;
			closer_0 : in std_logic;
			closer_data_valid_0 : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output is_good := (trigger_closer.aggregate(over: 0.01s, using: count) < 5)
    component is_good_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			trigger_closer_count_0_sw : in unsigned(63 downto 0);
			trigger_closer_count_0_sw_data_valid : in std_logic;
		    data_out : out bit_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* trigger_closer.aggregate(over: 0.01 s, using: count)
    component trigger_closer_count_0_sliding_window_entity
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
	signal lat_upd : std_logic;
	signal lat_upd_done : std_logic;
	signal lat_entity_data_0 : signed(63 downto 0);
	signal lat_entity_data_valid_0 : std_logic;
	signal lon_upd : std_logic;
	signal lon_upd_done : std_logic;
	signal lon_entity_data_0 : signed(63 downto 0);
	signal lon_entity_data_valid_0 : std_logic;
	signal distance_pe : std_logic;
	signal distance_eval : std_logic;
	signal distance_pe_done : std_logic;
	signal distance_eval_done : std_logic;
	signal distance_entity_data_0 : signed(63 downto 0);
	signal distance_entity_data_valid_0 : std_logic;
	signal distance_entity_data_1 : signed(63 downto 0);
	signal distance_entity_data_valid_1 : std_logic;
	signal closer_pe : std_logic;
	signal closer_eval : std_logic;
	signal closer_pe_done : std_logic;
	signal closer_eval_done : std_logic;
	signal closer_entity_data_0 : std_logic;
	signal closer_entity_data_valid_0 : std_logic;
	signal trigger_closer_pe : std_logic;
	signal trigger_closer_eval : std_logic;
	signal trigger_closer_pe_done : std_logic;
	signal trigger_closer_eval_done : std_logic;
	signal trigger_closer_entity_data_0 : std_logic;
	signal trigger_closer_entity_data_valid_0 : std_logic;
	signal is_good_pe : std_logic;
	signal is_good_eval : std_logic;
	signal is_good_pe_done : std_logic;
	signal is_good_eval_done : std_logic;
	signal is_good_entity_data_0 : std_logic;
	signal is_good_entity_data_valid_0 : std_logic;
	signal trigger_closer_count_0_evict : std_logic;
	signal trigger_closer_count_0_upd : std_logic;
	signal trigger_closer_count_0_request : std_logic;
	signal trigger_closer_count_0_entity_data : unsigned(63 downto 0);
	signal trigger_closer_count_0_entity_data_valid : std_logic;
	signal trigger_closer_count_0_evict_done : std_logic;
	signal trigger_closer_count_0_upd_done : std_logic;
	signal trigger_closer_count_0_request_done : std_logic;

    signal upd_and_pe_done : std_logic;
    signal evaluator_done : std_logic;
    signal valid_reg : std_logic;
    signal rst_en_done : std_logic;

begin
    -- Component Instantiation
	--* input lat : Int64
    lat_entity_instance: lat_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => lat_upd,
            data_in => lat,
			data_out(0) => lat_entity_data_0,
			data_valid_out(0) => lat_entity_data_valid_0,
            done_out => lat_upd_done
         );

	--* input lon : Int64
    lon_entity_instance: lon_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => lon_upd,
            data_in => lon,
			data_out(0) => lon_entity_data_0,
			data_valid_out(0) => lon_entity_data_valid_0,
            done_out => lon_upd_done
         );

	--* output distance := ((249 - lat) + (23 - lon))
    distance_entity_instance: distance_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => distance_pe,
            eval => distance_eval,
			lat_0 => lat_entity_data_0,
			lat_data_valid_0 => lat_entity_data_valid_0,
			lon_0 => lon_entity_data_0,
			lon_data_valid_0 => lon_entity_data_valid_0,
			data_out(0) => distance_entity_data_0,
			data_out(1) => distance_entity_data_1,
			data_valid_out(0) => distance_entity_data_valid_0,
			data_valid_out(1) => distance_entity_data_valid_1,
            pe_done_out => distance_pe_done,
            eval_done_out => distance_eval_done
        );

	--* output closer := (distance.offset(by: neg1).defaults(to: distance) >= distance)
    closer_entity_instance: closer_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => closer_pe,
            eval => closer_eval,
			distance_neg1 => distance_entity_data_1,
			distance_data_valid_neg1 => distance_entity_data_valid_1,
			distance_0 => distance_entity_data_0,
			distance_data_valid_0 => distance_entity_data_valid_0,
			data_out(0) => closer_entity_data_0,
			data_valid_out(0) => closer_entity_data_valid_0,
            pe_done_out => closer_pe_done,
            eval_done_out => closer_eval_done
        );

	--* output trigger_closer := (closer and (distance < 1))
    trigger_closer_entity_instance: trigger_closer_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => trigger_closer_pe,
            eval => trigger_closer_eval,
			distance_0 => distance_entity_data_0,
			distance_data_valid_0 => distance_entity_data_valid_0,
			closer_0 => closer_entity_data_0,
			closer_data_valid_0 => closer_entity_data_valid_0,
			data_out(0) => trigger_closer_entity_data_0,
			data_valid_out(0) => trigger_closer_entity_data_valid_0,
            pe_done_out => trigger_closer_pe_done,
            eval_done_out => trigger_closer_eval_done
        );

	--* output is_good := (trigger_closer.aggregate(over: 0.01s, using: count) < 5)
    is_good_entity_instance: is_good_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => is_good_pe,
            eval => is_good_eval,
			trigger_closer_count_0_sw => trigger_closer_count_0_entity_data,
			trigger_closer_count_0_sw_data_valid => trigger_closer_count_0_entity_data_valid,
			data_out(0) => is_good_entity_data_0,
			data_valid_out(0) => is_good_entity_data_valid_0,
            pe_done_out => is_good_pe_done,
            eval_done_out => is_good_eval_done
        );

	--* trigger_closer.aggregate(over: 0.01 s, using: count)
    trigger_closer_count_0_sliding_window_entity_instance: trigger_closer_count_0_sliding_window_entity
        port map (
            clk => clk,
            rst => rst,
            evict => trigger_closer_count_0_evict,
            upd => trigger_closer_count_0_upd,
            request => trigger_closer_count_0_request,
            time_in => input_time,
            data_in => trigger_closer_entity_data_0,
            data_out => trigger_closer_count_0_entity_data,
            data_valid_out => trigger_closer_count_0_entity_data_valid,
            evict_done_out => trigger_closer_count_0_evict_done,
            upd_done_out => trigger_closer_count_0_upd_done,
            request_done_out => trigger_closer_count_0_request_done
        );


    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            valid_reg <= '0';
				lat_upd <= '0';
				lon_upd <= '0';
				distance_pe <= '0';
				distance_eval <= '0';
				closer_pe <= '0';
				closer_eval <= '0';
				trigger_closer_pe <= '0';
				trigger_closer_eval <= '0';
				is_good_pe <= '0';
				is_good_eval <= '0';
				trigger_closer_count_0_evict <= '0';
				trigger_closer_count_0_upd <= '0';
				trigger_closer_count_0_request <= '0';
            upd_and_pe_done <= '1';
            evaluator_done <= '1';
            rst_en_done <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase
            if input_clk = '1' then
                if upd_and_pe_done = '0' then
                    -- Input Stream Updates
                    --* Input Streams in Specification 
					--* - lat 
					--* - lon 
					lat_upd <= lat_en;
					lon_upd <= lon_en;
                    -- Pseudo Evaluation Phase
                    --* Output Streams in Specification 
					--* - distance
					--* - closer
					--* - trigger_closer
					--* - is_good
					distance_pe <= distance_en;
					closer_pe <= closer_en;
					trigger_closer_pe <= trigger_closer_en;
					is_good_pe <= is_good_en;
                    -- Evict Phase
                    --* Sliding Windows in Specification 
					--* - trigger_closer.aggregate(over: 0.01 s, using: count)
					trigger_closer_count_0_evict <= '1';
                    upd_and_pe_done <= '1';
                    evaluator_done <= '0';
                else
                    -- Eval Phase
					--* output distance := ((249 - lat) + (23 - lon))
					--* Evaluation Phase of Output Stream distance is Influenced by No Lookup
					distance_eval <= distance_en and upd_and_pe_done;
					--* output closer := (distance.offset(by: neg1).defaults(to: distance) >= distance)
					--* Evaluation Phase of Output Stream closer is Influenced by the following Lookups: 
					--* - Synchronous Lookup: distance
					closer_eval <= closer_en and upd_and_pe_done and distance_eval_done;
					--* output trigger_closer := (closer and (distance < 1))
					--* Evaluation Phase of Output Stream trigger_closer is Influenced by the following Lookups: 
					--* - Synchronous Lookup: distance
					--* - Synchronous Lookup: closer
					trigger_closer_eval <= trigger_closer_en and upd_and_pe_done and distance_eval_done and closer_eval_done;
					--* output is_good := (trigger_closer.aggregate(over: 0.01s, using: count) < 5)
					--* Evaluation Phase of Output Stream is_good is Influenced by the following Lookups: 
					--* - Window Lookup: trigger_closer.aggregate(over: 0.01 s, using: count)
					is_good_eval <= is_good_en and upd_and_pe_done and trigger_closer_count_0_request_done;
                    -- SW Update Phase
					--* - trigger_closer.aggregate(over: 0.01 s, using: count) aggregates over trigger_closer
					trigger_closer_count_0_upd <= trigger_closer_eval_done and upd_and_pe_done;
                    -- SW Request Phase
					--* trigger_closer.aggregate(over: 0.01 s, using: count) has Source is_good
					trigger_closer_count_0_request <= is_good_en and upd_and_pe_done and (not trigger_closer_en or trigger_closer_count_0_upd_done);
                    -- Valid Assignment
					valid_reg <= '1' and distance_entity_data_valid_0 and closer_entity_data_valid_0 and trigger_closer_entity_data_valid_0 and is_good_entity_data_valid_0;
                    -- Evaluator Done assignment
					upd_and_pe_done <= '1' and (not lat_en or lat_upd_done) and (not lon_en or lon_upd_done) and (not distance_en or distance_pe_done) and (not closer_en or closer_pe_done) and (not trigger_closer_en or trigger_closer_pe_done) and (not is_good_en or is_good_pe_done) and (not trigger_closer_en or trigger_closer_count_0_evict_done);
					evaluator_done <= upd_and_pe_done and (not distance_en or distance_eval_done) and (not closer_en or closer_eval_done) and (not trigger_closer_en or trigger_closer_eval_done) and (not is_good_en or trigger_closer_count_0_request_done) and (not is_good_en or is_good_eval_done) and (not trigger_closer_en or trigger_closer_count_0_upd_done);
                end if;
            else
                upd_and_pe_done <= '0';
				lat_upd <= '0';
				lon_upd <= '0';
				distance_pe <= '0';
				distance_eval <= '0';
				closer_pe <= '0';
				closer_eval <= '0';
				trigger_closer_pe <= '0';
				trigger_closer_eval <= '0';
				is_good_pe <= '0';
				is_good_eval <= '0';
				trigger_closer_count_0_evict <= '0';
				trigger_closer_count_0_upd <= '0';
				trigger_closer_count_0_request <= '0';
            end if;
        end if;
    end process;

    -- output port assignment
	distance <= distance_entity_data_0;
	closer <= closer_entity_data_0;
	trigger_closer <= trigger_closer_entity_data_0;
	is_good <= is_good_entity_data_0;
    valid <= valid_reg;
    done <= evaluator_done;

end mixed;