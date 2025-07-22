library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity monitor is
    port (
        clk, tclk, qclk, eclk, rst : in std_logic;
        input_time : in std_logic_vector(63 downto 0);
        offline : in std_logic;
        new_input : in std_logic;
		lat_data_in : in std_logic_vector(63 downto 0);
		lat_data_in_new_input : in std_logic;
		lon_data_in : in std_logic_vector(63 downto 0);
		lon_data_in_new_input : in std_logic;
        time_stream : out std_logic_vector(63 downto 0);
		lat_stream: out std_logic_vector(63 downto 0);
		lon_stream: out std_logic_vector(63 downto 0);
		distance_stream: out std_logic_vector(63 downto 0);
		closer_stream: out std_logic;
		trigger_closer_stream: out std_logic;
		is_good_stream: out std_logic;
        lost_data : out std_logic
    );
end monitor;

architecture mixed of monitor is

    -- component declaration
    component high_level_controller is
        port (
            clk, rst : in std_logic;
            system_clk : in std_logic;
            time_data_in : in std_logic_vector(63 downto 0);
            new_input : in std_logic;
			lat_data_in : in std_logic_vector(63 downto 0);
			lat_push_in : std_logic;
			lon_data_in : in std_logic_vector(63 downto 0);
			lon_push_in : std_logic;
			lat_data_out : out signed(63 downto 0);
			lat_en_out : out std_logic;
			lon_data_out : out signed(63 downto 0);
			lon_en_out : out std_logic;
			distance_en_out : out std_logic;
			closer_en_out : out std_logic;
			trigger_closer_en_out : out std_logic;
			is_good_en_out : out std_logic;
            time_data_out : out unsigned(63 downto 0);
            push_data_in_query : out std_logic;
            lost_data : out std_logic
        );
    end component;

    component queue is
        port (
            clk, rst : in std_logic;
            push : in std_logic;
            time_data_in : in unsigned(63 downto 0);
			lat_data_in : in signed(63 downto 0);
			lat_en_in : in std_logic;
			lon_data_in : in signed(63 downto 0);
			lon_en_in : in std_logic;
			distance_en_in : in std_logic;
			closer_en_in : in std_logic;
			trigger_closer_en_in : in std_logic;
			is_good_en_in : in std_logic;
            full : out std_logic;
            pop : in std_logic;
            time_data_out : out unsigned(63 downto 0);
			lat_data_out : out signed(63 downto 0);
			lat_en_out : out std_logic;
			lon_data_out : out signed(63 downto 0);
			lon_en_out : out std_logic;
			distance_en_out : out std_logic;
			closer_en_out : out std_logic;
			trigger_closer_en_out : out std_logic;
			is_good_en_out : out std_logic;
            available : out std_logic
        );
    end component;

    component low_level_controller is
        port (
            clk, eclk, rst : in std_logic;
            time_in : in unsigned(63 downto 0);
			lat : in signed(63 downto 0);
			lat_en : in std_logic;
			lon : in signed(63 downto 0);
			lon_en : in std_logic;
			distance_en : in std_logic;
			closer_en : in std_logic;
			trigger_closer_en : in std_logic;
			is_good_en : in std_logic;
            data_available : in std_logic;
			distance : out signed(63 downto 0);
			closer : out std_logic;
			trigger_closer : out std_logic;
			is_good : out std_logic;
		    pop : out std_logic;
            eval_done : out std_logic
        );
    end component;

    -- signal declarationq
    -- timing_manager
    signal push_data_timing : std_logic;
    signal timing_manager_time : unsigned(63 downto 0);
	signal lat_data_timing : signed(63 downto 0);
	signal lat_en_timing : std_logic;
	signal lon_data_timing : signed(63 downto 0);
	signal lon_en_timing : std_logic;
	signal distance_en_timing : std_logic;
	signal closer_en_timing : std_logic;
	signal trigger_closer_en_timing : std_logic;
	signal is_good_en_timing : std_logic;
    -- query
    signal queue_is_full : std_logic;
    signal queue_time : unsigned(63 downto 0);
    signal pop_queue : std_logic;
	signal lat_data_queue : signed(63 downto 0);
	signal lat_en_queue : std_logic;
	signal lon_data_queue : signed(63 downto 0);
	signal lon_en_queue : std_logic;
	signal distance_en_queue : std_logic;
	signal closer_en_queue : std_logic;
	signal trigger_closer_en_queue : std_logic;
	signal is_good_en_queue : std_logic;
    signal queue_data_available : std_logic;
    -- evaluator
	signal distance_stream_evaluator : signed(63 downto 0);
	signal closer_stream_evaluator : std_logic;
	signal trigger_closer_stream_evaluator : std_logic;
	signal is_good_stream_evaluator : std_logic;
    -- monitor
    signal time_stream_reg : std_logic_vector(63 downto 0);
	signal lat_stream_reg : std_logic_vector(63 downto 0);
	signal lon_stream_reg : std_logic_vector(63 downto 0);
	signal distance_stream_reg : std_logic_vector(63 downto 0);
	signal closer_stream_reg : std_logic;
	signal trigger_closer_stream_reg : std_logic;
	signal is_good_stream_reg : std_logic;

    signal print : std_logic;

begin
    -- component instantiation
    high_level_controller_instance: high_level_controller
        port map (
            clk => tclk,
            system_clk => clk,
            rst => rst,
            time_data_in => input_time,
            new_input => new_input,
			lat_data_in => lat_data_in,
			lat_push_in => lat_data_in_new_input,
			lon_data_in => lon_data_in,
			lon_push_in => lon_data_in_new_input,
			lat_data_out => lat_data_timing,
			lat_en_out => lat_en_timing,
			lon_data_out => lon_data_timing,
			lon_en_out => lon_en_timing,
			distance_en_out => distance_en_timing,
			closer_en_out => closer_en_timing,
			trigger_closer_en_out => trigger_closer_en_timing,
			is_good_en_out => is_good_en_timing,
            time_data_out => timing_manager_time,
            push_data_in_query => push_data_timing,
            lost_data => lost_data
        );

    queue_instance: queue
        port map (
            clk => qclk,
            rst => rst,
            push => push_data_timing,
            time_data_in => timing_manager_time,
			lat_data_in => lat_data_timing,
			lat_en_in => lat_en_timing,
			lon_data_in => lon_data_timing,
			lon_en_in => lon_en_timing,
			distance_en_in => distance_en_timing,
			closer_en_in => closer_en_timing,
			trigger_closer_en_in => trigger_closer_en_timing,
			is_good_en_in => is_good_en_timing,
            full => queue_is_full,
            pop => pop_queue,
            time_data_out => queue_time,
			lat_data_out => lat_data_queue,
			lat_en_out => lat_en_queue,
			lon_data_out => lon_data_queue,
			lon_en_out => lon_en_queue,
			distance_en_out => distance_en_queue,
			closer_en_out => closer_en_queue,
			trigger_closer_en_out => trigger_closer_en_queue,
			is_good_en_out => is_good_en_queue,
            available => queue_data_available
        );

    low_level_controller_instance: low_level_controller
        port map (
            clk => clk,
            eclk => eclk,
            rst => rst,
            time_in => queue_time,
			lat => lat_data_queue,
			lat_en => lat_en_queue,
			lon => lon_data_queue,
			lon_en => lon_en_queue,
			distance_en => distance_en_queue,
			closer_en => closer_en_queue,
			trigger_closer_en => trigger_closer_en_queue,
			is_good_en => is_good_en_queue,
            data_available => queue_data_available,
			distance => distance_stream_evaluator,
			closer => closer_stream_evaluator,
			trigger_closer => trigger_closer_stream_evaluator,
			is_good => is_good_stream_evaluator,
            pop => pop_queue,
            eval_done => print
        );

    process(rst, print) begin
        if (rst = '1') then
            time_stream_reg <= (others => '0');
			lat_stream_reg <= (others => '0');
			lon_stream_reg <= (others => '0');
			distance_stream_reg <= (others => '0');
			closer_stream_reg <= '0';
			trigger_closer_stream_reg <= '0';
			is_good_stream_reg <= '0';
        elsif falling_edge(print) then
            time_stream_reg <= std_logic_vector(queue_time);
			lat_stream_reg <= std_logic_vector(lat_data_queue);
			lon_stream_reg <= std_logic_vector(lon_data_queue);
			distance_stream_reg <= std_logic_vector(distance_stream_evaluator);
			closer_stream_reg <= closer_stream_evaluator;
			trigger_closer_stream_reg <= trigger_closer_stream_evaluator;
			is_good_stream_reg <= is_good_stream_evaluator;
        end if;
    end process;

    time_stream <= time_stream_reg;
	lat_stream <= lat_stream_reg;
	lon_stream <= lon_stream_reg;
	distance_stream <= distance_stream_reg;
	closer_stream <= closer_stream_reg;
	trigger_closer_stream <= trigger_closer_stream_reg;
	is_good_stream <= is_good_stream_reg;

end mixed;
