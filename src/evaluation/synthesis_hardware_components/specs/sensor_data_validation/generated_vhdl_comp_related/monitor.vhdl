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
		gps_x_data_in : in std_logic_vector(63 downto 0);
		gps_x_data_in_new_input : in std_logic;
		num_satellites_data_in : in std_logic_vector(63 downto 0);
		num_satellites_data_in_new_input : in std_logic;
		imu_acc_x_data_in : in std_logic_vector(63 downto 0);
		imu_acc_x_data_in_new_input : in std_logic;
        time_stream : out std_logic_vector(63 downto 0);
		gps_x_stream: out std_logic_vector(63 downto 0);
		num_satellites_stream: out std_logic_vector(63 downto 0);
		imu_acc_x_stream: out std_logic_vector(63 downto 0);
		gps_emitted_enough_stream: out std_logic;
		few_satellites_stream: out std_logic;
		is_unreliable_gps_data_stream: out std_logic;
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
			gps_x_data_in : in std_logic_vector(63 downto 0);
			gps_x_push_in : std_logic;
			num_satellites_data_in : in std_logic_vector(63 downto 0);
			num_satellites_push_in : std_logic;
			imu_acc_x_data_in : in std_logic_vector(63 downto 0);
			imu_acc_x_push_in : std_logic;
			gps_x_data_out : out signed(63 downto 0);
			gps_x_en_out : out std_logic;
			num_satellites_data_out : out signed(63 downto 0);
			num_satellites_en_out : out std_logic;
			imu_acc_x_data_out : out signed(63 downto 0);
			imu_acc_x_en_out : out std_logic;
			gps_emitted_enough_en_out : out std_logic;
			few_satellites_en_out : out std_logic;
			is_unreliable_gps_data_en_out : out std_logic;
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
			gps_x_data_in : in signed(63 downto 0);
			gps_x_en_in : in std_logic;
			num_satellites_data_in : in signed(63 downto 0);
			num_satellites_en_in : in std_logic;
			imu_acc_x_data_in : in signed(63 downto 0);
			imu_acc_x_en_in : in std_logic;
			gps_emitted_enough_en_in : in std_logic;
			few_satellites_en_in : in std_logic;
			is_unreliable_gps_data_en_in : in std_logic;
            full : out std_logic;
            pop : in std_logic;
            time_data_out : out unsigned(63 downto 0);
			gps_x_data_out : out signed(63 downto 0);
			gps_x_en_out : out std_logic;
			num_satellites_data_out : out signed(63 downto 0);
			num_satellites_en_out : out std_logic;
			imu_acc_x_data_out : out signed(63 downto 0);
			imu_acc_x_en_out : out std_logic;
			gps_emitted_enough_en_out : out std_logic;
			few_satellites_en_out : out std_logic;
			is_unreliable_gps_data_en_out : out std_logic;
            available : out std_logic
        );
    end component;

    component low_level_controller is
        port (
            clk, eclk, rst : in std_logic;
            time_in : in unsigned(63 downto 0);
			gps_x : in signed(63 downto 0);
			gps_x_en : in std_logic;
			num_satellites : in signed(63 downto 0);
			num_satellites_en : in std_logic;
			imu_acc_x : in signed(63 downto 0);
			imu_acc_x_en : in std_logic;
			gps_emitted_enough_en : in std_logic;
			few_satellites_en : in std_logic;
			is_unreliable_gps_data_en : in std_logic;
            data_available : in std_logic;
			gps_emitted_enough : out std_logic;
			few_satellites : out std_logic;
			is_unreliable_gps_data : out std_logic;
		    pop : out std_logic;
            eval_done : out std_logic
        );
    end component;

    -- signal declarationq
    -- timing_manager
    signal push_data_timing : std_logic;
    signal timing_manager_time : unsigned(63 downto 0);
	signal gps_x_data_timing : signed(63 downto 0);
	signal gps_x_en_timing : std_logic;
	signal num_satellites_data_timing : signed(63 downto 0);
	signal num_satellites_en_timing : std_logic;
	signal imu_acc_x_data_timing : signed(63 downto 0);
	signal imu_acc_x_en_timing : std_logic;
	signal gps_emitted_enough_en_timing : std_logic;
	signal few_satellites_en_timing : std_logic;
	signal is_unreliable_gps_data_en_timing : std_logic;
    -- query
    signal queue_is_full : std_logic;
    signal queue_time : unsigned(63 downto 0);
    signal pop_queue : std_logic;
	signal gps_x_data_queue : signed(63 downto 0);
	signal gps_x_en_queue : std_logic;
	signal num_satellites_data_queue : signed(63 downto 0);
	signal num_satellites_en_queue : std_logic;
	signal imu_acc_x_data_queue : signed(63 downto 0);
	signal imu_acc_x_en_queue : std_logic;
	signal gps_emitted_enough_en_queue : std_logic;
	signal few_satellites_en_queue : std_logic;
	signal is_unreliable_gps_data_en_queue : std_logic;
    signal queue_data_available : std_logic;
    -- evaluator
	signal gps_emitted_enough_stream_evaluator : std_logic;
	signal few_satellites_stream_evaluator : std_logic;
	signal is_unreliable_gps_data_stream_evaluator : std_logic;
    -- monitor
    signal time_stream_reg : std_logic_vector(63 downto 0);
	signal gps_x_stream_reg : std_logic_vector(63 downto 0);
	signal num_satellites_stream_reg : std_logic_vector(63 downto 0);
	signal imu_acc_x_stream_reg : std_logic_vector(63 downto 0);
	signal gps_emitted_enough_stream_reg : std_logic;
	signal few_satellites_stream_reg : std_logic;
	signal is_unreliable_gps_data_stream_reg : std_logic;

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
			gps_x_data_in => gps_x_data_in,
			gps_x_push_in => gps_x_data_in_new_input,
			num_satellites_data_in => num_satellites_data_in,
			num_satellites_push_in => num_satellites_data_in_new_input,
			imu_acc_x_data_in => imu_acc_x_data_in,
			imu_acc_x_push_in => imu_acc_x_data_in_new_input,
			gps_x_data_out => gps_x_data_timing,
			gps_x_en_out => gps_x_en_timing,
			num_satellites_data_out => num_satellites_data_timing,
			num_satellites_en_out => num_satellites_en_timing,
			imu_acc_x_data_out => imu_acc_x_data_timing,
			imu_acc_x_en_out => imu_acc_x_en_timing,
			gps_emitted_enough_en_out => gps_emitted_enough_en_timing,
			few_satellites_en_out => few_satellites_en_timing,
			is_unreliable_gps_data_en_out => is_unreliable_gps_data_en_timing,
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
			gps_x_data_in => gps_x_data_timing,
			gps_x_en_in => gps_x_en_timing,
			num_satellites_data_in => num_satellites_data_timing,
			num_satellites_en_in => num_satellites_en_timing,
			imu_acc_x_data_in => imu_acc_x_data_timing,
			imu_acc_x_en_in => imu_acc_x_en_timing,
			gps_emitted_enough_en_in => gps_emitted_enough_en_timing,
			few_satellites_en_in => few_satellites_en_timing,
			is_unreliable_gps_data_en_in => is_unreliable_gps_data_en_timing,
            full => queue_is_full,
            pop => pop_queue,
            time_data_out => queue_time,
			gps_x_data_out => gps_x_data_queue,
			gps_x_en_out => gps_x_en_queue,
			num_satellites_data_out => num_satellites_data_queue,
			num_satellites_en_out => num_satellites_en_queue,
			imu_acc_x_data_out => imu_acc_x_data_queue,
			imu_acc_x_en_out => imu_acc_x_en_queue,
			gps_emitted_enough_en_out => gps_emitted_enough_en_queue,
			few_satellites_en_out => few_satellites_en_queue,
			is_unreliable_gps_data_en_out => is_unreliable_gps_data_en_queue,
            available => queue_data_available
        );

    low_level_controller_instance: low_level_controller
        port map (
            clk => clk,
            eclk => eclk,
            rst => rst,
            time_in => queue_time,
			gps_x => gps_x_data_queue,
			gps_x_en => gps_x_en_queue,
			num_satellites => num_satellites_data_queue,
			num_satellites_en => num_satellites_en_queue,
			imu_acc_x => imu_acc_x_data_queue,
			imu_acc_x_en => imu_acc_x_en_queue,
			gps_emitted_enough_en => gps_emitted_enough_en_queue,
			few_satellites_en => few_satellites_en_queue,
			is_unreliable_gps_data_en => is_unreliable_gps_data_en_queue,
            data_available => queue_data_available,
			gps_emitted_enough => gps_emitted_enough_stream_evaluator,
			few_satellites => few_satellites_stream_evaluator,
			is_unreliable_gps_data => is_unreliable_gps_data_stream_evaluator,
            pop => pop_queue,
            eval_done => print
        );

    process(rst, print) begin
        if (rst = '1') then
            time_stream_reg <= (others => '0');
			gps_x_stream_reg <= (others => '0');
			num_satellites_stream_reg <= (others => '0');
			imu_acc_x_stream_reg <= (others => '0');
			gps_emitted_enough_stream_reg <= '0';
			few_satellites_stream_reg <= '0';
			is_unreliable_gps_data_stream_reg <= '0';
        elsif falling_edge(print) then
            time_stream_reg <= std_logic_vector(queue_time);
			gps_x_stream_reg <= std_logic_vector(gps_x_data_queue);
			num_satellites_stream_reg <= std_logic_vector(num_satellites_data_queue);
			imu_acc_x_stream_reg <= std_logic_vector(imu_acc_x_data_queue);
			gps_emitted_enough_stream_reg <= gps_emitted_enough_stream_evaluator;
			few_satellites_stream_reg <= few_satellites_stream_evaluator;
			is_unreliable_gps_data_stream_reg <= is_unreliable_gps_data_stream_evaluator;
        end if;
    end process;

    time_stream <= time_stream_reg;
	gps_x_stream <= gps_x_stream_reg;
	num_satellites_stream <= num_satellites_stream_reg;
	imu_acc_x_stream <= imu_acc_x_stream_reg;
	gps_emitted_enough_stream <= gps_emitted_enough_stream_reg;
	few_satellites_stream <= few_satellites_stream_reg;
	is_unreliable_gps_data_stream <= is_unreliable_gps_data_stream_reg;

end mixed;
