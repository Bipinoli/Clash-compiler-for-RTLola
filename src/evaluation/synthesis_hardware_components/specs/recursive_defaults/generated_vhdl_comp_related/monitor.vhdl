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
		x_data_in : in std_logic_vector(63 downto 0);
		x_data_in_new_input : in std_logic;
		y_data_in : in std_logic_vector(63 downto 0);
		y_data_in_new_input : in std_logic;
        time_stream : out std_logic_vector(63 downto 0);
		x_stream: out std_logic_vector(63 downto 0);
		y_stream: out std_logic_vector(63 downto 0);
		a_stream: out std_logic_vector(63 downto 0);
		counts_stream: out std_logic_vector(63 downto 0);
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
			x_data_in : in std_logic_vector(63 downto 0);
			x_push_in : std_logic;
			y_data_in : in std_logic_vector(63 downto 0);
			y_push_in : std_logic;
			x_data_out : out signed(63 downto 0);
			x_en_out : out std_logic;
			y_data_out : out signed(63 downto 0);
			y_en_out : out std_logic;
			a_en_out : out std_logic;
			counts_en_out : out std_logic;
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
			x_data_in : in signed(63 downto 0);
			x_en_in : in std_logic;
			y_data_in : in signed(63 downto 0);
			y_en_in : in std_logic;
			a_en_in : in std_logic;
			counts_en_in : in std_logic;
            full : out std_logic;
            pop : in std_logic;
            time_data_out : out unsigned(63 downto 0);
			x_data_out : out signed(63 downto 0);
			x_en_out : out std_logic;
			y_data_out : out signed(63 downto 0);
			y_en_out : out std_logic;
			a_en_out : out std_logic;
			counts_en_out : out std_logic;
            available : out std_logic
        );
    end component;

    component low_level_controller is
        port (
            clk, eclk, rst : in std_logic;
            time_in : in unsigned(63 downto 0);
			x : in signed(63 downto 0);
			x_en : in std_logic;
			y : in signed(63 downto 0);
			y_en : in std_logic;
			a_en : in std_logic;
			counts_en : in std_logic;
            data_available : in std_logic;
			a : out signed(63 downto 0);
			counts : out unsigned(63 downto 0);
		    pop : out std_logic;
            eval_done : out std_logic
        );
    end component;

    -- signal declarationq
    -- timing_manager
    signal push_data_timing : std_logic;
    signal timing_manager_time : unsigned(63 downto 0);
	signal x_data_timing : signed(63 downto 0);
	signal x_en_timing : std_logic;
	signal y_data_timing : signed(63 downto 0);
	signal y_en_timing : std_logic;
	signal a_en_timing : std_logic;
	signal counts_en_timing : std_logic;
    -- query
    signal queue_is_full : std_logic;
    signal queue_time : unsigned(63 downto 0);
    signal pop_queue : std_logic;
	signal x_data_queue : signed(63 downto 0);
	signal x_en_queue : std_logic;
	signal y_data_queue : signed(63 downto 0);
	signal y_en_queue : std_logic;
	signal a_en_queue : std_logic;
	signal counts_en_queue : std_logic;
    signal queue_data_available : std_logic;
    -- evaluator
	signal a_stream_evaluator : signed(63 downto 0);
	signal counts_stream_evaluator : unsigned(63 downto 0);
    -- monitor
    signal time_stream_reg : std_logic_vector(63 downto 0);
	signal x_stream_reg : std_logic_vector(63 downto 0);
	signal y_stream_reg : std_logic_vector(63 downto 0);
	signal a_stream_reg : std_logic_vector(63 downto 0);
	signal counts_stream_reg : std_logic_vector(63 downto 0);

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
			x_data_in => x_data_in,
			x_push_in => x_data_in_new_input,
			y_data_in => y_data_in,
			y_push_in => y_data_in_new_input,
			x_data_out => x_data_timing,
			x_en_out => x_en_timing,
			y_data_out => y_data_timing,
			y_en_out => y_en_timing,
			a_en_out => a_en_timing,
			counts_en_out => counts_en_timing,
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
			x_data_in => x_data_timing,
			x_en_in => x_en_timing,
			y_data_in => y_data_timing,
			y_en_in => y_en_timing,
			a_en_in => a_en_timing,
			counts_en_in => counts_en_timing,
            full => queue_is_full,
            pop => pop_queue,
            time_data_out => queue_time,
			x_data_out => x_data_queue,
			x_en_out => x_en_queue,
			y_data_out => y_data_queue,
			y_en_out => y_en_queue,
			a_en_out => a_en_queue,
			counts_en_out => counts_en_queue,
            available => queue_data_available
        );

    low_level_controller_instance: low_level_controller
        port map (
            clk => clk,
            eclk => eclk,
            rst => rst,
            time_in => queue_time,
			x => x_data_queue,
			x_en => x_en_queue,
			y => y_data_queue,
			y_en => y_en_queue,
			a_en => a_en_queue,
			counts_en => counts_en_queue,
            data_available => queue_data_available,
			a => a_stream_evaluator,
			counts => counts_stream_evaluator,
            pop => pop_queue,
            eval_done => print
        );

    process(rst, print) begin
        if (rst = '1') then
            time_stream_reg <= (others => '0');
			x_stream_reg <= (others => '0');
			y_stream_reg <= (others => '0');
			a_stream_reg <= (others => '0');
			counts_stream_reg <= (others => '0');
        elsif falling_edge(print) then
            time_stream_reg <= std_logic_vector(queue_time);
			x_stream_reg <= std_logic_vector(x_data_queue);
			y_stream_reg <= std_logic_vector(y_data_queue);
			a_stream_reg <= std_logic_vector(a_stream_evaluator);
			counts_stream_reg <= std_logic_vector(counts_stream_evaluator);
        end if;
    end process;

    time_stream <= time_stream_reg;
	x_stream <= x_stream_reg;
	y_stream <= y_stream_reg;
	a_stream <= a_stream_reg;
	counts_stream <= counts_stream_reg;

end mixed;
