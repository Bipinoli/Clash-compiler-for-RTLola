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
		b_stream: out std_logic_vector(63 downto 0);
		c_stream: out std_logic_vector(63 downto 0);
		d_stream: out std_logic_vector(63 downto 0);
		e_stream: out std_logic_vector(63 downto 0);
		f_stream: out std_logic_vector(63 downto 0);
		g_stream: out std_logic_vector(63 downto 0);
		h_stream: out std_logic_vector(63 downto 0);
		i_stream: out std_logic_vector(63 downto 0);
		j_stream: out std_logic_vector(63 downto 0);
		k_stream: out std_logic_vector(63 downto 0);
		l_stream: out std_logic_vector(63 downto 0);
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
			b_en_out : out std_logic;
			c_en_out : out std_logic;
			d_en_out : out std_logic;
			e_en_out : out std_logic;
			f_en_out : out std_logic;
			g_en_out : out std_logic;
			h_en_out : out std_logic;
			i_en_out : out std_logic;
			j_en_out : out std_logic;
			k_en_out : out std_logic;
			l_en_out : out std_logic;
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
			b_en_in : in std_logic;
			c_en_in : in std_logic;
			d_en_in : in std_logic;
			e_en_in : in std_logic;
			f_en_in : in std_logic;
			g_en_in : in std_logic;
			h_en_in : in std_logic;
			i_en_in : in std_logic;
			j_en_in : in std_logic;
			k_en_in : in std_logic;
			l_en_in : in std_logic;
            full : out std_logic;
            pop : in std_logic;
            time_data_out : out unsigned(63 downto 0);
			x_data_out : out signed(63 downto 0);
			x_en_out : out std_logic;
			y_data_out : out signed(63 downto 0);
			y_en_out : out std_logic;
			a_en_out : out std_logic;
			b_en_out : out std_logic;
			c_en_out : out std_logic;
			d_en_out : out std_logic;
			e_en_out : out std_logic;
			f_en_out : out std_logic;
			g_en_out : out std_logic;
			h_en_out : out std_logic;
			i_en_out : out std_logic;
			j_en_out : out std_logic;
			k_en_out : out std_logic;
			l_en_out : out std_logic;
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
			b_en : in std_logic;
			c_en : in std_logic;
			d_en : in std_logic;
			e_en : in std_logic;
			f_en : in std_logic;
			g_en : in std_logic;
			h_en : in std_logic;
			i_en : in std_logic;
			j_en : in std_logic;
			k_en : in std_logic;
			l_en : in std_logic;
            data_available : in std_logic;
			a : out signed(63 downto 0);
			b : out signed(63 downto 0);
			c : out signed(63 downto 0);
			d : out signed(63 downto 0);
			e : out signed(63 downto 0);
			f : out signed(63 downto 0);
			g : out signed(63 downto 0);
			h : out signed(63 downto 0);
			i : out signed(63 downto 0);
			j : out signed(63 downto 0);
			k : out signed(63 downto 0);
			l : out signed(63 downto 0);
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
	signal b_en_timing : std_logic;
	signal c_en_timing : std_logic;
	signal d_en_timing : std_logic;
	signal e_en_timing : std_logic;
	signal f_en_timing : std_logic;
	signal g_en_timing : std_logic;
	signal h_en_timing : std_logic;
	signal i_en_timing : std_logic;
	signal j_en_timing : std_logic;
	signal k_en_timing : std_logic;
	signal l_en_timing : std_logic;
    -- query
    signal queue_is_full : std_logic;
    signal queue_time : unsigned(63 downto 0);
    signal pop_queue : std_logic;
	signal x_data_queue : signed(63 downto 0);
	signal x_en_queue : std_logic;
	signal y_data_queue : signed(63 downto 0);
	signal y_en_queue : std_logic;
	signal a_en_queue : std_logic;
	signal b_en_queue : std_logic;
	signal c_en_queue : std_logic;
	signal d_en_queue : std_logic;
	signal e_en_queue : std_logic;
	signal f_en_queue : std_logic;
	signal g_en_queue : std_logic;
	signal h_en_queue : std_logic;
	signal i_en_queue : std_logic;
	signal j_en_queue : std_logic;
	signal k_en_queue : std_logic;
	signal l_en_queue : std_logic;
    signal queue_data_available : std_logic;
    -- evaluator
	signal a_stream_evaluator : signed(63 downto 0);
	signal b_stream_evaluator : signed(63 downto 0);
	signal c_stream_evaluator : signed(63 downto 0);
	signal d_stream_evaluator : signed(63 downto 0);
	signal e_stream_evaluator : signed(63 downto 0);
	signal f_stream_evaluator : signed(63 downto 0);
	signal g_stream_evaluator : signed(63 downto 0);
	signal h_stream_evaluator : signed(63 downto 0);
	signal i_stream_evaluator : signed(63 downto 0);
	signal j_stream_evaluator : signed(63 downto 0);
	signal k_stream_evaluator : signed(63 downto 0);
	signal l_stream_evaluator : signed(63 downto 0);
    -- monitor
    signal time_stream_reg : std_logic_vector(63 downto 0);
	signal x_stream_reg : std_logic_vector(63 downto 0);
	signal y_stream_reg : std_logic_vector(63 downto 0);
	signal a_stream_reg : std_logic_vector(63 downto 0);
	signal b_stream_reg : std_logic_vector(63 downto 0);
	signal c_stream_reg : std_logic_vector(63 downto 0);
	signal d_stream_reg : std_logic_vector(63 downto 0);
	signal e_stream_reg : std_logic_vector(63 downto 0);
	signal f_stream_reg : std_logic_vector(63 downto 0);
	signal g_stream_reg : std_logic_vector(63 downto 0);
	signal h_stream_reg : std_logic_vector(63 downto 0);
	signal i_stream_reg : std_logic_vector(63 downto 0);
	signal j_stream_reg : std_logic_vector(63 downto 0);
	signal k_stream_reg : std_logic_vector(63 downto 0);
	signal l_stream_reg : std_logic_vector(63 downto 0);

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
			b_en_out => b_en_timing,
			c_en_out => c_en_timing,
			d_en_out => d_en_timing,
			e_en_out => e_en_timing,
			f_en_out => f_en_timing,
			g_en_out => g_en_timing,
			h_en_out => h_en_timing,
			i_en_out => i_en_timing,
			j_en_out => j_en_timing,
			k_en_out => k_en_timing,
			l_en_out => l_en_timing,
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
			b_en_in => b_en_timing,
			c_en_in => c_en_timing,
			d_en_in => d_en_timing,
			e_en_in => e_en_timing,
			f_en_in => f_en_timing,
			g_en_in => g_en_timing,
			h_en_in => h_en_timing,
			i_en_in => i_en_timing,
			j_en_in => j_en_timing,
			k_en_in => k_en_timing,
			l_en_in => l_en_timing,
            full => queue_is_full,
            pop => pop_queue,
            time_data_out => queue_time,
			x_data_out => x_data_queue,
			x_en_out => x_en_queue,
			y_data_out => y_data_queue,
			y_en_out => y_en_queue,
			a_en_out => a_en_queue,
			b_en_out => b_en_queue,
			c_en_out => c_en_queue,
			d_en_out => d_en_queue,
			e_en_out => e_en_queue,
			f_en_out => f_en_queue,
			g_en_out => g_en_queue,
			h_en_out => h_en_queue,
			i_en_out => i_en_queue,
			j_en_out => j_en_queue,
			k_en_out => k_en_queue,
			l_en_out => l_en_queue,
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
			b_en => b_en_queue,
			c_en => c_en_queue,
			d_en => d_en_queue,
			e_en => e_en_queue,
			f_en => f_en_queue,
			g_en => g_en_queue,
			h_en => h_en_queue,
			i_en => i_en_queue,
			j_en => j_en_queue,
			k_en => k_en_queue,
			l_en => l_en_queue,
            data_available => queue_data_available,
			a => a_stream_evaluator,
			b => b_stream_evaluator,
			c => c_stream_evaluator,
			d => d_stream_evaluator,
			e => e_stream_evaluator,
			f => f_stream_evaluator,
			g => g_stream_evaluator,
			h => h_stream_evaluator,
			i => i_stream_evaluator,
			j => j_stream_evaluator,
			k => k_stream_evaluator,
			l => l_stream_evaluator,
            pop => pop_queue,
            eval_done => print
        );

    process(rst, print) begin
        if (rst = '1') then
            time_stream_reg <= (others => '0');
			x_stream_reg <= (others => '0');
			y_stream_reg <= (others => '0');
			a_stream_reg <= (others => '0');
			b_stream_reg <= (others => '0');
			c_stream_reg <= (others => '0');
			d_stream_reg <= (others => '0');
			e_stream_reg <= (others => '0');
			f_stream_reg <= (others => '0');
			g_stream_reg <= (others => '0');
			h_stream_reg <= (others => '0');
			i_stream_reg <= (others => '0');
			j_stream_reg <= (others => '0');
			k_stream_reg <= (others => '0');
			l_stream_reg <= (others => '0');
        elsif falling_edge(print) then
            time_stream_reg <= std_logic_vector(queue_time);
			x_stream_reg <= std_logic_vector(x_data_queue);
			y_stream_reg <= std_logic_vector(y_data_queue);
			a_stream_reg <= std_logic_vector(a_stream_evaluator);
			b_stream_reg <= std_logic_vector(b_stream_evaluator);
			c_stream_reg <= std_logic_vector(c_stream_evaluator);
			d_stream_reg <= std_logic_vector(d_stream_evaluator);
			e_stream_reg <= std_logic_vector(e_stream_evaluator);
			f_stream_reg <= std_logic_vector(f_stream_evaluator);
			g_stream_reg <= std_logic_vector(g_stream_evaluator);
			h_stream_reg <= std_logic_vector(h_stream_evaluator);
			i_stream_reg <= std_logic_vector(i_stream_evaluator);
			j_stream_reg <= std_logic_vector(j_stream_evaluator);
			k_stream_reg <= std_logic_vector(k_stream_evaluator);
			l_stream_reg <= std_logic_vector(l_stream_evaluator);
        end if;
    end process;

    time_stream <= time_stream_reg;
	x_stream <= x_stream_reg;
	y_stream <= y_stream_reg;
	a_stream <= a_stream_reg;
	b_stream <= b_stream_reg;
	c_stream <= c_stream_reg;
	d_stream <= d_stream_reg;
	e_stream <= e_stream_reg;
	f_stream <= f_stream_reg;
	g_stream <= g_stream_reg;
	h_stream <= h_stream_reg;
	i_stream <= i_stream_reg;
	j_stream <= j_stream_reg;
	k_stream <= k_stream_reg;
	l_stream <= l_stream_reg;

end mixed;
