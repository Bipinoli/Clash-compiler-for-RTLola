library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity event_delay is
    port(
        clk, rst : in std_logic;
        time_in : in unsigned(63 downto 0);
		x_data_in : in signed(63 downto 0);
		x_push_in : in std_logic;
		y_data_in : in signed(63 downto 0);
		y_push_in : in std_logic;
        push_event_in : std_logic;
        time_out : out unsigned(63 downto 0);
		x_data_out : out signed(63 downto 0);
		x_push_out : out std_logic;
		y_data_out : out signed(63 downto 0);
		y_push_out : out std_logic;
        push_event_out : out std_logic
    );
end event_delay;

--* Input Streams and their Types in the Specification:
--* - x : Int64 *--
--* - y : Int64 *--

architecture behavioral of event_delay is

    -- Internal Signal Declarations
    signal time_value : unsigned(63 downto 0);
    signal push_event : std_logic;
	signal x_data_delayed : signed(63 downto 0);
	signal x_push_delayed : std_logic;
	signal y_data_delayed : signed(63 downto 0);
	signal y_push_delayed : std_logic;

    begin

    process(clk, rst) begin
        if (rst = '1') then
            -- Reset Phase
            time_value <= (others => '0');
            push_event <= '0';
			x_data_delayed <= to_signed(0, x_data_delayed'length);
			x_push_delayed <= '0';
			y_data_delayed <= to_signed(0, y_data_delayed'length);
			y_push_delayed <= '0';
        elsif (rising_edge(clk)) then
            -- Logic Phase: Map Inputs to Internal Signals to Receive a Delay
            if (push_event_in = '1') then
                time_value <= time_in;
                push_event <= push_event_in;
				x_data_delayed <= x_data_in;
				x_push_delayed <= x_push_in;
				y_data_delayed <= y_data_in;
				y_push_delayed <= y_push_in;
            else
                time_value <= (others => '0');
                push_event <= '0';
				x_data_delayed <= to_signed(0, x_data_delayed'length);
				x_push_delayed <= '0';
				y_data_delayed <= to_signed(0, y_data_delayed'length);
				y_push_delayed <= '0';
            end if;
        end if;
    end process;

    time_out <= time_value;
    push_event_out <= push_event;
	x_data_out <= x_data_delayed;
	x_push_out <= x_push_delayed;
	y_data_out <= y_data_delayed;
	y_push_out <= y_push_delayed;

end behavioral;