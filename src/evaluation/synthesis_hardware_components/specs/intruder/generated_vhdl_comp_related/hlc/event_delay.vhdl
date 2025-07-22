library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity event_delay is
    port(
        clk, rst : in std_logic;
        time_in : in unsigned(63 downto 0);
		lat_data_in : in signed(63 downto 0);
		lat_push_in : in std_logic;
		lon_data_in : in signed(63 downto 0);
		lon_push_in : in std_logic;
        push_event_in : std_logic;
        time_out : out unsigned(63 downto 0);
		lat_data_out : out signed(63 downto 0);
		lat_push_out : out std_logic;
		lon_data_out : out signed(63 downto 0);
		lon_push_out : out std_logic;
        push_event_out : out std_logic
    );
end event_delay;

--* Input Streams and their Types in the Specification:
--* - lat : Int64 *--
--* - lon : Int64 *--

architecture behavioral of event_delay is

    -- Internal Signal Declarations
    signal time_value : unsigned(63 downto 0);
    signal push_event : std_logic;
	signal lat_data_delayed : signed(63 downto 0);
	signal lat_push_delayed : std_logic;
	signal lon_data_delayed : signed(63 downto 0);
	signal lon_push_delayed : std_logic;

    begin

    process(clk, rst) begin
        if (rst = '1') then
            -- Reset Phase
            time_value <= (others => '0');
            push_event <= '0';
			lat_data_delayed <= to_signed(0, lat_data_delayed'length);
			lat_push_delayed <= '0';
			lon_data_delayed <= to_signed(0, lon_data_delayed'length);
			lon_push_delayed <= '0';
        elsif (rising_edge(clk)) then
            -- Logic Phase: Map Inputs to Internal Signals to Receive a Delay
            if (push_event_in = '1') then
                time_value <= time_in;
                push_event <= push_event_in;
				lat_data_delayed <= lat_data_in;
				lat_push_delayed <= lat_push_in;
				lon_data_delayed <= lon_data_in;
				lon_push_delayed <= lon_push_in;
            else
                time_value <= (others => '0');
                push_event <= '0';
				lat_data_delayed <= to_signed(0, lat_data_delayed'length);
				lat_push_delayed <= '0';
				lon_data_delayed <= to_signed(0, lon_data_delayed'length);
				lon_push_delayed <= '0';
            end if;
        end if;
    end process;

    time_out <= time_value;
    push_event_out <= push_event;
	lat_data_out <= lat_data_delayed;
	lat_push_out <= lat_push_delayed;
	lon_data_out <= lon_data_delayed;
	lon_push_out <= lon_push_delayed;

end behavioral;