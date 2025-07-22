library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity extInterface is
    port(
        clk, rst: in std_logic;
        time_in : in std_logic_vector(63 downto 0);
		lat_data_in : in std_logic_vector(63 downto 0);
		lat_push_in : in std_logic;
		lon_data_in : in std_logic_vector(63 downto 0);
		lon_push_in : in std_logic;
		lat_data_out : out signed(63 downto 0);
		lat_push_out : out std_logic;
		lon_data_out : out signed(63 downto 0);
		lon_push_out : out std_logic;
        time_out : out unsigned(63 downto 0)
    );
end extInterface;

--* Input Streams and their Types in the Specification: 
--* - input lat : Int64
--* - input lon : Int64

architecture behavioral of extInterface is

    -- Internal Signal Declarations
    signal time_converted : unsigned(63 downto 0);
	signal lat_parsed : signed(63 downto 0);
	signal lat_push_delayed : std_logic;
	signal lon_parsed : signed(63 downto 0);
	signal lon_push_delayed : std_logic;

begin

    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            time_converted <= to_unsigned(0, time_converted'length);
			lat_parsed <= (others => '0');
			lat_push_delayed <= '0';
			lon_parsed <= (others => '0');
			lon_push_delayed <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase: Convert Input in Numeric Types
            time_converted <= unsigned(time_in);
			--* input lat : Int64
			lat_parsed <= signed(lat_data_in);
			lat_push_delayed <= lat_push_in;
			--* input lon : Int64
			lon_parsed <= signed(lon_data_in);
			lon_push_delayed <= lon_push_in;
        end if;
    end process;

    time_out <= time_converted;
	lat_data_out <= lat_parsed;
	lat_push_out <= lat_push_delayed;
	lon_data_out <= lon_parsed;
	lon_push_out <= lon_push_delayed;

end behavioral;