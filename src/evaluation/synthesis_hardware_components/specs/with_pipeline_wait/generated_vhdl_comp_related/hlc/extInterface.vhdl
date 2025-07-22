library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;

entity extInterface is
    port(
        clk, rst: in std_logic;
        time_in : in std_logic_vector(63 downto 0);
		x_data_in : in std_logic_vector(63 downto 0);
		x_push_in : in std_logic;
		x_data_out : out signed(63 downto 0);
		x_push_out : out std_logic;
        time_out : out unsigned(63 downto 0)
    );
end extInterface;

--* Input Streams and their Types in the Specification: 
--* - input x : Int64

architecture behavioral of extInterface is

    -- Internal Signal Declarations
    signal time_converted : unsigned(63 downto 0);
	signal x_parsed : signed(63 downto 0);
	signal x_push_delayed : std_logic;

begin

    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            time_converted <= to_unsigned(0, time_converted'length);
			x_parsed <= (others => '0');
			x_push_delayed <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase: Convert Input in Numeric Types
            time_converted <= unsigned(time_in);
			--* input x : Int64
			x_parsed <= signed(x_data_in);
			x_push_delayed <= x_push_in;
        end if;
    end process;

    time_out <= time_converted;
	x_data_out <= x_parsed;
	x_push_out <= x_push_delayed;

end behavioral;