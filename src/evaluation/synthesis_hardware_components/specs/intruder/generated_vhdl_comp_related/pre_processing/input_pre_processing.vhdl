library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity input_pre_processing is
    port(
        clk, rst: in std_logic;
        time_in : in std_logic_vector(63 downto 0);
		lat_in : in std_logic_vector(63 downto 0);
		lat_in_new_input : in std_logic;
		lon_in : in std_logic_vector(63 downto 0);
		lon_in_new_input : in std_logic;
        new_input_in : in std_logic;
        time_out : out std_logic_vector(63 downto 0);
		lat_out : out std_logic_vector(63 downto 0);
		lat_out_new_input : out std_logic;
		lon_out : out std_logic_vector(63 downto 0);
		lon_out_new_input : out std_logic;
        new_input_out : out std_logic
    );
end input_pre_processing;

architecture behavioral of input_pre_processing is

    signal new_input : std_logic;
    signal time_reg : std_logic_vector(63 downto 0);
	signal lat_reg : std_logic_vector(63 downto 0);
	signal lat_reg_new_input : std_logic;
	signal lon_reg : std_logic_vector(63 downto 0);
	signal lon_reg_new_input : std_logic;

begin

    process(clk, rst) begin
        if (rst = '1') then
            -- set default
            new_input <= '0';
            time_reg <= (others => '0');
			lat_reg <= (others => '0');
			lat_reg_new_input <= '0';
			lon_reg <= (others => '0');
			lon_reg_new_input <= '0';
        elsif (rising_edge(clk)) then
            new_input <= new_input_in;
            time_reg <= time_in;
				lat_reg <= lat_in;
				lat_reg_new_input <= lat_in_new_input;
				lon_reg <= lon_in;
				lon_reg_new_input <= lon_in_new_input;
        end if;
    end process;

    new_input_out <= new_input;
    time_out <= time_reg;
	lat_out <= lat_reg;
	lat_out_new_input <= lat_reg_new_input;
	lon_out <= lon_reg;
	lon_out_new_input <= lon_reg_new_input;

end behavioral;