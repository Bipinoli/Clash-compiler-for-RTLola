library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity input_pre_processing is
    port(
        clk, rst: in std_logic;
        time_in : in std_logic_vector(63 downto 0);
		x_in : in std_logic_vector(63 downto 0);
		x_in_new_input : in std_logic;
		y_in : in std_logic_vector(63 downto 0);
		y_in_new_input : in std_logic;
        new_input_in : in std_logic;
        time_out : out std_logic_vector(63 downto 0);
		x_out : out std_logic_vector(63 downto 0);
		x_out_new_input : out std_logic;
		y_out : out std_logic_vector(63 downto 0);
		y_out_new_input : out std_logic;
        new_input_out : out std_logic
    );
end input_pre_processing;

architecture behavioral of input_pre_processing is

    signal new_input : std_logic;
    signal time_reg : std_logic_vector(63 downto 0);
	signal x_reg : std_logic_vector(63 downto 0);
	signal x_reg_new_input : std_logic;
	signal y_reg : std_logic_vector(63 downto 0);
	signal y_reg_new_input : std_logic;

begin

    process(clk, rst) begin
        if (rst = '1') then
            -- set default
            new_input <= '0';
            time_reg <= (others => '0');
			x_reg <= (others => '0');
			x_reg_new_input <= '0';
			y_reg <= (others => '0');
			y_reg_new_input <= '0';
        elsif (rising_edge(clk)) then
            new_input <= new_input_in;
            time_reg <= time_in;
				x_reg <= x_in;
				x_reg_new_input <= x_in_new_input;
				y_reg <= y_in;
				y_reg_new_input <= y_in_new_input;
        end if;
    end process;

    new_input_out <= new_input;
    time_out <= time_reg;
	x_out <= x_reg;
	x_out_new_input <= x_reg_new_input;
	y_out <= y_reg;
	y_out_new_input <= y_reg_new_input;

end behavioral;