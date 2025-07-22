library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;
use work.my_math_pkg.all;

--* Output Stream in the Specification
--* output acceleration_x_sinking : Bool := ((acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) < -5)
--* Input Dependencies:
--* Stream Lookups:
--* - acceleration_x_direction_change: -1, 0
--* Storage Requirement: 1
--* Output Dependencies:
--* Stream Lookups
--* - acceleration_x_periodic of Type Int64: 0, -1


entity acceleration_x_sinking_output_stream_entity is 
	port (
		clk, pe, eval, rst : in std_logic;
			acceleration_x_periodic_0 : in signed(63 downto 0);
			acceleration_x_periodic_data_valid_0 : in std_logic;
			acceleration_x_periodic_neg1 : in signed(63 downto 0);
			acceleration_x_periodic_data_valid_neg1 : in std_logic;
		data_out : out bit_array(1 downto 0);
		data_valid_out : out bit_array(1 downto 0);
		pe_done_out : out std_logic;
		eval_done_out : out std_logic
	);
end acceleration_x_sinking_output_stream_entity;

architecture behavioral of acceleration_x_sinking_output_stream_entity is

    signal pe_done : std_logic;
    signal eval_done : std_logic;
    signal data : bit_array(1 downto 0);
    signal data_valid : bit_array(1 downto 0);

    begin

    process (clk, rst)
        -- temporal variables
		variable temp_0: signed(63 downto 0) := (others => '0');
		variable temp_1: signed(63 downto 0) := (others => '0');
		variable temp_2: signed(63 downto 0) := (others => '0');
		variable temp_3: signed(63 downto 0) := (others => '0');
		variable temp_4: signed(63 downto 0) := (others => '0');
		variable temp_5: signed(63 downto 0) := (others => '0');
		variable temp_6: std_logic := '0';
	    variable updt : std_logic := '0';
    begin
	    if (rst='1') then
	        -- Reset Phase
		    data(data'high downto 0) <= (others => '0');
		    data_valid(data_valid'high downto 0) <= (others => '0');
		    pe_done <= '0';
		    eval_done <= '0';
	    elsif (rising_edge(clk)) then
	        -- Logic Phase
	        if (pe = '1' and pe_done = '0') then
	            -- Pseudo Evaluation
                data <= data(data'high-1 downto 0) & '0';
                data_valid <= data_valid(data_valid'high-1 downto 0) & '0';
                pe_done <= '1';
		    elsif (eval = '1' and eval_done = '0') then
				-- Evaluation
				--* temp_0 := acceleration_x_periodic 
				temp_0 := acceleration_x_periodic_0;
				--* temp_1 := acceleration_x_periodic.offset(by: neg1)
				temp_1 := acceleration_x_periodic_neg1;
				--* temp_2 := acceleration_x_periodic 
				temp_2 := acceleration_x_periodic_0;
				--* temp_3 := acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic) 
				temp_3 := sel(temp_1, temp_2, acceleration_x_periodic_data_valid_neg1);
				--* temp_4 := (acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) 
				temp_4 := temp_0 - temp_3;
				temp_5 := to_signed(-5, 64);
				--* temp_6 := ((acceleration_x_periodic - acceleration_x_periodic.offset(by: neg1).defaults(to: acceleration_x_periodic)) < -5) 
				temp_6 := to_std_logic(temp_4 < temp_5);
				updt := temp_6;
			    -- Register Update
			    data(0) <= updt;
			    data_valid(0) <= '1';
			    eval_done <= '1';
			elsif (pe = '0' and eval = '0') then
                -- Reset done Signals
                pe_done <= '0';
                eval_done <= '0';
		    end if;
	    end if;
    end process;

     -- Mapping: Register to Output Wires
    data_out <= data;
    data_valid_out <= data_valid;
    pe_done_out <= pe_done;
    eval_done_out <= eval_done;

end behavioral;
