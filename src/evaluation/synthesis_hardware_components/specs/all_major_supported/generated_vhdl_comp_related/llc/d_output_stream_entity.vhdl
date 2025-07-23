library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;
use work.my_math_pkg.all;

--* Output Stream in the Specification
--* output d : Bool := ((c.aggregate(over: 0.05s, using: count) < 10) and (c.hold().defaults(to: 0) > 15))
--* Input Dependencies:
--* Storage Requirement: 0
--* Output Dependencies:
--* Stream Lookups
--* - c of Type Int64: window SlidingWin(1), 0
--* Window Lookups:
--* - c.aggregate(over: 0.05 s, using: count) of type UInt64


entity d_output_stream_entity is 
	port (
		clk, pe, eval, rst : in std_logic;
			c_0 : in signed(63 downto 0);
			c_data_valid_0 : in std_logic;
			c_count_1_sw : in unsigned(63 downto 0);
			c_count_1_sw_data_valid : in std_logic;
		data_out : out bit_array(0 downto 0);
		data_valid_out : out bit_array(0 downto 0);
		pe_done_out : out std_logic;
		eval_done_out : out std_logic
	);
end d_output_stream_entity;

architecture behavioral of d_output_stream_entity is

    signal pe_done : std_logic;
    signal eval_done : std_logic;
    signal data : bit_array(0 downto 0);
    signal data_valid : bit_array(0 downto 0);

    begin

    process (clk, rst)
        -- temporal variables
		variable temp_0: unsigned(63 downto 0) := (others => '0');
		variable temp_1: unsigned(63 downto 0) := (others => '0');
		variable temp_2: std_logic := '0';
		variable temp_3: signed(63 downto 0) := (others => '0');
		variable temp_4: signed(63 downto 0) := (others => '0');
		variable temp_5: signed(63 downto 0) := (others => '0');
		variable temp_6: signed(63 downto 0) := (others => '0');
		variable temp_7: std_logic := '0';
		variable temp_8: std_logic := '0';
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
				--* temp_0 := c.aggregate(over: 0.05s, using: count) 
				temp_0 := c_count_1_sw;
				temp_1 := to_unsigned(10, 64);
				--* temp_2 := (c.aggregate(over: 0.05s, using: count) < 10) 
				temp_2 := to_std_logic(temp_0 < temp_1);
				--* temp_3 := c.hold() 
				temp_3 := c_0;
				temp_4 := to_signed(0, 64);
				--* temp_5 := c.hold().defaults(to: 0) 
				temp_5 := sel(temp_3, temp_4, c_data_valid_0);
				temp_6 := to_signed(15, 64);
				--* temp_7 := (c.hold().defaults(to: 0) > 15) 
				temp_7 := to_std_logic(temp_5 > temp_6);
				--* temp_8 := ((c.aggregate(over: 0.05s, using: count) < 10) and (c.hold().defaults(to: 0) > 15)) 
				temp_8 := temp_2 and temp_7;
				updt := temp_8;
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
