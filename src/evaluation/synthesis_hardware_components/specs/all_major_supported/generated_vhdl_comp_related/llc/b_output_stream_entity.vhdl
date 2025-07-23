library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;
use work.my_math_pkg.all;

--* Output Stream in the Specification
--* output b : Int64 := ((a + c.hold().defaults(to: 0)) + a.offset(by: neg2).defaults(to: (a + x)))
--* Input Dependencies:
--* Stream Lookups:
--* - a: -1
--* - c: 0
--* Window Lookups:
--* - c: (0.1, sum)
--* Storage Requirement: 1
--* Output Dependencies:
--* Stream Lookups
--* - x of Type Int64: 0
--* - a of Type Int64: 0, -2
--* - c of Type Int64: 0


entity b_output_stream_entity is 
	port (
		clk, pe, eval, rst : in std_logic;
			x_0 : in signed(63 downto 0);
			x_data_valid_0 : in std_logic;
			a_0 : in signed(63 downto 0);
			a_data_valid_0 : in std_logic;
			a_neg2 : in signed(63 downto 0);
			a_data_valid_neg2 : in std_logic;
			c_0 : in signed(63 downto 0);
			c_data_valid_0 : in std_logic;
		data_out : out signed64_array(1 downto 0);
		data_valid_out : out bit_array(1 downto 0);
		pe_done_out : out std_logic;
		eval_done_out : out std_logic
	);
end b_output_stream_entity;

architecture behavioral of b_output_stream_entity is

    signal pe_done : std_logic;
    signal eval_done : std_logic;
    signal data : signed64_array(1 downto 0);
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
		variable temp_6: signed(63 downto 0) := (others => '0');
		variable temp_7: signed(63 downto 0) := (others => '0');
		variable temp_8: signed(63 downto 0) := (others => '0');
		variable temp_9: signed(63 downto 0) := (others => '0');
		variable temp_10: signed(63 downto 0) := (others => '0');
	    variable updt : signed(63 downto 0) := (others => '0');
    begin
	    if (rst='1') then
	        -- Reset Phase
		    data(data'high downto 0) <= (others => (others => '0'));
		    data_valid(data_valid'high downto 0) <= (others => '0');
		    pe_done <= '0';
		    eval_done <= '0';
	    elsif (rising_edge(clk)) then
	        -- Logic Phase
	        if (pe = '1' and pe_done = '0') then
	            -- Pseudo Evaluation
                data <= data(data'high-1 downto 0) & to_signed(0, updt'length);
                data_valid <= data_valid(data_valid'high-1 downto 0) & '0';
                pe_done <= '1';
		    elsif (eval = '1' and eval_done = '0') then
				-- Evaluation
				--* temp_0 := a 
				temp_0 := a_0;
				--* temp_1 := c.hold() 
				temp_1 := c_0;
				temp_2 := to_signed(0, 64);
				--* temp_3 := c.hold().defaults(to: 0) 
				temp_3 := sel(temp_1, temp_2, c_data_valid_0);
				--* temp_4 := (a + c.hold().defaults(to: 0)) 
				temp_4 := temp_0 + temp_3;
				--* temp_5 := a.offset(by: neg2)
				temp_5 := a_neg2;
				--* temp_6 := a 
				temp_6 := a_0;
				--* temp_7 := x 
				temp_7 := x_0;
				--* temp_8 := (a + x) 
				temp_8 := temp_6 + temp_7;
				--* temp_9 := a.offset(by: neg2).defaults(to: (a + x)) 
				temp_9 := sel(temp_5, temp_8, a_data_valid_neg2);
				--* temp_10 := ((a + c.hold().defaults(to: 0)) + a.offset(by: neg2).defaults(to: (a + x))) 
				temp_10 := temp_4 + temp_9;
				updt := temp_10;
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
