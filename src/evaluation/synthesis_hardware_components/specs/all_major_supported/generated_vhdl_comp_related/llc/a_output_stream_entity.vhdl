library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;
use work.my_math_pkg.all;

--* Output Stream in the Specification
--* output a : Int64 := (x.offset(by: neg1).defaults(to: 0) + b.offset(by: neg1).defaults(to: 0))
--* Input Dependencies:
--* Stream Lookups:
--* - b: 0, -2
--* Storage Requirement: 2
--* Output Dependencies:
--* Stream Lookups
--* - x of Type Int64: -1
--* - b of Type Int64: -1


entity a_output_stream_entity is 
	port (
		clk, pe, eval, rst : in std_logic;
			x_neg1 : in signed(63 downto 0);
			x_data_valid_neg1 : in std_logic;
			b_neg1 : in signed(63 downto 0);
			b_data_valid_neg1 : in std_logic;
		data_out : out signed64_array(2 downto 0);
		data_valid_out : out bit_array(2 downto 0);
		pe_done_out : out std_logic;
		eval_done_out : out std_logic
	);
end a_output_stream_entity;

architecture behavioral of a_output_stream_entity is

    signal pe_done : std_logic;
    signal eval_done : std_logic;
    signal data : signed64_array(2 downto 0);
    signal data_valid : bit_array(2 downto 0);

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
				--* temp_0 := x.offset(by: neg1)
				temp_0 := x_neg1;
				temp_1 := to_signed(0, 64);
				--* temp_2 := x.offset(by: neg1).defaults(to: 0) 
				temp_2 := sel(temp_0, temp_1, x_data_valid_neg1);
				--* temp_3 := b.offset(by: neg1)
				temp_3 := b_neg1;
				temp_4 := to_signed(0, 64);
				--* temp_5 := b.offset(by: neg1).defaults(to: 0) 
				temp_5 := sel(temp_3, temp_4, b_data_valid_neg1);
				--* temp_6 := (x.offset(by: neg1).defaults(to: 0) + b.offset(by: neg1).defaults(to: 0)) 
				temp_6 := temp_2 + temp_5;
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
