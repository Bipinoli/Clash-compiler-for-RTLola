library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;
use work.my_math_pkg.all;

--* Output Stream in the Specification
--* output h : Int64 := ((g + k.hold().defaults(to: (a + b))) + l.hold().defaults(to: 0))
--* Input Dependencies:
--* Stream Lookups:
--* - f: 0
--* Storage Requirement: 0
--* Output Dependencies:
--* Stream Lookups
--* - a of Type Int64: 0
--* - b of Type Int64: 0
--* - g of Type Int64: 0
--* - k of Type Int64: 0
--* - l of Type Int64: 0


entity h_output_stream_entity is 
	port (
		clk, pe, eval, rst : in std_logic;
			a_0 : in signed(63 downto 0);
			a_data_valid_0 : in std_logic;
			b_0 : in signed(63 downto 0);
			b_data_valid_0 : in std_logic;
			g_0 : in signed(63 downto 0);
			g_data_valid_0 : in std_logic;
			k_0 : in signed(63 downto 0);
			k_data_valid_0 : in std_logic;
			l_0 : in signed(63 downto 0);
			l_data_valid_0 : in std_logic;
		data_out : out signed64_array(0 downto 0);
		data_valid_out : out bit_array(0 downto 0);
		pe_done_out : out std_logic;
		eval_done_out : out std_logic
	);
end h_output_stream_entity;

architecture behavioral of h_output_stream_entity is

    signal pe_done : std_logic;
    signal eval_done : std_logic;
    signal data : signed64_array(0 downto 0);
    signal data_valid : bit_array(0 downto 0);

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
				--* temp_0 := g 
				temp_0 := g_0;
				--* temp_1 := k.hold() 
				temp_1 := k_0;
				--* temp_2 := a 
				temp_2 := a_0;
				--* temp_3 := b 
				temp_3 := b_0;
				--* temp_4 := (a + b) 
				temp_4 := temp_2 + temp_3;
				--* temp_5 := k.hold().defaults(to: (a + b)) 
				temp_5 := sel(temp_1, temp_4, k_data_valid_0);
				--* temp_6 := (g + k.hold().defaults(to: (a + b))) 
				temp_6 := temp_0 + temp_5;
				--* temp_7 := l.hold() 
				temp_7 := l_0;
				temp_8 := to_signed(0, 64);
				--* temp_9 := l.hold().defaults(to: 0) 
				temp_9 := sel(temp_7, temp_8, l_data_valid_0);
				--* temp_10 := ((g + k.hold().defaults(to: (a + b))) + l.hold().defaults(to: 0)) 
				temp_10 := temp_6 + temp_9;
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
