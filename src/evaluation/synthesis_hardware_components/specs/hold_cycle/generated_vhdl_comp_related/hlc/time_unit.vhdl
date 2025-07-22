library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity time_unit is
    port(
        rst : in std_logic;
        clk : in std_logic;
        sys_time_out : out unsigned(63 downto 0)
    );
end time_unit;

architecture behavioral of time_unit is

    -- Internal Signal Declaration
    signal sys_time : unsigned(63 downto 0);

begin

    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            sys_time <= to_unsigned(0, sys_time'length);
        elsif rising_edge(clk) then
            -- Logic Phase
            --* Relation Clock Frequency and Period per Cycle: 25 MHZ <=> 40 ns
            sys_time <= sys_time + to_unsigned(40, sys_time'length);
        end if;
    end process;

    sys_time_out <= sys_time;

end behavioral;
