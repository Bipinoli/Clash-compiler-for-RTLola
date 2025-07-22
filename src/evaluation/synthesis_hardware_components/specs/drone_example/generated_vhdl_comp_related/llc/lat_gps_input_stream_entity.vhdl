library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

--* Input Stream in the Specification
--* input lat_gps : Int64
--* Input Dependencies:
--* Stream Lookups:
--* - gps_missed_beat: 
--* - gps_medium_loss: 
--* - gps_high_loss: 
--* - gps_very_high_loss: 
--* Window Lookups:
--* - gps_missed_beat: (0.055, count)
--* - gps_medium_loss: (0.01, count)
--* - gps_medium_loss: (0.01, count)
--* - gps_high_loss: (0.01, count)
--* - gps_high_loss: (0.01, count)
--* - gps_very_high_loss: (0.01, count)
--* Storage Requirement: 0

entity lat_gps_input_stream_entity is
    port (
        clk, upd, rst : in std_logic;
        data_in : in signed(63 downto 0);
        data_out : out signed64_array(0 downto 0);
        data_valid_out : out bit_array(0 downto 0);
        done_out : out std_logic
    );
end lat_gps_input_stream_entity;

architecture behavioral of lat_gps_input_stream_entity is

    -- Internal Signal Declarations
    signal done : std_logic;
    signal data : signed64_array(0 downto 0);
    signal data_valid : bit_array(0 downto 0);

    begin

    process (clk, rst) begin
        if (rst='1') then
            -- Reset Phase
            data(data'high downto 0) <= (others => (others => '0'));
            data_valid(data_valid'high downto 0) <= (others => '0');
            done <= '0';
        elsif (rising_edge(clk)) then
            -- Logic Phase
            if (upd = '1' and done = '0') then
                -- Register Update
                data <= data(data'high-1 downto 0) & data_in;
                data_valid <= data_valid(data_valid'high-1 downto 0) & '1';
                done <= '1';
            elsif (upd = '0') then
                -- Reset done Signal
                done <= '0';
            end if;
        end if;
    end process;

    -- Mapping Register to Output Wires
    data_out <= data;
    data_valid_out <= data_valid;
    done_out <= done;

end behavioral;
