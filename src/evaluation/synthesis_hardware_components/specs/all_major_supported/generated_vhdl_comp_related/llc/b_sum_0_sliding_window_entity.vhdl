library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

--* Sliding window in the specification:
--* b.aggregate(over: 0.1 s, using: sum)
--* Source Stream: c @1000Hz
--* Number of Buckets: 100
--* Time per Bucket: 0.001s
--* Input Type: Int64
--* Return Type: Int64

entity b_sum_0_sliding_window_entity is
    port (
        clk, rst : in std_logic;
        evict, upd, request : in std_logic;
        time_in : in unsigned(63 downto 0);
        data_in : in signed(63 downto 0);
        data_out : out signed(63 downto 0);
        data_valid_out : out std_logic;
        evict_done_out : out std_logic;
        upd_done_out : out std_logic;
        request_done_out : out std_logic
    );
end b_sum_0_sliding_window_entity;

architecture behavioral of b_sum_0_sliding_window_entity is

    -- Internal Signal Declarations
    signal last_ts_before_upd : unsigned(63 downto 0);
    signal sw_data : signed(63 downto 0);
    signal sw_data_valid : std_logic;
    -- Done Signals
    signal evict_done : std_logic;
    signal upd_done : std_logic;
    signal request_done : std_logic;
    -- Buckets
    signal ts_buckets : unsigned64_array(99 downto 0);
    signal sum_buckets : signed64_array(99 downto 0);
    signal data_valid_buckets : bit_array(99 downto 0);

    begin

    process (clk, rst) begin
        if (rst='1') then
            -- Reset Phase
            evict_done <= '0';
            upd_done <= '0';
            request_done <= '0';
            sw_data <= to_signed(0, sw_data'length);
            sw_data_valid <= '1';
            last_ts_before_upd <= to_unsigned(0, last_ts_before_upd'length);
            -- Reset Buckets
            ts_buckets(ts_buckets'high downto 0) <= (others => (others => '0'));
            data_valid_buckets(data_valid_buckets'high downto 0) <= (others => '1');
            sum_buckets(sum_buckets'high downto 0) <= (others => to_signed(0, sw_data'length));
        elsif (rising_edge(clk)) then
            -- Logic Phase
            if (evict = '1' and evict_done = '0') then
                -- Evict Case: New TimeStamp
                if time_in > last_ts_before_upd then
                    -- Update Timestamp
                    last_ts_before_upd <= last_ts_before_upd + to_unsigned(1000000, last_ts_before_upd'length);
                    -- Create New Bucket and Shift Bucket Array
                    ts_buckets <= ts_buckets(ts_buckets'high-1 downto 0) & (last_ts_before_upd + to_unsigned(1000000, ts_buckets(0)'length));
                    data_valid_buckets <= data_valid_buckets(data_valid_buckets'high-1 downto 0) & '1';
                    sum_buckets <= sum_buckets(sum_buckets'high-1 downto 0) & to_signed(0, sw_data'length);
                else
                    evict_done <= '1';
                end if;
            elsif (upd = '1' and upd_done = '0') then
                -- Update Case: Map New Input and Update Last Buckets Entry
                sum_buckets(0) <= sum_buckets(0) + data_in;
                upd_done <= '1';
            elsif (request = '1' and request_done = '0') then
                -- Request Case: Finalize Buckets
                sw_data <= sum_buckets(0) + sum_buckets(1) + sum_buckets(2) + sum_buckets(3) + sum_buckets(4) + sum_buckets(5) + sum_buckets(6) + sum_buckets(7) + sum_buckets(8) + sum_buckets(9) + sum_buckets(10) + sum_buckets(11) + sum_buckets(12) + sum_buckets(13) + sum_buckets(14) + sum_buckets(15) + sum_buckets(16) + sum_buckets(17) + sum_buckets(18) + sum_buckets(19) + sum_buckets(20) + sum_buckets(21) + sum_buckets(22) + sum_buckets(23) + sum_buckets(24) + sum_buckets(25) + sum_buckets(26) + sum_buckets(27) + sum_buckets(28) + sum_buckets(29) + sum_buckets(30) + sum_buckets(31) + sum_buckets(32) + sum_buckets(33) + sum_buckets(34) + sum_buckets(35) + sum_buckets(36) + sum_buckets(37) + sum_buckets(38) + sum_buckets(39) + sum_buckets(40) + sum_buckets(41) + sum_buckets(42) + sum_buckets(43) + sum_buckets(44) + sum_buckets(45) + sum_buckets(46) + sum_buckets(47) + sum_buckets(48) + sum_buckets(49) + sum_buckets(50) + sum_buckets(51) + sum_buckets(52) + sum_buckets(53) + sum_buckets(54) + sum_buckets(55) + sum_buckets(56) + sum_buckets(57) + sum_buckets(58) + sum_buckets(59) + sum_buckets(60) + sum_buckets(61) + sum_buckets(62) + sum_buckets(63) + sum_buckets(64) + sum_buckets(65) + sum_buckets(66) + sum_buckets(67) + sum_buckets(68) + sum_buckets(69) + sum_buckets(70) + sum_buckets(71) + sum_buckets(72) + sum_buckets(73) + sum_buckets(74) + sum_buckets(75) + sum_buckets(76) + sum_buckets(77) + sum_buckets(78) + sum_buckets(79) + sum_buckets(80) + sum_buckets(81) + sum_buckets(82) + sum_buckets(83) + sum_buckets(84) + sum_buckets(85) + sum_buckets(86) + sum_buckets(87) + sum_buckets(88) + sum_buckets(89) + sum_buckets(90) + sum_buckets(91) + sum_buckets(92) + sum_buckets(93) + sum_buckets(94) + sum_buckets(95) + sum_buckets(96) + sum_buckets(97) + sum_buckets(98) + sum_buckets(99);
                sw_data_valid <= '1';
                request_done <= '1';
            elsif (evict ='0' and upd = '0' and request = '0') then
                evict_done <= '0';
                upd_done <= '0';
                request_done <= '0';
            end if;
        end if;
    end process;

    -- Map Internal Signals to Output Wires
    data_out <= sw_data;
    data_valid_out <= sw_data_valid;
    evict_done_out <= evict_done;
    upd_done_out <= upd_done;
    request_done_out <= request_done;

end behavioral;
