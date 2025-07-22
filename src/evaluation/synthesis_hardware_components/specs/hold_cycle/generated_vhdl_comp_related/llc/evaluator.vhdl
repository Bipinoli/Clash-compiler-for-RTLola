library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.fixed_pkg.all;
use work.array_type_pkg.all;

entity evaluator is
    port (
        clk, input_clk, rst : in std_logic;
        input_time : in unsigned(63 downto 0);
		x : in signed(63 downto 0);
		x_en : in std_logic;
		y : in signed(63 downto 0);
		y_en : in std_logic;
		a_en : in std_logic;
		b_en : in std_logic;
		c_en : in std_logic;
		d_en : in std_logic;
		e_en : in std_logic;
		f_en : in std_logic;
		g_en : in std_logic;
		a : out signed(63 downto 0);
		b : out signed(63 downto 0);
		c : out signed(63 downto 0);
		d : out signed(63 downto 0);
		e : out signed(63 downto 0);
		f : out signed(63 downto 0);
		g : out signed(63 downto 0);
        done : out std_logic;
        valid : out std_logic
    );
end evaluator;

--* Specification:
--* input x : Int64
--* input y : Int64
--* output a := (x + 1)
--* output b := (y.hold().defaults(to: 1) + d.hold().defaults(to: 2))
--* output c := (b.hold().defaults(to: 10) + a.hold().defaults(to: 11))
--* output d := (c.hold().defaults(to: 0) + 1)
--* output e := (e.offset(by: neg1).defaults(to: 0) + 1)
--* output f := (e + 1)
--* output g := (f + 1)


architecture mixed of evaluator is

    -- Component Declaration
	--* input x : Int64
    component x_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* input y : Int64
    component y_input_stream_entity
	    port (
		    clk, upd, rst : in std_logic;
		    data_in : in signed(63 downto 0);
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    done_out : out std_logic
	    );
    end component;

	--* output a := (x + 1)
    component a_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			x_0 : in signed(63 downto 0);
			x_data_valid_0 : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output b := (y.hold().defaults(to: 1) + d.hold().defaults(to: 2))
    component b_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			y_0 : in signed(63 downto 0);
			y_data_valid_0 : in std_logic;
			d_0 : in signed(63 downto 0);
			d_data_valid_0 : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output c := (b.hold().defaults(to: 10) + a.hold().defaults(to: 11))
    component c_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			a_0 : in signed(63 downto 0);
			a_data_valid_0 : in std_logic;
			b_0 : in signed(63 downto 0);
			b_data_valid_0 : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output d := (c.hold().defaults(to: 0) + 1)
    component d_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			c_0 : in signed(63 downto 0);
			c_data_valid_0 : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output e := (e.offset(by: neg1).defaults(to: 0) + 1)
    component e_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			e_neg1 : in signed(63 downto 0);
			e_data_valid_neg1 : in std_logic;
		    data_out : out signed64_array(1 downto 0);
		    data_valid_out : out bit_array(1 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output f := (e + 1)
    component f_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			e_0 : in signed(63 downto 0);
			e_data_valid_0 : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;

	--* output g := (f + 1)
    component g_output_stream_entity
	    port (
		    clk, pe, eval, rst : in std_logic;
			f_0 : in signed(63 downto 0);
			f_data_valid_0 : in std_logic;
		    data_out : out signed64_array(0 downto 0);
		    data_valid_out : out bit_array(0 downto 0);
		    pe_done_out : out std_logic;
		    eval_done_out : out std_logic
	    );
    end component;


    -- Internal Signal Declarations
	signal x_upd : std_logic;
	signal x_upd_done : std_logic;
	signal x_entity_data_0 : signed(63 downto 0);
	signal x_entity_data_valid_0 : std_logic;
	signal y_upd : std_logic;
	signal y_upd_done : std_logic;
	signal y_entity_data_0 : signed(63 downto 0);
	signal y_entity_data_valid_0 : std_logic;
	signal a_pe : std_logic;
	signal a_eval : std_logic;
	signal a_pe_done : std_logic;
	signal a_eval_done : std_logic;
	signal a_entity_data_0 : signed(63 downto 0);
	signal a_entity_data_valid_0 : std_logic;
	signal b_pe : std_logic;
	signal b_eval : std_logic;
	signal b_pe_done : std_logic;
	signal b_eval_done : std_logic;
	signal b_entity_data_0 : signed(63 downto 0);
	signal b_entity_data_valid_0 : std_logic;
	signal c_pe : std_logic;
	signal c_eval : std_logic;
	signal c_pe_done : std_logic;
	signal c_eval_done : std_logic;
	signal c_entity_data_0 : signed(63 downto 0);
	signal c_entity_data_valid_0 : std_logic;
	signal d_pe : std_logic;
	signal d_eval : std_logic;
	signal d_pe_done : std_logic;
	signal d_eval_done : std_logic;
	signal d_entity_data_0 : signed(63 downto 0);
	signal d_entity_data_valid_0 : std_logic;
	signal e_pe : std_logic;
	signal e_eval : std_logic;
	signal e_pe_done : std_logic;
	signal e_eval_done : std_logic;
	signal e_entity_data_0 : signed(63 downto 0);
	signal e_entity_data_valid_0 : std_logic;
	signal e_entity_data_1 : signed(63 downto 0);
	signal e_entity_data_valid_1 : std_logic;
	signal f_pe : std_logic;
	signal f_eval : std_logic;
	signal f_pe_done : std_logic;
	signal f_eval_done : std_logic;
	signal f_entity_data_0 : signed(63 downto 0);
	signal f_entity_data_valid_0 : std_logic;
	signal g_pe : std_logic;
	signal g_eval : std_logic;
	signal g_pe_done : std_logic;
	signal g_eval_done : std_logic;
	signal g_entity_data_0 : signed(63 downto 0);
	signal g_entity_data_valid_0 : std_logic;

    signal upd_and_pe_done : std_logic;
    signal evaluator_done : std_logic;
    signal valid_reg : std_logic;
    signal rst_en_done : std_logic;

begin
    -- Component Instantiation
	--* input x : Int64
    x_entity_instance: x_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => x_upd,
            data_in => x,
			data_out(0) => x_entity_data_0,
			data_valid_out(0) => x_entity_data_valid_0,
            done_out => x_upd_done
         );

	--* input y : Int64
    y_entity_instance: y_input_stream_entity
        port map (
            clk => clk,
            rst => rst,
            upd => y_upd,
            data_in => y,
			data_out(0) => y_entity_data_0,
			data_valid_out(0) => y_entity_data_valid_0,
            done_out => y_upd_done
         );

	--* output a := (x + 1)
    a_entity_instance: a_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => a_pe,
            eval => a_eval,
			x_0 => x_entity_data_0,
			x_data_valid_0 => x_entity_data_valid_0,
			data_out(0) => a_entity_data_0,
			data_valid_out(0) => a_entity_data_valid_0,
            pe_done_out => a_pe_done,
            eval_done_out => a_eval_done
        );

	--* output b := (y.hold().defaults(to: 1) + d.hold().defaults(to: 2))
    b_entity_instance: b_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => b_pe,
            eval => b_eval,
			y_0 => y_entity_data_0,
			y_data_valid_0 => y_entity_data_valid_0,
			d_0 => d_entity_data_0,
			d_data_valid_0 => d_entity_data_valid_0,
			data_out(0) => b_entity_data_0,
			data_valid_out(0) => b_entity_data_valid_0,
            pe_done_out => b_pe_done,
            eval_done_out => b_eval_done
        );

	--* output c := (b.hold().defaults(to: 10) + a.hold().defaults(to: 11))
    c_entity_instance: c_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => c_pe,
            eval => c_eval,
			a_0 => a_entity_data_0,
			a_data_valid_0 => a_entity_data_valid_0,
			b_0 => b_entity_data_0,
			b_data_valid_0 => b_entity_data_valid_0,
			data_out(0) => c_entity_data_0,
			data_valid_out(0) => c_entity_data_valid_0,
            pe_done_out => c_pe_done,
            eval_done_out => c_eval_done
        );

	--* output d := (c.hold().defaults(to: 0) + 1)
    d_entity_instance: d_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => d_pe,
            eval => d_eval,
			c_0 => c_entity_data_0,
			c_data_valid_0 => c_entity_data_valid_0,
			data_out(0) => d_entity_data_0,
			data_valid_out(0) => d_entity_data_valid_0,
            pe_done_out => d_pe_done,
            eval_done_out => d_eval_done
        );

	--* output e := (e.offset(by: neg1).defaults(to: 0) + 1)
    e_entity_instance: e_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => e_pe,
            eval => e_eval,
			e_neg1 => e_entity_data_1,
			e_data_valid_neg1 => e_entity_data_valid_1,
			data_out(0) => e_entity_data_0,
			data_out(1) => e_entity_data_1,
			data_valid_out(0) => e_entity_data_valid_0,
			data_valid_out(1) => e_entity_data_valid_1,
            pe_done_out => e_pe_done,
            eval_done_out => e_eval_done
        );

	--* output f := (e + 1)
    f_entity_instance: f_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => f_pe,
            eval => f_eval,
			e_0 => e_entity_data_0,
			e_data_valid_0 => e_entity_data_valid_0,
			data_out(0) => f_entity_data_0,
			data_valid_out(0) => f_entity_data_valid_0,
            pe_done_out => f_pe_done,
            eval_done_out => f_eval_done
        );

	--* output g := (f + 1)
    g_entity_instance: g_output_stream_entity
        port map (
            clk => clk,
            rst => rst,
            pe => g_pe,
            eval => g_eval,
			f_0 => f_entity_data_0,
			f_data_valid_0 => f_entity_data_valid_0,
			data_out(0) => g_entity_data_0,
			data_valid_out(0) => g_entity_data_valid_0,
            pe_done_out => g_pe_done,
            eval_done_out => g_eval_done
        );


    process(clk, rst) begin
        if rst = '1' then
            -- Reset Phase
            valid_reg <= '0';
				x_upd <= '0';
				y_upd <= '0';
				a_pe <= '0';
				a_eval <= '0';
				b_pe <= '0';
				b_eval <= '0';
				c_pe <= '0';
				c_eval <= '0';
				d_pe <= '0';
				d_eval <= '0';
				e_pe <= '0';
				e_eval <= '0';
				f_pe <= '0';
				f_eval <= '0';
				g_pe <= '0';
				g_eval <= '0';
            upd_and_pe_done <= '1';
            evaluator_done <= '1';
            rst_en_done <= '0';
        elsif rising_edge(clk) then
            -- Logic Phase
            if input_clk = '1' then
                if upd_and_pe_done = '0' then
                    -- Input Stream Updates
                    --* Input Streams in Specification 
					--* - x 
					--* - y 
					x_upd <= x_en;
					y_upd <= y_en;
                    -- Pseudo Evaluation Phase
                    --* Output Streams in Specification 
					--* - a
					--* - b
					--* - c
					--* - d
					--* - e
					--* - f
					--* - g
					a_pe <= a_en;
					b_pe <= b_en;
					c_pe <= c_en;
					d_pe <= d_en;
					e_pe <= e_en;
					f_pe <= f_en;
					g_pe <= g_en;
                    -- Evict Phase
                    --* Sliding Windows in Specification 
                    upd_and_pe_done <= '1';
                    evaluator_done <= '0';
                else
                    -- Eval Phase
					--* output a := (x + 1)
					--* Evaluation Phase of Output Stream a is Influenced by No Lookup
					a_eval <= a_en and upd_and_pe_done;
					--* output b := (y.hold().defaults(to: 1) + d.hold().defaults(to: 2))
					--* Evaluation Phase of Output Stream b is Influenced by No Lookup
					b_eval <= b_en and upd_and_pe_done;
					--* output c := (b.hold().defaults(to: 10) + a.hold().defaults(to: 11))
					--* Evaluation Phase of Output Stream c is Influenced by No Lookup
					c_eval <= c_en and upd_and_pe_done;
					--* output d := (c.hold().defaults(to: 0) + 1)
					--* Evaluation Phase of Output Stream d is Influenced by No Lookup
					d_eval <= d_en and upd_and_pe_done;
					--* output e := (e.offset(by: neg1).defaults(to: 0) + 1)
					--* Evaluation Phase of Output Stream e is Influenced by No Lookup
					e_eval <= e_en and upd_and_pe_done;
					--* output f := (e + 1)
					--* Evaluation Phase of Output Stream f is Influenced by the following Lookups: 
					--* - Synchronous Lookup: e
					f_eval <= f_en and upd_and_pe_done and e_eval_done;
					--* output g := (f + 1)
					--* Evaluation Phase of Output Stream g is Influenced by the following Lookups: 
					--* - Synchronous Lookup: f
					g_eval <= g_en and upd_and_pe_done and f_eval_done;
                    -- SW Update Phase
                    -- SW Request Phase
                    -- Valid Assignment
					valid_reg <= '1' and a_entity_data_valid_0 and b_entity_data_valid_0 and c_entity_data_valid_0 and d_entity_data_valid_0 and e_entity_data_valid_0 and f_entity_data_valid_0 and g_entity_data_valid_0;
                    -- Evaluator Done assignment
					upd_and_pe_done <= '1' and (not x_en or x_upd_done) and (not y_en or y_upd_done) and (not a_en or a_pe_done) and (not b_en or b_pe_done) and (not c_en or c_pe_done) and (not d_en or d_pe_done) and (not e_en or e_pe_done) and (not f_en or f_pe_done) and (not g_en or g_pe_done);
					evaluator_done <= upd_and_pe_done and (not a_en or a_eval_done) and (not b_en or b_eval_done) and (not c_en or c_eval_done) and (not d_en or d_eval_done) and (not e_en or e_eval_done) and (not f_en or f_eval_done) and (not g_en or g_eval_done);
                end if;
            else
                upd_and_pe_done <= '0';
				x_upd <= '0';
				y_upd <= '0';
				a_pe <= '0';
				a_eval <= '0';
				b_pe <= '0';
				b_eval <= '0';
				c_pe <= '0';
				c_eval <= '0';
				d_pe <= '0';
				d_eval <= '0';
				e_pe <= '0';
				e_eval <= '0';
				f_pe <= '0';
				f_eval <= '0';
				g_pe <= '0';
				g_eval <= '0';
            end if;
        end if;
    end process;

    -- output port assignment
	a <= a_entity_data_0;
	b <= b_entity_data_0;
	c <= c_entity_data_0;
	d <= d_entity_data_0;
	e <= e_entity_data_0;
	f <= f_entity_data_0;
	g <= g_entity_data_0;
    valid <= valid_reg;
    done <= evaluator_done;

end mixed;