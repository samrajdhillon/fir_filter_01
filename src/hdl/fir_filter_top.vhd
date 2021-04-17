
-------------------------------------------------------------------------------
-- Objective
-- Construct a finite impulse response (FIR) filter with 4 coefficients (taps) in HDL along with a test-bench.  
-- Perform synthesis, placement and routing, and static timing analysis.  
-- Input data is assumed to have been digitized from a 16-bit analog-to-digital converter (ADC) at a sample rate of 2000 MHz.  
-- The FPGA logic is operating at a slower rate of 500 MHz.  
-- Output data is assumed to be driving a digital-to-analog converter at a sample rate of 2000 MHz.  
-- A block diagram of the FIR filter and interfaces is shown below:

-- Author : Surinder Pal Singh

-- Dependencies : Need four FIFO to buffer up ADC data and additional four FIFO TO queue up the data in order to sample it 
--                @ DAC clock rate
-------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

USE work.fir_filter_pkg.ALL;

LIBRARY unisim;

USE unisim.vcomponents.ALL;

ENTITY fir_filter_top IS
  GENERIC (
    SIM_MODE : STRING := "FALSE";
    g_Num_Bits : INTEGER := 16
  );
  PORT (
    FPGA_CLK_P : IN STD_LOGIC;
    FPGA_CLK_N : IN STD_LOGIC;
    ADC_CLK_P : IN STD_LOGIC;
    ADC_CLK_N : IN STD_LOGIC;
    ADC_DATA_P : IN VECTOR(g_NUM_Bits-1 DOWNTO 0);
    ADC_DATA_N : IN VECTOR(g_NUM_Bits-1 DOWNTO 0);

    Valid_in : IN STD_LOGIC;
    DAC_CLK_P : IN STD_LOGIC;
    DAC_CLK_N : IN STD_LOGIC;
    DAC_DATA_P : OUT VECTOR(g_NUM_Bits-1 DOWNTO 0);
    DAC_DATA_N : OUT VECTOR(g_NUM_Bits-1 DOWNTO 0);
    DAC_VALID_OUT : OUT STD_LOGIC
  );

END ENTITY fir_filter_top;
ARCHITECTURE behave OF fir_filter_top IS
  COMPONENT FIR_FIFO1
    PORT (
      clk : IN STD_LOGIC;
      srst : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
      full : OUT STD_LOGIC;
      almost_full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC;
      data_count : OUT STD_LOGIC_VECTOR(16 DOWNTO 0)
    );
  END COMPONENT;
  COMPONENT FIR_FIFO2
    PORT (
      clk : IN STD_LOGIC;
      srst : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
      wr_en : IN STD_LOGIC;
      rd_en : IN STD_LOGIC;
      dout : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
      full : OUT STD_LOGIC;
      almost_full : OUT STD_LOGIC;
      empty : OUT STD_LOGIC;
      data_count : OUT STD_LOGIC_VECTOR(16 DOWNTO 0)
    );
  END COMPONENT;

  COMPONENT FIR_FILTER IS
    GENERIC (
      NUM_OF_TAPS : INTEGER := 4;
      C_COEFS_LENGTH : INTEGER := 16;
      C_MULT_LENGTH : INTEGER := 32;
      C_ADD_LENGTH : INTEGER := 33;
      C_DATA_IN_LENGTH : INTEGER := 16;
      C_DATA_OUT_LENGTH : INTEGER := 16
    );
    PORT (
      i_clk : IN STD_LOGIC;
      i_rstb : IN STD_LOGIC;
      -- coefficient
      i_coeff_0 : IN STD_LOGIC_VECTOR(C_COEFS_LENGTH - 1 DOWNTO 0);
      i_coeff_1 : IN STD_LOGIC_VECTOR(C_COEFS_LENGTH - 1 DOWNTO 0);
      i_coeff_2 : IN STD_LOGIC_VECTOR(C_COEFS_LENGTH - 1 DOWNTO 0);
      i_coeff_3 : IN STD_LOGIC_VECTOR(C_COEFS_LENGTH - 1 DOWNTO 0);
      -- data input
      i_data : IN STD_LOGIC_VECTOR(C_DATA_IN_LENGTH - 1 DOWNTO 0);
      -- filtered data 
      o_data : OUT STD_LOGIC_VECTOR(C_DATA_OUT_LENGTH - 1 DOWNTO 0));
  END COMPONENT;
  COMPONENT IBUFGDS
    GENERIC (
      CAPACITANCE : STRING := "DONT_CARE";
      DIFF_TERM : BOOLEAN := FALSE;
      IBUF_DELAY_VALUE : STRING := "0";
      IBUF_LOW_PWR : BOOLEAN := TRUE;
      IOSTANDARD : STRING := "DEFAULT");

    PORT (
      O : OUT STD_LOGIC;
      I : IN STD_LOGIC;
      IB : IN STD_LOGIC
    );
  END COMPONENT;

  CONSTANT FIR_PIPELINE_DLY : INTEGER := 10;
  CONSTANT c_NUM_BITS : INTEGER := 16;
  CONSTANT c_SEED_COUNTER_BITS : INTEGER := 16;
  CONSTANT c_CLK_PERIOD : TIME := 2 ns;
  SIGNAL fir_data_in : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL fir_data_out : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL fir_data_out_IO : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);

  SIGNAL w_LFSR_Done : STD_LOGIC;
  SIGNAL i_Enable : STD_LOGIC;

  -- seed related
  SIGNAL i_Seed_DV : STD_LOGIC;
  SIGNAL i_Seed_Data : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL seed_counter : STD_LOGIC_VECTOR(c_SEED_COUNTER_BITS - 1 DOWNTO 0) := (OTHERS => '0');

  --reset 
  SIGNAL rst_n : STD_LOGIC := '0';
  SIGNAL reset_counter : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');
  SIGNAL d_valid_counter : STD_LOGIC_VECTOR(3 DOWNTO 0) := (OTHERS => '1');
  SIGNAL d_valid_counter_en : STD_LOGIC;

  SIGNAL d_read_counter : STD_LOGIC_VECTOR(1 DOWNTO 0) := (OTHERS => '1');
  SIGNAL d_read_counter_en : STD_LOGIC;
  SIGNAL d_wr_counter : STD_LOGIC_VECTOR(1 DOWNTO 0) := (OTHERS => '1');
  SIGNAL d_wr_counter_en : STD_LOGIC;
  -- clk
  SIGNAL FPGA_CLK : STD_LOGIC;
  SIGNAL ADC_CLK : STD_LOGIC;
  SIGNAL DAC_CLK : STD_LOGIC;

  SIGNAL stop_reset_timer : STD_LOGIC;
  SIGNAL MULT : MULT_TYPE;
  SIGNAL ADD : ADD_TYPE;

  SIGNAL s_adc_data_Valid : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  -- fir fifo 

  SIGNAL fifo1_din, fifo2_din, fifo3_din, fifo4_din : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL fifo1_wr_en, fifo2_wr_en, fifo3_wr_en, fifo4_wr_en : STD_LOGIC;
  SIGNAL fifo1_rd_en, fifo2_rd_en, fifo3_rd_en, fifo4_rd_en : STD_LOGIC;
  SIGNAL fifo1_dout, fifo2_dout, fifo3_dout, fifo4_dout : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL fifo1_rst, fifo2_rst, fifo3_rst, fifo4_rst : STD_LOGIC;
  SIGNAL fifo1_empty, fifo2_empty, fifo3_empty, fifo4_empty, fifo_data_valid : STD_LOGIC;

  SIGNAL dac_fifo1_din, dac_fifo2_din, dac_fifo3_din, dac_fifo4_din : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL dac_fifo1_wr_en, dac_fifo2_wr_en, dac_fifo3_wr_en, dac_fifo4_wr_en : STD_LOGIC;
  SIGNAL dac_fifo1_rd_en, dac_fifo2_rd_en, dac_fifo3_rd_en, dac_fifo4_rd_en : STD_LOGIC;
  SIGNAL dac_fifo1_dout, dac_fifo2_dout, dac_fifo3_dout, dac_fifo4_dout : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL dac_fifo1_rst, dac_fifo2_rst, dac_fifo3_rst, dac_fifo4_rst : STD_LOGIC;
  SIGNAL dac_fifo1_empty, dac_fifo2_empty, dac_fifo3_empty, dac_fifo4_empty : STD_LOGIC;
  SIGNAL dac_fifo1_data_count : STD_LOGIC_VECTOR(16 DOWNTO 0);

  SIGNAL d_strb_Q, d1_strb_Q, d2_strb_Q, d3_strb_Q, d4_strb_Q : STD_LOGIC;
  SIGNAL d1_strb_QQ, d2_strb_QQ, d3_strb_QQ, d4_strb_QQ : STD_LOGIC;
  SIGNAL d1_Q, d2_Q, d3_Q, d4_Q : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL d1_QQ, d2_QQ, d3_QQ, d4_QQ : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL d1_QQQ, d2_QQQ, d3_QQQ, d4_QQQ : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL d_out1_Q, d_out2_Q, d_out3_Q, d_out4_Q : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);

  SIGNAL s_fir_data_in, s_fir_data_in_Q, s_fir_data_in_QQ : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL s_fir_data_out, s_fir_data_out2 : STD_LOGIC_VECTOR(3 DOWNTO 0);
  SIGNAL s_dac_fifo_wr_strb, s_dac_fifo_rd_strb : STD_LOGIC_VECTOR(11 DOWNTO 0);
  SIGNAL fir_data_in_Q : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
BEGIN
  i_Seed_Data <= x"DEAD";
  -- fir_data_out_IO <= fir_data_out;


  G_DIFF_OBUF : FOR I IN 0 TO 15 GENERATE
  DIFF_OBUF : OBUFDS PORT MAP (
    I => fir_data_out_IO(I),
    O => DAC_DATA_P(I),
    OB => DAC_DATA_N(I)    
  );
  END GENERATE G_DIFF_OBUG;
  
  G_SNGL_IBUG : FOR I IN 0 TO 15 GENERATE
  SNGL_IBUF : IBUFGDS PORT MAP (
    O => fir_data_in(I),
    I => ADC_DATA_P(I),
    IB => ADC_DATA_N(I)    
  );
  END GENERATE G_SNGL_IBUG;

  -- GENERATE SINGLE ENDED ADC CLK FROM DIFFERENTIAL INPUT

  adc_ibuf : IBUFGDS
  GENERIC MAP (
    DIFF_TERM => FALSE,
    IBUF_LOW_PWR =>  FALSE)

  PORT MAP (
    O  => ADC_CLK,
    I  => ADC_CLK_P,
    IB => ADC_CLK_N
  );

  -- GENERATE SINGLE ENDED dac CLK FROM DIFFERENTIAL INPUT
  dac_clk_ibuf : IBUFGDS
  GENERIC MAP(
    DIFF_TERM => FALSE,
    IBUF_LOW_PWR => FALSE)
  PORT MAP(
    O => dac_clk,
    I => DAC_CLK_P,
    IB => DAC_CLK_N
  );

  -- GENERATE SINGLE ENDED fpga CLK FROM DIFFERENTIAL INPUT
  fpga_clk_ibuf : IBUFGDS
  GENERIC MAP(
    DIFF_TERM => FALSE,
    IBUF_LOW_PWR => FALSE)
  PORT MAP(
    O => fpga_clk,
    I => FPGA_CLK_P,
    IB => FPGA_CLK_N
  );

  fifo1_rst <= NOT rst_n;
  fifo2_rst <= NOT rst_n;
  fifo3_rst <= NOT rst_n;
  fifo4_rst <= NOT rst_n;
  dac_fifo1_rst <= NOT rst_n;
  dac_fifo2_rst <= NOT rst_n;
  dac_fifo3_rst <= NOT rst_n;
  dac_fifo4_rst <= NOT rst_n;

  U_FIR_FIFO1 : FIR_FIFO1
  PORT MAP(
    clk => fpga_clk,
    srst => fifo1_rst,
    din => fifo1_din,
    wr_en => fifo1_wr_en,
    rd_en => fifo1_rd_en,
    dout => fifo1_dout,
    full => OPEN,
    almost_full => OPEN,
    empty => fifo1_empty,
    data_count => OPEN
  );

  U_FIR_FIFO2 : FIR_FIFO1
  PORT MAP(
    clk => fpga_clk,
    srst => fifo2_rst,
    din => fifo2_din,
    wr_en => fifo2_wr_en,
    rd_en => fifo2_rd_en,
    dout => fifo2_dout,
    full => OPEN,
    almost_full => OPEN,
    empty => fifo2_empty,
    data_count => OPEN
  );

  U_FIR_FIFO3 : FIR_FIFO1
  PORT MAP(
    clk => fpga_clk,
    srst => fifo3_rst,
    din => fifo3_din,
    wr_en => fifo3_wr_en,
    rd_en => fifo3_rd_en,
    dout => fifo3_dout,
    full => OPEN,
    almost_full => OPEN,
    empty => fifo3_empty,
    data_count => OPEN
  );

  U_FIR_FIFO4 : FIR_FIFO1
  PORT MAP(
    clk => fpga_clk,
    srst => fifo4_rst,
    din => fifo4_din,
    wr_en => fifo4_wr_en,
    rd_en => fifo4_rd_en,
    dout => fifo4_dout,
    full => OPEN,
    almost_full => OPEN,
    empty => fifo4_empty,
    data_count => OPEN
  );

  U_DAC_FIR_FIFO1 : FIR_FIFO2
  PORT MAP(
    clk => fpga_clk,
    srst => dac_fifo1_rst,
    din => dac_fifo1_din,
    wr_en => dac_fifo1_wr_en,
    rd_en => dac_fifo1_rd_en,
    dout => dac_fifo1_dout,
    full => OPEN,
    almost_full => OPEN,
    empty => dac_fifo1_empty,
    data_count => dac_fifo1_data_count
  );

  U_DAC_FIR_FIFO2 : FIR_FIFO2
  PORT MAP(
    clk => fpga_clk,
    srst => dac_fifo2_rst,
    din => dac_fifo2_din,
    wr_en => dac_fifo2_wr_en,
    rd_en => dac_fifo2_rd_en,
    dout => dac_fifo2_dout,
    full => OPEN,
    almost_full => OPEN,
    empty => dac_fifo2_empty,
    data_count => OPEN
  );

  U_DAC_FIR_FIFO3 : FIR_FIFO2
  PORT MAP(
    clk => fpga_clk,
    srst => dac_fifo3_rst,
    din => dac_fifo3_din,
    wr_en => dac_fifo3_wr_en,
    rd_en => dac_fifo3_rd_en,
    dout => dac_fifo3_dout,
    full => OPEN,
    almost_full => OPEN,
    empty => dac_fifo3_empty,
    data_count => OPEN
  );

  U_DAC_FIR_FIFO4 : FIR_FIFO2
  PORT MAP(
    clk => fpga_clk,
    srst => dac_fifo4_rst,
    din => dac_fifo4_din,
    wr_en => dac_fifo4_wr_en,
    rd_en => dac_fifo4_rd_en,
    dout => dac_fifo4_dout,
    full => OPEN,
    almost_full => OPEN,
    empty => dac_fifo4_empty,
    data_count => OPEN
  );
  p_reset : PROCESS (adc_clk, stop_reset_timer)
  BEGIN
    IF rising_edge(adc_clk) THEN
      rst_n <= reset_counter(11);
      IF (stop_reset_timer = '0') THEN
        reset_counter <= reset_counter + b"1";
      END IF;
    END IF;
  END PROCESS p_reset;

  stop_reset_timer <= reset_counter(11);

  p_Seed : PROCESS (adc_clk, rst_n)
  BEGIN
    IF (rst_n = '0') THEN
      i_Seed_DV <= '1';
      i_Enable <= '0';
    ELSIF rising_edge(adc_clk) THEN
      --default
      i_Seed_DV <= '0';
      i_Enable <= '1';
      -- seed counter
      IF (seed_counter = 0) THEN
        i_Seed_DV <= '1';
        seed_counter <= (OTHERS => '1');
      ELSE
        seed_counter <= seed_counter - b"1";
      END IF;
    END IF;
  END PROCESS p_Seed;

  p_adc_data : PROCESS (adc_clk, rst_n)
  BEGIN
    IF (rst_n = '0') THEN
      d_valid_counter_en <= '0';
      d_valid_counter <= (OTHERS => '1');
      s_adc_data_Valid <= (OTHERS => '0');
    ELSIF rising_edge(adc_clk) THEN
      -- adc data acquisation into fir fifo cntrl
      IF (Valid_in = '1') THEN
        s_adc_data_Valid <= x"0001";
        d_strb_Q <= '0';
      ELSE
        s_adc_data_Valid <= s_adc_data_Valid(c_NUM_BITS - 2 DOWNTO 0) & b"0";
      END IF;
      -- determine where to load incoming adc data 
      d1_strb_Q <= s_adc_data_Valid(0) OR s_adc_data_Valid(4) OR s_adc_data_Valid(8) OR s_adc_data_Valid(12);
      d2_strb_Q <= s_adc_data_Valid(1) OR s_adc_data_Valid(5) OR s_adc_data_Valid(9) OR s_adc_data_Valid(13);
      d3_strb_Q <= s_adc_data_Valid(2) OR s_adc_data_Valid(6) OR s_adc_data_Valid(10) OR s_adc_data_Valid(14);
      d4_strb_Q <= s_adc_data_Valid(3) OR s_adc_data_Valid(7) OR s_adc_data_Valid(11) OR s_adc_data_Valid(15);

      d1_strb_QQ <= d1_strb_Q;
      d2_strb_QQ <= d2_strb_Q;
      d3_strb_QQ <= d3_strb_Q;
      d4_strb_QQ <= d4_strb_Q;

      -- TRIGGER FIFO ENABLE LOGIC AFTER LAST VALID REGISTER WR AND HOLD IT HIGH FOR VALID DATA PERIOD
      IF (d4_strb_Q = '1') THEN
        d_strb_Q <= '1';
        d_valid_counter_en <= '1';
      ELSIF (d_valid_counter = 0) THEN
        d_strb_Q <= '0';
        d_valid_counter_en <= '0';
      END IF;

      IF (d_valid_counter_en = '0') THEN
        d_valid_counter <= (OTHERS => '1');
      ELSIF (d_valid_counter_en <= '1') THEN
        d_valid_counter <= d_valid_counter - b"1";
      END IF;

      -- direct incoming adc data into four different register for parallelism to save and process it at slower clock rate
      IF (d1_strb_Q = '1') THEN
        d1_Q <= fir_data_in;
      ELSIF (d2_strb_Q = '1') THEN
        d2_Q <= fir_data_in;
      ELSIF (d3_strb_Q = '1') THEN
        d3_Q <= fir_data_in;
      ELSIF (d4_strb_Q = '1') THEN
        d4_Q <= fir_data_in;
      END IF;

      d1_QQ <= d1_Q;
      d2_QQ <= d2_Q;
      d3_QQ <= d3_Q;
      d4_QQ <= d4_Q;

      d1_QQQ <= d1_QQ;
      d2_QQQ <= d2_QQ;
      d3_QQQ <= d3_QQ;
      d4_QQQ <= d4_QQ;
    END IF;
  END PROCESS p_adc_data;
  p_load_fifo : PROCESS (fpga_clk, rst_n)
  BEGIN
    IF (rst_n = '0') THEN
      fifo1_din <= x"DEAD";
      fifo1_wr_en <= '0';
      fifo2_din <= x"BABE";
      fifo2_wr_en <= '0';
      fifo3_din <= x"BEEF";
      fifo3_wr_en <= '0';
      fifo4_din <= x"FABE";
      fifo4_wr_en <= '0';
      -- fifo3_rd_en  
      -- fifo3_dout  

    ELSIF rising_edge(fpga_clk) THEN
      --default values 
      fifo1_wr_en <= '0';
      fifo2_wr_en <= '0';
      fifo3_wr_en <= '0';
      fifo4_wr_en <= '0';

      -- GENERATE WR STRB FOR FIR FIFO
      IF (d_strb_Q = '1') THEN
        fifo1_din <= d1_QQQ;
        fifo1_wr_en <= '1';

        fifo2_din <= d2_QQQ;
        fifo2_wr_en <= '1';

        fifo3_din <= d3_QQ;
        fifo3_wr_en <= '1';

        fifo4_din <= d4_Q;
        fifo4_wr_en <= '1';
      END IF;
    END IF;
  END PROCESS p_load_fifo;

  p_fetch_fifo : PROCESS (fpga_clk, rst_n)
  BEGIN
    IF (rst_n = '0') THEN
      fifo1_rd_en <= '0';
      fifo2_rd_en <= '0';
      fifo3_rd_en <= '0';
      fifo4_rd_en <= '0';
      d_read_counter <= (OTHERS => '1');
      d_read_counter_en <= '0';
      s_fir_data_in_Q <= (OTHERS => '0');
      s_fir_data_in_QQ <= (OTHERS => '0');
    ELSIF rising_edge(fpga_clk) THEN
      --default values 
      fifo1_rd_en <= '0';
      fifo2_rd_en <= '0';
      fifo3_rd_en <= '0';
      fifo4_rd_en <= '0';

      d_out1_Q <= fifo1_dout;
      d_out2_Q <= fifo2_dout;
      d_out3_Q <= fifo3_dout;
      d_out4_Q <= fifo4_dout;
      s_fir_data_in <= x"1";

      fifo_data_valid <= NOT (fifo1_empty OR fifo2_empty OR fifo3_empty OR fifo4_empty);
      -- GENERATE rd STRB FOR FIR FIFO
      s_fir_data_in_Q <= s_fir_data_in;
      s_fir_data_in_QQ <= s_fir_data_in_Q;
      IF (d_read_counter = 0) THEN
        fifo1_rd_en <= '1';
        fifo2_rd_en <= '1';
        fifo3_rd_en <= '1';
        fifo4_rd_en <= '1';
      ELSIF (fifo_data_valid = '1') THEN
        d_read_counter_en <= '1';
        s_fir_data_in <= s_fir_data_in(2 DOWNTO 0) & b"0";
      ELSIF (fifo_data_valid = '0') THEN
        d_read_counter_en <= '0';
      END IF;

      IF (d_read_counter_en = '0') THEN
        d_read_counter <= (OTHERS => '1');
      ELSIF (d_read_counter_en = '1') THEN
        d_read_counter <= d_read_counter - b"1";
      END IF;

    END IF;
  END PROCESS p_fetch_fifo;

  p_data_pipeline : PROCESS (fpga_clk, rst_n)
  BEGIN
    IF (rst_n = '0') THEN
      fir_data_in_Q <= (OTHERS => '0');
    ELSIF rising_edge(fpga_clk) THEN
      --default values 

      IF (s_fir_data_in_QQ(0) = '1') THEN
        fir_data_in_Q <= d_out1_Q;
      ELSIF (s_fir_data_in_QQ(1) = '1') THEN
        fir_data_in_Q <= d_out2_Q;
      ELSIF (s_fir_data_in_QQ(2) = '1') THEN
        fir_data_in_Q <= d_out3_Q;
      ELSIF (s_fir_data_in_QQ(3) = '1') THEN
        fir_data_in_Q <= d_out4_Q;
      END IF;
    END IF;
  END PROCESS p_data_pipeline;
  p_dac_data_queue : PROCESS (fpga_clk, rst_n)
  BEGIN
    IF (rst_n = '0') THEN
      dac_fifo1_din <= x"DEAD";
      dac_fifo1_wr_en <= '0';
      dac_fifo2_din <= x"BABE";
      dac_fifo2_wr_en <= '0';
      dac_fifo3_din <= x"BEEF";
      dac_fifo3_wr_en <= '0';
      dac_fifo4_din <= x"FABE";
      dac_fifo4_wr_en <= '0';
      d_wr_counter <= (OTHERS => '1');
      s_dac_fifo_wr_strb <= (OTHERS => '0');
      d_wr_counter_en <= '0';
      s_fir_data_out <= x"1";
    ELSIF rising_edge(fpga_clk) THEN
      --default values 
      dac_fifo1_wr_en <= '0';
      dac_fifo2_wr_en <= '0';
      dac_fifo3_wr_en <= '0';
      dac_fifo4_wr_en <= '0';
      s_fir_data_out <= x"1";

      IF (d_read_counter_en = '1') THEN
        d_wr_counter_en <= '1';
        s_dac_fifo_wr_strb <= s_dac_fifo_wr_strb(10 DOWNTO 0) & d_wr_counter_en;
      ELSE
        d_wr_counter_en <= '0';
        s_dac_fifo_wr_strb <= (OTHERS => '0');
      END IF;

      IF (s_dac_fifo_wr_strb(FIR_PIPELINE_DLY) = '1') THEN
        s_fir_data_out <= s_fir_data_out(2 DOWNTO 0) & b"0";
        IF (s_fir_data_out(0) = '1') THEN
          dac_fifo1_wr_en <= '1';
          dac_fifo1_din <= fir_data_out;
        ELSIF (s_fir_data_out(1) = '1') THEN
          dac_fifo2_wr_en <= '1';
          dac_fifo2_din <= fir_data_out;
        ELSIF (s_fir_data_out(2) = '1') THEN
          dac_fifo3_wr_en <= '1';
          dac_fifo3_din <= fir_data_out;
        ELSIF (s_fir_data_out(3) = '1') THEN
          dac_fifo4_wr_en <= '1';
          dac_fifo4_din <= fir_data_out;
          s_fir_data_out <= x"1";
        END IF;
      END IF;

      IF (d_wr_counter = 0) THEN
        d_wr_counter <= (OTHERS => '1');
      ELSIF (d_wr_counter_en = '1') THEN
        d_wr_counter <= d_wr_counter - b"1";
      END IF;
    END IF;
  END PROCESS p_dac_data_queue;

  p_upconv_data_sample : PROCESS (dac_clk, rst_n)
  BEGIN
    IF (rst_n = '0') THEN
      dac_fifo1_rd_en <= '0';
      dac_fifo2_rd_en <= '0';
      dac_fifo3_rd_en <= '0';
      dac_fifo4_rd_en <= '0';
      d_rd_counter_en <= '0';
    ELSIF rising_edge(dac_clk) THEN
      --default values 
      dac_fifo1_rd_en <= '0';
      dac_fifo2_rd_en <= '0';
      dac_fifo3_rd_en <= '0';
      dac_fifo4_rd_en <= '0';
      IF (dac_fifo1_data_count(9) = '1') THEN -- 256 sample in dac_fifo1
        d_rd_counter_en <= '1';
        s_dac_fifo_rd_strb <= s_dac_fifo_rd_strb(10 DOWNTO 0) & d_rd_counter_en;
      ELSIF (dac_fifo1_empty = '1') THEN
        d_rd_counter_en <= '0';
        s_dac_fifo_rd_strb <= (OTHERS => '0');
      END IF;

      IF (d_rd_counter_en = '1') THEN
        dac_fifo1_rd_en <= '1';
        dac_fifo2_rd_en <= '1';
        dac_fifo3_rd_en <= '1';
        dac_fifo4_rd_en <= '1';
      END IF;

      IF (s_dac_fifo_rd_strb(FIR_PIPELINE_DLY) = '1') THEN
        s_fir_data_out2 <= s_fir_data_out2(2 DOWNTO 0) & b"0";
        IF (s_fir_data_out2(0) = '1') THEN
          fir_data_out_IO <= dac_fifo1_dout;
        ELSIF (s_fir_data_out2(1) = '1') THEN
          fir_data_out_IO <= dac_fifo2_dout;
        ELSIF (s_fir_data_out2(2) = '1') THEN
          fir_data_out_IO <= dac_fifo3_dout;
        ELSIF (s_fir_data_out2(3) = '1') THEN
          fir_data_out_IO <= dac_fifo4_dout;
          s_fir_data_out2 <= x"1";
        END IF;
      END IF;

      IF (d_rd_counter = 0) THEN
        d_rd_counter <= (OTHERS => '1');
      ELSIF (d_rd_counter_en = '1') THEN
        d_rd_counter <= d_wr_counter - b"1";
      END IF;
    END IF;
  END PROCESS p_upconv_data_sample;

  -- LFSR_1 : ENTITY work.LFSR
  --   GENERIC MAP(
  --     g_Num_Bits => c_NUM_BITS)
  --   PORT MAP(
  --     i_Clk => adc_clk,
  --     i_Enable => i_Enable,
  --     i_Seed_DV => i_Seed_DV,
  --     i_Seed_Data => i_Seed_Data,
  --     o_LFSR_Data => fir_data_in,
  --     o_LFSR_Done => w_LFSR_Done
  --   );
  
  U_FIR_FILTER : FIR_FILTER
  GENERIC MAP(
    NUM_OF_TAPS => NUM_OF_TAPS,
    C_COEFS_LENGTH => C_COEFS_LENGTH,
    C_MULT_LENGTH => C_MULT_LENGTH,
    C_ADD_LENGTH => C_ADD_LENGTH,
    C_DATA_IN_LENGTH => C_DATA_IN_LENGTH,
    C_DATA_OUT_LENGTH => C_DATA_OUT_LENGTH)
  PORT MAP(
    i_clk => fpga_clk,
    i_rstb => rst_n,
    -- coefficient
    i_coeff_0 => C_FIR_COEFS(0),
    i_coeff_1 => C_FIR_COEFS(1),
    i_coeff_2 => C_FIR_COEFS(2),
    i_coeff_3 => C_FIR_COEFS(3),
    -- data input
    i_data => fir_data_in_Q,
    -- filtered data 
    o_data => fir_data_out
  );
END ARCHITECTURE behave;