
-------------------------------------------------------------------------------
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
    DAC_CLK_P : IN STD_LOGIC;
    DAC_CLK_N : IN STD_LOGIC
  );

END ENTITY fir_filter_top;
ARCHITECTURE behave OF fir_filter_top IS

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

  CONSTANT c_NUM_BITS : INTEGER := 16;
  CONSTANT c_SEED_COUNTER_BITS : INTEGER := 16;
  CONSTANT c_CLK_PERIOD : TIME := 2 ns;


  SIGNAL fir_data_in : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL fir_data_out : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  
  SIGNAL w_LFSR_Done : STD_LOGIC;
  SIGNAL i_Enable : STD_LOGIC;

  -- seed related
  SIGNAL i_Seed_DV : STD_LOGIC;
  SIGNAL i_Seed_Data : STD_LOGIC_VECTOR(c_NUM_BITS - 1 DOWNTO 0);
  SIGNAL seed_counter : STD_LOGIC_VECTOR(c_SEED_COUNTER_BITS - 1 DOWNTO 0) := (OTHERS => '0');

  --reset 
  SIGNAL rst_n : STD_LOGIC := '0';
  SIGNAL reset_counter : STD_LOGIC_VECTOR(11 DOWNTO 0) := (OTHERS => '0');

  -- clk
  SIGNAL FPGA_CLK : STD_LOGIC;
  SIGNAL ADC_CLK : STD_LOGIC;
  SIGNAL DAC_CLK : STD_LOGIC;
  
  SIGNAL stop_reset_timer : STD_LOGIC;


  SIGNAL MULT : MULT_TYPE;
  SIGNAL ADD : ADD_TYPE;
BEGIN
  i_Seed_Data <= x"DEAD";

  -- GENERATE SINGLE ENDED ADC CLK FROM DIFFERENTIAL INPUT
  adc_clk_ibug : unisim.vcomponents.IBUFDS
  GENERIC MAP(
    DIFF_TERM => TRUE,
    DQS_BIAS => "FALSE",
    IOSTANDARD => "DEFAULT"
  )
  PORT MAP(
    O => adc_clk,
    I  => ADC_CLK_P,
    IB => ADC_CLK_N
  );
  -- GENERATE SINGLE ENDED ADC CLK FROM DIFFERENTIAL INPUT
  dac_clk_ibug : unisim.vcomponents.IBUFDS
  GENERIC MAP(
    DIFF_TERM => TRUE,
    DQS_BIAS => "FALSE",
    IOSTANDARD => "DEFAULT"
  )
  PORT MAP(
    O => dac_clk,
    I  => DAC_CLK_P,
    IB => DAC_CLK_N
  );
  -- GENERATE SINGLE ENDED ADC CLK FROM DIFFERENTIAL INPUT
  fpga_clk_ibug : unisim.vcomponents.IBUFDS
  GENERIC MAP(
    DIFF_TERM => TRUE,
    DQS_BIAS => "FALSE",
    IOSTANDARD => "DEFAULT"
  )
  PORT MAP(
    O => fpga_clk,
    I =>  FPGA_CLK_P,
    IB => FPGA_CLK_N
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

  LFSR_1 : ENTITY work.LFSR
    GENERIC MAP(
      g_Num_Bits => c_NUM_BITS)
    PORT MAP(
      i_Clk => adc_clk,
      i_Enable => i_Enable,
      i_Seed_DV => i_Seed_DV,
      i_Seed_Data => i_Seed_Data,
      o_LFSR_Data => fir_data_in,
      o_LFSR_Done => w_LFSR_Done
    );
  U_FIR_FILTER : FIR_FILTER
  GENERIC MAP(
    NUM_OF_TAPS => NUM_OF_TAPS,
    C_COEFS_LENGTH => C_COEFS_LENGTH,
    C_MULT_LENGTH => C_MULT_LENGTH,
    C_ADD_LENGTH => C_ADD_LENGTH,
    C_DATA_IN_LENGTH => C_DATA_IN_LENGTH,
    C_DATA_OUT_LENGTH => C_DATA_OUT_LENGTH)
  PORT MAP(
    i_clk => adc_clk,
    i_rstb => rst_n,
    -- coefficient
    i_coeff_0 => C_FIR_COEFS(0),
    i_coeff_1 => C_FIR_COEFS(1),
    i_coeff_2 => C_FIR_COEFS(2),
    i_coeff_3 => C_FIR_COEFS(3),
    -- data input
    i_data => fir_data_in,
    -- filtered data 
    o_data => fir_data_out
  );
END ARCHITECTURE behave;