LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;

package fir_filter_pkg is

    CONSTANT FIR_PIPELINE_DLY : INTEGER := 10;
    CONSTANT DAC_FIFO_SIZE : INTEGER := 16;
    CONSTANT ADC_FIFO_SIZE : INTEGER := 16;
    CONSTANT c_NUM_BITS : INTEGER := 16;
    CONSTANT c_SEED_COUNTER_BITS : INTEGER := 16;
    CONSTANT c_CLK_PERIOD : TIME := 2 ns;


    constant C_COEFS_LENGTH : integer := 16;
    constant C_MULT_LENGTH : integer := 32;
    constant C_ADD_LENGTH : integer := 33;
    constant C_DATA_IN_LENGTH : integer := 16;
    constant C_DATA_OUT_LENGTH : integer := 16;
    
    constant NUM_OF_TAPS : integer := 4;
    
	type COEFS_TYPE is array(NUM_OF_TAPS-1 downto 0) of std_logic_vector(C_COEFS_LENGTH-1 DOWNTO 0);
    type MULT_TYPE is array(NUM_OF_TAPS-1 downto 0) of std_logic_vector(C_COEFS_LENGTH-1 downto 0);
    type ADD_TYPE is array(NUM_OF_TAPS-1 downto 0) of std_logic_vector(C_COEFS_LENGTH-1 downto 0);


    type DAC_FIFO_DATA_CNT_TYPE is array(NUM_OF_TAPS-1 downto 0) of std_logic_vector(DAC_FIFO_SIZE-1 downto 0);
    type ADC_FIFO_DATA_CNT_TYPE is array(NUM_OF_TAPS-1 downto 0) of std_logic_vector(ADC_FIFO_SIZE-1 downto 0);
    
    type FIR_FILTER_DATA_TYPE is array(NUM_OF_TAPS-1 downto 0) of std_logic_vector(C_DATA_IN_LENGTH-1 downto 0);
    type FIR_FILTER_LOGIC_TYPE is array(NUM_OF_TAPS-1 downto 0) of std_logic;

    
    constant ZERO : std_logic_vector(C_COEFS_LENGTH-1 downto 0) := (others => '0');

    constant C_FIR_COEFS : COEFS_TYPE := (
        b"0010_0000_0110_1101",
        b"0000_1111_1101_0000",
        b"0110_1001_0110_1010",
        b"1101_1110_1101_1001"
    );
    constant ZERO_16 : std_logic_vector(15 downto 0) := (others => '0');

end fir_filter_pkg;