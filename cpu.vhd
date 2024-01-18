library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity cpu is
	  port
	  (
			clk   : in  std_logic;                       -- main clock
			reset : in  std_logic;                       -- reset button
			PCview : out std_logic_vector( 7 downto 0);  -- debugging outputs
			IRview : out std_logic_vector(15 downto 0);
			RAview : out std_logic_vector(15 downto 0);
			RBview : out std_logic_vector(15 downto 0);
			RCview : out std_logic_vector(15 downto 0);
			RDview : out std_logic_vector(15 downto 0);
			REview : out std_logic_vector(15 downto 0);
			iport : in  std_logic_vector(7 downto 0);    -- input port
			oport : out std_logic_vector(15 downto 0)  -- output port
		);
end entity;

architecture rtl of cpu is

	component ProgramRom
		port
		(
			address		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			clock		: IN STD_LOGIC  := '1';
			q		: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
		);
	end component;
	
	component DataRam
		port
		(
			address		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			clock		: IN STD_LOGIC  := '1';
			data		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
			wren		: IN STD_LOGIC ;
			q		: OUT STD_LOGIC_VECTOR (15 DOWNTO 0)
		);
	end component;
	
	component ALU
		port
		(
			srcA : in  unsigned(15 downto 0);         -- input A
			srcB : in  unsigned(15 downto 0);         -- input B
			op   : in  std_logic_vector(2 downto 0);  -- operation
			cr   : out std_logic_vector(3 downto 0);  -- condition outputs
			dest : out unsigned(15 downto 0)          -- output value
		);
	end component;

	signal state : std_logic_vector(3 downto 0);
	signal count : std_logic_vector(2 downto 0) := "000"; -- to pause in setup
	signal RA : std_logic_vector(15 downto 0);
	signal RB : std_logic_vector(15 downto 0);
	signal RC : std_logic_vector(15 downto 0);
	signal RD : std_logic_vector(15 downto 0);
	signal RE : std_logic_vector(15 downto 0);
	signal SP : std_logic_vector(15 downto 0);
	signal IR : std_logic_vector(15 downto 0);
	signal PC : std_logic_vector(7 downto 0);
	signal CR : std_logic_vector(3 downto 0);
	signal MAR : std_logic_vector(7 downto 0);
	signal MBR : std_logic_vector(15 downto 0);
	signal OUTREG : std_logic_vector(15 downto 0);
	
	signal srcA_bus : unsigned(15 downto 0);
	signal srcB_bus : unsigned(15 downto 0);
	signal op : std_logic_vector(2 downto 0);
	signal CR_bus : std_logic_vector(3 downto 0);
	signal dest_bus : unsigned(15 downto 0);
	
	signal ROM_out : std_logic_vector(15 downto 0);
	signal RAM_out : std_logic_vector(15 downto 0);
	signal RAM_we : std_logic;
	
begin

	ProgramRom1: ProgramRom
	port map(
		PC,
		clk,
		ROM_out);
	
	DataRam1: DataRam
	port map(
		MAR, 
		clk, 
		MBR, 
		RAM_we, 
		RAM_out);
	
	ALU1 : ALU
	port map(
		srcA_bus,
		srcB_bus,
		op,
		cr_bus,
		dest_bus);
		
	process (clk, reset)
	begin
		if reset = '0' then
			PC <= (others => '0');
			IR <= (others => '0');
			OUTREG <= (others => '0');
			MAR <= (others => '0');
			MBR <= (others => '0');
			RA <= (others => '0');
			RB <= (others => '0');
			RC <= (others => '0');
			RD <= (others => '0');
			RE <= (others => '0');
			SP <= (others => '0');
			CR <= (others => '0');
			count <= "000";
			state <= "0000";
		elsif (rising_edge(clk)) then
			case state is
				-- setup
				when "0000" =>
					if count = "111" then
						state <= "0001";
					end if;
					count <= count + 1;
				-- fetch
				when "0001" =>
					IR <= ROM_out;
					PC <= PC + 1;
					state <= "0010";
					-- implement transition to halt later
				-- execute setup
				when "0010" =>
					-- assign op for every instruction
					op <= IR(14 downto 12);
					case IR(15 downto 12) is
						-- load
						when "0000" =>
							if IR(11) = '1' then
								MAR <= IR(7 downto 0) + RE(7 downto 0);
							else
								MAR <= IR(7 downto 0);
							end if;
						-- store
						when "0001" =>
							if IR(11) = '1' then
								MAR <= IR(7 downto 0) + RE(7 downto 0);
							else
								MAR <= IR(7 downto 0);
							end if;
							
							case IR(10 downto 8) is
								when "000" => MBR <= RA;
								when "001" => MBR <= RB;
								when "010" => MBR <= RC;
								when "011" => MBR <= RD;
								when "100" => MBR <= RE;
								when "101" => MBR <= SP;
								when others => null;
							end case;
						-- unconditional branch
						when "0010" =>
							PC <= IR(7 downto 0);
						-- conditional branch, call, return, exit
						when "0011" =>
							op <= "011";
							case IR(11 downto 10) is
								-- conditional branch
								when "00" =>
									-- check condition
									case IR(9 downto 8) is
										when "00" =>
											if CR(0) = '1' then
												PC <= IR(7 downto 0);
											end if;
										when "01" =>
											if CR(1) = '1' then
												PC <= IR(7 downto 0);
											end if;
										when "10" =>
											if CR(2) = '1' then
												PC <= IR(7 downto 0);
											end if;
										when "11" =>
											if CR(3) = '1' then
												PC <= IR(7 downto 0);
											end if;
										when others =>
											null;
									end case;
								-- call
								when "01" =>
									PC <= IR(7 downto 0);
									MAR <= SP(7 downto 0);
									MBR <= "0000" & CR & PC;
									SP <= SP + 1;
								-- return
								when "10" =>
									MAR <= SP(7 downto 0) - 1;
									SP <= SP - 1;
								when others =>
									null;
							end case;
						-- push
						when "0100" =>
							MAR <= SP(7 downto 0);
							SP <= SP + 1;
							-- table C
							case IR(11 downto 9) is
								when "000" => MBR <= RA;
								when "001" => MBR <= RB;
								when "010" => MBR <= RC;
								when "011" => MBR <= RD;
								when "100" => MBR <= RE;
								when "101" => MBR <= SP;
								when "110" => MBR <= "00000000" & PC;
								when "111" => MBR <= "000000000000" & CR;
								when others => null;
							end case;
						-- pop
						when "0101" =>
							MAR <= SP(7 downto 0) - 1;
							SP <= SP - 1;
						-- add
						when "1000" =>
							-- set the operation
							-- set up srcA (table E)
							case IR(11 downto 9) is
								when "000" => srcA_bus <= unsigned(RA);
								when "001" => srcA_bus <= unsigned(RB);
								when "010" => srcA_bus <= unsigned(RC);
								when "011" => srcA_bus <= unsigned(RD);
								when "100" => srcA_bus <= unsigned(RE);
								when "101" => srcA_bus <= unsigned(SP);
								when "110" => srcA_bus <= (others => '0');
								when "111" => srcA_bus <= (others => '1');
								when others => null;
							end case;
							
							-- set up srcB (table E)
							case IR(8 downto 6) is
								when "000" => srcB_bus <= unsigned(RA);
								when "001" => srcB_bus <= unsigned(RB);
								when "010" => srcB_bus <= unsigned(RC);
								when "011" => srcB_bus <= unsigned(RD);
								when "100" => srcB_bus <= unsigned(RE);
								when "101" => srcB_bus <= unsigned(SP);
								when "110" => srcB_bus <= (others => '0');
								when "111" => srcB_bus <= (others => '1');
								when others => null;
							end case;
						-- subtract
						when "1001" =>
							-- set up srcA (table E)
							case IR(11 downto 9) is
								when "000" => srcA_bus <= unsigned(RA);
								when "001" => srcA_bus <= unsigned(RB);
								when "010" => srcA_bus <= unsigned(RC);
								when "011" => srcA_bus <= unsigned(RD);
								when "100" => srcA_bus <= unsigned(RE);
								when "101" => srcA_bus <= unsigned(SP);
								when "110" => srcA_bus <= (others => '0');
								when "111" => srcA_bus <= (others => '1');
								when others => null;
							end case;
							
							-- set up srcB (table E)
							case IR(8 downto 6) is
								when "000" => srcB_bus <= unsigned(RA);
								when "001" => srcB_bus <= unsigned(RB);
								when "010" => srcB_bus <= unsigned(RC);
								when "011" => srcB_bus <= unsigned(RD);
								when "100" => srcB_bus <= unsigned(RE);
								when "101" => srcB_bus <= unsigned(SP);
								when "110" => srcB_bus <= (others => '0');
								when "111" => srcB_bus <= (others => '1');
								when others => null;
							end case;
						-- and
						when "1010" =>
							-- set up srcA (table E)
							case IR(11 downto 9) is
								when "000" => srcA_bus <= unsigned(RA);
								when "001" => srcA_bus <= unsigned(RB);
								when "010" => srcA_bus <= unsigned(RC);
								when "011" => srcA_bus <= unsigned(RD);
								when "100" => srcA_bus <= unsigned(RE);
								when "101" => srcA_bus <= unsigned(SP);
								when "110" => srcA_bus <= (others => '0');
								when "111" => srcA_bus <= (others => '1');
								when others => null;
							end case;
							
							-- set up srcB (table E)
							case IR(8 downto 6) is
								when "000" => srcB_bus <= unsigned(RA);
								when "001" => srcB_bus <= unsigned(RB);
								when "010" => srcB_bus <= unsigned(RC);
								when "011" => srcB_bus <= unsigned(RD);
								when "100" => srcB_bus <= unsigned(RE);
								when "101" => srcB_bus <= unsigned(SP);
								when "110" => srcB_bus <= (others => '0');
								when "111" => srcB_bus <= (others => '1');
								when others => null;
							end case;
						-- or
						when "1011" =>
							-- set up srcA (table E)
							case IR(11 downto 9) is
								when "000" => srcA_bus <= unsigned(RA);
								when "001" => srcA_bus <= unsigned(RB);
								when "010" => srcA_bus <= unsigned(RC);
								when "011" => srcA_bus <= unsigned(RD);
								when "100" => srcA_bus <= unsigned(RE);
								when "101" => srcA_bus <= unsigned(SP);
								when "110" => srcA_bus <= unsigned("00000000" & PC);
								when "111" => srcA_bus <= (others => '1');
								when others => null;
							end case;
							
							-- set up srcB (table E)
							case IR(8 downto 6) is
								when "000" => srcB_bus <= unsigned(RA);
								when "001" => srcB_bus <= unsigned(RB);
								when "010" => srcB_bus <= unsigned(RC);
								when "011" => srcB_bus <= unsigned(RD);
								when "100" => srcB_bus <= unsigned(RE);
								when "101" => srcB_bus <= unsigned(SP);
								when "110" => srcB_bus <= (others => '0');
								when "111" => srcB_bus <= (others => '1');
								when others => null;
							end case;
						-- xor
						when "1100" =>
							-- set up srcA (table E)
							case IR(11 downto 9) is
								when "000" => srcA_bus <= unsigned(RA);
								when "001" => srcA_bus <= unsigned(RB);
								when "010" => srcA_bus <= unsigned(RC);
								when "011" => srcA_bus <= unsigned(RD);
								when "100" => srcA_bus <= unsigned(RE);
								when "101" => srcA_bus <= unsigned(SP);
								when "110" => srcA_bus <= (others => '0');
								when "111" => srcA_bus <= (others => '1');
								when others => null;
							end case;
							
							-- set up srcB (table E)
							case IR(8 downto 6) is
								when "000" => srcB_bus <= unsigned(RA);
								when "001" => srcB_bus <= unsigned(RB);
								when "010" => srcB_bus <= unsigned(RC);
								when "011" => srcB_bus <= unsigned(RD);
								when "100" => srcB_bus <= unsigned(RE);
								when "101" => srcB_bus <= unsigned(SP);
								when "110" => srcB_bus <= (others => '0');
								when "111" => srcB_bus <= (others => '1');
								when others => null;
							end case;
						-- shift
						when "1101" =>
							-- set up srcA (table E)
							case IR(10 downto 8) is
								when "000" => srcA_bus <= unsigned(RA);
								when "001" => srcA_bus <= unsigned(RB);
								when "010" => srcA_bus <= unsigned(RC);
								when "011" => srcA_bus <= unsigned(RD);
								when "100" => srcA_bus <= unsigned(RE);
								when "101" => srcA_bus <= unsigned(SP);
								when "110" => srcA_bus <= (others => '0');
								when "111" => srcA_bus <= (others => '1');
								when others => null;
							end case;
							
							-- add direction bit to low of srcB 
							srcB_bus(0) <= IR(11);
						-- rotate
						when "1110" =>
							-- set up srcA (table E)
							case IR(10 downto 8) is
								when "000" => srcA_bus <= unsigned(RA);
								when "001" => srcA_bus <= unsigned(RB);
								when "010" => srcA_bus <= unsigned(RC);
								when "011" => srcA_bus <= unsigned(RD);
								when "100" => srcA_bus <= unsigned(RE);
								when "101" => srcA_bus <= unsigned(SP);
								when "110" => srcA_bus <= (others => '0');
								when "111" => srcA_bus <= (others => '1');
								when others => null;
							end case;
							
							-- add direction to low of srcB
							srcB_bus(0) <= IR(11);
						-- move
						when "1111" =>
							-- IR bits sign-extended
							if IR(11) = '1' then
								if IR(10) = '0' then
									srcA_bus <= unsigned("00000000" & IR(10 downto 3));
								else
									srcA_bus <= unsigned("11111111" & IR(10 downto 3));
								end if;
								
							-- src location
							else
								-- table D
								case IR(10 downto 8) is
									when "000" => srcA_bus <= unsigned(RA);
									when "001" => srcA_bus <= unsigned(RB);
									when "010" => srcA_bus <= unsigned(RC);
									when "011" => srcA_bus <= unsigned(RD);
									when "100" => srcA_bus <= unsigned(RE);
									when "101" => srcA_bus <= unsigned(SP);
									when "110" => srcA_bus <= unsigned("00000000" & PC);
									when "111" => srcA_bus <= unsigned(IR);
									when others => null;
								end case;
							end if;
						when others =>
							null;
					end case;
					
					-- implement state switching (exit) at end of case statement
					-- b/c it cannot change state inside other statements
					-- else it will ignore any other assignments like op
					
					-- exit (halt)
					if IR(15 downto 10) = "001111" then
						state <= "1000";
					else
						state <= "0011";
					end if;
				-- execute ALU
				when "0011" =>
					if IR(15 downto 12) = "0001" then
						RAM_we <= '1';
					elsif IR(15 downto 10) = "001101" then
						RAM_we <= '1';
					elsif IR(15 downto 12) = "0100" then
						RAM_we <= '1';
					else
						RAM_we <= '0';
					end if;
					
					-- check if load
					if IR(15 downto 12) = "0000" then
						state <= "0100";
					-- check if pop
					elsif IR(15 downto 12) = "0101" then
						state <= "0100";
					-- check if return
					elsif IR(15 downto 10) = "001110" then
						state <= "0100";
					else
						state <= "0101";
					end if;
				-- execute mem wait
				when "0100" =>
					state <= "0101";
				-- execute write
				when "0101" =>
					RAM_we <= '0';
					
					case IR(15 downto 12) is
						-- load
						when "0000" =>
							-- table B
							case IR(10 downto 8) is
								when "000" => RA <= RAM_out;
								when "001" => RB <= RAM_out;
								when "010" => RC <= RAM_out;
								when "011" => RD <= RAM_out;
								when "100" => RE <= RAM_out;
								when "101" => SP <= RAM_out;
								when others =>
									null;
							end case;
							
						-- return
						when "0011" =>
							if IR(11 downto 10) = "10" then
								-- make sure this is in the right order
								PC <= RAM_out(7 downto 0);
								CR <= RAM_out(11 downto 8);
							end if;
						-- pop
						when "0101" =>
							case IR(11 downto 9) is
								when "000" => RA <= RAM_out;
								when "001" => RB <= RAM_out;
								when "010" => RC <= RAM_out;
								when "011" => RD <= RAM_out;
								when "100" => RE <= RAM_out;
								when "101" => SP <= RAM_out;
								-- also make sure correct bits
								when "110" => PC <= RAM_out(7 downto 0);
								when "111" => CR <= RAM_out(11 downto 8);
								when others => null;
							end case;
							
						-- store to output
						when "0110" =>
							case IR(11 downto 9) is
								when "000" => OUTREG <= RA;
								when "001" => OUTREG <= RB;
								when "010" => OUTREG <= RC;
								when "011" => OUTREG <= RD;
								when "100" => OUTREG <= RE;
								when "101" => OUTREG <= SP;
								when "110" => OUTREG <= "00000000" & PC;
								when "111" => OUTREG <= IR;
								when others =>null;
							end case;
			
						-- load from input
						when "0111" =>
							case IR(11 downto 9) is
								when "000" => RA <= "00000000" & iport;
								when "001" => RB <= "00000000" & iport;
								when "010" => RC <= "00000000" & iport;
								when "011" => RD <= "00000000" & iport;
								when "100" => RE <= "00000000" & iport;
								when "101" => SP <= "00000000" & iport;
								when others => null;
							end case;
						
						-- add
						when "1000" =>
							case IR(2 downto 0) is
								when "000" => RA <= std_logic_vector(dest_bus);
								when "001" => RB <= std_logic_vector(dest_bus);
								when "010" => RC <= std_logic_vector(dest_bus);
								when "011" => RD <= std_logic_vector(dest_bus);
								when "100" => RE <= std_logic_vector(dest_bus);
								when "101" => SP <= std_logic_vector(dest_bus);
								when others => null;
							end case;
							
							CR <= CR_bus;
						-- subtract
						when "1001" =>
							case IR(2 downto 0) is
								when "000" => RA <= std_logic_vector(dest_bus);
								when "001" => RB <= std_logic_vector(dest_bus);
								when "010" => RC <= std_logic_vector(dest_bus);
								when "011" => RD <= std_logic_vector(dest_bus);
								when "100" => RE <= std_logic_vector(dest_bus);
								when "101" => SP <= std_logic_vector(dest_bus);
								when others => null;
							end case;
							
							CR <= CR_bus;
						-- and
						when "1010" =>
							case IR(2 downto 0) is
								when "000" => RA <= std_logic_vector(dest_bus);
								when "001" => RB <= std_logic_vector(dest_bus);
								when "010" => RC <= std_logic_vector(dest_bus);
								when "011" => RD <= std_logic_vector(dest_bus);
								when "100" => RE <= std_logic_vector(dest_bus);
								when "101" => SP <= std_logic_vector(dest_bus);
								when others => null;
							end case;
							
							CR <= CR_bus;
						-- or
						when "1011" =>
							case IR(2 downto 0) is
								when "000" => RA <= std_logic_vector(dest_bus);
								when "001" => RB <= std_logic_vector(dest_bus);
								when "010" => RC <= std_logic_vector(dest_bus);
								when "011" => RD <= std_logic_vector(dest_bus);
								when "100" => RE <= std_logic_vector(dest_bus);
								when "101" => SP <= std_logic_vector(dest_bus);
								when others => null;
							end case;
							
							CR <= CR_bus;
						-- xor
						when "1100" =>
							case IR(2 downto 0) is
								when "000" => RA <= std_logic_vector(dest_bus);
								when "001" => RB <= std_logic_vector(dest_bus);
								when "010" => RC <= std_logic_vector(dest_bus);
								when "011" => RD <= std_logic_vector(dest_bus);
								when "100" => RE <= std_logic_vector(dest_bus);
								when "101" => SP <= std_logic_vector(dest_bus);
								when others => null;
							end case;
							
							CR <= CR_bus;
						-- shift
						when "1101" =>
							case IR(2 downto 0) is
								when "000" => RA <= std_logic_vector(dest_bus);
								when "001" => RB <= std_logic_vector(dest_bus);
								when "010" => RC <= std_logic_vector(dest_bus);
								when "011" => RD <= std_logic_vector(dest_bus);
								when "100" => RE <= std_logic_vector(dest_bus);
								when "101" => SP <= std_logic_vector(dest_bus);
								when others => null;
							end case;
							
							CR <= CR_bus;
						-- rotate
						when "1110" =>
							case IR(2 downto 0) is
								when "000" => RA <= std_logic_vector(dest_bus);
								when "001" => RB <= std_logic_vector(dest_bus);
								when "010" => RC <= std_logic_vector(dest_bus);
								when "011" => RD <= std_logic_vector(dest_bus);
								when "100" => RE <= std_logic_vector(dest_bus);
								when "101" => SP <= std_logic_vector(dest_bus);
								when others => null;
							end case;
							
							CR <= CR_bus;
						-- move
						when "1111" =>
							case IR(2 downto 0) is
								when "000" => RA <= std_logic_vector(dest_bus);
								when "001" => RB <= std_logic_vector(dest_bus);
								when "010" => RC <= std_logic_vector(dest_bus);
								when "011" => RD <= std_logic_vector(dest_bus);
								when "100" => RE <= std_logic_vector(dest_bus);
								when "101" => SP <= std_logic_vector(dest_bus);
								when others => null;
							end case;
							
							CR <= CR_bus;
						when others =>
							null;
					end case;
					
					-- implement state switching (return) at end of case statement
					-- b/c it cannot change state inside other statements
					-- else it will ignore any other assignments like op
					
					-- if return, then enter return pause states
					if IR(15 downto 10) = "001110" then
						state <= "0110";
					else
						state <= "0001";
					end if;
				-- return pause 1
				when "0110" =>
					state <= "0111";
				-- return pause 2
				when "0111" =>
					state <= "0001";
				-- halt
				when "1000" =>
					null;
				when others =>
					null;
			end case;
		end if;
	end process;
	
	PCView <= PC;
	IRView <= IR;
	RAView <= RA;
	RBView <= RB;
	RCView <= RC;
	RDView <= RD;
	REView <= RE;
	oport <= OUTREG;

end rtl;
