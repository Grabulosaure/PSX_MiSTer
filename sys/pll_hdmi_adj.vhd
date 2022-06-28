--------------------------------------------------------------------------------
-- HDMI PLL Adjust
--------------------------------------------------------------------------------

-- Changes the HDMI PLL frequency according to the scaler suggestions.
--------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY pll_hdmi_adj IS
  PORT (
    -- Scaler
    llena         : IN  std_logic; -- 0=Disabled 1=Enabled
    lltune        : IN  unsigned(15 DOWNTO 0); -- Outputs from scaler
    resync        : OUT std_logic;
    locked        : OUT std_logic;

    rego          : IN  std_logic;
    
    -- Signals from reconfig commands
    i_waitrequest : OUT std_logic;
    i_write       : IN  std_logic;
    i_address     : IN  unsigned(5 DOWNTO 0);
    i_writedata   : IN  unsigned(31 DOWNTO 0);

    -- Outputs to PLL_HDMI_CFG
    o_waitrequest : IN  std_logic;
    o_write       : OUT std_logic;
    o_address     : OUT unsigned(5 DOWNTO 0);
    o_writedata   : OUT unsigned(31 DOWNTO 0);
    
    ------------------------------------
    clk           : IN  std_logic;
    reset_na      : IN  std_logic
    );

BEGIN

  
END ENTITY pll_hdmi_adj;

--##############################################################################

ARCHITECTURE rtl OF pll_hdmi_adj IS

  ----------------------------------------------------------------------------
  CONSTANT DELAY_SMALL : natural := 4;
  CONSTANT DELAY_LARGE : natural := 4096; --256;
  
  CONSTANT RVCOMIN : natural := 8; -- 400MHz / 50MHz
  
  SUBTYPE uint4  IS natural RANGE 0 TO 2**4 - 1;
  SUBTYPE uint6  IS natural RANGE 0 TO 2**6 - 1;
  SUBTYPE uint12 IS natural RANGE 0 TO 2**12 - 1;
  SUBTYPE uint14 IS natural RANGE 0 TO 2**14 - 1;
  SUBTYPE uint24 IS natural RANGE 0 TO 2**24 - 1;
  SUBTYPE uint28 IS natural RANGE 0 TO 2**28 - 1;
  
  ----------------------------------------------------------------------------
  SIGNAL i_clk, i_ce, i_de, i_vss, i_inter, i_fl : std_logic;
  SIGNAL o_clk, o_ce, o_vss, o_clr : std_logic;
  
  SIGNAL i_de2,i_vss2,i_vssd : std_logic;
  SIGNAL i_linecpt,i_line : uint12; -- MAX 4096 pix / input line
  SIGNAL i_delay : uint14;

  SIGNAL o_clr2,o_oclr,o_clrs : std_logic :='0';
  SIGNAL o_ototal_cpt,o_ototal : uint24;
  SIGNAL o_ototal_up : std_logic :='0';
  SIGNAL o_resyncp,o_resyncp2 : std_logic;

  SIGNAL ivss, ivss2, ivss3 : std_logic;
  SIGNAL ovss, ovss2, ovss3 : std_logic;
  SIGNAL ev_istart,ev_iend,ev_i,ev_ialt : std_logic :='0';
  SIGNAL ev_ostart,ev_oend,ev_o : std_logic;
  SIGNAL oclrs,oclrs2 : std_logic;
  SIGNAL ritotal_cpt,ritotal,ripic,ritune : uint24;
  SIGNAL ropic,ropic_cpt,rodelay_cpt,rodelay : uint24;
  SIGNAL ototal_up,ototal_up2 : std_logic;
  SIGNAL oototal,odelay,posalign,posalign2,delay,delaym,delay_cpt : uint24;
  
  SIGNAL dsign,frame,alt,dja,dsignm : std_logic;
  SIGNAL phase  : std_logic; -- 0=FREQ 1=PHASE
  SIGNAL mode   : std_logic; -- 0=Reset Scaler 1=Correction
  
  TYPE enum_dstate IS (sIDLE,sIN,sOUT);
  SIGNAL dstate : enum_dstate;
  SIGNAL sta : uint4;
  TYPE enum_state IS (sIDLE,sCALC,sCALC2,sCALC3,sALIGN,sIUPDATE,sIUPDATE2);
  SIGNAL state : enum_state;
  SIGNAL reg_req,reg_ack,pc_req,pc_ack,resyncc : std_logic;
  SIGNAL pll_mul,pll_amul,mul_mem,mul_in : unsigned(39 DOWNTO 0);
  SIGNAL pll_div,pll_adiv,div_mem,div_in : unsigned(7 DOWNTO 0);

  TYPE enum_pcstate IS (sIDLE,sDIVI,sDIVI2,sMULTI);
  SIGNAL pc_state : enum_pcstate;
  SIGNAL pc_div : uint6;
  SIGNAL pc_rodi : uint28;
  SIGNAL pc_rem : unsigned(31 DOWNTO 0);
  SIGNAL pc_divcpt : uint6;
  
  TYPE enum_rstate IS (sIDLE,sWP1,sWP2,sWP3,sWP4,sWP5,sWP6,sWP7,sWP8,
                       sW1,sW2,sW3,sW4,sW5,sW6,sW7,sW8,sPOST);
  SIGNAL rstate : enum_rstate;
  SIGNAL pwrite, mem_valid,iupdate,iupdate2 : std_logic;
  SIGNAL paddress : unsigned(5 DOWNTO 0);
  SIGNAL pdata : unsigned(31 DOWNTO 0);
  SIGNAL rdelay : natural RANGE 0 TO 255;
  SIGNAL repeat : boolean;

  
  ----------------------------------------------------------------------------
BEGIN

  i_clk   <= lltune(0);
  i_ce    <= lltune(1);
  i_de    <= lltune(2);
  i_vss   <= lltune(3);
  i_inter <= lltune(4);
  i_fl    <= lltune(5);

  o_clk   <= lltune(8);
  o_ce    <= lltune(9);
  o_vss   <= lltune(10);
  o_clr   <= lltune(11);

  -- Délai image IN
  -- Mesure image IN/OUT. Resynchro...
  -- Mesure délai pour resync scaler
  -- Evènements
  -- Machine à états
  -- Calcul paramètres scaler mul/div
  -- interface bus PLL ADJ, prog

  ----------------------------------------------------------------------------
  -- IN : 4 lines delay to input
  IDelay:PROCESS(i_clk) IS
  BEGIN
    IF rising_edge(i_clk) THEN
      IF i_ce='1' THEN
        -- Measure input line time.
        i_de2<=i_de;
        
        IF i_de='1' AND i_de2='0' THEN
          i_linecpt<=0;
          IF i_vss='1' THEN
            i_line<=i_linecpt;
          END IF;
        ELSE
          i_linecpt<=(i_linecpt+1) MOD 4096;
        END IF;
        
        -- Delay 4 lines
        i_vss2<=i_vss;
        IF i_vss/=i_vss2 THEN
          i_delay<=0;
        ELSIF i_delay=i_line * 4 THEN
          i_vssd<=i_vss;
        ELSE
          i_delay<=(i_delay+1) MOD 16384;
        END IF;
      END IF;
    END IF;
  END PROCESS;
  
  ----------------------------------------------------------------------------
  -- OUT : Output frame cycle count.
  Ogen:PROCESS(o_clk) IS
  BEGIN
    IF rising_edge(o_clk) THEN
      IF o_ce='1' THEN
        o_clr2 <= o_clr;
        IF o_clr2='0' AND o_clr='1' THEN
          o_oclr<='1';
        ELSE
          o_oclr<='0';
        END IF;
        
        o_clrs <= o_clrs XOR o_oclr;

        IF o_oclr='1' THEN
          o_ototal_cpt<=1;
          o_ototal<=o_ototal_cpt;
          o_ototal_up<=NOT o_ototal_up;
        ELSE
          o_ototal_cpt<=o_ototal_cpt + 1;
        END IF;

        -- Change resync signal to output clock domain
        o_resyncp  <= resyncc;
        o_resyncp2 <= o_resyncp;
        resync   <= o_resyncp AND NOT o_resyncp2;
      END IF;
    END IF;
  END PROCESS;
  
  ----------------------------------------------------------------------------
  RCalc:PROCESS(clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      ivss  <= i_vssd; -- <ASYNC>
      ivss2 <= ivss;
      ivss3 <= ivss2;
      ev_istart <= NOT ivss3 AND     ivss2;
      ev_iend   <=     ivss3 AND NOT ivss2;

      ovss  <= o_vss; -- <ASYNC>
      ovss2 <= ovss;
      ovss3 <= ovss2;
      ev_ostart <= NOT ovss3 AND     ovss2;
      ev_oend   <=     ovss3 AND NOT ovss2;

      -- ITOTAL : Input frame duration
      ev_ialt<=ev_ialt XOR ev_iend;
      
      IF ev_istart='1' AND ev_ialt='0' THEN
        ritotal_cpt<=1;
        ritotal <= ritotal_cpt;
      ELSE
        ritotal_cpt<=ritotal_cpt+1;
      END IF;
      
      -- IPIC : Input image size
      IF ev_iend='1' AND ev_ialt='0' THEN
        ripic <= ritotal_cpt;
      END IF;

      -- OPIC : Output image size
      ropic_cpt<=ropic_cpt + 1;
      IF ev_ostart='1' THEN
        ropic_cpt <= 1;
      END IF;
      IF ev_oend='1' THEN
        ropic <= ropic_cpt;
      END IF;

      -- ODELAY : Delay between scaler counter clear and start of image
      oclrs <= o_clrs;
      oclrs2<= oclrs;
      IF oclrs2 /= oclrs THEN
        rodelay_cpt<=1;
      ELSE
        rodelay_cpt <= rodelay_cpt+1;
      END IF;
      IF ev_ostart='1' THEN
        rodelay <= rodelay_cpt;
      END IF;

      -- OTOTAL : Ouptut clock cycles per output frame.
      ototal_up <= o_ototal_up; -- <ASYNC>
      ototal_up2<= ototal_up;

      IF ototal_up2 /= ototal_up THEN
        oototal <= o_ototal;
      END IF;
    END IF;
  END PROCESS;

  ----------------------------------------------------------------------------
  -- Machine à états
  FSM:PROCESS(clk,reset_na) IS
  BEGIN
    IF rising_edge(clk) THEN
      -----------------------------------------------------
      IF ripic < ropic THEN
        ev_i  <= ev_istart;
        ev_o  <= ev_ostart;
        posalign <= (ritotal/2 - rodelay + 16777216 ) MOD 16777216;
      ELSE
        ev_i  <= ev_iend;
        ev_o  <= ev_oend;  
        posalign <= (ritotal/2 - rodelay  + ripic - ropic + 16777216) MOD 16777216;
      END IF;
      IF posalign>=ritotal/2 THEN
        posalign2<=posalign - ritotal/2;
      ELSE
        posalign2<=posalign;
      END IF;
      
      delay_cpt<=delay_cpt+1;
      frame <='0';

      IF ev_i='1' AND ev_o='1' THEN
        dstate<=sIDLE;
        delay <= 0;
        frame <='1';

      ELSIF ev_i='1' THEN
        IF dstate= sOUT THEN
          delay <= delay_cpt;
          dsign <='1'; -- IN after OUT : Clock too fast
          dstate<=sIDLE;
          frame <='1';
        ELSE
          delay_cpt<=0;
          dstate<=sIN; 
        END IF;
        
      ELSIF ev_o='1' THEN
        IF dstate=sIN THEN
          delay <= delay_cpt;
          dsign <='0'; -- OUT after IN : Clock too slow
          dstate<=sIDLE;
          frame <='1';
        ELSE
          delay_cpt<=0;
          dstate<=sOUT; 
        END IF;
      END IF;
      
      IF frame='1' AND sta > 0 THEN
        sta<=sta-1;
      END IF;

      IF delay>=DELAY_LARGE THEN
        delaym<=DELAY_LARGE;
      ELSE
        delaym<=delay;
      END IF;
      
      -----------------------------------------------------
      -- Délai important : Recalage périodes IN/OUT. Reset scaler
      -- Délai faible : Recalage péiodes IN/OUT.

      pc_req<='0';

      CASE state IS
        WHEN sIDLE =>
          reg_req<='0';

          IF frame='1' THEN
            IF alt='0' THEN
              -- PHASE
              IF delay >= DELAY_LARGE AND rego='1' THEN
                phase <= '0';
                mode  <= '1';
                IF dja='0' THEN
                  state <= sCALC;
                  dja   <= '1';
                END IF;
                alt <= dja;
					 alt <=  '0';
                sta   <= 0;
                dsignm <= dsign;
              ELSIF delay >= DELAY_LARGE THEN
                phase <= '1';
                mode  <= '0';
                --alt <= dja;
                IF dja='0' THEN
                  state <= sCALC;
                  dja   <= '1';
                END IF;
					 alt <= '0';
                IF dsignm /= dsign THEN
						alt <='1';
					 END IF;
					 sta   <= 0;
					 dsignm <= dsign;
              ELSIF delay >= DELAY_SMALL THEN
                phase <= '1';
                mode  <= '0';
                --IF dja='0' THEN
                  state <= sCALC;
                  dja   <= '1';
                --END IF;
                sta   <= 0;
                alt   <= '1'; --dja;
                dsignm <= dsign;
              END IF;
            ELSE
              -- FREQ
              -- Force correction freq après correction phase
              mode<='0';
              phase<='0';
              alt<='0';
              state<=sCALC;
              dja<='0';
            END IF;
          END IF;
          
          -- >> si déphasage important mais durées égale => recalage scaler sans recalc PLL
          iupdate2<=iupdate;
          IF iupdate/=iupdate2 THEN
            state<=sIUPDATE;
          END IF;
          
        WHEN sCALC =>
          IF phase='1' THEN
            IF dsign='0' THEN  -- OUT after IN : Clock too slow
              ritune <= ritotal - delaym / 8;
            ELSE               -- IN after OUT : Clock too fast
              ritune <= ritotal + delaym / 8;
            END IF;
          ELSE
            ritune <= ritotal;
          END IF;
          pc_req  <='1';
          pll_mul <= pll_amul;
          pll_div <= pll_adiv;
          IF pll_amul(39 DOWNTO 32)<2 OR pll_amul(39 DOWNTO 32)>=16 THEN
            pll_mul(39 DOWNTO 32)<=x"08";
          END IF;
          IF pll_adiv<2 OR pll_adiv>=6 THEN
            pll_div<=x"03";
          END IF;
          IF pc_ack='1' THEN
            pc_req<='0';
            state<=sCALC2;
          END IF;
          sta<=0;

        WHEN sCALC2 => -- Update registers
          reg_req <='1';
          state   <=sCALC3;

        WHEN sCALC3 =>
          IF reg_ack ='1' THEN
            reg_req<='0';
            IF mode = '1' THEN
              state <=sALIGN;
            ELSE
              state <=sIDLE;
             END IF;
          END IF;
          sta <= 0;

        WHEN sALIGN => -- Reset scaler output at right time
          IF ritotal_cpt = posalign2 AND sta=0 AND ev_ialt='1' THEN
            resyncc <= NOT resyncc;
            state <=sIDLE;
            sta   <= 1;
          END IF;

        WHEN sIUPDATE =>
          pll_mul <= mul_in;
          pll_div <= div_in;
          reg_req<='1';
          state<=sIUPDATE2;
          
        WHEN sIUPDATE2 =>
          IF reg_ack='1' THEN
            reg_req<='0';
            state <=sALIGN;
          END IF;
          sta<=0;
          
      END CASE;
      -----------------------------------------------------
      IF reset_na='0' THEN
        resyncc<='0';
        state <=sIDLE;
        sta   <= 3;
      END IF;
    END IF;
  END PROCESS;

  ----------------------------------------------------------------------------
  -- Compute PLL coefficients
  -- IN  : OOTOTAL, RITOTAL
  -- OUT : PLL_AMUL[39:0] , PLL_ADIV[7:0]
  PLLCalc:PROCESS(clk,reset_na) IS
  BEGIN
    IF rising_edge(clk) THEN
      pc_ack<='0';

      CASE pc_state IS
        WHEN sIDLE =>
          pc_div<=0;
          pc_rodi<=0;
          IF pc_req='1' AND pc_ack='0' THEN
            pc_state<=sDIVI;
          END IF;

        WHEN sDIVI =>
          -- Divider
          IF pc_rodi < RVCOMIN * ritune THEN
            pc_rodi<=pc_rodi + oototal*2;
            pc_div<=pc_div + 1;
          ELSE
            pc_state<=sDIVI2;
          END IF;

        WHEN sDIVI2 =>
          pc_divcpt<=0;
          pc_rem  <=to_unsigned(pc_rodi,32);
          pll_adiv<=to_unsigned(pc_div,8);
          pc_state <=sMULTI; 

        WHEN sMULTI =>
          IF pc_rem(31)='0' THEN
            pc_rem(31 DOWNTO 4)<=pc_rem(30 DOWNTO 3) - to_unsigned(ritune*64,28);
          ELSE
            pc_rem(31 DOWNTO 4)<=pc_rem(30 DOWNTO 3) + to_unsigned(ritune*64,28);
          END IF;
          pc_rem(3 DOWNTO 1) <= pc_rem(2 DOWNTO 0);
          pll_amul<=pll_amul(38 DOWNTO 0) & NOT pc_rem(31);
          pc_divcpt<=pc_divcpt+1;
          pll_amul(39)<='0';
          IF pc_divcpt=42 THEN
            pc_state<=sIDLE;
            pc_ack<='1';
          END IF;

      END CASE;
    END IF;
  END PROCESS;

  ----------------------------------------------------------------------------
  -- 000010 : Start reg "Write either 0 or 1 to start fractional PLL reconf.
  -- 000011 : N counter : Prescale counter => bypassed  15:8 = High count 7:0 = Low count 16 = Bypass
  -- 000100 : M counter : Feedback multiplication 15:8 = High count 7:0 = Low count 16 = Bypass 17 = Odd division
  -- 000111 : M counter : Fractional Value K
  -- 000101 : C counter : Post scale counter 15:8 = High count 7:0 = Low count 16 = Bypass

  Schmurtz:PROCESS(clk,reset_na) IS
    FUNCTION hilo(v : unsigned (7 DOWNTO 0)) RETURN unsigned IS
    BEGIN
      RETURN '0' & v(7 DOWNTO 1) & ('0' & (v(7 DOWNTO 1) + ("0" & v(0))));
    END FUNCTION;
  BEGIN
    IF rising_edge(clk) THEN
      ------------------------------------------------------
      reg_ack<='0';

      ------------------------------------------------------
      -- Update PLL registers
      CASE rstate IS
        WHEN sIDLE =>
          pwrite<='0';
          IF reg_req='1' AND reg_ack='0' THEN
            rstate<=sWP1;
          END IF;
          rdelay<=0;

        WHEN sWP1 => -- WaitRequest Mode
          pwrite<='1';
          pdata<=x"0000_0000";
          paddress<="000000";
          rstate<=sWP2;
          
        WHEN sWP2 =>
          IF o_waitrequest='0' THEN
            pwrite<='0';
            rstate<=sWP3;
          END IF;
          
        WHEN sWP3 => -- N counter
          pwrite<='1';
          paddress<="000011";
          pdata<=x"0001_0000";
          rstate<=sWP4;
          
        WHEN sWP4 =>
          IF o_waitrequest='0' THEN
            pwrite<='0';
            rstate<=sWP5;
          END IF;
          
        WHEN sWP5 => -- Charge Pump
          pwrite<='1';
          paddress<="001001";
          pdata<=x"0000_0002";
          rstate<=sWP6;
          
        WHEN sWP6 =>
          IF o_waitrequest='0' THEN
            pwrite<='0';
            rstate<=sWP7;
          END IF;
          
        WHEN sWP7 => -- BandWidth
          pwrite<='1';
          paddress<="001000";
          pdata<=x"0000_0007";
          rstate<=sWP8;
          
        WHEN sWP8 =>
          IF o_waitrequest='0' THEN
            pwrite<='0';
            rstate<=sW1;
          END IF;
          
        WHEN sW1 => -- Change M multiplier
          pdata<=x"0000" & hilo(pll_mul(39 DOWNTO 32));
          paddress<="000100";
          IF mem_valid='0' OR mul_mem(39 DOWNTO 32)/=pll_mul(39 DOWNTO 32) THEN
            pwrite<='1';
          END IF;
          rstate<=sW2;
          
        WHEN sW2 =>
          IF o_waitrequest='0' OR pwrite='0' THEN
            rstate<=sW3;
            pwrite<='0';
          END IF;
          
        WHEN sW3 => -- Change M fractional value : K
          pdata<=pll_mul(31 DOWNTO 0);
          paddress<="000111";
          IF mem_valid='0' OR mul_mem(31 DOWNTO 0)/=pll_mul(31 DOWNTO 0) THEN
            pwrite<='1';
          END IF;
          rstate<=sW4;
          
        WHEN sW4 =>
          IF o_waitrequest='0' OR pwrite='0' THEN
            rstate<=sW5;
            pwrite<='0';
          END IF;
          
        WHEN sW5 => -- Change C divisor
          pdata<=x"0000" & hilo(pll_div);
          paddress<="000101";
          IF mem_valid='0' OR div_mem/=pll_div THEN
            pwrite<='1';
          END IF;
          rstate<=sW6;
          
        WHEN sW6 =>
          IF o_waitrequest='0' OR pwrite='0' THEN
            rstate<=sW7;
            pwrite<='0';
          END IF;
          
        WHEN sW7 => -- Start reconfiguration
          pdata<=x"0000_0001";
          paddress<="000010";
          pwrite<='1';
          rstate<=sW8;
          
        WHEN sW8 =>
          mul_mem<=pll_mul;
          div_mem<=pll_div;
          mem_valid<='1';
          IF o_waitrequest='0' THEN
            pwrite<='0';
            rstate<=sPOST;
          END IF;

        WHEN sPOST =>
          rdelay<=rdelay+1;
          IF rdelay=16 THEN
            reg_ack<='1';
            rstate<=sIDLE;
          END IF;

      END CASE;

      ------------------------------------------------------
      IF reset_na='0' THEN
        mem_valid<='0';
        rstate <=sIDLE;
      END IF;

      ------------------------------------------------------
    END IF;
  END PROCESS Schmurtz;
  
  ----------------------------------------------------------------------------
  o_write    <=pwrite;
  o_address  <=paddress;
  o_writedata<=pdata;
  i_waitrequest<='0';

  imem:PROCESS(clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      IF i_write='1' THEN
        IF i_address="000100" THEN
          mul_in(39 DOWNTO 32) <= i_writedata(7 DOWNTO 0)+i_writedata(15 DOWNTO 8);
        END IF;
        IF i_address="000101" THEN
          div_in <= i_writedata(7 DOWNTO 0)+i_writedata(15 DOWNTO 8);
        END IF;
        IF i_address="000111" THEN
          mul_in(31 DOWNTO 0)<=i_writedata;
          iupdate<=NOT iupdate;
        END IF;
      END IF;
      IF reset_na='0' THEN
        iupdate<='0';
      END IF;
    END IF;
  END PROCESS;
  
  ----------------------------------------------------------------------------
  
END ARCHITECTURE rtl;

