-- StudyHall Economy Specification
-- Ada model of the Lean 4 formal axioms and truth conditions
-- for a sustainable Learn-to-Earn and Play-to-Earn educational economy.
-- Author: Steve Sloan (Prince Sloan)
pragma Ada_2022;

package Study_Hall is
   pragma Preelaborate;

   -- ---------------------------------------------------------------------------
   -- Source: where real-world fiat or tangible value enters the system.
   -- Mirrors Lean 4  `inductive Source`.
   -- ---------------------------------------------------------------------------
   type Source_Kind is (
      Institution,    -- schools, universities, districts
      Sponsor,        -- corporations, NIL brands, partners
      Philanthropy,   -- nonprofit or donor foundations
      User_Revenue,   -- subscriptions, tutoring, or app fees
      Treasury_Seed   -- initial capital reserve (seed, DAO, investors)
   );

   -- ---------------------------------------------------------------------------
   -- Flow: how value moves through the economy.
   -- Mirrors Lean 4  `inductive Flow`.
   -- ---------------------------------------------------------------------------
   type Flow_Kind is (
      Learn_To_Earn,  -- rewards for completing learning milestones
      Play_To_Earn,   -- rewards for gameplay / gamified progress
      Mentor_Reward,  -- compensation for mentors and tutors
      Reserve         -- capital held in reserve for future payouts
   );

   -- ---------------------------------------------------------------------------
   -- Funding matrix: which (Flow, Source) pairs are economically valid.
   -- Mirrors Lean 4  `def funded`.
   -- ---------------------------------------------------------------------------
   type Funding_Matrix is array (Flow_Kind, Source_Kind) of Boolean;

   Valid_Funding : constant Funding_Matrix :=
     (Learn_To_Earn => (Institution  => True,
                        Sponsor      => True,
                        Philanthropy => True,
                        User_Revenue => True,
                        Treasury_Seed => True),
      Play_To_Earn  => (Institution  => False,
                        Sponsor      => True,
                        Philanthropy => False,
                        User_Revenue => True,
                        Treasury_Seed => True),
      Mentor_Reward => (Institution  => True,
                        Sponsor      => False,
                        Philanthropy => False,
                        User_Revenue => True,
                        Treasury_Seed => False),
      Reserve       => (Institution  => True,
                        Sponsor      => True,
                        Philanthropy => True,
                        User_Revenue => True,
                        Treasury_Seed => True));

   -- ---------------------------------------------------------------------------
   -- isRealValue: all declared sources carry real / fiat-backed value.
   -- Mirrors Lean 4  `def isRealValue`.
   -- ---------------------------------------------------------------------------
   function Is_Real_Value (S : Source_Kind) return Boolean
      with Post => Is_Real_Value'Result = True;

   -- ---------------------------------------------------------------------------
   -- Is_Funded: returns True when flow F may be paid from source S.
   -- ---------------------------------------------------------------------------
   function Is_Funded (F : Flow_Kind; S : Source_Kind) return Boolean
      with Post => Is_Funded'Result = Valid_Funding (F, S);

   -- ---------------------------------------------------------------------------
   -- Has_Valid_Funding_Source: every flow has at least one valid source.
   -- This is part one of the SustainableSystem proposition.
   -- ---------------------------------------------------------------------------
   function Has_Valid_Funding_Source (F : Flow_Kind) return Boolean
      with Post => (if Has_Valid_Funding_Source'Result then
                      (for some S in Source_Kind => Valid_Funding (F, S)));

   -- ---------------------------------------------------------------------------
   -- No_Self_Minting: tokens cannot originate value without a real source.
   -- Corresponds to Lean 4 axiom `no_self_minting`.
   -- In Ada we express this as a provable function; the axiom is encoded in the
   -- invariant that every reward payout requires a non-zero funded source.
   -- ---------------------------------------------------------------------------
   function No_Self_Minting (F : Flow_Kind) return Boolean
      with Post => No_Self_Minting'Result = True;

   -- ---------------------------------------------------------------------------
   -- Sustainability result record: captures the three conditions.
   -- ---------------------------------------------------------------------------
   type Sustainability_Proof is record
      All_Flows_Funded     : Boolean;
      All_Sources_Real     : Boolean;
      No_Minting_Violation : Boolean;
   end record;

   function Is_Sustainable (P : Sustainability_Proof) return Boolean is
      (P.All_Flows_Funded and P.All_Sources_Real and P.No_Minting_Violation);

   -- ---------------------------------------------------------------------------
   -- Sustainability_Truth: verifies all three conditions and returns a proof.
   -- Mirrors Lean 4 theorem `sustainability_truth`.
   -- ---------------------------------------------------------------------------
   function Sustainability_Truth return Sustainability_Proof
      with Post => Is_Sustainable (Sustainability_Truth'Result);

   -- ---------------------------------------------------------------------------
   -- Reporting helpers
   -- ---------------------------------------------------------------------------
   procedure Print_Funding_Matrix;
   procedure Print_Sustainability_Report;

   function Source_Name (S : Source_Kind) return String;
   function Flow_Name   (F : Flow_Kind)   return String;

end Study_Hall;
