pragma Ada_2022;

with Interfaces;

package body HFT_SHA256 is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;

   subtype U8 is Interfaces.Unsigned_8;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;

   type Word_Array is array (Natural range <>) of U32;
   Round_Constants : constant Word_Array (0 .. 63) :=
     [16#428A2F98#, 16#71374491#, 16#B5C0FBCF#, 16#E9B5DBA5#,
      16#3956C25B#, 16#59F111F1#, 16#923F82A4#, 16#AB1C5ED5#,
      16#D807AA98#, 16#12835B01#, 16#243185BE#, 16#550C7DC3#,
      16#72BE5D74#, 16#80DEB1FE#, 16#9BDC06A7#, 16#C19BF174#,
      16#E49B69C1#, 16#EFBE4786#, 16#0FC19DC6#, 16#240CA1CC#,
      16#2DE92C6F#, 16#4A7484AA#, 16#5CB0A9DC#, 16#76F988DA#,
      16#983E5152#, 16#A831C66D#, 16#B00327C8#, 16#BF597FC7#,
      16#C6E00BF3#, 16#D5A79147#, 16#06CA6351#, 16#14292967#,
      16#27B70A85#, 16#2E1B2138#, 16#4D2C6DFC#, 16#53380D13#,
      16#650A7354#, 16#766A0ABB#, 16#81C2C92E#, 16#92722C85#,
      16#A2BFE8A1#, 16#A81A664B#, 16#C24B8B70#, 16#C76C51A3#,
      16#D192E819#, 16#D6990624#, 16#F40E3585#, 16#106AA070#,
      16#19A4C116#, 16#1E376C08#, 16#2748774C#, 16#34B0BCB5#,
      16#391C0CB3#, 16#4ED8AA4A#, 16#5B9CCA4F#, 16#682E6FF3#,
      16#748F82EE#, 16#78A5636F#, 16#84C87814#, 16#8CC70208#,
      16#90BEFFFA#, 16#A4506CEB#, 16#BEF9A3F7#, 16#C67178F2#];

   function Big_Sigma_0 (X : U32) return U32 is
     (Interfaces.Rotate_Right (X, 2) xor
      Interfaces.Rotate_Right (X, 13) xor
      Interfaces.Rotate_Right (X, 22));

   function Big_Sigma_1 (X : U32) return U32 is
     (Interfaces.Rotate_Right (X, 6) xor
      Interfaces.Rotate_Right (X, 11) xor
      Interfaces.Rotate_Right (X, 25));

   function Small_Sigma_0 (X : U32) return U32 is
     (Interfaces.Rotate_Right (X, 7) xor
      Interfaces.Rotate_Right (X, 18) xor
      Interfaces.Shift_Right (X, 3));

   function Small_Sigma_1 (X : U32) return U32 is
     (Interfaces.Rotate_Right (X, 17) xor
      Interfaces.Rotate_Right (X, 19) xor
      Interfaces.Shift_Right (X, 10));

   function Digest (Value : String) return Digest_Text is
      Input_Length  : constant Natural := Value'Length;
      Padded_Length : constant Natural :=
        ((Input_Length + 9 + 63) / 64) * 64;
      Bit_Length    : constant U64 := U64 (Input_Length) * 8;
      State : Word_Array (0 .. 7) :=
        [16#6A09E667#, 16#BB67AE85#, 16#3C6EF372#, 16#A54FF53A#,
         16#510E527F#, 16#9B05688C#, 16#1F83D9AB#, 16#5BE0CD19#];
      W : Word_Array (0 .. 63);

      function Byte_At (Position : Positive) return U8 is
         Shift : Natural;
      begin
         if Position <= Input_Length then
            return U8
              (Character'Pos (Value (Value'First + Position - 1)));
         elsif Position = Input_Length + 1 then
            return 16#80#;
         elsif Position > Padded_Length - 8 then
            Shift := (Padded_Length - Position) * 8;
            return U8
              (Interfaces.Shift_Right (Bit_Length, Shift) and 16#FF#);
         else
            return 0;
         end if;
      end Byte_At;
   begin
      for Block in 0 .. Padded_Length / 64 - 1 loop
         for I in 0 .. 15 loop
            declare
               Position : constant Positive := Block * 64 + I * 4 + 1;
            begin
               W (I) :=
                 Interfaces.Shift_Left (U32 (Byte_At (Position)), 24)
                 or Interfaces.Shift_Left
                   (U32 (Byte_At (Position + 1)), 16)
                 or Interfaces.Shift_Left
                   (U32 (Byte_At (Position + 2)), 8)
                 or U32 (Byte_At (Position + 3));
            end;
         end loop;
         for I in 16 .. 63 loop
            W (I) := Small_Sigma_1 (W (I - 2)) + W (I - 7)
              + Small_Sigma_0 (W (I - 15)) + W (I - 16);
         end loop;

         declare
            A : U32 := State (0);
            B : U32 := State (1);
            C : U32 := State (2);
            D : U32 := State (3);
            E : U32 := State (4);
            F : U32 := State (5);
            G : U32 := State (6);
            H : U32 := State (7);
            T1, T2 : U32;
         begin
            for I in 0 .. 63 loop
               T1 := H + Big_Sigma_1 (E)
                 + ((E and F) xor ((not E) and G))
                 + Round_Constants (I) + W (I);
               T2 := Big_Sigma_0 (A)
                 + ((A and B) xor (A and C) xor (B and C));
               H := G;
               G := F;
               F := E;
               E := D + T1;
               D := C;
               C := B;
               B := A;
               A := T1 + T2;
            end loop;
            State (0) := State (0) + A;
            State (1) := State (1) + B;
            State (2) := State (2) + C;
            State (3) := State (3) + D;
            State (4) := State (4) + E;
            State (5) := State (5) + F;
            State (6) := State (6) + G;
            State (7) := State (7) + H;
         end;
      end loop;

      declare
         Hex_Digits : constant String := "0123456789ABCDEF";
         Result     : Digest_Text;
         Position   : Positive := Result'First;
      begin
         for Word of State loop
            for Shift in reverse 0 .. 7 loop
               Result (Position) :=
                 Hex_Digits
                   (Natural
                      (Interfaces.Shift_Right (Word, Shift * 4)
                       and 16#F#) + 1);
               Position := Position + 1;
            end loop;
         end loop;
         return Result;
      end;
   end Digest;
end HFT_SHA256;
