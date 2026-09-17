pragma Ada_2022;

package HFT_SHA256 is
   subtype Digest_Text is String (1 .. 64);
   function Digest (Value : String) return Digest_Text;
end HFT_SHA256;
