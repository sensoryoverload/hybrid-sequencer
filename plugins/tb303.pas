// This is a Delphi class implementing a TB303 emulation

// It is really not optimized and may not sound 100% like
// a 303 , but it is nevertheless quite astonishing what you 
// can achieve with just a few lines of code!

// The code is based on several old C++ sources from the net,
// namely Andy Sloane's translation from gsyn with various adaptions
// like waveform, glide, accent, ... by me (Tobybear).

// As far as I could investigate, no GPL or other license applied to
// the various original code bits (VCO/VCF), so feel free to use this
// in your own applications, but please give credit and a link to my
// homepage: www.tobybear.de

// Have fun!

// Tobybear
// web: www.tobybear.de
// mail: tobybear@web.de

unit tb303;

interface

const
  ENVINC=64;

type
  Ttb303 = class
  private
    srate: double;
    cnt, cntmax: longint;
    vco_wav: integer;
    vco_inc_src,
    vco_inc,
    vco_inc_dest,
    vco_k,
    vcf_cutoff,
    vcf_envmod,
    vcf_envdecay,
    vcf_envdecayi,
    vcf_reso,
    vcf_rescoeff,
    vcf_e0,
    vcf_e1,
    vcf_c0,
    vcf_d1,
    vcf_d2,
    vcf_a,
    vcf_b,
    sld_spd,
    vcf_c,
    vca_accamt: single;
    vca_mode,
    vcf_envpos: integer;
    vca_attack,
    vca_decay,
    vca_a0,
    vca_a: single;

    procedure setcut(cut: single);
    procedure setres(res: single);
    procedure setenvmod(envmod: single);
    procedure setenvdec(envdec: single);
    procedure recalc;
  public
    constructor Create(sr: double);
    destructor Destroy; override;
    function Process: single;
    procedure NoteOn(note: integer; acc, glide: boolean);
    procedure NoteOff;

    property SampleRate: double read srate write srate;
    property Waveform: integer read vco_wav write vco_wav;
    property glideSpeed: single read sld_spd write sld_spd;
    property Cutoff: single read vcf_cutoff write setcut;
    property Resonance: single read vcf_reso write setres;
    property EnvMod: single read vcf_envmod write setenvmod;
    property EnvDecay: single read vcf_envdecayi write setenvdec;
    property AccAmt: single read vca_accamt write vca_accamt;
  end;

implementation

uses
  math;

procedure TTB303.setcut(cut: single);
begin
  vcf_cutoff := cut;
  recalc;
end;

procedure TTB303.setres(res: single);
begin
  vcf_reso := res;
  vcf_rescoeff := exp(-1.20+3.455 * vcf_reso);
  recalc;
end;

procedure TTB303.setenvmod(envmod: single);
begin
  vcf_envmod := envmod;
  recalc;
end;

procedure TTB303.setenvdec(envdec: single);
begin
  vcf_envdecayi := envdec;
  vcf_envdecay := (0.2 + (2.3 * vcf_envdecayi)) * srate;
  if vcf_envdecay < 1 then vcf_envdecay := 1;
  vcf_envdecay := power(0.1, 1 / vcf_envdecay * ENVINC);
end;

procedure TTB303.recalc;
begin
  vcf_e1 := exp(6.109 + 1.5876 * vcf_envmod + 2.1553 * vcf_cutoff - 1.2 * (1.0 - vcf_reso));
  vcf_e0 := exp(5.613 - 0.8 * vcf_envmod + 2.1553 * vcf_cutoff - 0.7696 * (1.0 - vcf_reso));
  vcf_e0 := vcf_e0 * pi / srate;
  vcf_e1 := vcf_e1 * pi / srate;
  vcf_e1 := vcf_e1 - vcf_e0;
  vcf_envpos := ENVINC;
end;

constructor TTB303.create(sr: double);
begin
  inherited create;

  sr := 44100;
  srate := sr;
  vco_inc_dest := (440 / srate);
  vco_inc := vco_inc_dest;
  vco_wav := 0;
  vco_k := 0;
  vcf_cutoff := 0;
  vcf_envmod := 0;
  vcf_reso := 0;
  vco_inc_dest := 0;
  vcf_envdecay := 0;
  vcf_envpos := ENVINC;
  vcf_a := 0;
  vcf_b := 0;
  vcf_d1 := 0;
  vcf_d2 := 0;
  vcf_c0 := 0;
  vcf_e0 := 0;
  vcf_e1 := 0;
  vca_mode := 2;
  vca_a := 0;
  vcf_rescoeff := 1;
  vca_attack := 1-0.94406088;
  vca_decay := 0.99897516;
  vca_a0 := 0.5;
  vca_accamt :=0.5;
  sld_spd := 0.1;
  setcut(0.9);
  setres(0.1);
  setenvmod(1);
  setenvdec(0.1);
  cntmax := 1;
end;

destructor TTB303.destroy;
begin
end;

function TTB303.Process:single;
var
  w, k: single;

  function rct(var x: single): single;
  begin
    if x < 0 then
      result := -0.5
    else
      result := 0.5
  end;

begin
  if (vcf_envpos >= ENVINC) then
  begin
    w := vcf_e0 + vcf_c0;
    k:= exp(-w / vcf_rescoeff);
    vcf_c0 := vcf_c0 * vcf_envdecay;
    vcf_a := 2 * cos(2 * w) * k;
    vcf_b := -k * k;
    vcf_c := 1 - vcf_a - vcf_b;
    vcf_envpos:=0;
  end;

  if vco_wav > 0 then
    result := vcf_a * vcf_d1 + vcf_b * vcf_d2 + vcf_c * rct(vco_k) * vca_a
  else
    result := vcf_a * vcf_d1 + vcf_b * vcf_d2 + vcf_c * vco_k * vca_a;

  vcf_d2 := vcf_d1;
  vcf_envpos := vcf_envpos + 1;
  vcf_d1 := result;
  
  inc(cnt);
  w := cnt / cntmax;

  if w < 1 then
  begin
    k := vco_inc_src * (1 - w) + w * vco_inc_dest
  end
  else
  begin
    vco_inc := vco_inc_dest;
    k := vco_inc;
  end;

  vco_k := vco_k + k;

  if (vco_k>0.5) then
  begin
    vco_k := vco_k - 1;
  end;

  if (vca_mode = 0) then
  begin
    vca_a := vca_a + (vca_a0 - vca_a) * vca_attack;
  end
  else if (vca_mode = 1) then
  begin
    vca_a := vca_a * vca_decay;
    if (vca_a < (1 / 65536)) then
    begin
      vca_a := 0;
      vca_mode := 2;
    end;
  end;
end;

procedure TTB303.noteon(note: integer; acc,glide: boolean);
begin
  vco_inc_src := vco_inc;
  vco_inc_dest := (440 / srate) * power(2, (note - 57) * (1 / 12));
  cntmax := round(srate * sld_spd);

  if glide then
  begin
    cnt := 0
  end
  else
  begin
    cnt := cntmax - 1;
  end;

  vca_mode := 0;
  vcf_c0 := vcf_e1;
  vcf_envpos := ENVINC;

  if acc then
  begin
    vca_a0 := 0.5 + vca_accamt * 0.5
  end
  else
  begin
    vca_a0 := 0.5;
  end;
end;

procedure TTB303.noteoff;
begin
  vca_a := 0;
  vca_mode := 2;
end;

end.

