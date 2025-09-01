with Ada.Text_IO;
with Ada.Numerics;
with Ada.Float_Text_IO;

use Ada.Text_IO;
use Ada.Numerics;
use Ada.Float_Text_IO;

procedure rvm is
	type F_1D_Array is array(Positive range <>) of Float;
	type F_4D_Array is array(
		Positive range <>,
		Positive range <>,
		Positive range <>,
		Positive range <>) of Float;

	type Field is record
		Nx	:Integer;
		Ny	:Integer;
		Nz	:Integer;
		Re	:Float;
		t	:Float;
		t_end	:Float;
		Lx	:Float;
		Ly	:Float;
		Lz	:Float;
		--Meshx	:F_1D_Array;
		--Meshy	:F_1D_Array;
		--Meshz	:F_1D_Array;
		--Pos	:F_4D_Array;
		--Velo	:F_4D_Array;
		--Vort	:F_4D_Array;
	end record;

	procedure TGV(F : in out Field) is
	begin
		F.Nx	:= 64;
		F.Ny	:= 64;
		F.Nz	:= 64;
		F.Re	:= 100.0;
		F.t	:= 0.0;
		F.t_end	:= 0.0;
		F.Lx	:= Pi;
		F.Ly	:= Pi;
		F.Lz	:= Pi;

		
		--for I in 1..F.Nx loop
		--	F.Meshx(I) := Float(I - 1) / Float(F.Nx - 1) * F.Lx;
		--end loop;
		--for I in 1..F.Ny loop
		--	F.Meshy(I) := Float(I - 1) / Float(F.Ny - 1) * F.Ly;
		--end loop;
		--for I in 1..F.Nz loop
		--	F.Meshz(I) := Float(I - 1) / Float(F.Nz - 1) * F.Lz;
		--end loop;
		
	end;

begin
	Put_Line("Field type created!");
end rvm;
