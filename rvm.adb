with Ada.Text_IO;
with Ada.Numerics;
with Ada.Float_Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Numerics.Elementary_Functions;

use Ada.Text_IO;
use Ada.Numerics;
use Ada.Float_Text_IO;
use Ada.Numerics.Elementary_Functions;
procedure rvm is
	type F_1D_Array is array(Positive range <>) of Float;
	type F_4D_Array is array(Positive range <>, Positive range <>, Positive range <>, Positive range <>) of Float;
	type F_1D_Array_Access is access F_1D_Array;
	type F_4D_Array_Access is access F_4D_Array;

	procedure Free_1D is new Ada.Unchecked_Deallocation(
		Object	=> F_1D_Array,
		Name	=> F_1D_Array_Access
	);

	procedure Free_4D is new Ada.Unchecked_Deallocation(
		Object	=> F_4D_Array,
		Name	=> F_4D_Array_Access
	);

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
		Meshx	:F_1D_Array_Access;
		Meshy	:F_1D_Array_Access;
		Meshz	:F_1D_Array_Access;
		Pos	:F_4D_Array_Access;
		Velo	:F_4D_Array_Access;
		Vort	:F_4D_Array_Access;
	end record;

	procedure Initialize_Field(F : in out Field) is
		Dx, Dy, Dz	: Float;
	begin
		Dx	:= F.Lx / Float(F.Nx - 1);
		Dy	:= F.Ly / Float(F.Ny - 1);
		Dz	:= F.Lz / Float(F.Nz - 1);
		
		F.Meshx	:= new F_1D_Array(1..F.Nx);
		F.Meshy	:= new F_1D_Array(1..F.Ny);
		F.Meshz	:= new F_1D_Array(1..F.Nz);
		
		for I in 1..F.Nx loop
			F.Meshx(I)	:= Float(I-1) * Dx;
		end loop;
		for I in 1..F.Ny loop
			F.Meshy(I)	:= Float(I-1) * Dy;
		end loop;
		for I in 1..F.Nz loop
			F.Meshz(I)	:= Float(I-1) * Dz;
		end loop;

		F.Pos	:= new F_4D_Array(1..F.Nx, 1..F.Ny, 1..F.Nz, 1..3);
		F.Velo	:= new F_4D_Array(1..F.Nx, 1..F.Ny, 1..F.Nz, 1..3);
		F.Vort	:= new F_4D_Array(1..F.Nx, 1..F.Ny, 1..F.Nz, 1..3);

		for I In 1..F.Nx loop
			for J in 1..F.Ny loop
				for K in 1..f.Nz loop
					F.Pos(I,J,K,1)		:= F.Meshx(I);
					F.Pos(I,J,K,2)		:= F.Meshy(J);
					F.Pos(I,J,K,3)		:= F.Meshz(K);
					for D in 1..3 loop
						F.Velo(I,J,K,D)	:= 0.0;
						F.Vort(I,J,K,D)	:= 0.0;
					end loop;
				end loop;
			end loop;
		end loop;
	end  Initialize_Field;
	
	procedure Finalize_Field(F : in out Field) is
	begin
		Free_1D(F.Meshx);
		Free_1D(F.Meshy);
		Free_1D(F.Meshz);
		Free_4D(F.Pos);
		Free_4D(F.Velo);
		Free_4D(F.Vort);
	end Finalize_Field;

	procedure Initialize_TGV(F : in out Field) is
		X, Y, Z : Float;
	begin
		for I In 1..F.Nx loop
			for J in 1..F.Ny loop
				for K in 1..f.Nz loop
					X	:= F.Pos(I,J,K,1);
					Y	:= F.Pos(I,J,K,2);
					Z	:= F.Pos(I,J,K,3);
					F.Vort(I,J,K,1)	:=   -   cos(X) * sin(Y) * sin(Z);
					F.Vort(I,J,K,2)	:=   -   sin(X) * cos(Y) * sin(Z);
					F.Vort(I,J,K,3)	:= 2.0 * sin(X) * sin(Y) * cos(Z);
				end loop;
			end loop;
		end loop;

	end Initialize_TGV;

	procedure Field_Info(F : in Field) is
	begin
		Put_Line("Reynold number: " & Float'Image(F.Re));
		Put_Line("Field Dominant size and meshing details");
		Put_Line("Field x length: " & Float'Image(F.Lx) & ", mesh number: " & Integer'Image(F.Nx));
		Put_Line("Field y length: " & Float'Image(F.Ly) & ", mesh number: " & Integer'Image(F.Ny));
		Put_Line("Field z length: " & Float'Image(F.Lz) & ", mesh number: " & Integer'Image(F.Nz));
	end Field_Info;
	
	F	: Field;
begin
	F.Nx	:= 32;
	F.Ny	:= 24;
	F.Nz	:= 48;
	F.Lx	:= 2.0 * Pi;
	F.Ly	:= 2.0 * Pi;
	F.Lz	:= 2.0 * Pi;
	F.Re	:= 100.0;
	F.t	:= 0.0;
	F.t_end	:= 20.0;
	Initialize_Field(F);
	Initialize_TGV(F);
	Field_Info(F);
end rvm;
