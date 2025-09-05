with Ada.Text_IO;
with Ada.Numerics;



use Ada.Text_IO;
use Ada.Numerics;

package RVM is
	type State is tagged private;
	type State_Access is access State;

	function Create(Nx, Ny, Nz: Positive; Lx, Ly, Lz, Re, T, T_end: Float)
		return State_Access;
	
	procedure Destroy(Self : in out State_Access);

	procedure Initialize(Self : in out State);

	procedure Step(Self : in out State);

	procedure Save_File(Self : State; Filename : String);
	function Load_File(Filename : String) return State_Access;

	procedure State_Info(Self : State_Access);
private
	type F_1D_Array is array(Positive range <>) of Float;
	type F_1D_Array_Access is access F_1D_Array;

	type F_4D_Array is array(Positive range <>, Positive range <>, Positive range <>, Positive range <>) of Float;
	type F_4D_Array_Access is access F_4D_Array;
	
	type Field is tagged record
		Name		: Ada.Strings.Unbounded.Unbounded_String;
		Nx, Ny, Nz	: Positive;
		Lx, Ly, Lz	: Float;
		Re		: Float;
		T, T_end	: Float;

		Meshx		: F_1D_Array_Access;
		Meshy		: F_1D_Array_Access;
		Meshz		: F_1D_Array_Access;

		Pos		: F_4D_Array_Access;
		Velo		: F_4D_Array_Access;
		Vort		: F_4D_Array_Access;
	end record;
end RVM;
