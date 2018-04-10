module OOPS =
struct
	module M0 =
	struct
		type edge = {arity : int; value : bool}
		let ( ->> ) blist edge = assert false
		let arity edge = edge.arity
		(* where true represent significant variables and false non significant ones *)
		let   neg edge =
			{arity = edge.arity; value = not edge.value}
		let ( *! ) edge0 edge1 = assert false
		let ( &! ) edge0 edge1 =
			assert(edge0.arity = edge1.arity);
			{arity = edge0.arity; value = edge0.value && edge1.value}
		let ( ^! ) edge0 edge1 =
			assert(edge0.arity = edge1.arity);
			{arity = edge0.arity; value = edge0.value <> edge1.value}
		let ( |! ) edge0 edge1 =
			assert(edge0.arity = edge1.arity);
			{arity = edge0.arity; value = edge0.value || edge1.value}
		let ( =! ) edge0 edge1 =
			assert(edge0.arity = edge1.arity);
			{arity = edge0.arity; value = edge0.value =  edge1.value}

		let cst value arity = {arity; value}
		let to_bool edge = Some edge.value
	end

	include GOops.MODULE(M0)

	module SUInt = SUInt.MODULE(M0) (* fixed Size INTeger *)
	module VUInt = VUInt.MODULE(M0) (* Variable size INTeger *)

end
