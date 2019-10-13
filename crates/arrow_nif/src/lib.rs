mod convert;
mod buffer;

use convert::decode_type;

use arrow::array::{ ArrayData, ArrayDataRef };
use arrow::datatypes::DataType;
use rustler;
use rustler::{Encoder, Env, NifResult, Term, ResourceArc};

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
    }
}

rustler::rustler_export_nifs! {
    "arrow",
    [
        ("new_array", 2, new_array),
        ("to_list", 1, to_list),
        ("from_binary", 1, from_binary),
        ("to_binary", 1, to_binary)
    ],
    Some(on_load)
}

struct ArrayResource {
    data_type: DataType,
    data: ArrayDataRef
}


pub fn on_load<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    rustler::resource_struct_init!(ArrayResource, env);
    true
}


fn new_array<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let data_type = decode_type(args[0])?;

    let array = ArrayData::builder(data_type.clone()).len(args[1].decode()?).build();

    let resource = ArrayResource { data_type: data_type.clone(), data: array };
    let resource = ResourceArc::new(resource);

    Ok((atoms::ok(), resource).encode(env))
}


fn to_list<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let res: ResourceArc<ArrayResource> = args[0].decode()?;
    let _data = &res.data;

    Ok(atoms::ok().encode(env))
}


fn from_binary<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    Ok(atoms::ok().encode(env))
}


fn to_binary<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    Ok(atoms::ok().encode(env))
}