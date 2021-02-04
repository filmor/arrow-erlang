mod buffer;
mod convert;

use convert::decode_type;

use arrow::array::{ArrayData, ArrayDataRef};
use arrow::datatypes::DataType;
use rustler::{self, NifResult};
use rustler::{Encoder, Env, ResourceArc, Term};

mod atoms {
    rustler::atoms! {
        ok,
    }
}

struct ArrayResource {
    data_type: DataType,
    data: ArrayDataRef,
}

pub fn on_load<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    rustler::resource!(ArrayResource, env);
    true
}

#[rustler::nif]
fn new_array<'a>(env: Env<'a>, data_type: Term<'a>, length: usize) -> NifResult<Term<'a>> {
    let data_type = decode_type(data_type)?;

    let array = ArrayData::builder(data_type.clone()).len(length).build();

    let resource = ArrayResource {
        data_type: data_type.clone(),
        data: array,
    };
    let resource = ResourceArc::new(resource);

    Ok((atoms::ok(), resource).encode(env))
}

rustler::init!("arrow", [new_array], load = on_load);
