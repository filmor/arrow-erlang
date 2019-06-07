use rustler::{Encoder, Env, Error, Term};

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
    }
}

rustler::rustler_export_nifs! {
    "arrow",
    [
        ("test", 0, test)
    ],
    None
}


fn test<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    Ok(atoms::ok().encode(env))
}
