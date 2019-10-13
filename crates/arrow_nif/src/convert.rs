use arrow::datatypes::{DataType, DateUnit, Field, IntervalUnit, TimeUnit};
use rustler::types::{Atom, ListIterator};
use rustler::{Error, Term};

mod atoms {
    rustler::rustler_atoms! {
        atom ok;

        atom boolean;
        atom int;
        atom uint;
        atom float;
        atom timestamp;
        atom date;
        atom time;
        atom interval;
        atom utf8;
        atom list;

        atom s;
        atom ms;
        atom us;
        atom ns;

        atom d;

        atom year_month;
        atom day_time;
    }
}

pub fn decode_type<'a>(term: Term<'a>) -> Result<DataType, Error> {
    if term.is_atom() {}
    if term.is_tuple() {
        if let Ok((name, modifier)) = term.decode::<(Atom, Term<'a>)>() {
            if name == atoms::int() {
                let length = modifier.decode()?;

                match length {
                    8 => return Ok(DataType::Int8),
                    16 => return Ok(DataType::Int16),
                    32 => return Ok(DataType::Int32),
                    64 => return Ok(DataType::Int64),
                    _ => return Err(Error::BadArg),
                }
            }
            if name == atoms::uint() {
                let length = modifier.decode()?;

                match length {
                    8 => return Ok(DataType::UInt8),
                    16 => return Ok(DataType::UInt16),
                    32 => return Ok(DataType::UInt32),
                    64 => return Ok(DataType::UInt64),
                    _ => return Err(Error::BadArg),
                }
            }
            if name == atoms::float() {
                let length = modifier.decode()?;

                match length {
                    16 => return Ok(DataType::Float16),
                    32 => return Ok(DataType::Float32),
                    64 => return Ok(DataType::Float64),
                    _ => return Err(Error::BadArg),
                }
            }

            if name == atoms::list() {
                let sub_type = decode_type(modifier)?;
                return Ok(DataType::List(Box::new(sub_type)));
            }

            if name == atoms::timestamp() {
                let unit = modifier.decode::<Atom>()?;
                let unit = if unit == atoms::s() {
                    TimeUnit::Second
                } else if unit == atoms::ms() {
                    TimeUnit::Millisecond
                } else if unit == atoms::us() {
                    TimeUnit::Microsecond
                } else if unit == atoms::ns() {
                    TimeUnit::Nanosecond
                } else {
                    return Err(Error::BadArg);
                };

                return Ok(DataType::Timestamp(unit));
            }

            if name == atoms::interval() {
                let unit = modifier.decode::<Atom>()?;
                if unit == atoms::year_month() {
                    return Ok(DataType::Interval(IntervalUnit::YearMonth));
                }
                if unit == atoms::day_time() {
                    return Ok(DataType::Interval(IntervalUnit::DayTime));
                }

                return Err(Error::BadArg);
            }
        }

        if let Ok((name, size, unit)) = term.decode::<(Atom, i32, Atom)>() {
            if name == atoms::date() {
                let unit = if unit == atoms::d() {
                    DateUnit::Day
                } else if unit == atoms::ms() {
                    DateUnit::Millisecond
                } else {
                    return Err(Error::BadArg);
                };

                match size {
                    32 => return Ok(DataType::Date32(unit)),
                    64 => return Ok(DataType::Date64(unit)),
                    _ => return Err(Error::BadArg),
                }
            }

            if name == atoms::time() {
                let unit = if unit == atoms::s() {
                    TimeUnit::Second
                } else if unit == atoms::ms() {
                    TimeUnit::Millisecond
                } else if unit == atoms::us() {
                    TimeUnit::Microsecond
                } else if unit == atoms::ns() {
                    TimeUnit::Nanosecond
                } else {
                    return Err(Error::BadArg);
                };

                match size {
                    32 => return Ok(DataType::Time32(unit)),
                    64 => return Ok(DataType::Time64(unit)),
                    _ => return Err(Error::BadArg),
                }
            }
        }
    }
    if term.is_list() {
        let iter: ListIterator = term.decode()?;

        let mut fields = vec![];

        for t in iter {
            let (name, field_type): (Atom, Term<'a>) = t.decode()?;
            let field_type = decode_type(field_type)?;
            let field = Field::new(format!("{:?}", name).as_str(), field_type, false);
            fields.push(field);
        }

        return Ok(DataType::Struct(fields));
    }

    Err(Error::BadArg)
}