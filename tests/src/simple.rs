use builder::Builder;
use std::borrow::Cow;
#[derive(Default, Builder)]
#[builder(optional)]
pub struct NamedFieldsStruct<'a> {
    alpha: bool,
    beta: Cow<'a, [i64]>,
    zeta: Cow<'a, str>,
    vault: i64,
}

#[derive(Default, Builder)]
#[builder(optional)]
pub struct TupleStruct<'a>(bool, Cow<'a, [i64]>, Cow<'a, str>, i64);

#[test]
fn named_fields() {}
