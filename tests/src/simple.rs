use builder::Builder;
use std::{borrow::Cow, path::Path};

#[derive(Default, Builder)]
#[builder(optional)]
pub struct NamedFieldsStruct<'a, 'b> {
    simple_value: bool,
    cow_infer: Cow<'a, [i64]>,
    #[builder(exclude)]
    exclude_other_lifetime: Cow<'b, Path>,
    str_infer: Cow<'a, str>,
    vault: i64,
}

/* #[derive(Default, Builder)]
#[builder(optional)]
pub struct TupleStruct<'a>(bool, Cow<'a, [i64]>, Cow<'a, str>, i64);
 */
#[test]
fn named_fields() {}
