use syn::{GenericArgument, PathArguments, Type, TypePath};

use crate::BuilderFieldAttrs;

pub(crate) fn infer_type<'a>(input: &Type, attrs: &mut BuilderFieldAttrs<'a>) {
    match input {
        Type::Array(_) => {
            attrs.builder_type = Some(crate::BuilderFieldType::Vector);
        }
        Type::Path(value) => {
            if matches_type_path(["Option"], &value)
                || matches_type_path(["std", "Option"], &value)
                || matches_type_path(["std", "option", "Option"], &value)
            {
                attrs.required = false;
            } else if matches_type_path(["Cow"], &value)
                || matches_type_path(["std", "borrow", "Cow"], &value)
            {
                if let Some(result) =
                    first_generic_from_arguments(&value.path.segments.last().unwrap().arguments)
                        .unwrap()
                        .into_iter()
                        .find_map(|item| match item {
                            GenericArgument::Type(value) => Some(value),
                            _ => None,
                        })
                {
                    match result {
                        Type::Path(value) => {
                            if matches_type_path(["String"], &value)
                                || matches_type_path(["std", "String"], &value)
                                || matches_type_path(["std", "string", "String"], &value)
                                || matches_type_path(["str"], &value)
                                || matches_type_path(["std", "str"], &value)
                            {
                                attrs.into = true;
                            } else {
                                attrs.unwrap = true;
                            }
                        }
                        Type::Slice(_) => {
                            attrs.unwrap = true;
                            attrs.builder_type = Some(crate::BuilderFieldType::Vector);
                        }
                        _ => {}
                    }
                }
            } else if matches_type_path(["Vec"], &value)
                || matches_type_path(["std", "Vec"], &value)
                || matches_type_path(["std", "vec", "Vec"], &value)
            {
                attrs.builder_type = Some(crate::BuilderFieldType::Vector);
            }
        }
        Type::Slice(_) => {
            attrs.builder_type = Some(crate::BuilderFieldType::Vector);
        }
        _ => {}
    }
}
fn matches_type_path<'a, const N: usize>(path: [&'static str; N], input: &TypePath) -> bool {
    let segments = input.path.segments.clone();
    if segments.len() != N {
        return false;
    }
    let mut iter = segments.iter().enumerate();
    while let Some((index, segment)) = iter.next() {
        if segment.ident != path[index] {
            return false;
        }
    }
    true
}
fn first_generic_from_arguments(
    args: &PathArguments,
) -> Option<impl IntoIterator<Item = GenericArgument>> {
    match args {
        PathArguments::None => None,
        PathArguments::AngleBracketed(value) => Some(value.args.clone()),
        PathArguments::Parenthesized(_) => None,
    }
}
