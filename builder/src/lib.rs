use std::{borrow::Cow, fmt::Display};

use convert_case::Casing;
use proc_macro2::{Span, TokenStream};
use quote::{IdentFragment, ToTokens, quote};
use syn::{
    DataStruct, DeriveInput, Field, GenericArgument, Generics, Ident, Index, Lifetime,
    LifetimeParam, LitStr, PathArguments, Type, TypeParam, Visibility, parse_macro_input,
    spanned::Spanned,
};
#[cfg(feature = "inference")]
mod inference;

const DEFAULT_GET_PREFIX: &'static str = "get_";
const DEFAULT_SET_PREFIX: &'static str = "set_";
const DEFAULT_VECTOR_PREFIX: &'static str = "with_";
/**
Macro for generating builders from structs.
*/
#[proc_macro_derive(Builder, attributes(builder))]
pub fn macro_builder(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(item as DeriveInput);
    let ident = item.ident;
    let attrs = BuilderInputAttrs::parse_attrs(item.attrs);
    let vis = item.vis;
    let generics = item.generics;
    match attrs {
        Ok(attrs) => match item.data {
            syn::Data::Struct(data) => BuilderInput::from_struct(ident, generics, vis, attrs, data)
                .map_or_else(
                    |err| err.to_compile_error(),
                    |value| value.to_token_stream(),
                )
                .into(),
            syn::Data::Enum(_) => syn::Error::new(Span::call_site(), "enums are unsupported")
                .to_compile_error()
                .into(),
            syn::Data::Union(_) => syn::Error::new(Span::call_site(), "unions are unsupported")
                .to_compile_error()
                .into(),
        },
        Err(err) => err.to_compile_error().into(),
    }
}
#[derive(Clone)]
enum FieldIdent {
    Ident(Ident),
    Index(Index),
}
#[derive(Clone)]
enum PhantomField<'a> {
    Lifetime(&'a LifetimeParam),
    Type(&'a TypeParam),
}
impl Display for FieldIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FieldIdent::Ident(ident) => Display::fmt(&ident, f),
            FieldIdent::Index(index) => IdentFragment::fmt(&index, f),
        }
    }
}
impl ToTokens for FieldIdent {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            FieldIdent::Ident(ident) => {
                ident.to_tokens(tokens);
            }
            FieldIdent::Index(index) => {
                index.to_tokens(tokens);
            }
        }
    }
}
#[derive(Clone)]
struct BuilderInput {
    source_ident: Ident,
    generics: Generics,
    builder_ident: Ident,
    builder_error_ident: Ident,
    fields: Vec<BuilderField>,
    vis: Visibility,
    tuple: bool,
}

impl BuilderInput {
    fn from_struct(
        ident: Ident,
        generics: Generics,
        vis: Visibility,
        attrs: BuilderInputAttrs,
        data: DataStruct,
    ) -> Result<Self, syn::Error> {
        let builder_ident = attrs.builder_ident.unwrap_or(Ident::new(
            format!("{}Builder", ident).as_str(),
            Span::call_site(),
        ));
        let builder_error_ident = attrs.error_ident.unwrap_or(Ident::new(
            format!("{}Error", builder_ident).as_str(),
            Span::call_site(),
        ));
        let mut builder_fields = Vec::new();
        let mut tuple = false;
        let get_prefix = attrs.get_prefix.map_or_else(
            || Cow::Borrowed(DEFAULT_GET_PREFIX),
            |value| Cow::Owned(value),
        );
        let set_prefix = attrs.set_prefix.map_or_else(
            || Cow::Borrowed(DEFAULT_SET_PREFIX),
            |value| Cow::Owned(value),
        );
        let vector_prefix = attrs.vector_prefix.map_or_else(
            || Cow::Borrowed(DEFAULT_VECTOR_PREFIX),
            |value| Cow::Owned(value),
        );
        for (index, field) in data.fields.iter().enumerate() {
            let attrs =
                BuilderFieldAttrs::parse_attrs(&field.ty, field.attrs.clone(), attrs.all_required)?;
            builder_fields.push(BuilderField::new(
                index,
                field.clone(),
                attrs,
                &builder_error_ident,
                &get_prefix,
                &set_prefix,
                &vector_prefix,
            )?);
            tuple = field.ident.is_none();
        }
        Ok(BuilderInput {
            source_ident: ident.clone(),
            generics,
            builder_ident,
            builder_error_ident,
            fields: builder_fields,
            vis,
            tuple,
        })
    }
}

#[derive(Clone)]
struct BuilderField {
    source_type: (TokenStream, FieldIdent),
    builder_type: Option<(TokenStream, FieldIdent)>,
    setter_type: Option<BuilderFnInfo>,
    getter_type: Option<BuilderFnInfo>,
    builder_source_conversion: TokenStream,
    error: Option<BuilderErrorInfo>,
}
#[derive(Clone)]
struct BuilderFnInfo {
    ident: Ident,
    args: TokenStream,
    return_info: TokenStream,
    body: TokenStream,
}
#[derive(Clone)]
struct BuilderErrorInfo {
    ident: Ident,
}
impl BuilderField {
    fn new(
        index: usize,
        field: Field,
        attrs: BuilderFieldAttrs,
        error_ident: &Ident,
        get_prefix: &str,
        set_prefix: &str,
        vector_prefix: &str,
    ) -> Result<Self, syn::Error> {
        let builder_field_type = attrs.builder_type.unwrap_or(BuilderFieldType::Scalar);
        let source_field_type = field.ty;
        let source_field_ident = field.ident.map_or_else(
            || FieldIdent::Index(Index::from(index)),
            |ident| FieldIdent::Ident(ident),
        );
        match builder_field_type {
            BuilderFieldType::Scalar => {
                let (builder_element_type, into) = if attrs.unwrap {
                    (unwrap_type(source_field_type.clone())?, true)
                } else {
                    (source_field_type.clone(), false)
                };
                let required = attrs.required;
                let builder_type = quote! { std::option::Option<#builder_element_type> };
                let builder_field_ident = source_field_ident.clone();
                let setter_ident = if let Some((name, full)) = attrs.setter_name {
                    if full {
                        Ident::new(name.as_ref(), Span::call_site())
                    } else {
                        Ident::new(
                            format!("{}{}", set_prefix, &name).as_str(),
                            Span::call_site(),
                        )
                    }
                } else {
                    Ident::new(
                        format!("{}{}", set_prefix, &builder_field_ident).as_str(),
                        Span::call_site(),
                    )
                };
                let getter_ident = if let Some((name, full)) = attrs.getter_name {
                    if full {
                        Ident::new(name.as_ref(), Span::call_site())
                    } else {
                        Ident::new(
                            format!("{}{}", get_prefix, name).as_str(),
                            Span::call_site(),
                        )
                    }
                } else {
                    match &builder_field_ident {
                        FieldIdent::Ident(ident) => ident.clone(),
                        FieldIdent::Index(index) => Ident::new(
                            format!("{}{}", get_prefix, index.index).as_str(),
                            Span::call_site(),
                        ),
                    }
                };
                let error_enum = if required {
                    Some(Ident::new(
                        format!(
                            "Missing{}",
                            builder_field_ident
                                .to_string()
                                .to_case(convert_case::Case::UpperCamel)
                        )
                        .as_str(),
                        Span::call_site(),
                    ))
                } else {
                    None
                };
                Ok(Self {
                    source_type: (
                        source_field_type.into_token_stream(),
                        source_field_ident.clone(),
                    ),
                    builder_type: Some((builder_type.clone(), builder_field_ident.clone())),
                    setter_type: Some(BuilderFnInfo {
                        ident: setter_ident,
                        args: if attrs.into {
                            quote! { &mut self, value: impl Into<#builder_element_type> }
                        } else {
                            quote! { &mut self, value: #builder_element_type }
                        },
                        return_info: quote! { () },
                        body: if attrs.into {
                            quote! { self.#builder_field_ident = Some(value.into()); }
                        } else {
                            quote! {
                                self.#builder_field_ident = Some(value);
                            }
                        },
                    }),
                    getter_type: Some(BuilderFnInfo {
                        ident: getter_ident,
                        args: quote! { &self },
                        return_info: quote! { &#builder_type },
                        body: quote! { &self.#builder_field_ident },
                    }),
                    builder_source_conversion: if into {
                        if required {
                            quote! { self.#builder_field_ident.ok_or(#error_ident::#error_enum)?.into() }
                        } else {
                            quote! { self.#builder_field_ident.unwrap_or_default().into() }
                        }
                    } else {
                        if required {
                            quote! { self.#builder_field_ident.ok_or(#error_ident::#error_enum)? }
                        } else {
                            quote! { self.#builder_field_ident.unwrap_or_default() }
                        }
                    },
                    error: error_enum.map(|ident| BuilderErrorInfo { ident }),
                })
            }
            BuilderFieldType::Vector => {
                let (builder_type, into) = if attrs.unwrap {
                    (unwrap_type(source_field_type.clone())?, true)
                } else {
                    (source_field_type.clone(), false)
                };
                let builder_element_type = unwrap_type(builder_type)?;
                let builder_field_type =
                    quote! { std::vec::Vec<#builder_element_type>}.into_token_stream();
                let builder_field_ident = source_field_ident.clone();
                let setter_ident = if let Some((name, full)) = attrs.setter_name {
                    if full {
                        Ident::new(name.as_ref(), Span::call_site())
                    } else {
                        Ident::new(
                            format!("{}{}", vector_prefix, &name).as_str(),
                            Span::call_site(),
                        )
                    }
                } else {
                    Ident::new(
                        format!("with_{}", &builder_field_ident).as_str(),
                        Span::call_site(),
                    )
                };
                let getter_ident = if let Some((name, full)) = attrs.getter_name {
                    if full {
                        Ident::new(name.as_ref(), Span::call_site())
                    } else {
                        Ident::new(
                            format!("{}{}", get_prefix, name).as_str(),
                            Span::call_site(),
                        )
                    }
                } else {
                    match &builder_field_ident {
                        FieldIdent::Ident(ident) => ident.clone(),
                        FieldIdent::Index(index) => Ident::new(
                            format!("{}{}", get_prefix, index.index).as_str(),
                            Span::call_site(),
                        ),
                    }
                };
                Ok(Self {
                    source_type: (
                        source_field_type.into_token_stream(),
                        source_field_ident.clone(),
                    ),
                    builder_type: Some((builder_field_type.clone(), builder_field_ident.clone())),
                    setter_type: Some(BuilderFnInfo {
                        ident: setter_ident,
                        args: if attrs.into {
                            quote! { &mut self, value: impl Into<#builder_element_type> }
                        } else {
                            quote! { &mut self, value: #builder_element_type }
                        },
                        return_info: quote! { () },
                        body: if attrs.into {
                            quote! {
                                self.#builder_field_ident.push(value.into());
                            }
                        } else {
                            quote! {
                                self.#builder_field_ident.push(value);
                            }
                        },
                    }),
                    getter_type: Some(BuilderFnInfo {
                        ident: getter_ident,
                        args: quote! { &self },
                        return_info: quote! { &[#builder_element_type] },
                        body: quote! { &self.#builder_field_ident },
                    }),
                    builder_source_conversion: if into {
                        quote! { self.#builder_field_ident.into() }
                    } else {
                        quote! { self.#builder_field_ident }
                    },
                    error: None,
                })
            }
            BuilderFieldType::Excluded => {
                let conversion = quote! { <#source_field_type>::default() };
                Ok(Self {
                    source_type: (source_field_type.into_token_stream(), source_field_ident),
                    builder_type: None,
                    setter_type: None,
                    getter_type: None,
                    builder_source_conversion: conversion,
                    error: None,
                })
            }
        }
    }
}

#[derive(Clone, Copy)]
enum BuilderFieldType {
    Scalar,
    Vector,
    Excluded,
}

#[derive(Debug, Clone, Default)]
struct BuilderInputAttrs {
    builder_ident: Option<Ident>,
    error_ident: Option<Ident>,
    all_required: bool,
    set_prefix: Option<String>,
    get_prefix: Option<String>,
    vector_prefix: Option<String>,
}
impl BuilderInputAttrs {
    fn parse_attrs(attrs: impl IntoIterator<Item = syn::Attribute>) -> Result<Self, syn::Error> {
        let mut output = Self::default();
        for attr in attrs.into_iter() {
            if attr.path().is_ident("builder") {
                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("name") {
                        output.builder_ident = Some(Ident::new(
                            meta.value()?.parse::<LitStr>()?.value().as_str(),
                            Span::call_site(),
                        ));
                    } else if meta.path.is_ident("error_name") {
                        output.error_ident = Some(Ident::new(
                            meta.value()?.parse::<LitStr>()?.value().as_str(),
                            Span::call_site(),
                        ));
                    } else if meta.path.is_ident("get_prefix") {
                        output.get_prefix = Some(meta.value()?.parse::<LitStr>()?.value());
                    } else if meta.path.is_ident("set_prefix") {
                        output.set_prefix = Some(meta.value()?.parse::<LitStr>()?.value());
                    } else if meta.path.is_ident("vector_prefix") {
                        output.vector_prefix = Some(meta.value()?.parse::<LitStr>()?.value());
                    } else if meta.path.is_ident("optional") {
                        output.all_required = false;
                    } else if meta.path.is_ident("required") {
                        output.all_required = true;
                    } else {
                        return Err(syn::Error::new(
                            Span::call_site(),
                            format!("unknown attribute '{:?}'", meta.path.get_ident()).as_str(),
                        ));
                    }
                    Ok(())
                })?;
            }
        }
        Ok(output)
    }
}
#[derive(Clone, Default)]
struct BuilderFieldAttrs<'a> {
    unwrap: bool,
    builder_type: Option<BuilderFieldType>,
    required: bool,
    into: bool,
    getter_name: Option<(Cow<'a, str>, bool)>,
    setter_name: Option<(Cow<'a, str>, bool)>,
}
impl<'a> BuilderFieldAttrs<'a> {
    fn parse_attrs(
        ty: &Type,
        attrs: impl IntoIterator<Item = syn::Attribute>,
        default_required: bool,
    ) -> Result<Self, syn::Error> {
        let mut output = Self::default();
        #[cfg(feature = "inference")]
        inference::infer_type(ty, &mut output);
        output.required = default_required;
        let mut builder_type_set = false;
        for attr in attrs.into_iter() {
            if attr.path().is_ident("builder") {
                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("exclude") {
                        if !builder_type_set {
                            output.builder_type = Some(BuilderFieldType::Excluded);
                            builder_type_set = true;
                        } else {
                            return Err(syn::Error::new(
                                Span::call_site(),
                                format!(
                                    "conflicting attribute '{:?}'",
                                    meta.path.get_ident().unwrap()
                                )
                                .as_str(),
                            ));
                        }
                    } else if meta.path.is_ident("scalar") {
                        if !builder_type_set {
                            output.builder_type = Some(BuilderFieldType::Scalar);
                            builder_type_set = true;
                        } else {
                            return Err(syn::Error::new(
                                Span::call_site(),
                                format!(
                                    "conflicting attribute '{:?}'",
                                    meta.path.get_ident().unwrap()
                                )
                                .as_str(),
                            ));
                        }
                    } else if meta.path.is_ident("vector") {
                        if !builder_type_set {
                            output.builder_type = Some(BuilderFieldType::Vector);
                            builder_type_set = true;
                        } else {
                            return Err(syn::Error::new(
                                Span::call_site(),
                                format!(
                                    "conflicting attribute '{:?}'",
                                    meta.path.get_ident().unwrap()
                                )
                                .as_str(),
                            ));
                        }
                    } else if meta.path.is_ident("unwrap") {
                        output.unwrap = true;
                    } else if meta.path.is_ident("into") {
                        output.into = true;
                    } else if meta.path.is_ident("optional") {
                        output.required = false;
                    } else if meta.path.is_ident("required") {
                        output.required = true;
                    } else if meta.path.is_ident("getter") {
                        if output.getter_name.is_none() {
                            output.getter_name =
                                Some((meta.value()?.parse::<LitStr>()?.value().into(), true));
                        } else {
                            return Err(syn::Error::new(
                                Span::call_site(),
                                format!(
                                    "conflicting attribute '{:?}'",
                                    meta.path.get_ident().unwrap()
                                )
                                .as_str(),
                            ));
                        }
                    } else if meta.path.is_ident("getter_name") {
                        if output.getter_name.is_none() {
                            output.getter_name =
                                Some((meta.value()?.parse::<LitStr>()?.value().into(), false));
                        } else {
                            return Err(syn::Error::new(
                                Span::call_site(),
                                format!(
                                    "conflicting attribute '{:?}'",
                                    meta.path.get_ident().unwrap()
                                )
                                .as_str(),
                            ));
                        }
                    } else if meta.path.is_ident("setter") {
                        if output.setter_name.is_none() {
                            output.setter_name =
                                Some((meta.value()?.parse::<LitStr>()?.value().into(), true));
                        } else {
                            return Err(syn::Error::new(
                                Span::call_site(),
                                format!(
                                    "conflicting attribute '{:?}'",
                                    meta.path.get_ident().unwrap()
                                )
                                .as_str(),
                            ));
                        }
                    } else if meta.path.is_ident("setter_name") {
                        if output.setter_name.is_none() {
                            output.setter_name =
                                Some((meta.value()?.parse::<LitStr>()?.value().into(), false));
                        } else {
                            return Err(syn::Error::new(
                                Span::call_site(),
                                format!(
                                    "conflicting attribute '{:?}'",
                                    meta.path.get_ident().unwrap()
                                )
                                .as_str(),
                            ));
                        }
                    } else {
                        return Err(syn::Error::new(
                            Span::call_site(),
                            format!("unknown attribute '{:?}'", meta.path.get_ident()).as_str(),
                        ));
                    }
                    Ok(())
                })?;
            }
        }
        Ok(output)
    }
}

impl ToTokens for BuilderInput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let vis = &self.vis;
        let ident = &self.builder_ident;
        let source = &self.source_ident;
        let (impls, tys, wheres) = &self.generics.split_for_impl();
        let builder_fields = &self.fields;
        let builder_error_ident = &self.builder_error_ident;
        let tuple = &self.tuple;
        let mut getters = Vec::new();
        let mut setters = Vec::new();
        let mut initiators = Vec::new();
        let mut fields = Vec::new();

        let mut errors = Vec::new();
        let mut error_fields = Vec::new();
        for field in builder_fields {
            let BuilderField {
                source_type,
                builder_type,
                setter_type,
                getter_type,
                builder_source_conversion,
                error,
            } = field;
            let (_, source_ident) = source_type;
            initiators.push(quote! {
                #source_ident: #builder_source_conversion
            });
            if let Some((builder_type, builder_ident)) = builder_type {
                match builder_ident {
                    FieldIdent::Ident(ident) => {
                        fields.push(quote! {
                            #ident: #builder_type
                        });
                    }
                    FieldIdent::Index(_) => {
                        fields.push(quote! {
                            #builder_type
                        });
                    }
                }

                if let Some(BuilderFnInfo {
                    ident,
                    args,
                    return_info,
                    body,
                }) = getter_type
                {
                    getters.push(quote! {
                        pub fn #ident(#args) -> #return_info {
                            #body
                        }
                    });
                }
                if let Some(BuilderFnInfo {
                    ident,
                    args,
                    return_info,
                    body,
                }) = setter_type
                {
                    setters.push(quote! {
                        pub fn #ident(#args) -> #return_info {
                            #body
                        }
                    });
                }
                if let Some(BuilderErrorInfo { ident }) = error {
                    errors.push(ident);
                    error_fields.push(builder_ident.to_string());
                }
            }
        }
        let build_fn = if errors.is_empty() {
            quote! {
                pub fn build(self) -> #source #tys {
                    #source {
                        #(#initiators),*
                    }
                }
            }
        } else {
            quote! {
                pub fn build(self) -> std::result::Result<#source #tys,#builder_error_ident> {
                    Ok(
                        #source {
                            #(#initiators),*
                        }
                    )
                }
            }
        };
        let phantom_field_iter = self
            .generics
            .lifetimes()
            .map(|value| PhantomField::Lifetime(value))
            .chain(
                self.generics
                    .type_params()
                    .map(|value| PhantomField::Type(value)),
            );
        let builder_body = if *tuple {
            for item in phantom_field_iter {
                match item {
                    PhantomField::Lifetime(item) => {
                        fields
                            .push(quote! {std::marker::PhantomData<& #item ()>}.to_token_stream());
                    }
                    PhantomField::Type(item) => {
                        fields.push(quote! {std::marker::PhantomData<#item>}.to_token_stream());
                    }
                }
            }
            quote! { (#(#fields),*); }
        } else {
            for (index, item) in phantom_field_iter.enumerate() {
                let phantom_field_ident = syn::Ident::new(
                    format!("___builder_field_{}", index).as_str(),
                    Span::call_site(),
                );
                match item {
                    PhantomField::Lifetime(item) => {
                        fields.push(
                            quote! {#phantom_field_ident: std::marker::PhantomData<& #item ()>}
                                .to_token_stream(),
                        );
                    }
                    PhantomField::Type(item) => {
                        fields.push(
                            quote! {#phantom_field_ident: std::marker::PhantomData<#item>}
                                .to_token_stream(),
                        );
                    }
                }
            }
            quote! { { #(#fields),* } }
        };
        quote! {
            impl #impls #source #tys #wheres {
                pub fn builder() -> #ident #tys {
                    #ident::default()
                }
            }
            #[derive(Debug,Clone,Default)]
            #vis struct #ident #tys #wheres  #builder_body
            impl #impls #ident #tys #wheres {
                #(#getters)*
                #(#setters)*
                #build_fn

            }

        }
        .to_tokens(tokens);
        if !errors.is_empty() {
            quote! {
                #[derive(Debug,Clone,Copy)]
                #vis enum #builder_error_ident {
                    #(#errors),*
                }
                impl std::fmt::Display for #builder_error_ident {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        match self {
                            #(
                                #errors => write!(f, "{} is required", #error_fields)
                            ),*
                        }
                    }
                }
                impl std::error::Error for #builder_error_ident {}
            }
            .to_tokens(tokens);
        }
    }
}

fn unwrap_type(value: Type) -> Result<Type, syn::Error> {
    let span = value.span();
    match value {
        Type::Array(value) => Ok((*value.elem).clone()),
        Type::Path(value) => {
            if let Some(value) = value.path.segments.last() {
                if let PathArguments::AngleBracketed(arguments) = &value.arguments {
                    let arguments = arguments
                        .args
                        .clone()
                        .into_iter()
                        .filter_map(|arg| {
                            if let GenericArgument::Type(arg) = arg {
                                Some(arg)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();
                    if arguments.len() == 1 {
                        Ok(arguments.first().unwrap().clone())
                    } else if arguments.len() > 1 {
                        Err(syn::Error::new(
                            span,
                            "unable to unwrap type: too many generic types",
                        ))
                    } else {
                        Err(syn::Error::new(
                            span,
                            "unable to unwrap type: missing generic type",
                        ))
                    }
                } else {
                    Err(syn::Error::new(span, "unable to unwrap type"))
                }
            } else {
                Err(syn::Error::new(
                    span,
                    "unable to unwrap type: no segments found",
                ))
            }
        }
        Type::Reference(value) => Ok((*value.elem).clone()),
        Type::Slice(value) => Ok((*value.elem).clone()),
        _ => Err(syn::Error::new(span, "unable to unwrap type")),
    }
}
