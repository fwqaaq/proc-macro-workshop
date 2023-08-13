use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::visit::{self, Visit};
use syn::{
    parse_macro_input, parse_quote, parse_str, punctuated::Punctuated, token::Comma,
    AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput, Expr, ExprLit, Field,
    Fields, FieldsNamed, GenericArgument, GenericParam, Lit, Meta, MetaNameValue, Path,
    PathArguments, PathSegment, Type, TypePath,
};

/////////
#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let token = parse_macro_input!(input as DeriveInput);
    // eprintln!("token: {:#?}", token);
    match generate(&token) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

struct TypePathVisitor {
    generic_type_names: Vec<String>,
    associated_types: HashMap<String, Vec<TypePath>>,
}

impl<'a> Visit<'a> for TypePathVisitor {
    fn visit_type_path(&mut self, node: &'a TypePath) {
        if node.path.segments.len() >= 2 {
            let generic_type_name = node.path.segments[0].ident.to_string();
            if self.generic_type_names.contains(&generic_type_name) {
                self.associated_types
                    .entry(generic_type_name)
                    .or_insert(Vec::new())
                    .push(node.clone());
            }
        }
        visit::visit_type_path(self, node);
    }
}

fn generate(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let ret = generate_debug_trait(input)?;
    Ok(ret)
}

fn get_fields_from_input(input: &DeriveInput) -> syn::Result<&Punctuated<Field, Comma>> {
    if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(input, "expected struct"))
}

fn generate_debug_trait(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let fields = get_fields_from_input(input)?;
    let struct_name = &input.ident;
    let mut generics = input.generics.clone();

    // 08 escape hatch
    if let Some(hatch) = get_struct_bound_attr(input) {
        generics.make_where_clause();
        generics
            .where_clause
            .as_mut()
            .unwrap()
            .predicates
            .push(parse_str(hatch.as_str()).unwrap());
    } else {
        // 05 phantom data
        let (mut field_type_names, mut phantom_type_names) = (Vec::new(), Vec::new());
        for field in fields {
            if let Some(field_type_name) = get_field_type_name(field) {
                field_type_names.push(field_type_name);
            }

            if let Some(phantom_type_name) = get_phantom_generic_type_name(field) {
                phantom_type_names.push(phantom_type_name);
            }
        }

        // 07 associated type
        let associted_types = get_generic_associated_types(input);
        // eprintln!("associted_types: {:#?}", associted_types);

        // 04 type parameter
        for g in generics.params.iter_mut() {
            if let GenericParam::Type(ref mut t) = g {
                let generic_type_name = t.ident.to_string();
                // eprintln!("generic_type_name: {}", generic_type_name);
                // 如果是 phantomdata 类型，就不需要为 T 添加约束，除非 T 本身也被直接使用
                if phantom_type_names.contains(&generic_type_name)
                    && !field_type_names.contains(&generic_type_name)
                {
                    continue;
                }

                // 如果是关联类型，就不要对 T 添加约束
                if associted_types.contains_key(&generic_type_name)
                    && !field_type_names.contains(&generic_type_name)
                {
                    continue;
                }

                t.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
        // 07 associated type: 为关联类型添加 where 子句
        generics.make_where_clause();
        for (_, associated_types) in associted_types {
            for associated_type in associated_types {
                generics
                    .where_clause
                    .as_mut()
                    .unwrap()
                    .predicates
                    .push(parse_quote!(
                        #associated_type: std::fmt::Debug
                    ));
            }
        }

        // eprintln!("generics: {:#?}", generics);
    }

    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    // quote!: 使用 Ident 类型不会带有双引号，使用 String 类型会带有双引号
    let struct_name_str = struct_name.to_string();

    let fmt_function_content = generate_fmt_body(fields.clone(), struct_name_str)?;

    let ret = quote!(
        impl #impl_generics std::fmt::Debug for #struct_name #type_generics #where_clause {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result{
                #fmt_function_content
            }
        }
    );

    Ok(ret)
}

// 02 impl debug
fn generate_fmt_body(
    fields: Punctuated<Field, Comma>,
    struct_name_str: String,
) -> syn::Result<TokenStream2> {
    let mut fmt_body = TokenStream2::new();
    fmt_body.extend(quote! {
        fmt.debug_struct(#struct_name_str)
    });

    for field in fields {
        let field_name = &field.ident.as_ref().unwrap();
        let field_name_str = field_name.to_string();

        let attr = &field.attrs;
        if attr.is_empty() {
            fmt_body.extend(quote!(
                .field(#field_name_str, &self.#field_name)
            ));
            continue;
        }

        // 03 custom format
        let debug_fmt_field = generate_field_attribute(attr.first().unwrap())?;
        fmt_body.extend(quote!(
            .field(#field_name_str, &format_args!(#debug_fmt_field, &self.#field_name))
        ));
    }

    fmt_body.extend(quote!(
        .finish()
    ));
    Ok(fmt_body)
}

// 03 custom format
fn generate_field_attribute(attr: &Attribute) -> syn::Result<String> {
    if let Attribute {
        meta: Meta::NameValue(MetaNameValue { path, value, .. }),
        ..
    } = attr
    {
        if path.is_ident("debug") {
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(lit), ..
            }) = value
            {
                let debug_fmt_field = lit.value();
                return Ok(debug_fmt_field);
            }
        }

        syn::Error::new_spanned(path, "expected `debug = \"...\"`");
    }
    Err(syn::Error::new_spanned(attr, "Unkonwn attribute"))
}

// 05 phantom data
fn get_phantom_generic_type_name(field: &Field) -> Option<String> {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = &field.ty
    {
        if let Some(PathSegment { ident, arguments }) = segments.last() {
            if ident == "PhantomData" {
                if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args, ..
                }) = arguments
                {
                    if let Some(GenericArgument::Type(Type::Path(TypePath {
                        path: Path { segments, .. },
                        ..
                    }))) = args.first()
                    {
                        if let Some(PathSegment { ident, .. }) = segments.last() {
                            return Some(ident.to_string());
                        }
                    }
                }
            }
        }
    }
    None
}

fn get_field_type_name(field: &Field) -> Option<String> {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = &field.ty
    {
        if let Some(PathSegment { ident, .. }) = segments.last() {
            return Some(ident.to_string());
        }
    }
    None
}

// 07 associated type
fn get_generic_associated_types(input: &DeriveInput) -> HashMap<String, Vec<TypePath>> {
    let origin_generic_param_names = input
        .generics
        .params
        .iter()
        .filter_map(|generic_ty| {
            if let GenericParam::Type(ty) = generic_ty {
                return Some(ty.ident.to_string());
            }
            None
        })
        .collect();
    let mut visitor = TypePathVisitor {
        generic_type_names: origin_generic_param_names,
        associated_types: HashMap::new(),
    };

    visitor.visit_derive_input(input);
    visitor.associated_types
}

// 08 escape hatch
fn get_struct_bound_attr(input: &DeriveInput) -> Option<String> {
    let attr = input.attrs.last()?;
    let parse_attr = attr.parse_args::<Meta>().unwrap();
    if let Meta::NameValue(MetaNameValue { path, value, .. }) = parse_attr {
        if path.is_ident("bound") {
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(lit), ..
            }) = value
            {
                return Some(lit.value());
            }
        }
    }
    None
}
