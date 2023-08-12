use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Comma,
    AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Expr, ExprLit, Field, Fields,
    FieldsNamed, GenericArgument, Ident, Lit, Meta, MetaNameValue, Path, PathArguments,
    PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match generate(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn get_fields(input: &DeriveInput) -> syn::Result<&Punctuated<Field, Comma>> {
    if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = input.data
    {
        // 返回 named 字段类型
        return Ok(named);
    }
    Err(syn::Error::new(
        input.span(),
        "Must be a struct".to_string(),
    ))
}

// 02 create builder
fn geterate_builder_struct_field(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let fields = get_fields(input)?;

    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let types = fields
        .iter()
        .map(|f| {
            // 06 optional：Option
            if let Some(inner_ty) = get_generic_inner_field(&f.ty, "Option") {
                return Ok(quote!(std::option::Option<#inner_ty>));
            }
            // 07 repeated field, 拥有 each 属性，那么就不使用 Option 包裹
            if get_each_attr(f)?.is_some() {
                let with_each_field = &f.ty;
                return Ok(quote!(#with_each_field));
            }
            let no_option_ty = &f.ty;
            Ok(quote!(std::option::Option<#no_option_ty>))
        })
        .collect::<syn::Result<Vec<_>>>();

    let types = types?;
    let ret = quote! {
        #(#idents: #types), *
    };

    // 新结构体字段类型
    Ok(ret)
}

fn generate_builder_impl_factory(input: &DeriveInput) -> syn::Result<Vec<TokenStream2>> {
    let fields = get_fields(input)?;

    let init_fields = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            // 07 repeated field，拥有 each 属性的工厂函数，返回一个空的 Vec
            if get_each_attr(f)?.is_some() {
                return Ok(quote! {
                    #name: std::vec::Vec::new()
                });
            }
            Ok(quote! {
                #name: std::option::Option::None
            })
        })
        .collect::<syn::Result<Vec<_>>>();
    init_fields
}

// 03 setter function
fn generate_setter_function(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let fields = get_fields(input)?;

    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let types = fields
        .iter()
        .map(|f| {
            // 06 optional：Option
            if let Some(inner_type) = get_generic_inner_field(&f.ty, "Option") {
                return inner_type;
            }
            &f.ty
        })
        .collect::<Vec<_>>();

    let mut final_tokenstream = TokenStream2::new();

    for (index, (ident, r#type)) in idents.iter().zip(types.iter()).enumerate() {
        let tokenstream_piece = if let Some(ref user_specified_ident) =
            get_each_attr(&fields[index])?
        {
            let inner_type = get_generic_inner_field(r#type, "Vec")
                .ok_or(syn::Error::new(fields[index].span(), "Must be a Vec"))?;
            // eprintln!(
            //     "{:#?} == {:#?}: {}",
            //     user_specified_ident,
            //     ident.as_ref().unwrap(),
            //     user_specified_ident == ident.as_ref().unwrap()
            // );

            let mut tokenstream_piece = quote! {
                fn #user_specified_ident(&mut self, #user_specified_ident: #inner_type) -> &mut Self{
                    self.#ident.push(#user_specified_ident);
                    self
                }
            };

            // 07 repeated field：如果用户指定的字段不同，那么 args 指定的参数将替换原有的内容
            if user_specified_ident != ident.as_ref().unwrap() {
                tokenstream_piece.extend(quote! {
                    fn #ident(&mut self, #ident: #r#type) -> &mut Self{
                        self.#ident = #ident.clone();
                        self
                    }
                })
            }
            tokenstream_piece
        } else {
            quote! {
                fn #ident(&mut self, #ident: #r#type) -> &mut Self{
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        };

        final_tokenstream.extend(tokenstream_piece);
    }
    Ok(final_tokenstream)
}

// 04 call build
fn generate_call_build(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let fields = get_fields(input)?;
    let types = fields.iter().map(|f| &f.ty).collect::<Vec<_>>();

    let mut checker_code = vec![];

    #[allow(unused_doc_comments)]
    /**
     * //生成一个 build 函数
     * pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
     *
     *     if self.execuable.is_none() {
     *         let err = format!("{} field is missing", "execuable");
     *         return Err(err);
     *     }
     *     /// ...
     *    Ok(Command {
     *        execuable: self.execuable.clone().unwrap(),
     *        /// ...
     *    })
     * }
     */
    // 检查是否有字段未赋值
    for idx in 0..fields.len() {
        // 06 optional：Option new! && 07 repeated field 排除 each 字段的检查
        if get_generic_inner_field(types[idx], "Option").is_none()
            && get_each_attr(&fields[idx])?.is_none()
        {
            let ident = &fields[idx].ident;
            checker_code.push(quote! {
                if self.#ident.is_none() {
                    let err = format!("{} field is missing", stringify!(#ident));
                    return std::result::Result::Err(err.into());
                }
            })
        }
    }

    // 生成 Command 结构体
    let mut fill_command = vec![];
    for idx in 0..fields.len() {
        let ident = &fields[idx].ident;
        // 06 optional：Option，只有当字段类型不是 Option 时，才调用 unwrap
        if get_each_attr(&fields[idx])?.is_some() {
            fill_command.push(quote! {
                #ident: self.#ident.clone()
            })
        } else if get_generic_inner_field(&fields[idx].ty, "Option").is_none() {
            fill_command.push(quote! {
                #ident: self.#ident.clone().unwrap()
            })
        } else {
            fill_command.push(quote! {
                #ident: self.#ident.clone()
            })
        }
    }

    let struct_field = &input.ident;

    let token_stream = quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_field, std::boxed::Box<dyn std::error::Error>> {
            #(#checker_code)*

            let ret = #struct_field {
                #( #fill_command ), *
            };
            std::result::Result::Ok(ret)
        }
    };

    Ok(token_stream)
}

// 06 otpional field
/// other_ident_name: 获取某个包裹字段中的 T（Option<T>）
fn get_generic_inner_field<'a>(ty: &'a Type, other_ident_name: &str) -> Option<&'a Type> {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        // 只有最后一个元素才会包含 Option
        if let Some(PathSegment { ident, arguments }) = segments.last() {
            if ident == other_ident_name {
                // Option<T> 中的 T 只有一个
                if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args, ..
                }) = arguments
                {
                    if let Some(GenericArgument::Type(inner_type)) = args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }
    None
}

// 07 repeated field（获取 each 属性）
fn get_each_attr(field: &Field) -> syn::Result<Option<Ident>> {
    for attr in &field.attrs {
        if attr.path().is_ident("builder") {
            // 解析该类型：#[builder(each = "...")]
            let meta = attr.parse_args::<Meta>().unwrap();
            if let Meta::NameValue(MetaNameValue {
                path: Path { segments, .. },
                value,
                ..
            }) = meta
            {
                if let Some(PathSegment { ident, .. }) = segments.first() {
                    if ident == "each" {
                        if let Expr::Lit(ExprLit {
                            lit: Lit::Str(lit_str),
                            ..
                        }) = value
                        {
                            return Ok(Some(Ident::new(lit_str.value().as_str(), attr.span())));
                        }
                    }

                    if ident != "each" {
                        // eprintln!("list: {:#?}", list);
                        return Err(syn::Error::new_spanned(
                            attr,
                            r#"expected `builder(each = "...")`"#,
                        ));
                    }
                }
            }
        }
    }
    Ok(None)
}

fn generate(input: &DeriveInput) -> syn::Result<TokenStream2> {
    // 原结构体字段类型
    let struct_ident = &input.ident;

    let struct_name_literal = struct_ident.to_string();
    let builder_name_literal = format!("{}Builder", struct_name_literal);

    // 新结构体字段名称
    let builder_name_ident = Ident::new(&builder_name_literal, input.span());

    // 结构体内部字段
    let builder_struct_fields = geterate_builder_struct_field(input)?;

    //  builder 初始化字段
    let builder_impl_init_fields = generate_builder_impl_factory(input)?;

    // setter 函数
    let builder_setter_functions = generate_setter_function(input)?;

    // 调用 build
    let generate_build_function = generate_call_build(input)?;

    let ret = quote::quote!(
        pub struct #builder_name_ident{
            #builder_struct_fields
        }
        impl #struct_ident {
            pub fn builder() -> #builder_name_ident {
                #builder_name_ident{
                    #(#builder_impl_init_fields),*
                }
            }
        }

        impl #builder_name_ident {
            #builder_setter_functions

            #generate_build_function
        }
    );
    Ok(ret)
}

// #[proc_macro_derive(Explore, attributes(Foo))]
// pub fn explore(input: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(input as DeriveInput);
//     let attr = input.attrs.first().unwrap();
//     // 解析该类型：#[Foo::Bar(test = "test", handle1 = "handle1")]
//     eprintln!("{:#?}", attr);
//     TokenStream2::new().into()
// }
