use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, token::Comma,
    AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Field, Fields, FieldsNamed,
    GenericArgument, Ident, Path, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder)]
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
            if let Some(inner_ty) = get_optional_inner_fields(&f.ty) {
                return inner_ty;
            }
            &f.ty
        })
        .collect::<Vec<_>>();

    let ret = quote! {
        #(#idents: std::option::Option<#types>), *
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
            quote! {
                #name: std::option::Option::None
            }
        })
        .collect::<Vec<_>>();
    Ok(init_fields)
}

// 03 setter function
fn generate_setter_function(input: &DeriveInput) -> syn::Result<TokenStream2> {
    let fields = get_fields(input)?;

    let idents = fields.iter().map(|f| &f.ident).collect::<Vec<_>>();
    let types = fields
        .iter()
        .map(|f| {
            // 06 optional：Option
            if let Some(inner_type) = get_optional_inner_fields(&f.ty) {
                return inner_type;
            }
            &f.ty
        })
        .collect::<Vec<_>>();

    let mut final_tokenstream = TokenStream2::new();

    for (ident, r#type) in idents.iter().zip(types.iter()) {
        let tokenstream_piece = quote! {
            fn #ident(&mut self, #ident: #r#type) -> &mut Self{
                self.#ident = std::option::Option::Some(#ident);
                self
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
        // 06 optional：Option new!
        if get_optional_inner_fields(types[idx]).is_none() {
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
        if get_optional_inner_fields(&fields[idx].ty).is_none() {
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
fn get_optional_inner_fields(ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        // 只有最后一个元素才会包含 Option
        if let Some(PathSegment { ident, arguments }) = segments.last() {
            if ident == "Option" {
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
