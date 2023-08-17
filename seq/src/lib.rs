use std::iter;

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Literal, TokenStream as TokenStream2, TokenTree};
use quote::quote;
use syn::{buffer, parse::Parse, Ident};
struct SeqParser {
    variable_ident: Ident,
    start: isize,
    end: isize,
    body: TokenStream2,
}

impl Parse for SeqParser {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let variable_ident: Ident = input.parse::<Ident>()?;
        input.parse::<syn::Token![in]>()?;
        let start = input.parse::<syn::LitInt>()?.base10_parse()?;
        input.parse::<syn::Token![..]>()?;

        let mut inc = false;
        if input.peek(syn::Token![=]) {
            input.parse::<syn::Token![=]>()?;
            inc = true;
        }

        let end = input.parse::<syn::LitInt>()?.base10_parse()?;

        let body_buf;
        syn::braced!(body_buf in input);
        let body = body_buf.parse::<TokenStream2>()?;

        let t = SeqParser {
            variable_ident,
            start,
            end: if inc { end + 1 } else { end },
            body,
        };

        Ok(t)
    }
}

impl SeqParser {
    fn generate(&self, ts: &TokenStream2, n: isize) -> TokenStream2 {
        let buf = ts.clone().into_iter().collect::<Vec<_>>();
        let (mut ret, mut idx) = (TokenStream2::new(), 0);

        while idx < buf.len() {
            let tree_node = &buf[idx];
            // eprintln!("tree_node: {:#?}", tree_node);
            match tree_node {
                TokenTree::Group(g) => {
                    // 如果是括号包含的内容，我们就要递归处理内部的TokenStream
                    let new_stream = self.generate(&g.stream(), n);
                    // 这里需要注意，上一行中g.stream()返回的是Group内部的TokenStream，
                    // 也就是说不包含括号本身，所以要在下面重新套上一层括号，而且括号的
                    // 种类要与原来保持一致。
                    let mut wrap_in_group = proc_macro2::Group::new(g.delimiter(), new_stream);

                    // 匹配特定的括号，而不是整个
                    wrap_in_group.set_span(g.span());
                    ret.extend(quote!(#wrap_in_group));
                }
                TokenTree::Ident(prefix) => {
                    if idx + 2 < buf.len() {
                        if let TokenTree::Punct(p) = &buf[idx + 1] {
                            if p.as_char() == '~' {
                                if let TokenTree::Ident(i) = &buf[idx + 2] {
                                    // 04-paste-ident.rs，N 前缀和匹配的 Ident 相同，则继续
                                    if i == &self.variable_ident
                                    // && prefix.span().end() == p.span().start()
                                    // && p.span().end() == i.span().start()
                                    {
                                        let new_ident_lit = format!("{}{}", prefix, n);
                                        let new_ident = Ident::new(&new_ident_lit, prefix.span());
                                        ret.extend(quote!(#new_ident));
                                        idx += 3;
                                        continue;
                                    }
                                }
                            }
                        }
                    }

                    // 如果是一个Ident，那么看一下是否为要替换的变量标识符，如果是则替换，
                    // 如果不是则透传。
                    if prefix == &self.variable_ident {
                        // 注意第二关的测试用例中，过程宏期待的是一个Literal，所以为了
                        // 通过，我们也要产生一个Literal
                        let new_ident = Literal::i64_unsuffixed(n as i64);
                        // 直接替换 N 到 n
                        ret.extend(quote!(#new_ident));
                        idx += 1;
                        continue;
                    } else {
                        ret.extend(iter::once(tree_node.clone()));
                    }
                }
                _ => {
                    // 对于其它的元素（也就是Punct和Literal），原封不动透传
                    ret.extend(iter::once(tree_node.clone()));
                }
            }
            idx += 1;
        }
        ret
    }
    fn find_block_to_expand_and_do_expand(
        &self,
        mut cursor: buffer::Cursor,
    ) -> (TokenStream2, bool) {
        let (mut found, mut ret) = (false, TokenStream2::new());
        while !cursor.eof() {
            if let Some((punct_prefix, cursor_1)) = cursor.punct() {
                if punct_prefix.as_char() == '#' {
                    if let Some((group_cur, _, cursor_2)) = cursor_1.group(Delimiter::Parenthesis) {
                        if let Some((punct_suffix, cursor_3)) = cursor_2.punct() {
                            if punct_suffix.as_char() == '*' {
                                for i in self.start..self.end {
                                    let t = self.generate(&group_cur.token_stream(), i);
                                    ret.extend(t);
                                }
                                cursor = cursor_3;
                                found = true;
                                continue;
                            }
                        }
                    }
                }
            }
            if let Some((group_cur, _, next_cur)) = cursor.group(proc_macro2::Delimiter::Brace) {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote::quote!({#t}));
                cursor = next_cur;
                continue;
            } else if let Some((group_cur, _, next_cur)) =
                cursor.group(proc_macro2::Delimiter::Bracket)
            {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote::quote!([#t]));
                cursor = next_cur;
                continue;
            } else if let Some((group_cur, _, next_cur)) =
                cursor.group(proc_macro2::Delimiter::Parenthesis)
            {
                let (t, f) = self.find_block_to_expand_and_do_expand(group_cur);
                found = f;
                ret.extend(quote::quote!((#t)));
                cursor = next_cur;
                continue;
            } else if let Some((punct, next_cur)) = cursor.punct() {
                ret.extend(quote::quote!(#punct));
                cursor = next_cur;
                continue;
            } else if let Some((ident, next_cur)) = cursor.ident() {
                ret.extend(quote::quote!(#ident));
                cursor = next_cur;
                continue;
            } else if let Some((literal, next_cur)) = cursor.literal() {
                ret.extend(quote::quote!(#literal));
                cursor = next_cur;
                continue;
            } else if let Some((lifetime, next_cur)) = cursor.lifetime() {
                // lifetime这种特殊的分类也是用cursor模式来处理的时候特有的，之前`proc_macro2::TokenTree`里面没有定义这个分类
                ret.extend(quote::quote!(#lifetime));
                cursor = next_cur;
                continue;
            }
        }

        (ret, found)
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let sp = syn::parse_macro_input!(input as SeqParser);

    let mut ret = TokenStream2::new();
    // 以下1行第五关新加，从TokenStream创建TokenBuffer
    let buffer = buffer::TokenBuffer::new2(sp.body.clone());

    // 以下4行第五关新加，首先尝试寻找`#(xxxxxxxxx)*`模式的代码块
    let (ret_1, expanded) = sp.find_block_to_expand_and_do_expand(buffer.begin());
    if expanded {
        return ret_1.into();
    }

    // 走到这里，说明`#(xxxxxxxxx)*`这个模式没有匹配到，那么重新使用上一关的方式，在整个代码块中尝试展开
    for i in sp.start..sp.end {
        ret.extend(sp.generate(&sp.body, i))
    }

    ret.into()
}
