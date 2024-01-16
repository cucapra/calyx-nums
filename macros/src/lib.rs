use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident};

#[proc_macro_derive(Mangle)]
pub fn derive_mangle(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let buffer = format_ident!("w");

    let source_name = source_name(name);
    let (code, statements) = mangle_data(&buffer, &input.data);

    let expanded = quote! {
        impl Mangle for #name {
            fn mangle<W>(&self, #buffer: &mut W) -> ::std::fmt::Result
            where
                W: ::std::fmt::Write,
            {
                #buffer.write_str(concat!(#code, #source_name))?;
                #statements
                #buffer.write_char('E')
            }
        }
    };

    TokenStream::from(expanded)
}

/// Computes a [`<source-name>`] given an [`<identifier>`].
///
/// [`<identifier>`]:
///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.identifier
/// [`<source-name>`]:
///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.source-name
fn source_name(ident: &Ident) -> String {
    let ident = ident.to_string();

    format!("{}{}", ident.len(), ident)
}

/// Generates a sequence of statements which format the given data. Also returns
/// the code indicating the kind of the data.
///
/// The statements emit the following productions:
///
/// - A `struct` is formatted as a sequence of [`<expression>`].
/// - An `enum` is formatted as a single [`<number>`].
///
/// [`<expression>`]:
///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.expression
/// [`<number>`]:
///     https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangle.number
fn mangle_data(buffer: &Ident, data: &Data) -> (&'static str, TokenStream2) {
    match data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => {
                let calls = fields.named.iter().map(|field| {
                    let name = &field.ident;
                    let span = field.span();

                    quote_spanned! {span=>
                        Mangle::mangle(&self.#name, #buffer)
                    }
                });

                let statements = quote! { #( #calls?; )* };

                ("tl", statements)
            }
            _ => unimplemented!(),
        },
        Data::Enum(_) => {
            let statements = quote! {
                write!(#buffer, "{}", *self as usize)?;
            };

            ("L", statements)
        }
        _ => unimplemented!(),
    }
}
