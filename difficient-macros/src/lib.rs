extern crate proc_macro;

use std::str::FromStr;

use darling::{
    ast::{Data, Fields, Style},
    FromDeriveInput, FromField, FromMeta, FromVariant,
};
use heck::{ToKebabCase, ToLowerCamelCase, ToSnakeCase};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{DeriveInput, Generics, Ident};

#[derive(Debug, FromField)]
#[darling(attributes(diffable, serde), allow_unknown_fields)]
struct StructLike {
    ident: Option<syn::Ident>,
    ty: syn::Type,
    #[darling(default)]
    atomic: bool,
    #[darling(default)]
    skip: bool,
    rename: Option<String>,
}

#[derive(Debug, FromVariant)]
#[darling(attributes(serde), allow_unknown_fields)]
struct EnumData {
    ident: syn::Ident,
    fields: Fields<StructLike>,
    rename: Option<String>, // TODO
    rename_all: Option<String>,
}

/// Used for attributes on structs or enums
#[derive(FromMeta, Debug, Default)]
#[darling(allow_unknown_fields)]
struct ContainerSerdeAttrs {
    rename_all: Option<String>,
    tag: Option<String>,
    content: Option<String>,
    #[darling(default)]
    untagged: bool,
}

impl ContainerSerdeAttrs {
    fn variant_tag(&self) -> SerdeVariantTag {
        if let Some(tag) = self.tag.as_ref() {
            if let Some(c) = self.content.as_ref() {
                SerdeVariantTag::Adjacent {
                    tag: tag.clone(),
                    content: c.clone(),
                }
            } else {
                SerdeVariantTag::Internal { tag: tag.clone() }
            }
        } else if self.untagged {
            SerdeVariantTag::Untagged
        } else {
            SerdeVariantTag::External
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum SerdeVariantTag {
    External,
    Internal { tag: String },
    Adjacent { tag: String, content: String },
    Untagged,
}

enum SerdeRenameAllCase {
    Lower,
    Upper,
    Snake,
    Camel,
    Pascal,
    ScreamingSnake,
    Kebab,
    ScreamingKebab,
}

impl FromStr for SerdeRenameAllCase {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use SerdeRenameAllCase as S;
        let ok = match s {
            "lowercase" => S::Lower,
            "UPPERCASE" => S::Upper,
            "PascalCase" => S::Pascal,
            "camelCase" => S::Camel,
            "snake_case" => S::Snake,
            "SCREAMING_SNAKE_CASE" => S::ScreamingSnake,
            "kebab-case" => S::Kebab,
            "SCREAMING-KEBAB-CASE" => S::ScreamingKebab,
            other => return Err(format!("bad rename: {other}")),
        };
        Ok(ok)
    }
}

impl SerdeRenameAllCase {
    fn do_rename(&self, ident: &Ident) -> String {
        use SerdeRenameAllCase as S;
        match self {
            S::Snake => ident.to_string().to_snake_case(),
            S::Camel => ident.to_string().to_lower_camel_case(),
            S::Kebab => ident.to_string().to_kebab_case(),
            S::Lower => ident.to_string().to_lowercase(),
            S::Upper | S::Pascal | S::ScreamingSnake | S::ScreamingKebab => {
                todo!("Difficient does not support case: {{self:?}}")
            }
        }
    }
}

impl ToTokens for SerdeVariantTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let tok = match self {
            SerdeVariantTag::External => quote! { difficient::SerdeVariantTag::External },
            SerdeVariantTag::Internal { tag } => {
                quote! { difficient::SerdeVariantTag::Internal { tag: #tag.to_string() } }
            }
            SerdeVariantTag::Adjacent { tag, content } => {
                quote! { difficient::SerdeVariantTag::Adjacent {
                    tag: #tag.to_string(), content: #content.to_string()
                } }
            }
            SerdeVariantTag::Untagged => {
                quote! { difficient::SerdeVariantTag::Untagged }
            }
        };
        tokens.extend(tok);
    }
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(diffable, serde))]
struct DeriveDiffable {
    ident: syn::Ident,
    vis: syn::Visibility,
    data: Data<EnumData, StructLike>,
    generics: Generics,
    #[darling(default)]
    visit_transparent: bool,
    #[darling(default)]
    atomic: bool,
    #[darling(flatten)]
    serde: ContainerSerdeAttrs,
}

impl DeriveDiffable {
    fn derive(&self, serde_feature: bool, derive_visitor: bool) -> TokenStream {
        if !self.generics.params.is_empty() {
            panic!("derive(Diffable) does not support generic parameters")
        }

        let name = &self.ident;

        // short-circuit branch for top-level atomic
        if self.atomic {
            let has_any_skipped_fields = match &self.data {
                Data::Enum(variants) => variants.iter().any(|ed| ed.fields.iter().any(|f| f.skip)),
                Data::Struct(fields) => fields.iter().any(|f| f.skip),
            };
            if has_any_skipped_fields {
                panic!("cannot skip fields in atomic diff");
            }
            return quote! {
                impl<'a> difficient::Diffable<'a> for #name {
                    type Diff = difficient::AtomicDiff<'a, Self>;

                    fn diff(&self, other: &'a Self) -> Self::Diff {
                        Self::Diff::new(self, other)
                    }
                }
            };
        }

        let diff_ty = format_ident!("{}Diff", self.ident);
        let vis = &self.vis;
        let serde_derive = serde_feature.then(|| quote! { #[derive(serde::Serialize)] });
        let serde_container_rename_all: Option<SerdeRenameAllCase> = self
            .serde
            .rename_all
            .as_deref()
            .and_then(|s| s.parse::<SerdeRenameAllCase>().ok());

        match &self.data {
            Data::Enum(variants) => {
                let tag = self.serde.variant_tag();
                enum_impl(
                    variants,
                    tag,
                    name,
                    diff_ty,
                    vis,
                    serde_derive,
                    serde_container_rename_all,
                    derive_visitor,
                    self.visit_transparent,
                )
            }
            Data::Struct(fields) => {
                assert!(
                    !self.visit_transparent,
                    "'transparent' annotation only applies to enums"
                );
                struct_impl(
                    fields,
                    name,
                    diff_ty,
                    vis,
                    serde_derive,
                    serde_container_rename_all,
                )
            }
        }
    }
}

impl ToTokens for DeriveDiffable {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let serde_feature = cfg!(feature = "serde_impl");
        let derive_visitor = cfg!(feature = "visitor_impl");
        tokens.extend(self.derive(serde_feature, derive_visitor));
    }
}

#[allow(clippy::too_many_arguments)] // what nonsense
fn enum_impl(
    variants: &[EnumData],
    variant_tag: SerdeVariantTag,
    name: &Ident,
    diff_ty: Ident,
    vis: &syn::Visibility,
    serde_derive: Option<TokenStream>,
    serde_container_rename_all: Option<SerdeRenameAllCase>,
    derive_visitor: bool,
    transparent: bool,
) -> TokenStream {
    let var_name: Vec<&Ident> = variants.iter().map(|ed| &ed.ident).collect();
    let is_fieldless = variants.iter().all(|ed| ed.fields.is_empty());
    let lifetime = if is_fieldless {
        quote! {}
    } else {
        quote! { <'a> }
    };
    let var_diff_def = variants.iter().map(|var| {
        let ty: Vec<_> = var.fields.iter().map(|data| &data.ty).collect();
        let field_diff_ty: Vec<_> = var
            .fields
            .iter()
            .zip(ty.iter())
            // totally ignore fields that are annotated with `#[diffable(skip)]`
            .filter(|(f, _)| !f.skip)
            .map(|(f, ty)| {
                if f.atomic {
                    quote! {
                        difficient::AtomicDiff<'a, #ty>
                    }
                } else {
                    quote! {
                        <#ty as difficient::Diffable<'a>>::Diff
                    }
                }
            })
            .collect();
        match var.fields.style {
            Style::Unit => quote! {},
            Style::Tuple => {
                quote! {
                    (
                        #(  #field_diff_ty, )*
                    )
                }
            }
            Style::Struct => {
                let field = var
                    .fields
                    .iter()
                    .filter(|f| !f.skip)
                    .map(|data| &data.ident)
                    .collect::<Vec<_>>();
                quote! {
                    {
                        #( #field: #field_diff_ty, )*
                    }
                }
            }
        }
    });

    let enum_definition = quote! {
        #[derive(Debug, Clone, PartialEq)]
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
        #[allow(dead_code)]
        #[automatically_derived]
        #serde_derive
        #vis enum #diff_ty #lifetime {
            #(
                #var_name #var_diff_def,
            )*
        }
    };

    let variant_diff_impl = variants.iter().zip(var_name.iter()).map(|(var, var_name)| {
        let pattern_match_left = pattern_match(&var.fields, "left", true);
        let pattern_match_right = pattern_match(&var.fields, "right", true);
        let diff_impl = variant_diff_body(&diff_ty, var_name, &var.fields);
        quote! {
            (Self::#var_name #pattern_match_left, Self::#var_name #pattern_match_right)  => {
                #diff_impl
            }
        }
    });

    let diffable_impl = quote! {
        impl<'a> difficient::Diffable<'a> for #name {
            type Diff = difficient::DeepDiff<'a, Self, #diff_ty #lifetime>;

            #[allow(non_snake_case)]
            fn diff(&self, other: &'a Self) -> Self::Diff {
                use difficient::Replace as _;
                match (self, other) {
                    #(
                        #variant_diff_impl
                    ),*
                    _ => difficient::DeepDiff::Replaced(other)
                }
            }
        }
    };

    let apply_body = variants
        .iter()
        .zip(var_name.iter())
        .map(|(var, var_name)| {
            let pat_l = prefixed_idents(&var.fields, "left", false);
            let pat_r = prefixed_idents(&var.fields, "right", false);
            let pattern_match_left = pattern_match(&var.fields, "left", false);
            let pattern_match_right = pattern_match(&var.fields, "right", true);
            quote! {
                (Self::#var_name #pattern_match_left, #name::#var_name #pattern_match_right)  => {
                    #( #pat_l.apply_to_base(#pat_r, errs); )*
                }
            }
        })
        .collect::<Vec<_>>();

    let apply_impl = quote! {
        impl #lifetime difficient::Apply for #diff_ty #lifetime {
            type Parent = #name;
            fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<difficient::ApplyError>) {
                match (self, source) {
                    #( #apply_body )*
                    _ => errs.push(difficient::ApplyError::MismatchingEnum),
                }
            }
        }
    };

    let visit_enum_variant_impl = variants.iter().zip(var_name.iter()).map(|(var, var_name)| {
        let ident = get_idents(&var.fields);
        let num_non_skipped_fields = var.fields.iter().filter(|f| !f.skip).count();
        let serde_var_rename = var.rename.as_ref().map(|r| quote! { Some(#r) }).unwrap_or_else(||
            match serde_container_rename_all.as_ref().map(|r| r.do_rename(var_name)) {
            Some(r) => quote! { Some(#r) },
            None => quote! { None },
        });
        match var.fields.style {
            Style::Tuple => {
                if num_non_skipped_fields == 0 {
                    return quote! {
                        Self:: #var_name () => {}
                    }
                }
                let position = 0..(var.fields.len());
                let var_name_str = var_name.to_string();
                if transparent {
                    quote! {
                        Self:: #var_name ( #( #ident, )* ) => {
                            #(
                             if !#ident.is_unchanged() {
                                 #ident.accept(visitor);
                             }
                            )*
                        }
                    }
                } else {
                    quote! {
                        Self:: #var_name ( #( #ident, )* ) => {
                            visitor.enter(difficient::Enter::Variant{
                                name: #var_name_str, serde_rename: #serde_var_rename, serde_tag: #variant_tag
                            });
                            #(
                             if !#ident.is_unchanged() {
                                 visitor.enter(difficient::Enter::PositionalField(#position));
                                 #ident.accept(visitor);
                                 visitor.exit();
                             }
                            )*
                            visitor.exit();
                        }
                    }
                }
            }
            Style::Struct => {
                if num_non_skipped_fields == 0 {
                    return quote! {
                        Self:: #var_name {} => {}
                    }
                }
                let var_name_str = var_name.to_string();
                let ident_str = ident.iter().map(|i| i.to_string());
                let var_rename_all: Option<SerdeRenameAllCase> = var.rename_all.as_ref().map(|r| r.parse().unwrap());
                let serde_field_rename: Vec<_> = var.fields
                    .iter()
                    .map(|f| {
                        if let Some(n) = f.rename.as_ref() {
                            quote! { Some(#n) }
                        } else if let Some(r) = var_rename_all.as_ref() {
                            let re = r.do_rename(f.ident.as_ref().unwrap());
                            quote! { Some(#re) }
                        } else {
                            quote! { None }
                        }
                    })
                    .collect();
                quote! {
                    Self:: #var_name { #( #ident, )* } => {
                        visitor.enter(difficient::Enter::Variant{
                            name: #var_name_str, serde_rename: #serde_var_rename, serde_tag: #variant_tag
                        });
                        #(
                         if !#ident.is_unchanged() {
                             visitor.enter(difficient::Enter::NamedField {
                                 name: #ident_str, serde_rename: #serde_field_rename
                             });
                             #ident.accept(visitor);
                             visitor.exit();
                         }
                        )*
                        visitor.exit();
                    }
                }
            }
            Style::Unit => quote! {
                Self:: #var_name => {}
            }
        }
    });

    let visitor_impl = derive_visitor.then(|| {
        quote! {
            impl #lifetime difficient::AcceptVisitor for #diff_ty #lifetime {
                fn accept<V: difficient::Visitor>(&self, visitor: &mut V) {
                    use difficient::Replace as _;
                    match self {
                        #( #visit_enum_variant_impl ),*
                    }
                }
            }
        }
    });

    quote! {
        #enum_definition

        #diffable_impl

        #apply_impl

        #visitor_impl
    }
}

fn struct_impl(
    fields: &Fields<StructLike>,
    name: &Ident,
    diff_ty: Ident,
    vis: &syn::Visibility,
    serde_derive: Option<TokenStream>,
    serde_rename_all: Option<SerdeRenameAllCase>,
) -> TokenStream {
    let ty = fields.iter().map(|data| &data.ty).collect::<Vec<_>>();
    let num_skipped_fields = fields.iter().filter(|f| f.skip).count();
    let num_non_skipped_fields = fields.iter().filter(|f| !f.skip).count();
    if matches!(fields.style, Style::Unit) || num_non_skipped_fields == 0 {
        // short-circuit return
        return quote! {
            impl<'a> difficient::Diffable<'a> for #name {
                type Diff = difficient::Id<Self>;

                fn diff(&self, other: &'a Self) -> Self::Diff {
                    difficient::Id::new()
                }
            }
        };
    };

    let field = get_idents(fields);
    let field_diff_ty: Vec<_> = fields
        .iter()
        .zip(ty.iter())
        // totally ignore fields that are annotated with `#[diffable(skip)]`
        .filter(|(f, _)| !f.skip)
        .map(|(f, ty)| {
            if f.atomic {
                quote! {
                    difficient::AtomicDiff<'a, #ty>
                }
            } else {
                quote! {
                    <#ty as difficient::Diffable<'a>>::Diff
                }
            }
        })
        .collect();
    let source_accessor = get_accessors(fields, false);
    let diff_accessor = get_accessors(fields, true);
    let diff_ty_def = match fields.style {
        Style::Tuple => {
            quote! {
                #vis struct #diff_ty<'a>(
                    #(
                        #field_diff_ty,
                    )*
                );
            }
        }
        Style::Struct => {
            quote! {
                #vis struct #diff_ty<'a> {
                    #(
                        #field: #field_diff_ty,
                    )*
                }
            }
        }
        Style::Unit => unreachable!(),
    };
    let patch_ctor = match fields.style {
        Style::Tuple => quote! {
            #diff_ty( #( #field, )* )
        },
        Style::Struct => quote! {
            #diff_ty{ #( #field ),* }
        },
        Style::Unit => unreachable!(),
    };

    let field_diff_impl: Vec<_> = fields
        .iter()
        .zip(source_accessor.iter())
        .map(|(f, accessor)| {
            if f.atomic {
                quote! {
                    difficient::AtomicDiff::new(&self.#accessor, &other.#accessor)
                }
            } else {
                quote! {
                    self.#accessor.diff(&other.#accessor)
                }
            }
        })
        .collect();

    let (struct_diff_type, replaced_impl) = if num_skipped_fields > 0 {
        (
            quote! {
                difficient::PatchOnlyDiff<#diff_ty<'a>>
            },
            quote! {
                        Self::Diff::Patched(#patch_ctor)
            },
        )
    } else {
        (
            quote! {
                difficient::DeepDiff<'a, Self, #diff_ty<'a>>
            },
            quote! {
                        Self::Diff::Replaced(other)
            },
        )
    };

    let diffable_impl = quote! {
        impl<'a> difficient::Diffable<'a> for #name {
            type Diff = #struct_diff_type;

            #[allow(non_snake_case)]
            fn diff(&self, other: &'a Self) -> Self::Diff {
                use difficient::Replace as _;
                #(
                    let #field = #field_diff_impl;
                )*
                if #( #field.is_unchanged() && )* true {
                    Self::Diff::Unchanged
                } else if #( #field.is_replaced() && )* true {
                    #replaced_impl
                } else {
                    Self::Diff::Patched(#patch_ctor)
                }
            }
        }
    };

    let apply_impl = quote! {
        impl<'a> difficient::Apply for #diff_ty<'a> {
            type Parent = #name;
            #[allow(non_snake_case)]
            fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<difficient::ApplyError>) {
                #( self.#diff_accessor.apply_to_base(&mut source.#source_accessor, errs); )*
            }
        }
    };

    let struct_field_visit_impl = {
        let ident = get_idents(fields);
        match fields.style {
            Style::Tuple => {
                let position = 0..(fields.len());
                quote! {
                    let Self ( #( #ident, )* ) = self;
                    #(
                         if !#ident.is_unchanged() {
                             visitor.enter(difficient::Enter::PositionalField(#position));
                             #ident.accept(visitor);
                             visitor.exit();
                         }
                    )*

                }
            }
            Style::Struct => {
                let ident_str = ident.iter().map(|i| i.to_string());
                let serde_rename: Vec<_> = fields
                    .iter()
                    .filter(|f| !f.skip)
                    .map(|f| {
                        if let Some(n) = f.rename.as_ref() {
                            quote! { Some(#n) }
                        } else if let Some(r) = serde_rename_all.as_ref() {
                            let re = r.do_rename(f.ident.as_ref().unwrap());
                            quote! { Some(#re) }
                        } else {
                            quote! { None }
                        }
                    })
                    .collect();
                assert_eq!(ident_str.len(), serde_rename.len());
                quote! {
                    let Self { #( #ident, )* } = self;
                    #(
                         if !#ident.is_unchanged() {
                             visitor.enter(difficient::Enter::NamedField{
                                 name: #ident_str, serde_rename: #serde_rename
                             });
                             #ident.accept(visitor);
                             visitor.exit();
                         }
                    )*

                }
            }
            Style::Unit => quote! {},
        }
    };

    let visitor_impl = serde_derive.is_some().then(|| {
        quote! {
            impl<'a> difficient::AcceptVisitor for #diff_ty <'a> {
                fn accept<V: difficient::Visitor>(&self, visitor: &mut V) {
                    use difficient::Replace as _;
                    #struct_field_visit_impl
                }
            }
        }
    });

    quote! {
        #[derive(Debug, Clone, PartialEq)]
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
        #[allow(dead_code)]
        #[automatically_derived]
        #serde_derive
        #diff_ty_def

        #diffable_impl

        #apply_impl

        #visitor_impl
    }
}

fn variant_diff_body(
    diff_ty: &Ident,
    variant_name: &Ident,
    fields: &Fields<StructLike>,
) -> TokenStream {
    let num_non_skipped_fields = fields.iter().filter(|f| !f.skip).count();
    let ident = get_idents(fields);

    let patch_ctor = match fields.style {
        Style::Tuple => quote! {
            // round brackets
            #diff_ty::#variant_name(
                # ( #ident, )*
            )
        },
        Style::Struct => quote! {
            // curly brackets
            #diff_ty::#variant_name {
                # ( #ident, )*
            }
        },
        Style::Unit => quote! {},
    };

    if num_non_skipped_fields == 0 {
        return quote! {
            // no fields -> by definition they are unchanged
            difficient::DeepDiff::Unchanged
        };
    }

    match fields.style {
        Style::Unit => unreachable!(), // dealt with above, unit types have zero fields
        Style::Tuple | Style::Struct => {
            let left_ident = prefixed_idents(fields, "left", false);
            let right_ident = prefixed_idents(fields, "right", false);

            let field_diff_impl: Vec<_> = fields
                .iter()
                // totally ignore fields that are annotated with `#[diffable(skip)]`
                .filter(|f| !f.skip)
                .zip(ident.iter())
                .zip(left_ident.iter())
                .zip(right_ident.iter())
                .map(|(((f, ident), left), right)| {
                    if f.atomic {
                        quote! {
                            let #ident = difficient::AtomicDiff::new(#left, #right);
                        }
                    } else {
                        quote! {
                            let #ident = #left.diff(#right);
                        }
                    }
                })
                .collect();

            // We have a decision to make here. Suppose we have an enum where
            // the enum variant has *not* changed but every field of the variant has.
            // Should we send a Patch or a Replaced? Our reasoning is that replacing
            // the enum variant is a more destructive operation than necessary (the
            // variant and the fields of the variant are separate bits of data) and
            // therefore we decide that we should always send a Patch not a Replaced
            // in such cases

            quote! {
                #(
                    #field_diff_impl
                )*
                if #( #ident.is_unchanged() && )* true {
                    difficient::DeepDiff::Unchanged
                } else {
                    difficient::DeepDiff::Patched(#patch_ctor)
                }
            }
        }
    }
}

fn pattern_match(fields: &Fields<StructLike>, prefix: &str, include_skipped: bool) -> TokenStream {
    let pattern = prefixed_idents(fields, prefix, include_skipped);
    match fields.style {
        Style::Unit => quote! {},
        Style::Tuple => {
            quote! {
                (
                    #(  #pattern, )*
                )
            }
        }
        Style::Struct => {
            let id = fields
                .iter()
                .filter(|f| !f.skip || include_skipped)
                .map(|data| &data.ident)
                .collect::<Vec<_>>();
            quote! {
                {
                    #( #id: #pattern, )*
                }
            }
        }
    }
}

// tokens for field idents
fn get_idents(fields: &Fields<StructLike>) -> Vec<Ident> {
    fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !f.skip)
        .map(|(ix, struct_like)| {
            if let Some(field_name) = &struct_like.ident {
                field_name.clone()
            } else {
                // if the field has no name (tuple-like), fall back to 'f0', 'f1' etc
                format_ident!("f{ix}")
            }
        })
        .collect()
}

// as above but with a given prefix, useful for pattern matching
fn prefixed_idents(fields: &Fields<StructLike>, prefix: &str, include_skipped: bool) -> Vec<Ident> {
    fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !f.skip || include_skipped)
        .map(|(ix, struct_like)| {
            if struct_like.skip {
                format_ident!("_")
            } else if let Some(field_name) = &struct_like.ident {
                format_ident!("{prefix}_{field_name}")
            } else {
                format_ident!("{prefix}_{ix}")
            }
        })
        .collect()
}

// the tokens to access fields from parent struct ('self.my_field', or 'self.0')
// `monotonic_index` will force the indexes to increment monotonically even if
// some of the fields are skipped (relevant for tuple structs)
fn get_accessors(fields: &Fields<StructLike>, monotonic_index: bool) -> Vec<TokenStream> {
    let mut monotonic_count = monotonic_index.then_some(0);
    fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !f.skip)
        .map(|(ix, struct_like)| {
            if let Some(field_name) = &struct_like.ident {
                quote! { #field_name }
            } else {
                let ix = monotonic_count.unwrap_or(ix);
                monotonic_count = monotonic_count.map(|ix| ix + 1);
                let ix = syn::Index::from(ix);
                quote! { #ix }
            }
        })
        .collect()
}

#[proc_macro_derive(Diffable, attributes(diffable, serde))]
pub fn derive_diffable(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: DeriveInput = syn::parse(tokens).unwrap();
    let diff = DeriveDiffable::from_derive_input(&ast).unwrap();
    quote! { #diff }.into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_derive_struct() {
        let input = r#"
        #[derive(Diffable)]
        #[serde(rename_all = "camelCase", some_fake_field)]
        struct SimpleStruct {
            x: i32,
            #[serde(rename = "yyy")]
            y: String,
            #[diffable(atomic)]
            z_for_zelda: Vec<Fake>,
        }
        "#;

        let parsed = syn::parse_str(input).unwrap();
        let diff = DeriveDiffable::from_derive_input(&parsed).unwrap();

        let expect = quote! {
        #[derive(Debug, Clone, PartialEq)]
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
        #[allow(dead_code)]
        #[automatically_derived]
        #[derive(serde::Serialize)]
        struct SimpleStructDiff<'a> {
            x: <i32 as difficient::Diffable<'a>>::Diff,
            y: <String as difficient::Diffable<'a>>::Diff,
            z_for_zelda: difficient::AtomicDiff<'a, Vec<Fake>>,
        }
        impl<'a> difficient::Diffable<'a> for SimpleStruct {
            type Diff = difficient::DeepDiff<'a, Self, SimpleStructDiff<'a>>;
            #[allow(non_snake_case)]
            fn diff(&self, other: &'a Self) -> Self::Diff {
                use difficient::Replace as _;
                let x = self.x.diff(&other.x);
                let y = self.y.diff(&other.y);
                let z_for_zelda = difficient::AtomicDiff::new(&self.z_for_zelda, &other.z_for_zelda);
                if x.is_unchanged() && y.is_unchanged() && z_for_zelda.is_unchanged() && true {
                    Self::Diff::Unchanged
                } else if x.is_replaced() && y.is_replaced() && z_for_zelda.is_replaced() && true {
                    Self::Diff::Replaced(other)
                } else {
                    Self::Diff::Patched(SimpleStructDiff { x, y, z_for_zelda })
                }
            }
        }
        impl<'a> difficient::Apply for SimpleStructDiff<'a> {
            type Parent = SimpleStruct;
            #[allow(non_snake_case)]
            fn apply_to_base(
                &self,
                source: &mut Self::Parent,
                errs: &mut Vec<difficient::ApplyError>,
            ) {
                self.x.apply_to_base(&mut source.x, errs);
                self.y.apply_to_base(&mut source.y, errs);
                self.z_for_zelda.apply_to_base(&mut source.z_for_zelda, errs);
            }
        }
        impl<'a> difficient::AcceptVisitor for SimpleStructDiff<'a> {
            fn accept<V: difficient::Visitor>(&self, visitor: &mut V) {
                use difficient::Replace as _;
                let Self { x, y, z_for_zelda } = self;
                if !x.is_unchanged() {
                    visitor
                        .enter(difficient::Enter::NamedField {
                            name: "x",
                            serde_rename: Some("x"),
                        });
                    x.accept(visitor);
                    visitor.exit();
                }
                if !y.is_unchanged() {
                    visitor
                        .enter(difficient::Enter::NamedField {
                            name: "y",
                            serde_rename: Some("yyy"),
                        });
                    y.accept(visitor);
                    visitor.exit();
                }
                if !z_for_zelda.is_unchanged() {
                    visitor
                        .enter(difficient::Enter::NamedField {
                            name: "z_for_zelda",
                            serde_rename: Some("zForZelda"),
                        });
                    z_for_zelda.accept(visitor);
                    visitor.exit();
                }
            }
        }
        };

        let pretty = {
            let derived = diff.derive(true, true);
            let f: syn::File = syn::parse2(derived).unwrap();
            prettyplease::unparse(&f)
        };
        let expect = {
            let f: syn::File = syn::parse2(expect).unwrap();
            prettyplease::unparse(&f)
        };
        pretty_assertions::assert_eq!(pretty.to_string(), expect.to_string());
    }

    #[test]
    fn test_derive_skipped_field() {
        let input = r#"
        #[derive(Diffable)]
        struct SkipStruct {
            x: i32,
            #[diffable(skip)]
            y: String,
            z: u64,
        }
        "#;
        let parsed = syn::parse_str(input).unwrap();
        let diff = DeriveDiffable::from_derive_input(&parsed).unwrap();

        let expect = quote! {
        #[derive(Debug, Clone, PartialEq)]
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
        #[allow(dead_code)]
        #[automatically_derived]
        #[derive(serde::Serialize)]
        struct SkipStructDiff<'a> {
            x: <i32 as difficient::Diffable<'a>>::Diff,
            z: <u64 as difficient::Diffable<'a>>::Diff,
        }
        impl<'a> difficient::Diffable<'a> for SkipStruct {
            type Diff = difficient::PatchOnlyDiff<SkipStructDiff<'a>>;
            #[allow(non_snake_case)]
            fn diff(&self, other: &'a Self) -> Self::Diff {
                use difficient::Replace as _;
                let x = self.x.diff(&other.x);
                let z = self.z.diff(&other.z);
                if x.is_unchanged() && z.is_unchanged() && true {
                    Self::Diff::Unchanged
                } else if x.is_replaced() && z.is_replaced() && true {
                    Self::Diff::Patched(SkipStructDiff { x, z })
                } else {
                    Self::Diff::Patched(SkipStructDiff { x, z })
                }
            }
        }
        impl<'a> difficient::Apply for SkipStructDiff<'a> {
            type Parent = SkipStruct;
            #[allow(non_snake_case)]
            fn apply_to_base(
                &self,
                source: &mut Self::Parent,
                errs: &mut Vec<difficient::ApplyError>,
            ) {
                self.x.apply_to_base(&mut source.x, errs);
                self.z.apply_to_base(&mut source.z, errs);
            }
        }
        impl<'a> difficient::AcceptVisitor for SkipStructDiff<'a> {
            fn accept<V: difficient::Visitor>(&self, visitor: &mut V) {
                use difficient::Replace as _;
                let Self { x, z } = self;
                if !x.is_unchanged() {
                    visitor
                        .enter(difficient::Enter::NamedField {
                            name: "x",
                            serde_rename: None,
                        });
                    x.accept(visitor);
                    visitor.exit();
                }
                if !z.is_unchanged() {
                    visitor
                        .enter(difficient::Enter::NamedField {
                            name: "z",
                            serde_rename: None,
                        });
                    z.accept(visitor);
                    visitor.exit();
                }
            }
        }
        };

        let pretty = {
            let derived = diff.derive(true, true);
            let f: syn::File = syn::parse2(derived).unwrap();
            prettyplease::unparse(&f)
        };
        let expect = {
            let f: syn::File = syn::parse2(expect).unwrap();
            prettyplease::unparse(&f)
        };
        pretty_assertions::assert_eq!(pretty.to_string(), expect.to_string());
    }

    #[test]
    fn test_derive_tuple_struct() {
        let input = r#"
        #[derive(Diffable)]
        struct TupleStruct(i32, #[diffable(skip)] String, i64, #[diffable(skip)] F);
        "#;
        let parsed = syn::parse_str(input).unwrap();
        let diff = DeriveDiffable::from_derive_input(&parsed).unwrap();

        let expect = quote! {
            #[derive(Debug, Clone, PartialEq)]
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            #[allow(dead_code)]
            #[automatically_derived]
            #[derive(serde::Serialize)]
            struct TupleStructDiff<'a>(
                <i32 as difficient::Diffable<'a>>::Diff,
                <i64 as difficient::Diffable<'a>>::Diff,
            );
            impl<'a> difficient::Diffable<'a> for TupleStruct {
                type Diff = difficient::PatchOnlyDiff<TupleStructDiff<'a>>;
                #[allow(non_snake_case)]
                fn diff(&self, other: &'a Self) -> Self::Diff {
                    use difficient::Replace as _;
                    let f0 = self.0.diff(&other.0);
                    let f2 = self.2.diff(&other.2);
                    if f0.is_unchanged() && f2.is_unchanged() && true {
                        Self::Diff::Unchanged
                    } else if f0.is_replaced() && f2.is_replaced() && true {
                        Self::Diff::Patched(TupleStructDiff(f0, f2))
                    } else {
                        Self::Diff::Patched(TupleStructDiff(f0, f2))
                    }
                }
            }
            impl<'a> difficient::Apply for TupleStructDiff<'a> {
                type Parent = TupleStruct;
                #[allow(non_snake_case)]
                fn apply_to_base(
                    &self,
                    source: &mut Self::Parent,
                    errs: &mut Vec<difficient::ApplyError>,
                ) {
                    self.0.apply_to_base(&mut source.0, errs);
                    self.1.apply_to_base(&mut source.2, errs);
                }
            }
            impl<'a> difficient::AcceptVisitor for TupleStructDiff<'a> {
                fn accept<V: difficient::Visitor>(&self, visitor: &mut V) {
                    use difficient::Replace as _;
                    let Self(f0, f2) = self;
                    if !f0.is_unchanged() {
                        visitor.enter(difficient::Enter::PositionalField(0usize));
                        f0.accept(visitor);
                        visitor.exit();
                    }
                    if !f2.is_unchanged() {
                        visitor.enter(difficient::Enter::PositionalField(1usize));
                        f2.accept(visitor);
                        visitor.exit();
                    }
                }
            }
        };
        let pretty = {
            let derived = diff.derive(true, true);
            let f: syn::File = syn::parse2(derived).unwrap();
            prettyplease::unparse(&f)
        };
        let expect = {
            let f: syn::File = syn::parse2(expect).unwrap();
            prettyplease::unparse(&f)
        };
        pretty_assertions::assert_eq!(pretty.to_string(), expect.to_string());
    }

    #[test]
    fn test_derive_enum() {
        let input = r#"
        #[derive(Diffable)]
        #[serde(tag = "my_tag", rename_all = "kebab-case")]
        enum SimpleEnum {
            First,
            #[serde(rename = "SecondTheBest")]
            Second(i32, String),
            #[serde(rename_all = "camelCase")]
            ThirdThing {
                #[serde(rename = "x-the-unknown")]
                x: i32,
                #[diffable(atomic)]
                y_y: Vec<Fake>,
            }
        }
        "#;

        let parsed = syn::parse_str(input).unwrap();
        let diff = DeriveDiffable::from_derive_input(&parsed).unwrap();

        let expect = quote! {
        #[derive(Debug, Clone, PartialEq)]
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
        #[allow(dead_code)]
        #[automatically_derived]
        #[derive(serde::Serialize)]
        enum SimpleEnumDiff<'a> {
            First,
            Second(
                <i32 as difficient::Diffable<'a>>::Diff,
                <String as difficient::Diffable<'a>>::Diff,
            ),
            ThirdThing {
                x: <i32 as difficient::Diffable<'a>>::Diff,
                y_y: difficient::AtomicDiff<'a, Vec<Fake>>,
            },
        }
        impl<'a> difficient::Diffable<'a> for SimpleEnum {
            type Diff = difficient::DeepDiff<'a, Self, SimpleEnumDiff<'a>>;
            #[allow(non_snake_case)]
            fn diff(&self, other: &'a Self) -> Self::Diff {
                use difficient::Replace as _;
                match (self, other) {
                    (Self::First, Self::First) => difficient::DeepDiff::Unchanged,
                    (Self::Second(left_0, left_1), Self::Second(right_0, right_1)) => {
                        let f0 = left_0.diff(right_0);
                        let f1 = left_1.diff(right_1);
                        if f0.is_unchanged() && f1.is_unchanged() && true {
                            difficient::DeepDiff::Unchanged
                        } else {
                            difficient::DeepDiff::Patched(SimpleEnumDiff::Second(f0, f1))
                        }
                    }
                    (
                        Self::ThirdThing {
                            x: left_x,
                            y_y: left_y_y,
                        },
                        Self::ThirdThing {
                            x: right_x,
                            y_y: right_y_y,
                        },
                    ) => {
                        let x = left_x.diff(right_x);
                        let y_y = difficient::AtomicDiff::new(left_y_y, right_y_y);
                        if x.is_unchanged() && y_y.is_unchanged() && true {
                            difficient::DeepDiff::Unchanged
                        } else {
                            difficient::DeepDiff::Patched(SimpleEnumDiff::ThirdThing { x, y_y })
                        }
                    }
                    _ => difficient::DeepDiff::Replaced(other),
                }
            }
        }
        impl<'a> difficient::Apply for SimpleEnumDiff<'a> {
            type Parent = SimpleEnum;
            fn apply_to_base(
                &self,
                source: &mut Self::Parent,
                errs: &mut Vec<difficient::ApplyError>,
            ) {
                match (self, source) {
                    (Self::First, SimpleEnum::First) => {}
                    (Self::Second(left_0, left_1), SimpleEnum::Second(right_0, right_1)) => {
                        left_0.apply_to_base(right_0, errs);
                        left_1.apply_to_base(right_1, errs);
                    }
                    (
                        Self::ThirdThing {
                            x: left_x,
                            y_y: left_y_y,
                        },
                        SimpleEnum::ThirdThing {
                            x: right_x,
                            y_y: right_y_y,
                        },
                    ) => {
                        left_x.apply_to_base(right_x, errs);
                        left_y_y.apply_to_base(right_y_y, errs);
                    }
                    _ => errs.push(difficient::ApplyError::MismatchingEnum),
                }
            }
        }
        impl<'a> difficient::AcceptVisitor for SimpleEnumDiff<'a> {
            fn accept<V: difficient::Visitor>(&self, visitor: &mut V) {
                use difficient::Replace as _;
                match self {
                    Self::First => {}
                    Self::Second(f0, f1) => {
                        visitor
                            .enter(difficient::Enter::Variant {
                                name: "Second",
                                serde_rename: Some("SecondTheBest"),
                                serde_tag: difficient::SerdeVariantTag::Internal {
                                    tag: "my_tag".to_string()
                                },
                            });
                        if !f0.is_unchanged() {
                            visitor.enter(difficient::Enter::PositionalField(0usize));
                            f0.accept(visitor);
                            visitor.exit();
                        }
                        if !f1.is_unchanged() {
                            visitor.enter(difficient::Enter::PositionalField(1usize));
                            f1.accept(visitor);
                            visitor.exit();
                        }
                        visitor.exit();
                    }
                    Self::ThirdThing { x, y_y } => {
                        visitor
                            .enter(difficient::Enter::Variant {
                                name: "ThirdThing",
                                serde_rename: Some("third-thing"),
                                serde_tag: difficient::SerdeVariantTag::Internal {
                                    tag: "my_tag".to_string()
                                },
                            });
                        if !x.is_unchanged() {
                            visitor
                                .enter(difficient::Enter::NamedField {
                                    name: "x",
                                    serde_rename: Some("x-the-unknown"),
                                });
                            x.accept(visitor);
                            visitor.exit();
                        }
                        if !y_y.is_unchanged() {
                            visitor
                                .enter(difficient::Enter::NamedField {
                                    name: "y_y",
                                    serde_rename: Some("yY"),
                                });
                            y_y.accept(visitor);
                            visitor.exit();
                        }
                        visitor.exit();
                    }
                }
            }
        }
        };

        let pretty = {
            let derived = diff.derive(true, true);
            let f: syn::File = syn::parse2(derived).unwrap();
            prettyplease::unparse(&f)
        };
        let expect = {
            let f: syn::File = syn::parse2(expect).unwrap();
            prettyplease::unparse(&f)
        };
        pretty_assertions::assert_eq!(pretty.to_string(), expect.to_string());
    }

    #[test]
    fn test_derive_skippable_enum() {
        let input = r#"
        #[derive(Diffable)]
        enum SkipEnum {
            First(#[diffable(skip)] i32, u64),
            Second {
                x: i32,
                #[diffable(skip)]
                y: i32,
                z: String
            }
        }
        "#;

        let parsed = syn::parse_str(input).unwrap();
        let diff = DeriveDiffable::from_derive_input(&parsed).unwrap();

        let expect = quote! {
        #[derive(Debug, Clone, PartialEq)]
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
        #[allow(dead_code)]
        #[automatically_derived]
        #[derive(serde::Serialize)]
        enum SkipEnumDiff<'a> {
            First(<u64 as difficient::Diffable<'a>>::Diff),
            Second {
                x: <i32 as difficient::Diffable<'a>>::Diff,
                z: <String as difficient::Diffable<'a>>::Diff,
            },
        }
        impl<'a> difficient::Diffable<'a> for SkipEnum {
            type Diff = difficient::DeepDiff<'a, Self, SkipEnumDiff<'a>>;
            #[allow(non_snake_case)]
            fn diff(&self, other: &'a Self) -> Self::Diff {
                use difficient::Replace as _;
                match (self, other) {
                    (Self::First(_, left_1), Self::First(_, right_1)) => {
                        let f1 = left_1.diff(right_1);
                        if f1.is_unchanged() && true {
                            difficient::DeepDiff::Unchanged
                        } else {
                            difficient::DeepDiff::Patched(SkipEnumDiff::First(f1))
                        }
                    }
                    (
                        Self::Second {
                            x: left_x, y: _, z: left_z
                        },
                        Self::Second {
                            x: right_x, y: _, z: right_z
                        },
                    ) => {
                        let x = left_x.diff(right_x);
                        let z = left_z.diff(right_z);
                        if x.is_unchanged() && z.is_unchanged() && true {
                            difficient::DeepDiff::Unchanged
                        } else {
                            difficient::DeepDiff::Patched(SkipEnumDiff::Second { x, z })
                        }
                    }
                    _ => difficient::DeepDiff::Replaced(other),
                }
            }
        }
        impl<'a> difficient::Apply for SkipEnumDiff<'a> {
            type Parent = SkipEnum;
            fn apply_to_base(
                &self,
                source: &mut Self::Parent,
                errs: &mut Vec<difficient::ApplyError>,
            ) {
                match (self, source) {
                    (Self::First(left_1), SkipEnum::First(_, right_1)) => {
                        left_1.apply_to_base(right_1, errs);
                    }
                    (
                        Self::Second {
                            x: left_x,
                            z: left_z,
                        },
                        SkipEnum::Second {
                            x: right_x,
                            y: _,
                            z: right_z
                        },
                    ) => {
                        left_x.apply_to_base(right_x, errs);
                        left_z.apply_to_base(right_z, errs);
                    }
                    _ => errs.push(difficient::ApplyError::MismatchingEnum),
                }
            }
        }
        impl<'a> difficient::AcceptVisitor for SkipEnumDiff<'a> {
            fn accept<V: difficient::Visitor>(&self, visitor: &mut V) {
                use difficient::Replace as _;
                match self {
                    Self::First(f1) => {
                        visitor
                            .enter(difficient::Enter::Variant {
                                name: "First",
                                serde_rename: None,
                                serde_tag: difficient::SerdeVariantTag::External,
                            });
                        if !f1.is_unchanged() {
                            visitor.enter(difficient::Enter::PositionalField(0usize));
                            f1.accept(visitor);
                            visitor.exit();
                        }
                        visitor.exit();
                    }
                    Self::Second{ x, z } => {
                        visitor
                            .enter(difficient::Enter::Variant {
                                name: "Second",
                                serde_rename: None,
                                serde_tag: difficient::SerdeVariantTag::External
                            });
                        if !x.is_unchanged() {
                            visitor.enter(difficient::Enter::NamedField{
                                name: "x",
                                serde_rename: None
                            });
                            x.accept(visitor);
                            visitor.exit();
                        }
                        if !z.is_unchanged() {
                            visitor.enter(difficient::Enter::NamedField{
                                name: "z",
                                serde_rename: None
                            });
                            z.accept(visitor);
                            visitor.exit();
                        }
                        visitor.exit();
                    }
                }
            }
        }
        };

        let pretty = {
            let derived = diff.derive(true, true);
            let f: syn::File = syn::parse2(derived).unwrap();
            prettyplease::unparse(&f)
        };
        let expect = {
            let f: syn::File = syn::parse2(expect).unwrap();
            prettyplease::unparse(&f)
        };
        pretty_assertions::assert_eq!(pretty.to_string(), expect.to_string());
    }

    #[test]
    fn test_derive_atomic() {
        let input = r#"
        #[derive(Diffable)]
        #[diffable(atomic)]
        enum Atomic {
            First(X),
            Second { x: Y, y: Z }
        }
        "#;

        let parsed = syn::parse_str(input).unwrap();
        let diff = DeriveDiffable::from_derive_input(&parsed).unwrap();

        let expect = quote! {
            impl<'a> difficient::Diffable<'a> for Atomic {
                type Diff = difficient::AtomicDiff<'a, Self>;

                fn diff(&self, other: &'a Self) -> Self::Diff {
                    Self::Diff::new(self, other)
                }
            }
        };
        let pretty = {
            let derived = diff.derive(true, true);
            let f: syn::File = syn::parse2(derived).unwrap();
            prettyplease::unparse(&f)
        };
        let expect = {
            let f: syn::File = syn::parse2(expect).unwrap();
            prettyplease::unparse(&f)
        };
        pretty_assertions::assert_eq!(pretty.to_string(), expect.to_string());
    }
}
