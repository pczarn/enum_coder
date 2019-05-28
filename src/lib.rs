#![feature(proc_macro_def_site, bind_by_move_pattern_guards)]

extern crate proc_macro;
extern crate syn;
extern crate quote;

// #[macro_use]
// extern crate log;
// extern crate env_logger;

use std::collections::{HashMap, HashSet};
use std::mem;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    ItemEnum, ItemFn, Ident, Result, Meta, Fields,
    FieldsNamed, Stmt, Member, NestedMeta, Expr, FieldValue,
    ExprPath, ExprStruct, Path,
    parse_quote,    
};
use syn::parse::{Parse, ParseStream};
use syn::fold::{Fold, fold_stmt};
use syn::export::Span;

struct InstructionDefinition {
    definition: ItemEnum,
    fns: Vec<ItemFn>,
}

struct FoldContext {
    instruction_name: Ident,
    variants: HashMap<Ident, Option<FieldsNamed>>,
    missing_field_fn_name: Option<Ident>,
    missing_field_args: Vec<Ident>,
    vec_name: Ident,
}

enum ProcessFn {
    MissingField(ItemFn, Vec<Ident>),
    GenerateList(ItemFn),
    NormalFn(ItemFn),
}

impl InstructionDefinition {
    fn variants(&mut self) -> HashMap<Ident, Option<FieldsNamed>> {
        let mut variants = HashMap::new();
        for variant in &self.definition.variants {
            let fields_named = if let Fields::Named(fields_named) = variant.fields.clone() {
                Some(fields_named)
            } else {
                None
            };
            variants.insert(variant.ident.clone(), fields_named);
        }
        variants
    }

    fn process_fns(&mut self) {
        fn extract_attr(item: &mut ItemFn, needle_name: &str) -> Option<Vec<Ident>> {
            let mut result = None;
            item.attrs.retain(|attr| {
                // search the haystack
                match attr.parse_meta() {
                    Ok(Meta::Word(name)) if name.to_string() == needle_name => {
                        assert!(result.is_none());
                        result = Some(vec![]);
                        false
                    }
                    Ok(Meta::List(list)) if list.ident.to_string() == needle_name => {
                        assert!(result.is_none());
                        result = Some(list.nested.into_iter().filter_map(|nested_meta| {
                            if let NestedMeta::Meta(Meta::Word(arg)) = nested_meta {
                                Some(arg)
                            } else {
                                None
                            }
                        }).collect());
                        false
                    }
                    _ => true
                }
            });
            result
        }


        fn process_fn(mut item: ItemFn) -> ProcessFn {
            let is_missing_field = extract_attr(&mut item, "missing_field");
            let does_generate_list = extract_attr(&mut item, "generate_list");
            match (is_missing_field, does_generate_list) {
                (Some(..), Some(..)) => unimplemented!(),
                (Some(missing_field_args), None) => {
                    ProcessFn::MissingField(item, missing_field_args)
                }
                (None, Some(generate_list)) => {
                    assert!(generate_list.is_empty(), "expected no arguments in an attribute");
                    ProcessFn::GenerateList(item)
                }
                (None, None) => {
                    ProcessFn::NormalFn(item)
                }
            }
        }

        let mut context = FoldContext {
            instruction_name: self.definition.ident.clone(),
            variants: self.variants(),
            missing_field_fn_name: None,
            missing_field_args: vec![],
            vec_name: Ident::new("list", Span::call_site()),
        };
        let fns = mem::replace(&mut self.fns, vec![]);
        let processed_fns: Vec<_> = fns.into_iter().map(process_fn).collect();
        for elem in &processed_fns {
            match elem {
                &ProcessFn::MissingField(ref item, ref missing_field_args) => {
                    context.missing_field_fn_name = Some(item.ident.clone());
                    context.missing_field_args = missing_field_args.clone();
                }
                _ => {}
            }
        }
        self.fns = processed_fns.into_iter().map(|elem| {
            match elem {
                ProcessFn::MissingField(item, _) => item,
                ProcessFn::GenerateList(item) => context.transform_generate_list_fn(item),
                ProcessFn::NormalFn(item) => item,
            }
        }).collect();
    }
}

impl Parse for InstructionDefinition {
    fn parse(input: ParseStream) -> Result<Self> {
        let definition = input.parse()?;
        let mut fns = vec![];
        while let Ok(item_fn) = input.parse() {
            fns.push(item_fn);
        }
        Ok(InstructionDefinition {
            definition,
            fns,
        })
    }
}

impl FoldContext {
    fn transform_generate_list_fn(&mut self, mut item: ItemFn) -> ItemFn {
        let list = self.vec_name.clone();
        let begin: Stmt = parse_quote! {
            let mut #list = vec![];
        };
        let end: Expr = parse_quote! {
            #list
        };
        item.block.stmts.insert(0, begin);
        item.block.stmts.push(Stmt::Expr(end));
        self.fold_item_fn(item)
    }

    fn definition(&self, path: &Path) -> Definition {
        if path.segments.len() == 1 {
            let name = &path.segments[0].ident;
            match self.variants.get(name) {
                Some(&Some(ref def_fields)) => {
                    Definition::Named(def_fields.clone())
                }
                Some(&None) => {
                    Definition::Unnamed
                }
                _ => Definition::Undefined
            }
        } else {
            Definition::Undefined
        }
    }

    fn missing_fields(&self, def_fields: FieldsNamed, expr_fields: Option<&ExprStruct>) -> Vec<FieldValue> {
        let mut expr_field_names = HashSet::new();
        if let Some(expr_fields) = expr_fields {
            for field_value in &expr_fields.fields {
                let name = match &field_value.member {
                    &Member::Named(ref name) => name.clone(),
                    &Member::Unnamed(..) => continue,
                };
                expr_field_names.insert(name);
            }
        }
        def_fields.named.into_iter().filter_map(|def_field| {
            let def_field_name = def_field.ident.clone().expect("unnamed field??");
            if !expr_field_names.contains(&def_field_name) {
                let def_field_name_str = def_field_name.to_string();
                let missing_field_fn_name = &self.missing_field_fn_name;
                let args = &self.missing_field_args;
                Some(FieldValue {
                    attrs: vec![],
                    member: Member::Named(def_field_name.clone()),
                    colon_token: Some(Default::default()),
                    expr: parse_quote! { #missing_field_fn_name(#(#args,)* #def_field_name_str) },
                })
            } else {
                None
            }
        }).collect()
    }
}

enum Definition {
    Named(FieldsNamed),
    Unnamed,
    Undefined,
}

impl Definition {
    fn fields(self) -> Option<FieldsNamed> {
        match self {
            Definition::Named(fields) => Some(fields),
            Definition::Unnamed => None,
            Definition::Undefined => None,
        }
    }
}

impl Fold for FoldContext {
    fn fold_stmt(&mut self, stmt: Stmt) -> Stmt {
        let new_expr = match &stmt {
            &Stmt::Expr(Expr::Struct(ref expr_struct)) |
            &Stmt::Semi(Expr::Struct(ref expr_struct), _)
                =>
            {
                let mut expr_struct = expr_struct.clone();
                self.definition(&expr_struct.path).fields().map(|def_fields| {
                    let missing_fields = self.missing_fields(def_fields, Some(&expr_struct));
                    expr_struct.fields.extend(missing_fields);
                    Expr::Struct(expr_struct)
                })
            }
            &Stmt::Expr(Expr::Call(ref expr_call)) |
            &Stmt::Semi(Expr::Call(ref expr_call), _)
                =>
            {
                if let &Expr::Path(ExprPath { ref path, .. }) = &*expr_call.func {
                    if expr_call.args.is_empty() {
                        match self.definition(path) {
                            Definition::Named(def_fields) => {
                                let fields = self.missing_fields(def_fields, None);
                                Some(Expr::Struct(parse_quote! {
                                    #path { #(#fields),* }
                                }))
                            }
                            Definition::Unnamed => {
                                Some((*expr_call.func).clone())
                            }
                            Definition::Undefined => {
                                None
                            }
                        }
                    } else {
                        match self.definition(path) {
                            Definition::Named(..) | Definition::Unnamed => {
                                Some(Expr::Call(expr_call.clone()))
                            }
                            Definition::Undefined => {
                                None
                            }
                        }
                    }
                } else {
                    None
                }
            }
            _ => None,
        };
        if let Some(new_expr_tail) = new_expr {
            let vec_name = &self.vec_name;
            let instruction_name = &self.instruction_name;
            parse_quote! { #vec_name.push(#instruction_name::#new_expr_tail); }
        } else {
            fold_stmt(self, stmt)
        }
    }
}

#[proc_macro]
pub fn enum_coder(input: TokenStream) -> TokenStream {
    let mut input = syn::parse_macro_input!(input as InstructionDefinition);
    // let _ = env_logger::init();
    // info!("{:#?}", input.fns);
    input.process_fns();
    let definition = &input.definition;
    let fns = &input.fns;
    let tokens = quote! {
        #definition
        #(#fns)*
    };
    // info!("{}", tokens);
    TokenStream::from(tokens)
}
