use chumsky::prelude::SimpleSpan;
use chumsky::span::Span;

/// This is a spec formulation based on the formal grammar of the WIT format.
/// It leaves out purely syntactical tokens that don't have any semantic meaning.
/// See: https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md#lexical-structure


pub struct WorldId(String);
pub struct InterfaceId(String);
pub struct TypeId(String);
pub struct ItemId(String);

/// Spec: Corresponds to `comment`.
pub enum Comment {
    /// Single line comment
    Single(String),
    /// Multi line comment
    Multi(String),
}

/// Spec: Corresponds to `wit-file`.
pub struct WitFile {
    pub package_decl: Option<PackageDecl>,
    pub items: Vec<WitFileItem>,
}

pub enum WitFileItem {
    TopLevelUse(TopLevelUseItem),
    Interface(InterfaceItem),
    World(WorldItem),
}

pub struct PackageDecl {
    pub namespace: Vec<String>,
    /// Slash-separated path
    pub name: Vec<String>,
    pub version: Option<semver::Version>,
}

pub struct UsePathExternal {
    pub namespace: Vec<String>,
    pub name: Vec<String>,
}

/// Spec: Corresponds to `use-path`.
pub struct UsePath {
    /// If this is empty, it means the current namespace
    pub external: Option<UsePathExternal>,
    pub target: String,
    pub version: Option<semver::Version>,
}

/// Spec: Corresponds to `use-item`.
pub struct UseItem {
    pub use_path: UsePath,
    pub use_names: Vec<UseNamesItem>,
}

/// Spec: Corresponds to `use-names-item`.
pub struct UseNamesItem {
    pub name: ItemId,
    pub as_name: ItemId,
}

/// Spec: Corresponds to `toplevel-use-item`.
pub struct TopLevelUseItem {
    pub path: UsePath,
    /// If there is an `as <name>` at the end of the use declaration.
    pub as_name: Option<String>,
}

/// Spec: Corresponds to `world-item`.
pub struct World {
    pub id: WorldId,
    pub items: Vec<WorldItem>,
}

/// Spec: Corresponds to `world-items`.
pub enum WorldItem {
    Export(ExportItem),
    Import(ImportItem),
    Use(UseItem),
    Typedef(TypedefItem),
    Include(IncludeItem),
}

/// Spec: Corresponds to `export-item`.
/// /// Note: Function and Interface have a different syntax here than usual.
pub enum ExportItem {
    Use(UsePath),
    Func(FuncItem),
    Interface(InterfaceItem),
}

pub enum ExternType {
    Function(FuncType),
    Interface(InlineInterface),
}

/// Spec: Corresponds to `import-item`.
/// Note: Function and Interface have a different syntax here than usual.
pub enum ImportItem {
    Use(UsePath),
    Func(FuncItem),
    Interface(InterfaceItem),
}

/// Spec: Corresponds to `include-item`.
pub struct IncludeItem {
    pub use_path: UsePath,
    /// A list of zero or more names given to items inside the target of the use_path.
    pub with_clause: Vec<IncludeNamesItem>
}

/// Spec: Corresponds to `include-names-item`.
pub struct IncludeNamesItem {
    pub name: ItemId,
    pub as_name: ItemId,
}

pub struct InlineInterface {
    pub items: Vec<InterfaceItem>,
}

/// Spec: Corresponds to `interface-items`.
pub struct Interface {
    pub id: InterfaceId,
    pub items: Vec<InterfaceItem>,
}

/// Spec: Corresponds to `interface-item`.
pub enum InterfaceItem {
    Typedef(TypedefItem),
    Use(UseItem),
    Function(FuncItem),
}

/// Spec: Corresponds to `typedef-item`.
pub enum TypedefItem {
    Resource(ResourceItem),
    Variant(VariantItem),
    Record(RecordItem),
    Flags(FlagsItem),
    Enum(EnumItem),
    Type(TypeItem),
}

/// Spec: Corresponds to `func-item`.
pub struct FuncItem {
    pub id: ItemId,
    pub ty: FuncType,
}

/// The same as FuncItem, but indicates that this function is a static function of a resource.
pub struct StaticFuncItem(pub FuncItem);

/// Spec: Corresponds to `func-type`.
pub struct FuncType {
    pub parameters: Vec<NamedType>,
    pub result: ResultList,
}

/// Spec: Corresponds to `resource-item`.
pub struct ResourceItem {
    pub id: ItemId,
    pub methods: Vec<ResourceMethod>,
}

/// Spec: Corresponds to `resource-method`.
pub enum ResourceMethod {
    Func(FuncItem),
    Static(StaticFuncItem),
    Constructor {
        parameters: Vec<NamedType>,
    }
}

/// Spec: Corresponds to `variant-items`.
pub struct VariantItem {
    pub id: ItemId,
    pub variants: Vec<VariantCase>,
}

/// Spec: Corresponds to `variant-case`.
pub enum VariantCase {
    Unit(String),
    Tuple {
        name: String,
        ty: Type,
    }
}

/// Spec: Corresponds to `record-item`
pub struct RecordItem {
    pub id: ItemId,
    pub fields: Vec<RecordField>,
}

/// Spec: Corresponds to `record-field`.
pub struct RecordField {
    pub id: ItemId,
    pub ty: Type,
}

/// Spec: Corresponds to `flag-items`.
pub struct FlagsItem {
    pub id: ItemId,
    pub flags: Vec<String>,
}

/// Spec: Corresponds to `enum-items`.
pub struct EnumItem {
    pub id: ItemId,
    pub variants: Vec<String>,
}

/// Spec: Corresponds to `type-item`.
pub struct TypeItem {
    pub alias_id: TypeId,
    pub original_id: Type,
}

/// Spec: Corresponds to `result-list`.
pub enum ResultList {
    Type(Type),
    Tuple(Vec<NamedType>),
}

/// Spec: Corresponds to `named-type`.
pub struct NamedType {
    pub name: String,
    pub ty: Type,
}

/// Spec: Corresponds to `ty`.
pub enum Type {
    U8, U16, U32, U64,
    S8, S16, S32, S64,
    Float32, Float64,
    Char, Bool, String,
    Tuple(Vec<Type>),
    List(Box<Type>),
    Option(Box<Type>),
    Result {
        ok: Option<Box<Type>>,
        err: Option<Box<Type>>,
    },
    Handle(Handle),
    Id(TypeId),
}

/// Spec: Corresponds to `handle`.
pub enum Handle {
    Owned(ItemId),
    Borrowed(ItemId),
}
