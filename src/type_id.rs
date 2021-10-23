#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

impl TypeId {
    /// Returns a new typeId, with index larger than all of the builtin types.
    pub fn new() -> Self {
        Self::default()
    }

    pub fn increase(&mut self) -> Self {
        let out = *self;
        self.0 += 1;
        out
    }
}

impl Default for TypeId {
    fn default() -> Self {
        builtin::MAX_TYPE_ID
    }
}

impl From<TypeId> for usize {
    fn from(id: TypeId) -> Self {
        id.0
    }
}

pub mod builtin {
    use super::TypeId;

    pub const ERROR_ID: TypeId = TypeId(0);
    pub const UNIT_ID: TypeId = TypeId(1);
    pub const INT_ID: TypeId = TypeId(2);
    pub const UINT_ID: TypeId = TypeId(3);
    pub const FLOAT_ID: TypeId = TypeId(4);

    pub const IVEC_ID: [TypeId; 5] = [TypeId(0), TypeId(5), TypeId(6), TypeId(7), TypeId(8)];
    pub const UVEC_ID: [TypeId; 5] = [TypeId(0), TypeId(9), TypeId(10), TypeId(11), TypeId(12)];
    pub const FVEC_ID: [TypeId; 5] = [TypeId(0), TypeId(13), TypeId(14), TypeId(15), TypeId(16)];

    pub const ID_ID: TypeId = UVEC_ID[3];

    pub const IMAGE_ID: TypeId = TypeId(17);

    pub const BOOL_ID: TypeId = TypeId(18);

    pub(super) const MAX_TYPE_ID: TypeId = TypeId(19);
}
