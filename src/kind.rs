pub trait Kind {
    type Kinds;

    fn kind(&self) -> Self::Kinds;
}

#[macro_export]
macro_rules! kinds {
    ($base:ident, $kind:ident, $( $v:ident $( ( $($vty:ty = $vval:expr),* ) )?),* $(,)?) => {
        #[derive(Debug)]
        pub enum $base {
            $( $v $( ( $($vty),* ) )?, )*
        }
        impl $crate::kind::Kind for $base {
            type Kinds = $kind;

            fn kind(&self) -> $kind {
                $kind(std::mem::discriminant(self))
            }
        }

        #[derive(PartialEq, Eq, Clone, Copy)]
        pub struct $kind(std::mem::Discriminant<$base>);

        impl $kind {
            $(
                #[allow(non_upper_case_globals, dead_code)]
                pub const $v: Self = $kind (
                    std::mem::discriminant(
                        &( $base::$v $( ( $($vval),* ) )? )
                    )
                );
            )*
        }
    };
}
