pub trait Kind {
    type Kinds;

    fn kind(&self) -> Self::Kinds;
}

#[macro_export]
macro_rules! kinds {
    ($b:ident, $k:ident, $( $v:ident $( ( $($vty:ty = $vval:expr),* ) )?),* $(,)?) => {
        #[derive(Debug)]
        pub enum $b {
            $( $v $( ( $($vty),* ) )?, )*
        }
        impl $crate::kind::Kind for $b {
            type Kinds = $k;

            fn kind(&self) -> $k {
                $k (std::mem::discriminant(self))
            }
        }

        #[derive(PartialEq, Eq, Clone, Copy)]
        pub struct $k(std::mem::Discriminant<$b>);

        impl $k {
            $(
                #[allow(non_upper_case_globals, dead_code)]
                pub const $v: Self = $k (
                    std::mem::discriminant(
                        &( $b::$v $( ( $($vval),* ) )? )
                    )
                );
            )*
        }
    };
}
