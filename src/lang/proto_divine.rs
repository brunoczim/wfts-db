#[macro_export]
macro_rules! proto_divine_morpheme {
    [$($phoneme:expr),*] => {{
        #[allow(unused_imports)]
        use $crate::lang::proto_divine::{
            Vowel::*,
            Obstruent::*,
            Sonorant::*,
            Consonant::*,
            Morpheme,
        };
        Morpheme::from_phoneme_seq([$($phoneme.into()),*]).unwrap()
    }};
    [$($phoneme:expr,)*] => { $crate::proto_divine_morpheme![$($phoneme),*] };
}

#[macro_export]
macro_rules! proto_divine_word {
    {
        { $([$($prefix:tt)*]),* },
        [$($nucleus:tt),*],
        { $([$($sufix:tt)*]),* } $(,)?
    } => {
        use $crate::lang::proto_divine::Word;
        Word {
            prefixes: vec![$($crate::proto_divine_morpheme![$($prefix)*]),*],
            nucleus: $crate::proto_divine_morpheme![$($prefix)*],
            suffixes: vec![$($crate::proto_divine_morpheme![$($prefix)*]),*],
        },
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Vowel {
    A,
    E,
    I,
    Ao,
    O,
    U,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Obstruent {
    P,
    T,
    K,
    F,
    S,
    X,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Sonorant {
    M,
    N,
    Ng,
    W,
    L,
    J,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Consonant {
    Obstruent(Obstruent),
    Sonorant(Sonorant),
}

impl From<Obstruent> for Consonant {
    fn from(value: Obstruent) -> Self {
        Self::Obstruent(value)
    }
}

impl From<Sonorant> for Consonant {
    fn from(value: Sonorant) -> Self {
        Self::Sonorant(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Phoneme {
    Vowel(Vowel),
    Consonant(Consonant),
}

impl From<Vowel> for Phoneme {
    fn from(value: Vowel) -> Self {
        Self::Vowel(value)
    }
}

impl From<Consonant> for Phoneme {
    fn from(value: Consonant) -> Self {
        Self::Consonant(value)
    }
}

impl From<Obstruent> for Phoneme {
    fn from(value: Obstruent) -> Self {
        Self::Consonant(value.into())
    }
}

impl From<Sonorant> for Phoneme {
    fn from(value: Sonorant) -> Self {
        Self::Consonant(value.into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Onset {
    initial: Option<Obstruent>,
    medial: Option<Sonorant>,
}

#[derive(Debug)]
pub enum MorphemeError {
    EmptySequence,
    NoNucleus,
    TooHeavy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Morpheme {
    pub onset: Onset,
    pub nucleus: Vowel,
    pub coda: Option<Consonant>,
}

impl Morpheme {
    pub fn from_phoneme_seq<I>(sequence: I) -> Result<Self, MorphemeError>
    where
        I: IntoIterator<Item = Phoneme>,
    {
        let coda_state =
            |mut iterator: I::IntoIter, onset, nucleus| match iterator.next() {
                Some(Phoneme::Consonant(coda)) => {
                    if iterator.next().is_some() {
                        Err(MorphemeError::TooHeavy)?
                    }
                    Ok(Morpheme { onset, nucleus, coda: Some(coda) })
                },
                Some(Phoneme::Vowel(_)) => Err(MorphemeError::TooHeavy),
                None => Ok(Morpheme { onset, nucleus, coda: None }),
            };

        let nucleus_state = |mut iterator: I::IntoIter, onset| {
            let Some(phoneme) = iterator.next() else {
                Err(MorphemeError::NoNucleus)?
            };

            match phoneme {
                Phoneme::Consonant(_) => Err(MorphemeError::NoNucleus),
                Phoneme::Vowel(nucleus) => coda_state(iterator, onset, nucleus),
            }
        };

        let medial_state = |mut iterator: I::IntoIter, initial| {
            let Some(phoneme) = iterator.next() else {
                Err(MorphemeError::NoNucleus)?
            };

            match phoneme {
                Phoneme::Consonant(Consonant::Obstruent(_)) => {
                    Err(MorphemeError::NoNucleus)?
                },
                Phoneme::Consonant(Consonant::Sonorant(medial)) => {
                    nucleus_state(
                        iterator,
                        Onset { initial: Some(initial), medial: Some(medial) },
                    )
                },
                Phoneme::Vowel(nucleus) => coda_state(
                    iterator,
                    Onset { initial: Some(initial), medial: None },
                    nucleus,
                ),
            }
        };

        let initial_state = |mut iterator: I::IntoIter| {
            let Some(phoneme) = iterator.next() else {
                Err(MorphemeError::EmptySequence)?
            };

            match phoneme {
                Phoneme::Consonant(Consonant::Obstruent(initial)) => {
                    medial_state(iterator, initial)
                },
                Phoneme::Consonant(Consonant::Sonorant(medial)) => {
                    nucleus_state(
                        iterator,
                        Onset { initial: None, medial: Some(medial) },
                    )
                },
                Phoneme::Vowel(nucleus) => coda_state(
                    iterator,
                    Onset { initial: None, medial: None },
                    nucleus,
                ),
            }
        };

        initial_state(sequence.into_iter())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word {
    pub prefixes: Vec<Morpheme>,
    pub stem: Morpheme,
    pub suffixes: Vec<Morpheme>,
}

#[cfg(test)]
mod test {
    use crate::lang::proto_divine::{
        Morpheme,
        Obstruent,
        Onset,
        Sonorant,
        Vowel,
    };

    #[test]
    fn morpheme_macro_correct_only_vowel() {
        let morpheme = proto_divine_morpheme![A];
        assert_eq!(
            morpheme,
            Morpheme {
                onset: Onset { initial: None, medial: None },
                nucleus: Vowel::A.into(),
                coda: None,
            },
        );
    }

    #[test]
    fn morpheme_macro_correct_full() {
        let morpheme = proto_divine_morpheme![P, L, E, J];
        assert_eq!(
            morpheme,
            Morpheme {
                onset: Onset {
                    initial: Some(Obstruent::P.into()),
                    medial: Some(Sonorant::L.into())
                },
                nucleus: Vowel::E.into(),
                coda: Some(Sonorant::J.into()),
            },
        );
    }

    #[test]
    fn morpheme_macro_correct_no_coda() {
        let morpheme = proto_divine_morpheme![T, N, Ao];
        assert_eq!(
            morpheme,
            Morpheme {
                onset: Onset {
                    initial: Some(Obstruent::T.into()),
                    medial: Some(Sonorant::N.into())
                },
                nucleus: Vowel::Ao.into(),
                coda: None,
            },
        );
    }

    #[test]
    fn morpheme_macro_correct_no_onset() {
        let morpheme = proto_divine_morpheme![I, M];
        assert_eq!(
            morpheme,
            Morpheme {
                onset: Onset { initial: None, medial: None },
                nucleus: Vowel::I.into(),
                coda: Some(Sonorant::M.into()),
            },
        );
    }

    #[test]
    fn morpheme_macro_correct_no_sonorant() {
        let morpheme = proto_divine_morpheme![S, O];
        assert_eq!(
            morpheme,
            Morpheme {
                onset: Onset {
                    initial: Some(Obstruent::S.into()),
                    medial: None
                },
                nucleus: Vowel::O.into(),
                coda: None,
            },
        );
    }

    #[test]
    fn morpheme_macro_correct_no_obstruent() {
        let morpheme = proto_divine_morpheme![Ng, U];
        assert_eq!(
            morpheme,
            Morpheme {
                onset: Onset {
                    initial: None,
                    medial: Some(Sonorant::Ng.into())
                },
                nucleus: Vowel::U.into(),
                coda: None,
            },
        );
    }

    #[test]
    #[should_panic]
    fn morpheme_macro_incorrect_empty() {
        let _ = proto_divine_morpheme![];
    }

    #[test]
    #[should_panic]
    fn morpheme_macro_incorrect_multi_nuclei() {
        let _ = proto_divine_morpheme![A, P, A];
    }

    #[test]
    #[should_panic]
    fn morpheme_macro_incorrect_onset_too_long() {
        let _ = proto_divine_morpheme![K, X, M, O];
    }

    #[test]
    #[should_panic]
    fn morpheme_macro_incorrect_onset_too_many_obst() {
        let _ = proto_divine_morpheme![K, X, O];
    }

    #[test]
    #[should_panic]
    fn morpheme_macro_incorrect_onset_bad_order() {
        let _ = proto_divine_morpheme![L, X, O];
    }

    #[test]
    #[should_panic]
    fn morpheme_macro_incorrect_coda_too_long() {
        let _ = proto_divine_morpheme![O, J, S];
    }

    #[test]
    #[should_panic]
    fn morpheme_macro_incorrect_no_nucleus() {
        let _ = proto_divine_morpheme![S, J];
    }
}
