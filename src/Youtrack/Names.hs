{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Youtrack.Names
    ( FamilyName(..)
    , GivenName(..)
    , FullName(..)
    -- * Printing
    , printFullNameEastern
    , printFullNameWestern
    )
where

import           GHC.Generics                (Generic)
import           Data.Function               (on)
import           Data.Monoid
import           Data.String
import           Test.QuickCheck

newtype FamilyName = FamilyName { fromFamilyName ∷ String } deriving (Eq, Generic, Ord, IsString)
newtype GivenName  = GivenName  { fromGivenName  ∷ String } deriving (Eq, Generic, Ord, IsString)

-- * A simplistic, yet somewhat useful representation of full names.
data  FullName
    = FullName { familyName ∷ FamilyName
               , givenName  ∷ GivenName }
      deriving (Eq, Generic)
instance Ord FullName where
    compare x y = case (compare `on` familyName) x y of
                    EQ  → (compare `on` givenName) x y
                    ord → ord

instance Arbitrary FamilyName where  arbitrary = elements family_names
instance Arbitrary GivenName  where  arbitrary = elements given_names
instance Arbitrary FullName   where  arbitrary = FullName <$> arbitrary <*> arbitrary

instance Show FamilyName      where show = fromFamilyName
instance Show GivenName       where show = fromGivenName

printFullNameEastern ∷ FullName → String
printFullNameEastern FullName{..} = show familyName <> " " <> show givenName
printFullNameWestern ∷ FullName → String
printFullNameWestern FullName{..} = show givenName  <> " " <> show familyName

instance Show FullName        where show = printFullNameWestern

given_names ∷ [GivenName]
given_names = [ "Mehdi", "Youssef", "Aziz", "Karim"                                                                -- Tunissia
              , "Mohammed", "Ahmed", "Ali", "Hamza", "Ibrahim", "Mahmoud", "Abdallah", "Tareq", "Hassan", "Khaled" -- Libya
              , "Deven", "Ishaan", "Neil", "Aryan", "Rohan", "Arnav", "Nikhil", "David", "Armaan", "Suraj"         -- Indians
              , "Gabrielle", "Amelia", "Tianna", "Brianna", "Jada"                                                 -- Jamaica
              , "Anya", "Arushi", "Aditi", "Shreya", "Anjali", "Kavya", "Nisha", "Nishi", "Diya", "Riya", "Ruhi"   -- Indians
              , "Gabrielle", "Amelia", "Tianna", "Brianna", "Jada"                                                 -- Jamaica
              , "Emma", "Olivia", "Sophia", "Zoe", "Emily", "Avery", "Isabella", "Charlotte", "Lily", "Ava"        -- Quebec
              , "Wei", "Jie", "Hao", "Yi", "Jun", "Feng", "Yong", "Jian", "Bin"                                    -- China
              , "Amir-Ali", "AbulFazl", "Amir-Hossein", "Ali", "Mohammad", "Amir-Mohammad", "Mahdi", "Hossein"     -- Iran
              , "Naranbaatar", "Batkhaаn", "Baаtar", "Chuluun", "Sukhbaаtar"                                       -- Mongolia
              , "Krishna", "Kiran", "Bishal", "Bibek", "Siddhartha", "Manish", "Mahesh", "Kamal"                   -- Nepal
              , "Somchai", "Somsak", "Somporn", "Somboon", "Prasert"                                               -- Thailand
              , "Jing", "Ying", "Yan", "Li", "Xiaoyan", "Xinyi", "Jie", "Lili", "Xiaomei"                          -- China
              , "Milica", "Anđela", "Jovana", "Ana", "Teodora", "Katarina", "Marija", "Sara"                       -- Serbia
              , "Ellen", "Saga", "Amanda", "Elsa", "Ida", "Emma", "Stella", "Ebba"                                 -- Sweden
              ]

family_names ∷ [FamilyName]
family_names = [ "Ховханнисян", "Маммадов", "Chokroborti", "Wáng", "Беридзе", "Devi"
               , "Cohen", "Satō", "Kim", "Santos", "Chén", "Yılmaz", "Nguyễn", "Li"
               , "Rodríguez", "Martínez", "Smith", "Fernández", "Silva", "González"
               , "Quispe", "Tjon", "Kelmendi", "Gruber", "Іваноў", "Peeters", "Hodžić"
               , "Kovačević", "Dimitrov", "Horvat", "Novák", "Nielsen", "Tamm", "Joensen"
               , "Korhonen", "Johansson", "Martin", "Müller", "Papadopoulos", "Nagy"
               , "Ó Murchú", "Rossi", "Zogaj", "Bērziņš", "Kazlauskas", "Schmit", "Borg"
               , "Andov", "Rusu", "De Jong", "Janssen", "Van den Berg", "Hansen", "Nowak"
               , "Silva", "Popa", "Смирно́в", "Јовановић", "Kováč", "Krajnc", "García"
               , "González", "Andersson", "Bianchi", "Meier", "Melnyk", "Wilson", "Díaz"
               , "Reyes", "Hernández", "López" ]
