-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE OverloadedStrings #-}

module Test.VCard.Types.Param.Type (tests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import VCard.Parse (parse)
import VCard.Serialize (serialize)
import VCard.Types.Param.Generic (GenericParam (..))
import VCard.Types.Param.Type
  ( TypeParamGeneral,
    TypeParamRelated,
    TypeParamTel,
    TypeValue (..),
  )
import VCard.Types.Textual
  ( CaseInsensitiveLower (..),
    CaseInsensitiveUpper (..),
  )
import VCard.Util.Symbol (symbolSing)

tests :: TestTree
tests =
  testGroup
    "Type"
    [ test_TypeParamGeneral,
      test_TypeParamTel,
      test_TypeParamRelated
    ]

test_TypeParamGeneral :: TestTree
test_TypeParamGeneral =
  testGroup
    "TypeParamGeneral"
    [ test_TypeParamGeneral_parse,
      test_TypeParamGeneral_serialize
    ]

test_TypeParamTel :: TestTree
test_TypeParamTel =
  testGroup
    "TypeParamTel"
    [ test_TypeParamTel_parse,
      test_TypeParamTel_serialize
    ]

test_TypeParamRelated :: TestTree
test_TypeParamRelated =
  testGroup
    "TypeParamRelated"
    [ test_TypeParamRelated_parse,
      test_TypeParamRelated_serialize
    ]

test_TypeParamGeneral_parse :: TestTree
test_TypeParamGeneral_parse =
  testCase "parse" $ do
    (parse "TYPE=home" @?=) $
      Just @TypeParamGeneral
        ( GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"TYPE"),
              genericParamValue =
                NonEmpty.singleton $
                  TypeValue (CaseInsensitiveLower (symbolSing @"home"))
            }
        )

    (parse "type=work,X-ab1-c" @?=) $
      Just @TypeParamGeneral
        ( GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"type"),
              genericParamValue =
                TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                  :| [ TypeValue (CaseInsensitiveLower (symbolSing @"X-ab1-c"))
                     ]
            }
        )

    --
    parse "TYPE=" @?= (Nothing @TypeParamGeneral)
    parse "TYPE=text" @?= (Nothing @TypeParamGeneral)
    parse "TYPE=xabc" @?= (Nothing @TypeParamGeneral)
    parse "TYPE=x-" @?= (Nothing @TypeParamGeneral)
    parse "TYPE= work" @?= (Nothing @TypeParamGeneral)
    parse "TYPE =work" @?= (Nothing @TypeParamGeneral)
    parse " TYPE=work" @?= (Nothing @TypeParamGeneral)
    parse "TYPE=work;" @?= (Nothing @TypeParamGeneral)
    parse "TYPE=work:" @?= (Nothing @TypeParamGeneral)
    --
    parse "TYPE=work,text,x-abc" @?= (Nothing @TypeParamGeneral)
    parse "TYPE= work,home,x-abc" @?= (Nothing @TypeParamGeneral)
    parse "TYPE=work, home,x-abc" @?= (Nothing @TypeParamGeneral)
    parse "TYPE=work,home,x-abc;" @?= (Nothing @TypeParamGeneral)
    parse "TYPE=work,home,x-abc:" @?= (Nothing @TypeParamGeneral)

test_TypeParamGeneral_serialize :: TestTree
test_TypeParamGeneral_serialize =
  testCase "serialize" $ do
    serialize @TypeParamGeneral
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            genericParamValue =
              NonEmpty.singleton $
                TypeValue (CaseInsensitiveLower (symbolSing @"home"))
          }
      )
      @?= "TYPE=home"

    serialize @TypeParamGeneral
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"type"),
            genericParamValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"X-ab1-c"))
                   ]
          }
      )
      @?= "type=work,X-ab1-c"

test_TypeParamTel_parse :: TestTree
test_TypeParamTel_parse =
  testCase "TypeParamTel" $ do
    (parse "TYPE=home" @?=) $
      Just @TypeParamTel
        ( GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"TYPE"),
              genericParamValue =
                NonEmpty.singleton $
                  TypeValue (CaseInsensitiveLower (symbolSing @"home"))
            }
        )

    (parse "Type=work,text,voice" @?=) $
      Just @TypeParamTel
        ( GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"Type"),
              genericParamValue =
                TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                  :| [ TypeValue (CaseInsensitiveLower (symbolSing @"text")),
                       TypeValue (CaseInsensitiveLower (symbolSing @"voice"))
                     ]
            }
        )

    (parse "type=fax,CELL,Video,pAger,textphone,x-ab1-c" @?=) $
      Just @TypeParamTel $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"type"),
            genericParamValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"fax"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"CELL")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"Video")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"pAger")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"textphone")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"x-ab1-c"))
                   ]
          }

    --
    parse "TYPE=" @?= (Nothing @TypeParamTel)
    parse "TYPE=contact" @?= (Nothing @TypeParamTel)
    parse "TYPE=xabc" @?= (Nothing @TypeParamTel)
    parse "TYPE=x-" @?= (Nothing @TypeParamTel)
    parse "TYPE= work" @?= (Nothing @TypeParamTel)
    parse "TYPE =work" @?= (Nothing @TypeParamTel)
    parse " TYPE=work" @?= (Nothing @TypeParamTel)
    parse "TYPE=work;" @?= (Nothing @TypeParamTel)
    parse "TYPE=work:" @?= (Nothing @TypeParamTel)
    --
    parse "TYPE=work,contact,x-abc" @?= (Nothing @TypeParamTel)
    parse "TYPE=work ,home,x-abc" @?= (Nothing @TypeParamTel)
    parse "TYPE=work, home,x-abc" @?= (Nothing @TypeParamTel)
    parse "TYPE=work,home,x-abc;" @?= (Nothing @TypeParamTel)
    parse "TYPE=work,home,x-abc:" @?= (Nothing @TypeParamTel)

test_TypeParamTel_serialize :: TestTree
test_TypeParamTel_serialize =
  testCase "serialize" $ do
    serialize @TypeParamTel
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            genericParamValue =
              NonEmpty.singleton $
                TypeValue (CaseInsensitiveLower (symbolSing @"home"))
          }
      )
      @?= "TYPE=home"

    serialize @TypeParamTel
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"Type"),
            genericParamValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"text")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"voice"))
                   ]
          }
      )
      @?= "Type=work,text,voice"

    serialize
      @TypeParamTel
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"type"),
            genericParamValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"fax"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"CELL")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"Video")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"pAger")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"textphone")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"x-ab1-c"))
                   ]
          }
      )
      @?= "type=fax,CELL,Video,pAger,textphone,x-ab1-c"

test_TypeParamRelated_parse :: TestTree
test_TypeParamRelated_parse =
  testCase "TypeParamRelated" $ do
    (parse "TYPE=home" @?=) $
      Just @TypeParamRelated
        ( GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"TYPE"),
              genericParamValue =
                NonEmpty.singleton $
                  TypeValue (CaseInsensitiveLower (symbolSing @"home"))
            }
        )

    (parse "Type=work,contact,acquaintance" @?=) $
      Just @TypeParamRelated
        ( GenericParam
            { genericParamName = CaseInsensitiveUpper (symbolSing @"Type"),
              genericParamValue =
                TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                  :| [ TypeValue (CaseInsensitiveLower (symbolSing @"contact")),
                       TypeValue
                         (CaseInsensitiveLower (symbolSing @"acquaintance"))
                     ]
            }
        )

    (parse "type=friend,MET,Co-Worker,cOlleague,co-residenT,neighbor" @?=) $
      Just @TypeParamRelated $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"type"),
            genericParamValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"friend"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"MET")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"Co-Worker")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"cOlleague")),
                     TypeValue
                       (CaseInsensitiveLower (symbolSing @"co-residenT")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"neighbor"))
                   ]
          }

    (parse "TYPE=child,parent,sibling,spouse,kin,muse,crush,date" @?=) $
      Just @TypeParamRelated $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            genericParamValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"child"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"parent")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"sibling")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"spouse")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"kin")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"muse")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"crush")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"date"))
                   ]
          }

    (parse "TYPE=me,agent,emergency,x-ab1-c" @?=) $
      Just @TypeParamRelated $
        GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            genericParamValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"me"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"agent")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"emergency")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"x-ab1-c"))
                   ]
          }

    --
    parse "TYPE=" @?= (Nothing @TypeParamRelated)
    parse "TYPE=text" @?= (Nothing @TypeParamRelated)
    parse "TYPE=xabc" @?= (Nothing @TypeParamRelated)
    parse "TYPE=x-" @?= (Nothing @TypeParamRelated)
    parse "TYPE= work" @?= (Nothing @TypeParamRelated)
    parse "TYPE =work" @?= (Nothing @TypeParamRelated)
    parse " TYPE=work" @?= (Nothing @TypeParamRelated)
    parse "TYPE=work;" @?= (Nothing @TypeParamRelated)
    parse "TYPE=work:" @?= (Nothing @TypeParamRelated)
    --
    parse "TYPE=work,text,x-abc" @?= (Nothing @TypeParamRelated)
    parse "TYPE=work ,home,x-abc" @?= (Nothing @TypeParamRelated)
    parse "TYPE=work, home,x-abc" @?= (Nothing @TypeParamRelated)
    parse "TYPE=work,home,x-abc;" @?= (Nothing @TypeParamRelated)
    parse "TYPE=work,home,x-abc:" @?= (Nothing @TypeParamRelated)

test_TypeParamRelated_serialize :: TestTree
test_TypeParamRelated_serialize =
  testCase "serialize" $ do
    serialize @TypeParamRelated
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            genericParamValue =
              NonEmpty.singleton $
                TypeValue (CaseInsensitiveLower (symbolSing @"home"))
          }
      )
      @?= "TYPE=home"

    serialize @TypeParamRelated
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"Type"),
            genericParamValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"contact")),
                     TypeValue
                       (CaseInsensitiveLower (symbolSing @"acquaintance"))
                   ]
          }
      )
      @?= "Type=work,contact,acquaintance"

    serialize @TypeParamRelated
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"type"),
            genericParamValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"friend"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"MET")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"Co-Worker")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"cOlleague")),
                     TypeValue
                       (CaseInsensitiveLower (symbolSing @"co-residenT")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"neighbor"))
                   ]
          }
      )
      @?= "type=friend,MET,Co-Worker,cOlleague,co-residenT,neighbor"

    serialize @TypeParamRelated
      ( GenericParam
          { genericParamName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            genericParamValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"child"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"parent")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"sibling")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"spouse")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"kin")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"muse")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"crush")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"date"))
                   ]
          }
      )
      @?= "TYPE=child,parent,sibling,spouse,kin,muse,crush,date"
