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
import VCard.Types.Param.Generic (Param (..))
import VCard.Types.Param.Type
  ( TypeGeneral,
    TypeRelated,
    TypeTel,
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
    [ test_TypeGeneral,
      test_TypeTel,
      test_TypeRelated
    ]

test_TypeGeneral :: TestTree
test_TypeGeneral =
  testGroup
    "TypeGeneral"
    [ test_TypeGeneral_parse,
      test_TypeGeneral_serialize
    ]

test_TypeTel :: TestTree
test_TypeTel =
  testGroup
    "TypeTel"
    [ test_TypeTel_parse,
      test_TypeTel_serialize
    ]

test_TypeRelated :: TestTree
test_TypeRelated =
  testGroup
    "TypeRelated"
    [ test_TypeRelated_parse,
      test_TypeRelated_serialize
    ]

test_TypeGeneral_parse :: TestTree
test_TypeGeneral_parse =
  testCase "parse" $ do
    (parse "TYPE=home" @?=) $
      Just @TypeGeneral
        ( Param
            { paramName = CaseInsensitiveUpper (symbolSing @"TYPE"),
              paramValue =
                NonEmpty.singleton $
                  TypeValue (CaseInsensitiveLower (symbolSing @"home"))
            }
        )

    (parse "type=work,X-ab1-c" @?=) $
      Just @TypeGeneral
        ( Param
            { paramName = CaseInsensitiveUpper (symbolSing @"type"),
              paramValue =
                TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                  :| [ TypeValue (CaseInsensitiveLower (symbolSing @"X-ab1-c"))
                     ]
            }
        )

    --
    parse "TYPE=" @?= (Nothing @TypeGeneral)
    parse "TYPE=text" @?= (Nothing @TypeGeneral)
    parse "TYPE=xabc" @?= (Nothing @TypeGeneral)
    parse "TYPE=x-" @?= (Nothing @TypeGeneral)
    parse "TYPE= work" @?= (Nothing @TypeGeneral)
    parse "TYPE =work" @?= (Nothing @TypeGeneral)
    parse " TYPE=work" @?= (Nothing @TypeGeneral)
    parse "TYPE=work;" @?= (Nothing @TypeGeneral)
    parse "TYPE=work:" @?= (Nothing @TypeGeneral)
    --
    parse "TYPE=work,text,x-abc" @?= (Nothing @TypeGeneral)
    parse "TYPE= work,home,x-abc" @?= (Nothing @TypeGeneral)
    parse "TYPE=work, home,x-abc" @?= (Nothing @TypeGeneral)
    parse "TYPE=work,home,x-abc;" @?= (Nothing @TypeGeneral)
    parse "TYPE=work,home,x-abc:" @?= (Nothing @TypeGeneral)

test_TypeGeneral_serialize :: TestTree
test_TypeGeneral_serialize =
  testCase "serialize" $ do
    serialize @TypeGeneral
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            paramValue =
              NonEmpty.singleton $
                TypeValue (CaseInsensitiveLower (symbolSing @"home"))
          }
      )
      @?= "TYPE=home"

    serialize @TypeGeneral
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"type"),
            paramValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"X-ab1-c"))
                   ]
          }
      )
      @?= "type=work,X-ab1-c"

test_TypeTel_parse :: TestTree
test_TypeTel_parse =
  testCase "TypeTel" $ do
    (parse "TYPE=home" @?=) $
      Just @TypeTel
        ( Param
            { paramName = CaseInsensitiveUpper (symbolSing @"TYPE"),
              paramValue =
                NonEmpty.singleton $
                  TypeValue (CaseInsensitiveLower (symbolSing @"home"))
            }
        )

    (parse "Type=work,text,voice" @?=) $
      Just @TypeTel
        ( Param
            { paramName = CaseInsensitiveUpper (symbolSing @"Type"),
              paramValue =
                TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                  :| [ TypeValue (CaseInsensitiveLower (symbolSing @"text")),
                       TypeValue (CaseInsensitiveLower (symbolSing @"voice"))
                     ]
            }
        )

    (parse "type=fax,CELL,Video,pAger,textphone,x-ab1-c" @?=) $
      Just @TypeTel $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"type"),
            paramValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"fax"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"CELL")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"Video")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"pAger")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"textphone")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"x-ab1-c"))
                   ]
          }

    --
    parse "TYPE=" @?= (Nothing @TypeTel)
    parse "TYPE=contact" @?= (Nothing @TypeTel)
    parse "TYPE=xabc" @?= (Nothing @TypeTel)
    parse "TYPE=x-" @?= (Nothing @TypeTel)
    parse "TYPE= work" @?= (Nothing @TypeTel)
    parse "TYPE =work" @?= (Nothing @TypeTel)
    parse " TYPE=work" @?= (Nothing @TypeTel)
    parse "TYPE=work;" @?= (Nothing @TypeTel)
    parse "TYPE=work:" @?= (Nothing @TypeTel)
    --
    parse "TYPE=work,contact,x-abc" @?= (Nothing @TypeTel)
    parse "TYPE=work ,home,x-abc" @?= (Nothing @TypeTel)
    parse "TYPE=work, home,x-abc" @?= (Nothing @TypeTel)
    parse "TYPE=work,home,x-abc;" @?= (Nothing @TypeTel)
    parse "TYPE=work,home,x-abc:" @?= (Nothing @TypeTel)

test_TypeTel_serialize :: TestTree
test_TypeTel_serialize =
  testCase "serialize" $ do
    serialize @TypeTel
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            paramValue =
              NonEmpty.singleton $
                TypeValue (CaseInsensitiveLower (symbolSing @"home"))
          }
      )
      @?= "TYPE=home"

    serialize @TypeTel
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"Type"),
            paramValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"text")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"voice"))
                   ]
          }
      )
      @?= "Type=work,text,voice"

    serialize
      @TypeTel
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"type"),
            paramValue =
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

test_TypeRelated_parse :: TestTree
test_TypeRelated_parse =
  testCase "TypeRelated" $ do
    (parse "TYPE=home" @?=) $
      Just @TypeRelated
        ( Param
            { paramName = CaseInsensitiveUpper (symbolSing @"TYPE"),
              paramValue =
                NonEmpty.singleton $
                  TypeValue (CaseInsensitiveLower (symbolSing @"home"))
            }
        )

    (parse "Type=work,contact,acquaintance" @?=) $
      Just @TypeRelated
        ( Param
            { paramName = CaseInsensitiveUpper (symbolSing @"Type"),
              paramValue =
                TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                  :| [ TypeValue (CaseInsensitiveLower (symbolSing @"contact")),
                       TypeValue
                         (CaseInsensitiveLower (symbolSing @"acquaintance"))
                     ]
            }
        )

    (parse "type=friend,MET,Co-Worker,cOlleague,co-residenT,neighbor" @?=) $
      Just @TypeRelated $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"type"),
            paramValue =
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
      Just @TypeRelated $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            paramValue =
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
      Just @TypeRelated $
        Param
          { paramName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            paramValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"me"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"agent")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"emergency")),
                     TypeValue (CaseInsensitiveLower (symbolSing @"x-ab1-c"))
                   ]
          }

    --
    parse "TYPE=" @?= (Nothing @TypeRelated)
    parse "TYPE=text" @?= (Nothing @TypeRelated)
    parse "TYPE=xabc" @?= (Nothing @TypeRelated)
    parse "TYPE=x-" @?= (Nothing @TypeRelated)
    parse "TYPE= work" @?= (Nothing @TypeRelated)
    parse "TYPE =work" @?= (Nothing @TypeRelated)
    parse " TYPE=work" @?= (Nothing @TypeRelated)
    parse "TYPE=work;" @?= (Nothing @TypeRelated)
    parse "TYPE=work:" @?= (Nothing @TypeRelated)
    --
    parse "TYPE=work,text,x-abc" @?= (Nothing @TypeRelated)
    parse "TYPE=work ,home,x-abc" @?= (Nothing @TypeRelated)
    parse "TYPE=work, home,x-abc" @?= (Nothing @TypeRelated)
    parse "TYPE=work,home,x-abc;" @?= (Nothing @TypeRelated)
    parse "TYPE=work,home,x-abc:" @?= (Nothing @TypeRelated)

test_TypeRelated_serialize :: TestTree
test_TypeRelated_serialize =
  testCase "serialize" $ do
    serialize @TypeRelated
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            paramValue =
              NonEmpty.singleton $
                TypeValue (CaseInsensitiveLower (symbolSing @"home"))
          }
      )
      @?= "TYPE=home"

    serialize @TypeRelated
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"Type"),
            paramValue =
              TypeValue (CaseInsensitiveLower (symbolSing @"work"))
                :| [ TypeValue (CaseInsensitiveLower (symbolSing @"contact")),
                     TypeValue
                       (CaseInsensitiveLower (symbolSing @"acquaintance"))
                   ]
          }
      )
      @?= "Type=work,contact,acquaintance"

    serialize @TypeRelated
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"type"),
            paramValue =
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

    serialize @TypeRelated
      ( Param
          { paramName = CaseInsensitiveUpper (symbolSing @"TYPE"),
            paramValue =
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
