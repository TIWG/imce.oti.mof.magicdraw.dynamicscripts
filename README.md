# JPL IMCE MagicDraw Dynamic Scripts for operating on MD models through the OMG Tool-Neutral Interoperability (OTI) MOF Information Schema & API

[![Build Status](https://travis-ci.org/TIWG/imce.oti.mof.magicdraw.dynamicscripts.svg?branch=master)](https://travis-ci.org/TIWG/imce.oti.mof.magicdraw.dynamicscripts)
[ ![Download](https://api.bintray.com/packages/tiwg/org.omg.tiwg/imce.oti.mof.magicdraw.dynamicscripts/images/download.svg) ](https://bintray.com/tiwg/org.omg.tiwg/imce.oti.mof.magicdraw.dynamicscripts/_latestVersion)
 
# Usage

## Exporting MagicDraw 18's UML 2.5 metamodel & PrimitiveTypes

- Open `Options > Environment > General > Save/Load`

  By default, `Save UUID` is unchecked.
  Check `Save UUID`
  *Restart* MagicDraw (Environment options are save only on exit)

- Open `org.omg.oti.mof.schema/resources/FundamentalConcepts.mdzip`

  This resource is a refactored copy of the "UML2 Metamodel with attributes", version 2.5
  (See MagicDraw Help > Resource/Plugin Manager > Samples)

  Refactoring summary:

  - Move all Enumerations defined in the UML metamodel to the PrimitiveTypes library.

    In OTI MOF, datatypes (incl. Enumerations) must be defined in a library resource.
    In OTI MOF, the only Classifiers in a metamodel resource are metaclasses and meta associations.

  - Name all 12 associations defined in the MagicDraw UML2.5 implementation.

    Compared with OMG UML 2.5, NoMagic's MagicDraw UML2.5 metamodel has 12 extra unnamed associations.

- Exporting MD18's PrimitiveTypes library to normalized OTI MOF Json

  Invoke the DynamicScripts toolbar menu: `DynamicScripts > OMG Tool Infrastructure / MOF Json > Export Libraries OTI DocumentSet configuration to OTI MOF Json Resources`
  
  A file chooser opens prompting for a directory where to save OTI MOF normalized *.json files.
  Then, another file chooser opens prompting for an OTI DocumentSet configuration file.
  Select [resources/MagicDraw18-implementation.library.documentSetConfiguration.json]

- Exporting MD18's UML metamdel to normalized OTI MOF Json

  Invoke the DynamicScripts toolbar menu: `DynamicScripts > OMG Tool Infrastructure / MOF Json > Export Metamodels OTI DocumentSet configuration to OTI MOF Json Resources`
  
  A file chooser opens prompting for a directory where to save OTI MOF normalized *.json files.
  Then, another file chooser opens prompting for an OTI DocumentSet configuration file.
  Select [resources/MagicDraw18-implementation.metamodel.documentSetConfiguration.json]

- Unfortunately, it's pointless to do this for MagicDraw's built-in profiles (e.g. StandardProfile, SysML, ..)
  because these resources do not contain UUIDs. Even with `Save UUIDs`, the UUID of elements in built-in resources
  are generated fresh on every load of these resources.

## Exporting models

- Example: [resources/examples/HybridSUV.mdzip]

  This model has been refactored from the SysML sample in `<md.install>/samples/SysML/hybrid sport utility vehicle.mdzip`
  as follows:

  - The `HSUVModel` package imports the `ModelingDomain::Automotive Value Types` package.

  - The `ModelingDomain::Automotive Value Types` package imports the `SIDefinitions` package.

  - The `ModelingDomain::Automotive Value Types` package applies the `SysML` profile.

- To export the HSUV model

  - Select the following in the browser or a diagram:

    - `UML Standard Profile::StandardProfile`
    - `SysML`
    - `QUDV`
    - `SIDefinitions`
    - `ModelingDomain::Automotive Value Types`
    - `HSUVModel`

  - From any selected package, invoke the context menu:
    `DynamicScriptsContextMenu > OMG Tool Infrastructure / MOF Json > Export as OTI MOF Models`

  - A modal dialog will prompt for a `*.documentSetConfiguration.json` file.

    Select [resources/examples/HybridSUV.documentSetConfiguration.json]

    This file contains OTI Characteristics for several packages/profiles identified by their OTI ToolSpecificID.
    Example:

    ```
      {
        "otiCharacteristics" : {
          "packageURI" : "http://www.omg.org/spec/SysML/HybridSUV/AutomotiveValueTypes",
          "documentURL" : "http://www.omg.org/spec/SysML/HybridSUV/AutomotiveValueTypes.xmi",
          "artifactKind" : {
            "type" : "OTISerializableModelLibraryArtifactKind"
          },
          "nsPrefix" : "AutomotiveValueTypes",
          "uuidPrefix" : "org.omg.sysml.HybridSUV.AutomotiveValueTypes"
        },
        "toolSpecificPackageID" : "_16_0beta1_1b500480_1221552131488_238103_4240",
        "toolSpecificPackageURL" : "mdel://_16_0beta1_1b500480_1221552131488_238103_4240?projectName=HybridSUV&elementName=Package+Automotive+Value+Types&metaType=Package&elementQName=ModelingDomain%3A%3AAutomotive+Value+Types",
        "overrideID" : [ ],
        "overrideUUID" : [ ],
        "excludeNestedElements" : [ ]
      },
      {
        "otiCharacteristics" : {
          "packageURI" : "http://www.omg.org/spec/SysML/HybridSUV/HSUVModel",
          "documentURL" : "http://www.omg.org/spec/SysML/HybridSUV/HSUVModel.xmi",
          "artifactKind" : {
            "type" : "OTISerializableModelLibraryArtifactKind"
          },
          "nsPrefix" : "HSUVModel",
          "uuidPrefix" : "org.omg.sysml.HybridSUV.HSUVModel"
        },
        "toolSpecificPackageID" : "_16_0beta1_1b500480_1221551007562_281002_3929",
        "toolSpecificPackageURL" : "mdel://_16_0beta1_1b500480_1221551007562_281002_3929?projectName=hybrid+sport+utility+vehicle&elementName=Package+HSUVModel&metaType=Package&elementQName=HSUVModel",
        "overrideID" : [ ],
        "overrideUUID" : [ ],
        "excludeNestedElements" : [ ]
      }
    ```

    At the end, there will be a file `<md.install>/dynamicScripts/ MagicDraw-PROJECT-584d20e658a78d4832cdef764e4d76.zip`
    containing OTI MOF Json serialization for OTI MOF Profile & Model resources corresponding to the selected packages.

    Example:

    ```
    M Filemode      Length  Date         Time      File
    - ----------  --------  -----------  --------  ---------------------------------------------------------
      -rw-rw-rw-     54012  13-Jun-2016  16:00:00  www.omg.org/spec/UML/20131201/SysML.xmi
      -rw-rw-rw-      9131  13-Jun-2016  16:00:00  www.omg.org/spec/UML/20131001/StandardProfile.xmi
      -rw-rw-rw-     19268  13-Jun-2016  16:00:00  www.omg.org/spec/SysML/HybridSUV/AutomotiveValueTypes.xmi
      -rw-rw-rw-    155469  13-Jun-2016  16:00:00  www.omg.org/spec/SysML/20120322/QUDV.xmi
      -rw-rw-rw-    976022  13-Jun-2016  16:00:00  www.omg.org/spec/SysML/HybridSUV/HSUVModel.xmi
      -rw-rw-rw-    628804  13-Jun-2016  16:00:00  www.omg.org/spec/SysML/20120322/ISO-80000-1-QUDV.xmi
    - ----------  --------  -----------  --------  ---------------------------------------------------------
                   1842706                         6 files
    ```

# Limitations

The OTI MOF Json relies on the UUID of every element for cross-referencing.
MagicDraw 18 provides limited support for this in the following sense:

- The `Save UUID` environment option must be set for user-created profiles & models.

Without this option set, exporting profiles/models will produce different results on different instances of MagicDraw
because MagicDraw creates fresh UUIDs in this mode.

With this option set, MagicDraw retains the UUIDs of user-defined profiles/models.
However, the UUIDs of every element in a MagicDraw-provided profile/library/sample is created fresh
as if `Save UUID` were unset because these resources do not have UUIDs.

- Except for UML and PrimitiveTypes, exporting a set of packages as OTI MOF Model resources requires including in the
selection all profiles applied (directly or indirectly) and all packages imported (directly or indirectly).

This works with or without `Save UUID` enabled.
If `Save UUID` is not enabled, the results will be different for different instances of MagicDraw.
If `Save UUID` is enabled, the results will be reproducible only for user-created profiles/packages
that were created with `Save UUID` enabled.

Note that MagicDraw's `SysML` and `UML Standard Profile::StandardProfile` do not have UUIDs:
any element exported from these profiles will have freshly created UUID for every instance of MagicDraw.