## **Dtableone 1.1.0**

*****

#### Resubmission to CRAN
- This release addresses all the feedback received from CRAN. The title has been shortened, documentation improved, and example execution updated according to CRAN policies.

#### Major improvements

- Revised `CreateTableD2` documentation to detail the output structure and its interpretation.
- Ensured examples in documentation are executable, replacing `\dontrun{}` with `\donttest{}` where appropriate.
- Shortened package title in DESCRIPTION to comply with CRAN's character limit.
- Updated `loadPackage` function to alert the user to install missing packages without automatic installation.

#### Bug fixes

- No specific bugs were reported in the previous version. Ongoing maintenance to ensure compatibility and performance.

#### Notes

- Adapted the package to better meet CRAN policies and improve overall user experience.
- The package continues to extend the functionalities of 'DTComPair', 'tableone', and 'gtsummary' packages.

## **Dtableone 1.0.0**

*****

- Initial release.

#### Major changes

- Implemented `CreateTableD2` function for comparing the diagnostic performance of two tests on the same subjects within clinical studies.
- Added functionality to present results in a clear and comprehensive tabular form directly through the R console.

#### Minor changes

- Added data documentation for sample datasets `data1`, `data2`, `data3`, and `data4`.
- Improved user-friendliness of the package documentation and examples.

#### Notes

- This package is inspired by and extends the functionalities of the 'DTComPair', 'tableone', and 'gtsummary' packages, offering a user-friendly approach to the tabular comparison of diagnostic tests in paired clinical data.
