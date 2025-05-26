## Rendering the manuscript

The manuscript was generated with `R 4.1.3`.

This version is required because newer versions of `R` combined with `flextable` can produce `.docx` files that appear valid, but fail to open in Microsoft Word.

### Recommended approach

Use [rig  as your R version manager](https://github.com/r-lib/rig) and follow the installation instructions for your system.


Once rig is installed, install R version 4.1.3 using:

```
rig add 4.1.3
```

Then, go to the project root and type this in your terminal:

```
rig run --r-version 4.1.3 -e "rmarkdown::render('manuscript/manuscript.Rmd')"
```

### System-wide approach

For **Windows users**, if you have `R 4.1.3` installed outside of `rig`, you can run this in your Powershell: 

```
& "C:\Program Files\R\R-4.1.3\bin\Rscript.exe" -e "rmarkdown::render('manuscript/manuscript.Rmd')"
```

or from Git Bash

```
"/c/Program Files/R/R-4.1.3/bin/Rscript.exe" -e "rmarkdown::render('manuscript/manuscript.Rmd')"
```

On **Mac or Linux**, specify the proper path to your installation using this command:

```
/path/to/R-4.1.3/bin/Rscript -e "rmarkdown::render('manuscript/manuscript.Rmd')"
```