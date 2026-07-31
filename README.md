![PEDSnet Animated Logo](images/PEDSnetLogo.gif)
# PEDSnet Reusable Code Repo Template

PEDSnet Reusable Code Repo Template is a blank GitHub repository and README template for reusable code submitted to [PEDSpace](https://pedsnet.org/metadata/home) for ingestion. The [BLANK_README](BLANK_README.md) template is stripped-down fork of the [Best-README-Template](https://github.com/othneildrew/Best-README-Template). We ask that you minimally include the information outlined in this document. 

## How To Use This Template
1. "Use this Template" in GitHub under the PEDSnet organization[^1].
2. Open [`BLANK_README.md`](BLANK_README.md).
3. Change or add any images that best describe your project[^2].
4. Find and replace the following placeholders:
   - `repo_name`: The repository name in snake_case format (e.g., `my_project_repo`). This must correspond to the repo name in the GitHub repo's URL.
   - `project_title`: A human-readable, Title Case name for your project (e.g., "My Project Title").
   - `project_description`: A concise description of your project (1-2 sentences).
   - `project_license`: The license type for your project (e.g., MIT, GPL-3.0).
5. Update [`LICENSE.txt`](LICENSE.txt) to reflect your submission's copyright license.
6. Follow the instructions under <a href="#reproducibility">Reproducibility</a>.
7. Delete this `README.md` and rename `BLANK_README.md` to `README.md`.
8. Commit and push to `origin/main`.

### Reproducibility

If you are doing ongoing R development work, we strongly encourage that you do so using [`renv`](https://rstudio.github.io/renv/index.html) e.g. [`renv::init()`](https://rstudio.github.io/renv/reference/init.html) and [`renv::snapshot()`](https://rstudio.github.io/renv/reference/snapshot.html). This will make not only documentation easier but can also improve your development workflow in the long run by ensuring version consistency and improving reproducibility.

Below are the minimum submission guidelines:

#### Required
- **Code Repositories (not packaged):**
  - Must include an `renv.lock` file generated from the most up-to-date R environment capabale of successfully executing the code.
- **Packaged Repositories:**
  - `renv.lock` file is optional but must document:
    - R version used for development.
    - Key dependency versions[^4].

#### Optional
- Use an [`renv` project library](https://rstudio.github.io/renv/reference/init.html) for version consistency and improved development workflow.

#### Steps:
1. Make sure that the project root is the working directory of your R session e.g `setwd('/path/to/project/dir')`.
2. Generate the `renv.lock` file:
   ```R
   renv::snapshot()
   ```
3. To restore the R environment:
   ```R
   renv::restore()
   ```

[^1]: Click on the "Use this Template" button in the corner of the page to create a repository using this template, and then clone this repository locally to edit. I recommend [VSCode](https://code.visualstudio.com/) with the [Markdown Preview Enanched extension](https://marketplace.visualstudio.com/items?itemName=shd101wyy.markdown-preview-enhanced), but you can also use a plaintext text editor.

[^2]: Swap out `images/screenshot.png` with a relevant image for your deposit (optional) and uncomment and edit lines 25 and 26 in to display it.

[^3]: Just remember to `.gitignore` your library (e.g. `echo renv/ >> .gitignore`). This repo already has an extensive coverage of ignored files and directories.

[^4]: If you are submitting a package **without** an `renv.lock` file, you can use `utils::sessionInfo()` or (preferably) [`sessioninfo::session_info(pkgs = "attached")`](https://sessioninfo.r-lib.org/reference/session_info.html) from [the *sessioninfo* package](https://sessioninfo.r-lib.org/index.html#installation) to generate an output to include under the [Built With](BLANK_TEMPLATE.md#built-with) section of the template.
