## Objectives 🎯

This project aims to manage data quality by automating several workflows:

- **Get data.** For both the demand- and supply-side surveys:
  - Download all data
  - Combine data from all versions
- **Validate interviews.** 
  - Check logical data for errors and statistical outliers.
  - Recommend actions to take
  - Create propose interviews to reject
  - Create a report to monitor both validations done by this program and validations done by Survey Solutions.
- **Reject flagged interviews.**
  - Collect interviews to be rejected
  - Reject them on the server, posting interview- and question-level comments
- **Create monitoring report.**
  - Computes statistics for monitoring indicators
  - Creates a report of those statistics

## Installation 🔌

### Prequisites 🧰

- [R](#r)
- [RTools](#rtools)
- [RStudio](#rstudio)

<details>

<summary>
Open to see more details 👁️
</summary>

#### R

- Follow this [link](https://cran.r-project.org/)
- Click on your operating system
- Click on `base`
- Download and install (e.g.,
  [this](https://cran.r-project.org/bin/windows/base/R-4.4.2-win.exe) for Windows)

#### RTools

Necessary when run on a Windows operaterating system

- Follow this [link](https://cran.r-project.org/)
- Click on `Windows`
- Click on `RTools`
- Download
  (e.g.,[this](https://cran.r-project.org/bin/windows/Rtools/rtools44/files/rtools44-6335-6327.exe) for a 64bit  architecture)
- Install in the default location suggested by the installer
(e.g., `C:\rtools4'`)

This program allows R to compile source code written in C++ and and that used by certain packages to be more performant (e.g., `{dplyr}`).

#### RStudio

- Follow this [link](https://posit.co/download/rstudio-desktop/)
- Click on the `DOWNLOAD RSTUDIO` button
- Select the right file for your operating system
- Download and install (e.g.,
  [this](https://download1.rstudio.org/electron/windows/RStudio-2024.09.1-394.exe) for Windows)

RStudio is required for a few reasons:

1. Provides a good interface for using R
2. Ships with [Quarto](https://quarto.org/), a program that this project will use for creating reports

</details>

## Usage 👩‍💻

### Initial setup ⚙️

### Routine use ♻️

- Open 📂
- Launch 🚀
- Choose 👉
- Consume 👀

#### Open 📂

<!-- How/why to open as a project -->

#### Launch 🚀

To run the program:

- Open `run_workflow.R`
- Source the script

#### Choose 👉

Once this script is run, you will be asked to choose a workflow:

```
── Data Quality Workflows ───────────────────────────────────
Which workflow would you like to run? 

1: Get data
2: Validate interviews
3: Reject flagged interviews
4: Create monitoring report

Selection: 
```

To execute a workflow, enter the number of your choice and press `Enter`.

Here is what each workflow entails:

- **Get data.** For both the demand- and supply-side surveys:
  - Download all data
  - Combine all versions
- **Validate interviews.** 
  - Check logical data for errors and statistical outliers.
  - Recommend actions to take
  - Create propose interviews to reject
  - Create a report to monitor both validations done by this program and validations done by Survey Solutions.
- **Reject flagged interviews.**
  - Collect interviews to be rejected
  - Reject them on the server, posting interview- and question-level comments
- **Create monitoring report.**
  - Computes statistics for monitoring indicators
  - Creates a report of those statistics

#### Consume 👀

##### Data

Find the combined data.

For the demand-side survey: `01_data/01_demand/02_combined`

For the supply-side survey: `01_data/02_supply/02_combined`

##### Validation errors

The validation workflow produces two sets of outputs

1. Recommendations
2. Rejections
3. Reports

Both of these are currently only available for the demand-side survey

###### Recommendations

The validation workflow reviews interviews, identifies issues, and recommends actions accordingly:

- **Reject.** If there 1+ validation issue and no potentially explanatory comment, neither for the interview overall nor for questions involved in the issue(s) found. These interviews can be rejected.
- **Review.** If would have been rejected except for potentially explanatory comments. These interviews require human review, at the very least to read the comments.
- **Follow-up.** If there is 1+ validation that has already been the basis for prior rejection. Since rejection has not lead to remedying the issue, survey managers need to follow-up to understand why not.

The results can be found in `02_validation/01/demand/01_recommendations`.

There are several files present that can be understood as follows:

- Intermediary files:
  - `attribs`. Files of data attributes used to create issues.
  - `issues`. File of issue-level observations.
  - `interviews_validated`. Interview ID of interviews validated by the system.
- Recommendation files.
  - Actions:
    - `to_reject_*`. Interviews that contains 1 or more issue that warrants rejection, without any potentially explanatory comments.
    - `to_review_*`. `Interviews that would otherwise be rejected except that there are potentially explanatory comments to be reviewed, whether comments on the interview as a whole or comments on the questions involved in the issues.
    - `to_follow_up_*`. Interviews that were rejected once already and contain unresolved issues.
  - Contents:
    - `*_ids`. Interview-level file containing the interview__id and interview__key of cases that fall into this category.
    - `*_issues`. Issue-level file of issues for the interviews in this category.
    - `*_api`. Contains the information that the API needs to reject the interview: interview__id, reject_comment (a concatenation of all issue messages), and interview__status.

###### Rejections

To help with rejections, the program copies recommended rejections to `02_validation/01/demand/01_decisions`.

The `to_reject_api.xlsx` contains recommendations and can be edited by the user, whether that editing mean removing interviews, adding interviews, or editing the reasons for rejection.

The contents of this file will be used to reject interviews with the rejection workflow.

###### Reports

The validation workflow produces two sets of reports on the issues found by the validation workflow and by Survey Solutions:

- Headquarters report
- Team-level report (🚧 Not yet implemented 🚧)

The Headquarters report provides an overview of the top issues overall and the number of issues by team.

##### Rejected interviews

See [rejections](#rejections) above.

##### Monitoring report

🚧 Not yet implemented 🚧

<!-- ## Troubleshooting 🔨 -->