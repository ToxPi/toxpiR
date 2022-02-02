Filing issues
-------------

Please read these points carefully and follow them while filing issues.

- **One issue for one purpose**. Don't add more than one *bug*, *feature request*, or *documentation request* on to the same issue. Take the time to read through the current issues to ensure your issue is not already listed.
- If you've found a *bug*, thank you for reporting! Please include a reproducible example of your bug in the issue. 
- If you need *support* or have a general *question*, please consider asking the question on [StackOverflow](http://www.stackoverflow.com)
- For the project contributors, please label new issues using the following rules:
  - *bugs* should be labeled "bug"
  - *feature requests* or *suggestions* should be labeled "enhancement"
  - *questions* or *requests for support* should be labeled "question"
  
Pull Requests
-------------

Please file an issue before creating PRs so that it can be discussed first *before* you invest time implementing it.

1. Please create all pull requests (PR) against the `master` branch.
2. Create **one PR per feature/bug fix**. Each PR should be associated with an Issue.
3. Create a branch for that feature/bug fix, named 'issue-N' where N is the Issue number, and use that as a base for your pull requests. Pull requests directly against your version of `master` will not be accepted.
4. Please squash temporary stage commits together before issuing a PR.
5. All commit messages should have two components: (1) a headerer on the first line beginning with "issue-N:" and containing no more than 50 characters, and (2) a body with 1 empty line after the header then at least a sentence or two in the commit body detailing all changes and justifications. Lines in the commit body should be wrapped to no more than 72 characters per line, and can contain multiple paragraphs.<sup>[1](#myfootnote1)</sup>
5. In your pull request's description, please state clearly as to what your PR does, i.e., what FR or bug your PR addresses, along with the issue number. For e.g, "Closes #717: tcplLoadData no longer errors with missing data."
7. Please build and test the package using `R CMD check --as-cran` against your branch source package archive `.tar.gz` file. You may want to add `--no-manual`, `--no-build-vignettes` or `--ignore-vignettes` (R 3.3.0+) options to reduce dependencies required to perform check. PRs that fail `check` cannot be merged.
8. The NEWS file also has to be updated while fixing or implementing an issue. It should mention the issue number and what the issue is being closed. Also add a "Thanks to @your_name for the PR".

**References:** If you are not sure how to issue a PR, but would like to contribute, these links should help get you started:

1. **[How to Github: Fork, Branch, Track, Squash and Pull request](https://gun.io/blog/how-to-github-fork-branch-and-pull-request/)**.
2. **[Squashing Github pull requests into a single commit](http://eli.thegreenplace.net/2014/02/19/squashing-github-pull-requests-into-a-single-commit)**.

*This guide was modified from the contributing guide for the [data.table](https://github.com/Rdatatable/data.table) repository*

<a name="myfootnote1">1</a>: To make it easier to count the characters per line you can edit your $HOME/.vimrc ($HOME/_vimrc on Windows) to include ":set ruler" which will display the line and position numbers in the bottom right corner of the terminal when editing the commit messages.
