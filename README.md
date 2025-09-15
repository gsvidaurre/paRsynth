`paRsynth` uses Parsons code to convert character strings to frequency modulated vocalizations with different amounts of individual identity and group membership information. The functions in this package were developed for the sonification of character strings that represent vocal signals with varying amounts of social information. The resulting audio files (created using the soundgen package) can be used in downstream bioacoustics analyses.

The original code for these functions was first developed by Arlena (Ari) Cross and Dr. Grace Smith-Vidaurre in collaboration with Dr. Vanessa Ferdinand. This package was primarily designed for in-house use by the Smith-Vidaurre and Ferdinand labs, but will be made publicly available for others to use and modify as they see fit. Current code colaborators in the Smith-Vidaurre lab include Raneem Samman and Alexandra Juárez. Grace Smith-Vidaurre is the code owner.

**Code collaboration guidelines:**

Each collaborator who helps out with code development and documentation for this package will need a local version of this repository on their computer and some way of interfacing with GitHub (e.g. Git on the command line, GitHub Desktop). Currently, the collaborators working on code development are Raneem Samman, Alexandra Juárez, and Grace Smith-Vidaurre. Others who are interested in contributing directly to the package should first contact Grace Smith-Vidaurre (smithvid[at]msu.edu).

We will each stick to the following guidelines for collaborative code development:

To *communicate that we need to make updates to the package*, you should use the following workflow:

  1. Create a new issue using the naming convention "[Unit tests]", "[Functions]", or "[Documentation]" before the unique issue title. These prefixes are helpful for indicating what the issue will be about.

  2. Label the issue by category. For instance, use the label "bug" to fix a bug, or "enhancement" to add a new feature.

  3. Assign the issue to yourself or one of the current code collaborators.


To *start working on an issue*:

  1. Make sure the main branch of your local repository of paRsynth is up to date.

  2. Create a new branch on your local repository from the current version of the main branch. This branch should have the same name as the issue that you're working on, but with the prefix "PAR-". For instance, if you just created issue 50, then the branch you will create should be called "PAR-50".

  3. Move into the new branch for the current issue before making any changes to files or directory structures.

  4. Once you're working in the new branch, you can start modifying files to address the current issue.

  5. You should commit and push changes regularly to GitHub (multiple times within each coding session), and make sure that these changes are published to the remote version of the same branch that you're working on. In your commit messages, you should start each message with the name of the branch and issue. For example, if you fixed a typo related to code development for issue 50, the summary for your commit message should be "PAR-50: Fixed a missing comma".


To *communicate that you've addressed an issue*:

  1. First, you should check that the changes you've made are looking good and won't break anything. If you modified any code associated with functions or unit tests, then you should run all unit tests for the package and make sure all unit tests pass before continuing to the next step. If you modified any documentation associated with the package, then make sure that the documentation reads clearly and does not have typos.

  2. Once you've confirmed that the changes are looking good, then you can commit your most recent changes and push them to GitHub.

  3. Next, you should create a Pull Request to merge your changes with the main branch. You should name the Pull Request the same as the branch that you are requesting to merge. For instance, if you want to merge branch PAR-50, then you would add a title for the Pull Request like "PAR-50: Fixing a typo in code".

  3. The Pull Request notification will be sent to the paRsynth code owner (Smith-Vidaurre), who will review whether the changes from your branch can be integrated into the main branch without conflicts and whether the changes you've made look good. You may need to fix code or documentation if there are conflicts or problems at this Pull Request reviewing stage. If so, then you'll need to run Steps 1 and 2 in this section again before creating another Pull Request

  4. Once a Pull Request is accepted and merged, then you can mark the associated issue as "Closed". You do not need to delete the branch for the issue that was closed. The code owner will periodically delete branches over time.

**Package usage and maintenance**

This is a new package that is still under development for our own research purposes. As such, we are currently not working to make the functions generally applicable for many different purposes or users. Anyone interested in using these functions should be careful to implement their own tests and validation. Please cite the package if you find it useful for your own work.

We follow a semantic versioning system to track package versions:

MAJOR (1.0.0 → 2.0.0): Backward-incompatible changes, major new features, or stable milestone releases (e.g., Version 1 = first full release, Version 2 = next major release).
MINOR (1.1.0 → 1.2.0): Backward-compatible feature additions (e.g., new functions, analysis pipelines).
PATCH (1.1.1 → 1.1.2): Bug fixes, typos, or small tweaks that do not affect compatibility.