This package uses Parsons code to convert character strings to frequency modulated vocalizations with different amounts of individual identity and group membership information. These functions in this package were developed for the sonification of character strings that represent vocal signals with varying amounts of social information in agent-based models. The resulting audio files (created using the soundgen package) can be used in bioacoustics analyses downstream of the agent-based models.

The original code for these functions was developed by Ari Cross in collaboration with Dr. Vanessa Ferdinand and Dr. Grace Smith-Vidaurre. This package was primarily designed for in-house use by the Smith-Vidaurre and Ferdinand labs, but will be made publicly available for others to use and modify as they see fit.

**Code collaboration guidelines:**

Each collaborator who helps out with code development and documentation for this package will need a local version of this repository on their computer and some way of interfacing with GitHub (e.g. Git on the command line, GitHub Desktop). We will each stick to the following guidelines for collaborative code development:

To communicate that we need to make updates to the package, you should use the following workflow:

  1. Create a new issue using the naming convention "PAR-" before the unique issue number

  2. Label the issue by category (fix a bug, add a feature, update documentation, etc)

  3. Assign the issue to yourself or one of the collaborators (Cross, Smith-Vidaurre, Ferdinand)


To start working on an issue:

  1. Make sure your local repository is up to date

  2. Create a new branch on your local repository from the main branch. This branch should have the same name as the issue that you're working on, for instance "PAR-50" for issue 50

  3. Move into the new branch for the current issue before making changes to files

  4. Once you're working in the new branch, you can start modifying files to address the current issue

  5. You should commit and push changes regularly to GitHub, but make sure that these changes are published to the remote version of the same branch that you're working on. In your commit messages, you should start each message with the name of the branch and issue. For example, if you fixed a typo related to code development for issue PAR-50, a commit message could read as "PAR-50: Fixed a missing comma"


To communicate that you've addressed an issue:

  1. First, you should check that the changes you've made are looking good and won't break anything. If you modified files with code, make sure that you run all unit testing for the package. If you modified documentation, make sure that it reads clearly and does not have typos

  2. Once you've confirmed that the changes are looking good, then you can commit your most recent changes and push them to GitHub, and create a Pull Request on GitHub to merge your changes with the main branch

  3. The Pull Request notification will be sent to the code owner (Smith-Vidaurre), who will review whether the changes from your branch can be integrated into the main branch without conflicts. You may need to fix code or documentation if there are conflicts or problems at this Pull Request reviewing stage, and then you'll need to run Steps 1 and 2 in this section again before creating another Pull Request

  4. Once a Pull Request is accepted and merged, then you can mark the associated issue as "Closed". You do not need to delete the branch for the issue that was closed
