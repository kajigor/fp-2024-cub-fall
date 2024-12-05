I had to write a spellcheck program. I decided to make a CLI application.

The app has 3 commands:
- `spellchecker check <filename>`
- `spellchecker dict add <word>`
- `spellchecker dict remove <word>`

It requires a dictionary which can be downloaded using `get_dict.sh` script. The default dictionary filename is `dictionary.json`,  it can be changed with the `-d/--dictionary` option.

Main libraries used are the following:
- `aeson` for parsing dictionary data
- `unordered-containers` for storing the dictionary in memory. I decided to this package as it provides HashSet/HashMap and thus is more performant than the `containers` package
- `optparse-applicative` is used for handling program arguments

My program is based on the algorithm described at https://norvig.com/spell-correct.html. I decided to use it because it can handle most misspellings and is simple to implement. I also used prebuilt dictionary from the `pyspellchecker` python package.

Given that the HashMap lookups are O(1), this algorithm has the following complexity:
- O(1) if the given word is in the dictionary
- O(n) if the given word has Levenshtein distance 1 to any word in the dictionary
- O(n^2) otherwise

My program is structured in 3 directories:
- `app` containes all the code for interacting with the end user: argument parsing, command handling and output formatting. Each of these 3 functions is in its own file.
- `src` contains all the library code that can be used with any user interface: Dictionary type, the spellcheck function and helpers, and some utility functions
- `test` contains the tests for the library code
