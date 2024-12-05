# static-server
```
(Project for Stanislav Sandler) Regex Parser With Web Interface
(10 points) Web interface.
(30 points) Provide real-time feedback as users type their regex patterns and test strings.
(10 points) Allow users to save and load their regex patterns and test strings.
(20 points) Debuggins session: display regex matching step-by-step
(15 points) Error reporting for invalid inputs.
(15 points) Unit and property-based tests.
```
Running the server:
```
    stack build
    stack run
```
After that, go to `http://localhost:8080`
The idea was to create a debugger for regex. This required my own Regex engine implementation, so it was simplified. \

