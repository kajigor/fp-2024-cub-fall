module MainPage (mainPage) where

import qualified Data.ByteString.Lazy.Char8 as B

mainPage :: B.ByteString
mainPage = B.pack 
    "<!DOCTYPE html>\
    \<html lang=\"en\">\
    \<head>\
    \    <meta charset=\"UTF-8\">\
    \    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\
    \    <title>Regex Debugger</title>\
    \    <style>\
    \        .container { max-width: 800px; margin: 0 auto; padding: 20px; }\
    \        .input-group { margin-bottom: 15px; }\
    \        .debug-container { margin-top: 20px; }\
    \        .debug-nav { margin: 10px 0; }\
    \        .debug-step { font-family: monospace; white-space: pre; }\
    \        .step-counter { margin: 0 10px; }\
    \        button { margin: 0 5px; padding: 5px 10px; }\
    \        .match-result { font-weight: bold; margin-bottom: 10px; }\
    \        .match-success { color: green; }\
    \        .match-failure { color: red; }\
    \    </style>\
    \</head>\
    \<body>\
    \    <div class=\"container\">\
    \        <h1>Regex Debugger</h1>\
    \        <div class=\"input-group\">\
    \            <label for=\"regexField\">Regex:</label><br>\
    \            <input id=\"regexField\" type=\"text\" placeholder=\"Enter regex\" style=\"width: 100%;\">\
    \        </div>\
    \        <div class=\"input-group\">\
    \            <label for=\"inputField\">Input:</label><br>\
    \            <input id=\"inputField\" type=\"text\" placeholder=\"Enter input text\" style=\"width: 100%;\">\
    \        </div>\
    \        <div id=\"result\" class=\"debug-container\">\
    \            <div id=\"matchResult\" class=\"match-result\"></div>\
    \            <div class=\"debug-nav\">\
    \                <button id=\"prevStep\" disabled>Previous</button>\
    \                <span id=\"stepCounter\" class=\"step-counter\">Step 0 / 0</span>\
    \                <button id=\"nextStep\" disabled>Next</button>\
    \            </div>\
    \            <div id=\"debugStep\" class=\"debug-step\"></div>\
    \        </div>\
    \    </div>\
    \    <script>\
    \        let currentStep = 0;\
    \        let debugSteps = [];\
    \        \
    \        const socket = new WebSocket('ws://' + window.location.host + '/ws');\
    \        const regexField = document.getElementById('regexField');\
    \        const inputField = document.getElementById('inputField');\
    \        const matchResult = document.getElementById('matchResult');\
    \        const debugStep = document.getElementById('debugStep');\
    \        const prevButton = document.getElementById('prevStep');\
    \        const nextButton = document.getElementById('nextStep');\
    \        const stepCounter = document.getElementById('stepCounter');\
    \        \
    \        function updateDebugView() {\
    \            if (debugSteps.length === 0) {\
    \                debugStep.textContent = '';\
    \                stepCounter.textContent = 'Step 0 / 0';\
    \                prevButton.disabled = true;\
    \                nextButton.disabled = true;\
    \                return;\
    \            }\
    \            \
    \            debugStep.textContent = debugSteps[currentStep];\
    \            stepCounter.textContent = `Step ${currentStep + 1} / ${debugSteps.length}`;\
    \            prevButton.disabled = currentStep === 0;\
    \            nextButton.disabled = currentStep === debugSteps.length - 1;\
    \        }\
    \        \
    \        function sendUpdate() {\
    \            const message = {\
    \                regex: regexField.value,\
    \                input: inputField.value\
    \            };\
    \            socket.send(JSON.stringify(message));\
    \        }\
    \        \
    \        prevButton.addEventListener('click', () => {\
    \            if (currentStep > 0) {\
    \                currentStep--;\
    \                updateDebugView();\
    \            }\
    \        });\
    \        \
    \        nextButton.addEventListener('click', () => {\
    \            if (currentStep < debugSteps.length - 1) {\
    \                currentStep++;\
    \                updateDebugView();\
    \            }\
    \        });\
    \        \
    \        regexField.addEventListener('input', sendUpdate);\
    \        inputField.addEventListener('input', sendUpdate);\
    \        \
    \        socket.onmessage = (event) => {\
    \            try {\
    \                const response = JSON.parse(event.data);\
    \                debugSteps = response.debugSteps;\
    \                currentStep = 0;\
    \                \
    \                matchResult.textContent = response.isMatch ? 'Match ✓' : 'No Match ✗';\
    \                matchResult.className = 'match-result ' + \
    \                    (response.isMatch ? 'match-success' : 'match-failure');\
    \                \
    \                updateDebugView();\
    \            } catch (e) {\
    \                matchResult.innerHTML = event.data;\
    \                debugSteps = [];\
    \                updateDebugView();\
    \            }\
    \        };\
    \    </script>\
    \</body>\
    \</html>"
