"""Displays build errors to students"""

import json
import sys

def main(output, scores, problem, tasks, taskScores):
    """Display scores."""
    with open(output, 'r') as file:
        outputString = file.read()

    outputString = "Compilation failed with the following output:\n\n" + outputString

    with open(scores) as file:
        scoresJSON = json.load(file)

    tasks = tasks.split(' ')
    taskScores = list(map(int, taskScores.split(' ')))

    for i in range(len(tasks)):
        currJSON = scoresJSON["tests"]
        scoresJSON["tests"] = currJSON + [{"name": f'{problem}: {tasks[i]}', "score": 0.0, "max_score": taskScores[i], "output": outputString}]

    with open(scores, "w") as scoreFile:
        json.dump(
            scoresJSON,
            scoreFile,
        )


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5])
