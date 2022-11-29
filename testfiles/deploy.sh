# Finds a config.gal in ./templates and runs through the rules. 
# Uses the data files to populate mustache templated values with the last argument taking precedence.
# More or less like `kt deploy -e base -e callisto-dev`
app magic ./templates --data env/base --data env/callisto-dev