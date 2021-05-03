package main

import (
	"encoding/json"
	"fmt"
	"os"

	aep "github.com/LilyStilson/aftereffects-aep-parser"
)

func main() {
	projectPath := os.Args[1]
	project, err := aep.Open(projectPath)
		
	var comps []aep.Item

	for _, value := range project.Items {
		if value.ItemType == aep.ItemTypeComposition {
			comps = append(comps, *value)
		}
	}
	
	JSONComp, err := json.Marshal(comps)

	fmt.Println(string(JSONComp))

	if err != nil {
    	panic(err)
	}
}


