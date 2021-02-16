package main

import (
	"encoding/json"
	"fmt"
	"os"

	aep "github.com/LilyStilson/aftereffects-aep-parser"
)

func main() {
	projectPath := os.Args[1]
	//projectPath := "E:\\YandexDisk\\Acer\\Footages (AE)\\AErender Launcher Benchmark Projects\\SuperEffectiveBros - Mograph Practice\\AEPRTC_Eclipse_rev57(ForDist)_2020.aep"
	//projectPath := "Mograph Icons.aep"
	//projectPath := "E:\\YandexDisk\\Acer\\Footages (AE)\\MindCool Intro (converted).aep"
	project, err := aep.Open(projectPath)
	// "E:\YandexDisk\Acer\Footages (AE)\MindCool Intro (converted).aep"
	/*fmt.Printf("%-48v| %-16v | %-6v | %-6v | %-10v\n%v\n", 
		"Composition", 
		"Frames",  
		"Length", 
		"FPS",
		"Dimension",
		"---------------------------------------------------------------------------------",
	)*/

	
	var comps []aep.Item

	for _, value := range project.Items {
		if value.ItemType == aep.ItemTypeComposition {
			comps = append(comps, *value)

			/*fmt.Printf("%-48v| [%6d, %6d] | %6d | %6.2f | [%5d, %5d]\n", 
				value.Name, 
				uint32(value.Frames[0]), 
				uint32(value.Frames[1]), 
				uint32(value.FootageSeconds), 
				value.FootageFramerate,
				value.FootageDimensions[0],
				value.FootageDimensions[1],
			)*/
		}
	}
	
	JSONComp, err := json.Marshal(comps)

	fmt.Println(string(JSONComp))

	//json_msg, err := json.Marshal(Compositions)
	
	if err != nil {
    	panic(err)
	}
}


