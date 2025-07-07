#include "../include/main.h"

int main(int argc, char* argv[]){
    std::cout << "testing the rapidfuzz library:\n";

    //get input string
    std::string str1;


    //read file
    std::cout << "opening file...\n";
    std::ifstream dictfile("data/dict.json");
    if(!dictfile){
        std::cout << "failed opening dictionary\n";
        return 1;
    }
    std::cout << "done\n";
    std::cout << "parsing json...\n";
    json data = json::parse(dictfile);
    std::cout << "done\n";
    

    //loop through dict to get ipas
    for (auto it = data.begin(); it != data.end(); ++it) {
        std::cout << it.key() << "\n";
    }

    while(true){
        std::getline(std::cin, str1);
        if (data.contains(str1)) {
            const auto& words = data[str1];  // this will be a json array

            for (const auto& word : words) {
                std::cout << word << "\n";
            }
        } else {
            std::cout << "IPA key not found\n";
        }
    }

/*

    double score = rapidfuzz::fuzz::ratio(str1, str2);

    std::cout << "\nthe score between: \n\"" << str1 << "\"\n\"" << str2 << "\"\nis: " << score << "\n";



*/




    return 0;
}



