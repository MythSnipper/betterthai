#include "../include/main.h"

int main(int argc, char* argv[]){
    std::cout << "testing the rapidfuzz library:\n";


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


    std::string str;
    std::vector<std::string> best_ipas;
    std::vector<double> best_values;

    

    while(true){
        //get input string
        std::getline(std::cin, str);
        if(data.contains(str)){
            const auto& words = data[str];
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



