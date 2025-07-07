#include "../include/main.h"


int main(int argc, char* argv[]){
    std::cout << "testing betterthai:\n";

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


    std::vector<double> best_values;
    std::vector<std::string> best_ipas;


    std::string str;
    std::cout << "string: ";
    std::getline(std::cin, str);

    while(str != "exit"){
        best_values.clear();
        best_ipas.clear();

        //loop through dict
        for(auto it = data.begin(); it != data.end(); it++){
            std::string dictStr = it.key();
            if(str[0] != dictStr[0]){
                continue;
            }
            double score = rapidfuzz::fuzz::ratio(str, dictStr);
            jesus(dictStr, score, best_ipas, best_values);
        }
        std::cout << "done\n";
        std::cout << "best IPAs:\n";
        for(uint8_t i=0;i<best_ipas.size();i++){
            std::cout << best_ipas[i] << ": " << best_values[i] << "\n";
        }


        std::cout << "string: ";
        std::getline(std::cin, str);
    }


    return 0;
}

void jesus(const std::string& dictStr, double score,
           std::vector<std::string>& best_ipas,
           std::vector<double>& best_values)
{   
    // If less than 20 elements, just insert in sorted position
    if (best_values.size() < 20) {
        // Find insertion point (descending order)
        auto it = std::lower_bound(best_values.rbegin(), best_values.rend(), score, std::greater<double>());
        int index = best_values.size() - (it - best_values.rbegin());
        best_values.insert(best_values.begin() + index, score);
        best_ipas.insert(best_ipas.begin() + index, dictStr);
    }
    else if (score > best_values.back()) {
        // Replace lowest score entry
        // Remove last elements
        best_values.pop_back();
        best_ipas.pop_back();

        // Insert new entry in correct place
        auto it = std::lower_bound(best_values.rbegin(), best_values.rend(), score, std::greater<double>());
        int index = best_values.size() - (it - best_values.rbegin());
        best_values.insert(best_values.begin() + index, score);
        best_ipas.insert(best_ipas.begin() + index, dictStr);
    }
}