#include <iostream>
#include "rapidfuzz/fuzz.hpp"


using rapidfuzz::fuzz::ratio;


int main(int argc, char* argv[]){
    std::cout << "testing the rapidfuzz library:\n";

    std::string str1;
    std::string str2;
    
    std::getline(std::cin, str1);
    std::getline(std::cin, str2);

    double score = rapidfuzz::fuzz::ratio(str1, str2);

    std::cout << "\nthe score between: \n\"" << str1 << "\"\n\"" << str2 << "\"\nis: " << score << "\n";

    return 0;
}




