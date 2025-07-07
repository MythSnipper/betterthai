#include "../include/main.h"

template <typename Sentence1,
        typename Iterable, typename Sentence2 = typename Iterable::value_type>
std::vector<std::pair<Sentence2, double>>
extract(const Sentence1& query, const Iterable& choices, const double score_cutoff = 0.0)
{
    std::vector<std::pair<Sentence2, double>> results;
    rapidfuzz::fuzz::CachedRatio<typename Sentence1::value_type> scorer(query);

    for (const auto& choice : choices) {
        double score = scorer.similarity(choice, score_cutoff);
        if (score >= score_cutoff) {
            results.emplace_back(choice, score);
        }
    }

    return results;
}

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

    //Declare stuff so work:
    std::string str;
    std::vector<std::string> ipa_keys;
    

    //loop through dict to get ipas
    for (auto it = data.begin(); it != data.end(); ++it) {
        ipa_keys.push_back(it.key());
    }

    while(true){
        std::cout << "\nType a word (or 'exit'):\n> ";
        //get input string
        std::getline(std::cin, str);
        
        if (str == "exit") break;

         // Run fuzzy matching against keys
        std::vector<std::pair<std::string, double>> matches =
            extract<std::string, std::vector<std::string>>(str, ipa_keys, 50.0);

        if (!matches.empty()) {
            std::cout << "Best matches:\n";
            for (const auto& [match, score] : matches) {
                std::cout << "- " << match << " (score: " << score << ")\n";
                const auto& words = data[match];
                for (const auto& word : words) {
                    std::cout << "  → " << word << "\n";
            }
        }
    } else {
        std::cout << "No good fuzzy matches found.\n";
    }

    return 0;
}
}



