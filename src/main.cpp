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

int main(int argc, char* argv[]) {
    std::cout << "testing betterthai:\n";

    // Read file
    std::ifstream dictfile("data/dict.json");
    if (!dictfile) {
        std::cout << "failed opening dictionary\n";
        return 1;
    }

    std::cout << "parsing json...\n";
    json data = json::parse(dictfile);
    std::cout << "done\n";

    // Extract all keys into a list
    std::vector<std::string> ipa_keys;
    for (auto it = data.begin(); it != data.end(); ++it) {
        ipa_keys.push_back(it.key());
    }

    std::string str;
    std::cout << "string: ";
    std::getline(std::cin, str);

    while (str != "exit") {
        // Run fuzzy match
        auto matches = extract<std::string, std::vector<std::string>>(str, ipa_keys, 0.0);

        // Sort by descending score
        std::sort(matches.begin(), matches.end(),
                  [](const auto& a, const auto& b) { return a.second > b.second; });

        // Trim to top 20
        if (matches.size() > 20) matches.resize(20);

        std::cout << "Top matches:\n";
        for (const auto& [match, score] : matches) {
            std::cout << match << " (" << score << "):\n";
            const auto& ipas = data[match];
            for (const auto& ipa : ipas) {
                std::cout << "  → " << ipa << "\n";
            }
        }

        std::cout << "\nstring: ";
        std::getline(std::cin, str);
    }

    return 0;
}
