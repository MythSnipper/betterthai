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

bool is_arch_linux() {
    std::ifstream os_release("/etc/os-release");
    std::string line;
    while (std::getline(os_release, line)) {
        if (line.find("ID=arch") != std::string::npos) {
            return true;
        }
    }
    return false;
}

std::string run_espeak_ng(std::string text){
    if (is_arch_linux()) {
        std::string command = "espeak-ng --ipa=3 -qx \"" + text + "\"";
        int result = system(command.c_str());
        if (result != 0) {
            std::cerr << "espeak-ng failed\n";
        }
        
    } else {
        std::cout << "Not running on Arch Linux. Imagine not using arch linux lol :skull: \nFailure! Install arch today for only FREE!\nI USE ARCH BTW\n";
        return "";
    }
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
    str = run_espeak_ng(str);

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
        str = run_espeak_ng(str);
    }

    return 0;
}
